#' get_cluster_summary
#'
#' A function to obtain a summary of data by coordinated cluster.
#'
#' @param output A list output resulting from the function get_coord_shares.
#' @param labels Automatically generate a cluster label using account titles and descriptions. This relies on OpenAI APIs and expects the API Bearer in the OPENAI_API_KEY environment variable.
#'
#' @return A data frame that summarizes data for each coordinated cluster, including:
#' - The average number of subscribers of entities within a cluster.
#' - The proportion of coordinated shares over the total shares (coorshare_ratio).
#' - The average coordinated score (avg_cooRscore), which measures the dispersion (gini) in the distribution of domains that are coordinatedly shared by the cluster (0-1). Higher values correspond to higher concentration (fewer different domains linked).
#' - The top coordinatedly shared domains (ranked by the number of shares) and the total number of coordinatedly shared domains.
#' If the NewsGuard API is provided, this function also returns an estimate of the trustworthiness of the domains used by the cluster. If the label parameter is set to TRUE and an OpenAI token is provided, the function also returns an automatically generated label for each cluster.
#'
#' @details The Gini values are computed using the Gini coefficient on the proportions of unique domains each cluster shared. The Gini coefficient is a measure of the degree of concentration (inequality) of a variable in a distribution.
#' It ranges between 0 and 1: the more equal the distribution, the lower its Gini index. When a cluster shared just one domain, the value of the variable is set to 1. It is calculated separately for full_domains (e.g. www.foxnews.com, video.foxnews.com) and parent domains (foxnews.com).
#'
#' The avg_cooRscore is a measure of cluster coordination. Higher values imply higher coordination.
#' Its value is calculated by dividing, for each entity in a coordinated network, its \code{\link[igraph]{strength}} by its \code{\link[igraph]{degree}}, and then calculating the average by cluster of these values.
#'
#' The coorshare_ratio.avg is an additional measure of cluster coordination ranging from 0 (no shares coordinated) to 1 (all shares coordinated).
#'
#' @examples
#' # Build the cluster summary
#' cluster_summary <- get_cluster_summary(output, label=TRUE)
#'
#' # Cluster the clusters rowwise mutate
#' clusters <- hclust(dist(cluster_summary[, 2:4]))
#' plot(clusters)
#'
#' @importFrom dplyr %>% group_by summarise mutate top_n arrange rowwise n slice_head select
#' @importFrom urltools suffix_extract domain
#' @importFrom DescTools Gini
#' @importFrom openai create_chat_completion
#' @import reticulate
#'
#' @export

get_cluster_summary <- function(output, labels=FALSE){

  ct_shares_marked.df <- output[[1]]
  highly_connected_coordinated_entities <- output[[3]]
  rm(output)

  ct_shares_marked.df <- subset(ct_shares_marked.df, ct_shares_marked.df$is_coordinated==TRUE) # subset to save resource by working on coordinated shares only

  # average gini-domains and hosts
  ct_shares_marked.df$full_domain <- urltools::domain(ct_shares_marked.df$expanded) # eg. www.foxnews.com, video.foxnews.com, nation.foxnews.com
  ct_shares_marked.df$parent_domain <- paste(urltools::suffix_extract(ct_shares_marked.df$full_domain)$domain, urltools::suffix_extract(ct_shares_marked.df$full_domain)$suffix, sep = ".")

  # add the cluster id to the ct_shares_marked.df
  ct_shares_marked.df <- merge(x=ct_shares_marked.df,
                               y=highly_connected_coordinated_entities[,c("name", "cluster", "component")],
                               by.x = "account.url",
                               by.y = "name")

  # summarise
  summary_entities <- highly_connected_coordinated_entities %>%
    dplyr::group_by(component, cluster) %>%
    dplyr::summarise(entities = n(),
                     avg.subscriberCount = mean(account.avg.subscriberCount),
                     cooRshare_ratio.avg = mean(coord.shares/(shares+coord.shares)),
                     cooRscore.avg = mean(strength/degree),
                     pageAdminTopCountry = unique(account.pageAdminTopCountry, na.rm = TRUE)[which.max(tabulate(match(account.pageAdminTopCountry, unique(account.pageAdminTopCountry))))],
                     facebook_page = length(account.accountType[account.accountType=="facebook_page"]),
                     facebook_group = length(account.accountType[account.accountType=="facebook_group"]),
                     facebook_profile = length(account.accountType[account.accountType=="facebook_profile"]))

  if (labels & Sys.getenv("OPENAI_API_KEY")!="") {

    # Create a list to store the indices of each link in the cluster
    cluster_indices <- lapply(1:nrow(summary_entities), function(x) list())

    temp_df <- data.frame(cluster_id = integer(), label = character())

    cat("\nAuto-labeling clusters with OpenAI gpt-3.5-turbo (https://platform.openai.com/)...\n")

    pb <- utils::txtProgressBar(min = 0, max = nrow(summary_entities), width = 100, style = 3)

    for (j in 1:nrow(summary_entities)) {

      cluster_accounts <- subset(highly_connected_coordinated_entities, highly_connected_coordinated_entities$cluster==j)
      cluster_accounts <- arrange(cluster_accounts, strength)
      cluster_accounts$comtxt <- paste(cluster_accounts$account.name, ifelse(cluster_accounts$account.pageDescription!="NA", paste0(" - ", cluster_accounts$account.pageDescription), ""))
      cluster_text <- select(cluster_accounts, c("comtxt"))

      n <- nrow(cluster_accounts)

      # Store the indices of the cluster links
      cluster_indices[[j]] <- 1:nrow(cluster_text)

      while (TRUE) {
        # maximize account samples

        cluster_sample_indices <- cluster_indices[[j]][1:n]
        cluster_sample <- cluster_text[cluster_sample_indices, , drop = FALSE]
        cluster_accounts <- dplyr::slice_head(.data = cluster_accounts, n = n)
        text <- paste(trimws(cluster_accounts$comtxt), collapse = "\n")

        # Calculate the total tokens in the message, including the completion tokens
        total_tokens <- num_tokens_from_string(text, "cl100k_base", "gpt-3.5-turbo") + 256

        if (total_tokens <= 3900) {
          # Update the cluster_indices list to remove the processed indices
          cluster_indices[[j]] <- setdiff(cluster_indices[[j]], cluster_sample_indices)
          break
        }

        if (total_tokens > 3900*2) {
          n <- n - 10
        } else { n <- n -1}
      }

      msg <- list(list("role" = "system",
                       "content" = "You are a researcher investigating coordinated and inauthentic behavior on Facebook and Instagram. Your objective is to generate concise, descriptive labels in English that capture the shared characteristics of clusters of Facebook or Instagram accounts.\n\n"),
                  list("role" = "user",
                       "content" = paste("I will supply a list of accounts for each cluster. For each account, you will receive a text that combines the account title and, if available, the account description. Identify the shared features among these accounts:\n\n",
                                         text,
                                         "\n\n",
                                         "English label:")))

      res <- tryCatch(
        {
          openai::create_chat_completion(model = "gpt-3.5-turbo",
                                         messages = msg,
                                         temperature = 0,
                                         top_p = 1,
                                         max_tokens = 256)
        },
        error = function(cond) {
          return(NULL)
        })

      if (!is.null(res)) {
        temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = res$choices$message.content))
      } else {
        # try one more time
        res <- tryCatch(
          {
            openai::create_chat_completion(model = "gpt-3.5-turbo",
                                           messages = msg,
                                           temperature = 0,
                                           top_p = 1,
                                           max_tokens = 256)
          },
          error = function(cond) {
            return(NA)
          })

        if (!is.null(res)) {
          temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = res$choices$message.content))
        } else {
          temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = "API failed!"))
        }
      }
      utils::setTxtProgressBar(pb, pb$getVal() + 1)
      text <- NULL
      Sys.sleep(0.5)
    }

    summary_entities <- merge(summary_entities, temp_df, by.x = "cluster", by.y = "cluster_id")
  }

  # query the the newsguardtech.com API
  if (!(Sys.getenv('NG_KEY')=="" & Sys.getenv('NG_SECRET')=="")){

    domain_score <- get_newsguard_domain_scores(data.frame(parent_domain = unique(ct_shares_marked.df$parent_domain), newsguard_score = NA))

    cat("\nAlmost done...\n")

    # add the newsguardtech.com score to the domains
    ct_shares_marked.df <- merge(x=ct_shares_marked.df,
                                 y=domain_score,
                                 by="parent_domain")

    summary_domains <- ct_shares_marked.df %>%
      dplyr::group_by(cluster) %>%
      summarise(unique.full_domain = length(unique(full_domain)),
                unique.parent_domain = length(unique(parent_domain)),
                newsguard.parent_domain.score = mean(newsguard_score, na.rm = T),
                newsguard.parent_domain.rated = length(unique(parent_domain[!is.na(newsguard_score)]))) %>%
      mutate(newsguard.parent_domain.prop = newsguard.parent_domain.rated/unique.parent_domain)
  }
  else{
    summary_domains <- ct_shares_marked.df %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(unique.full_domain = length(unique(full_domain)),
                       unique.parent_domain = length(unique(parent_domain)))
  }

  summary <- merge(summary_entities, summary_domains, by = "cluster")
  rm(summary_entities, summary_domains)

  summary <- summary %>%
    dplyr::rowwise() %>%
    dplyr::mutate(gini.full_domain = DescTools::Gini(prop.table(table(ct_shares_marked.df[ct_shares_marked.df$cluster==cluster, "full_domain"]))),
                  gini.parent_domain = DescTools::Gini(prop.table(table(ct_shares_marked.df[ct_shares_marked.df$cluster==cluster, "parent_domain"])))) %>%
    dplyr::mutate(gini.full_domain = ifelse(is.nan(gini.full_domain), 1, gini.full_domain),
                  gini.parent_domain = ifelse(is.nan(gini.parent_domain), 1, gini.parent_domain)) %>%
    dplyr::mutate(top.full_domain = paste(top_n(arrange(data.frame(table(ct_shares_marked.df[ct_shares_marked.df$cluster==cluster, "full_domain"])), desc(Freq)),
                                                n=5, wt="Freq")$Var1, collapse = ", "),
                  top.parent_domain = paste(top_n(arrange(data.frame(table(ct_shares_marked.df[ct_shares_marked.df$cluster==cluster, "parent_domain"])), desc(Freq)),
                                                  n=5, wt="Freq")$Var1, collapse = ", "))

  cat("Cluster summary done!\n")

  return(summary)
}
