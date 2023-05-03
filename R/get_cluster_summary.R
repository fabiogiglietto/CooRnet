#' get_cluster_summary
#'
#' A function to get summary data by coordinated cluster
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param labels auto-generate a cluster label using account's title and descriptions. Relies on Openai APIs. Expects the API Bearer in OPENAI_API_KEY environment variable.
#'
#'
#' @return A data frame that summarizes data for each coordinated cluster. The data includes:
#' - The average number of subscribers of entities in a cluster.
#' - The proportion of coordinated shares over the total shares (coorshare_ratio).
#' - The average coordinated score (avg_cooRscore), which measures the dispersion (gini) in the distribution of domains that are coordinatedly shared by the cluster (0-1). Higher values correspond to higher concentration (fewer different domains linked).
#' - The top coordinatedly shared domains (ranked by the number of shares) and the total number of coordinatedly shared domains.
#' If the NewsGuard API is provided, this function also returns an estimate of the trustworthiness of the domains used by the cluster. If the label parameter is set to TRUE and an OpenAI token is provided, the function also returns an automatically generated label for each cluster.
#'
#'
#' @details The gini values are computed by using the Gini coefficient on the proportions of unique domains each cluster shared. The Gini coefficient is a measure of the degree of concentration (inequality) of a variable in a distribution.
#' It ranges between 0 and 1: the more nearly equal the distribution, the lower its Gini index. When a cluster shared just one domain, the value of the variable is set to 1. It is calculated separately for full_domains (e.g. www.foxnews.com, video.foxnews.com) and parent domains (foxnews.com)
#'
#' The cooRscore.avg is a measures of cluster coordination. Higher values implies higher coordination.
#' Its value is calculated by dividing, for each entity in a coordinated network, its \code{\link[igraph]{strength}} by its \code{\link[igraph]{degree}}, and then calculating the average by cluster of these values.
#'
#' The cooRshare_ratio.avg is an addional measure of cluster coordination ranging from 0 (no shares coordinated) to 1 (all shares coordinated).
#'
#' @examples
#'   # get the top ten posts containing URLs shared by each network cluster and by engagement
#'   cluster_summary <- get_cluster_summary(output, label=TRUE)
#'
#'   # clustering the clusters rowwise mutate
#'   clusters <- hclust(dist(cluster_summary[, 2:4]))
#'   plot(clusters)
#'
#' @importFrom dplyr %>% group_by summarise mutate top_n arrange rowwise n
#' @importFrom urltools suffix_extract domain
#' @importFrom DescTools Gini
#' @importFrom openai create_chat_completion
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

    summary_entities$label <- NA

    cat("\nAuto-labelling clusters with OpenAI gpt-3.5-turbo (https://platform.openai.com/)...\n")

    pb <- utils::txtProgressBar(min = 0, max = nrow(summary_entities), width = 100, style = 3)

    for (j in 1:nrow(summary_entities)) {

      cluster_accounts <- subset(highly_connected_coordinated_entities, highly_connected_coordinated_entities$cluster==j)
      cluster_accounts <- arrange(cluster_accounts, strength)

      n <- ifelse(nrow(cluster_accounts)/100*20>5, round(nrow(cluster_accounts)/100*20,0), 5)

      cluster_accounts <- dplyr::slice_head(.data = cluster_accounts, n = n)
      cluster_accounts$comtxt <- paste(cluster_accounts$account.name, ifelse(cluster_accounts$account.pageDescription!="NA", cluster_accounts$account.pageDescription, ""))

      text <- paste(trimws(cluster_accounts$comtxt), collapse = "\n")

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
        error=function(cond) {
          return(NULL)
        })

      if (!is.null(res)) {

        summary_entities$label[j] <- res$choices$message.content

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
          error=function(cond) {
            return(NA)
          })

        if (!is.null(res)) {
          summary_entities$label[j] <- res$choices$message.content
        } else {
          summary_entities$label[j] <- "API failed!"
        }
      }
      utils::setTxtProgressBar(pb, pb$getVal()+1)
      Sys.sleep(0.5)
    }
  }

  # query the the newsguardtech.com API
  if (!(Sys.getenv('NG_KEY')=="" & Sys.getenv('NG_SECRET')=="")){

    cat("\nGetting domains rating from NewsGuard (https://www.newsguardtech.com)...\n")

    domain_score <- data.frame(parent_domain = unique(ct_shares_marked.df$parent_domain), newsguard_score = NA)
    total <- nrow(domain_score) # progress bar

    pb <- utils::txtProgressBar(min = 0, max = total, width = 100, style = 3)
    ng_bearer <- get_ng_bearer()

    for (i in 1:nrow(domain_score)) {

      utils::setTxtProgressBar(pb, pb$getVal()+1)
      parent_domain <- domain_score$parent_domain[i]
      query <- httr::GET(paste0("https://api.newsguardtech.com/v3/check/", parent_domain),
                         httr::add_headers(Authorization = paste0("Bearer ",ng_bearer)))
      tryCatch(
        {
          if (query$status_code == 200) {
            json <- httr::content(query, as = "text", encoding = "UTF-8")
            c <- jsonlite::fromJSON(json, flatten = TRUE)
            if (length(c$score) > 0) {
              domain_score$newsguard_score[i] <- c$score
            }
          }
          else {
            print(paste(query$status_code, i))
            write(paste("Unexpected http response code by the News Guard API", query$status_code, "on domain", parent_domain), file = "log.txt", append = TRUE)
          }
        },
        error=function(cond) {
          print(paste("Error:", message(cond), "on URL:", i))
          write(paste("Error:", message(cond), "on URL:", i), file = "log.txt", append = TRUE)
        },
        finally={
          Sys.sleep(0.01)
        })
    }

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
