#' get_component_summary
#'
#' A function to get summary data by coordinated component
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param mongo_url string: address of the MongoDB server in standard URI Format. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_database string: name of the MongoDB database where the shares have been saved. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_cluster logical: set to TRUE if you are using a MongoDB cluster instead of standalone instance (default FALSE)
#' @param newsguard_info logical: set to TRUE to get NewsGuard rating for domains shared by the component. NewsGuard credentials required (default FALSE)
#'
#' @return A data frame containing summary data by each coordinated component:
#' the average subscribers number of entities in a component,
#' the proportion of coordinated shares over the total shares (coorshare_ratio), the average coordinated score (avg_cooRscore),
#' a measure of dispersion (gini) in the distribution of domains coordinatedly shared by the component (0-1). Higher values correspond to an higher concentration (less different domains linked),
#' the top 5 coordinatedly shared domains (ranked by n. of shares),
#' the total number coordinatedly shared of domains
#'
#'
#' @details The gini values are computed by using the Gini coefficient on the proportions of unique domains each component shared. The Gini coefficient is a measure of the degree of concentration (inequality) of a variable in a distribution.
#' It ranges between 0 and 1: the more nearly equal the distribution, the lower its Gini index. When a component shared just one domain, the value of the variable is set to 1. It is calculated separately for full_domains (e.g. www.foxnews.com, video.foxnews.com) and parent domains (foxnews.com)
#'
#' The cooRscore.avg is a measures of component coordination. Higher values implies higher coordination.
#' Its value is calculated by dividing, for each entity in a coordinated network, its \code{\link[igraph]{strength}} by its \code{\link[igraph]{degree}}, and then calculating the average by component of these values.
#'
#' The cooRshare_ratio.avg is an addional measure of component coordination ranging from 0 (no shares coordinated) to 1 (all shares coordinated).
#'
#' @examples
#'   # get the top ten posts containing URLs shared by each network component and by engagement
#'   component_summary <- get_component_summary(output)
#'
#'   # clustering the components rowwise mutate
#'   clusters <- hclust(dist(component_summary[, 2:4]))
#'   plot(clusters)
#'
#' @importFrom dplyr %>% group_by summarise mutate top_n arrange rowwise
#' @importFrom urltools suffix_extract domain
#' @importFrom DescTools Gini
#'
#' @export

get_component_summary <- function(output,
                                  mongo_url = NULL,
                                  mongo_database = NULL,
                                  mongo_cluster = TRUE,
                                  newsguard_info = FALSE){

  if (!(is.data.frame(output[[1]]))) {

    ct_shares_marked_mdb <- output[[1]]

    if(is.null(ct_shares_marked_mdb)) {
      if(is.null(mongo_url)) stop("Please provide the address of the MongoDB server used to store the posts that shared your URLs")
      if(is.null(mongo_database)) stop("Please provide a name for the MongoDB database used to store CooRnet's ct_shares_marked output")

      # open a connection to the database
      ct_shares_marked_mdb <-  connect_mongodb_cluster("coord_shares_info", mongo_database, mongo_url, mongo_cluster)
    }

    if (ct_shares_marked_mdb$count() == 0) stop("Please provide a name of an already existing mongoDB database. To do so, use get_ctshares function before calling this function.")

    ct_shares_marked.df <- as.data.frame(ct_shares_marked_mdb$find('{}')) # import collection as a dataframe
  }
  else ct_shares_marked.df <- output[[1]]

  names(ct_shares_marked.df) <- gsub("_", ".", names(ct_shares_marked.df))
  highly_connected_coordinated_entities <- output[[3]]
  rm(output)

  ct_shares_marked.df <- subset(ct_shares_marked.df, ct_shares_marked.df$is.coordinated==TRUE) # subset to save resource by working on coordinated shares only

  # average gini-domains and hosts
  ct_shares_marked.df$full_domain <- urltools::domain(ct_shares_marked.df$expanded) # eg. www.foxnews.com, video.foxnews.com, nation.foxnews.com
  ct_shares_marked.df$parent_domain <- paste(urltools::suffix_extract(ct_shares_marked.df$full_domain)$domain, urltools::suffix_extract(ct_shares_marked.df$full_domain)$suffix, sep = ".")

  # add the component id to the ct_shares_marked.df
  ct_shares_marked.df <- ct_shares_marked.df %>%
                         dplyr::mutate(account.url = account.url)
  ct_shares_marked.df <- merge(x=ct_shares_marked.df,
                               y=highly_connected_coordinated_entities[,c("name", "component")],
                               by.x = "account.url",
                               by.y = "name")
  ct_shares_marked.df <- ct_shares_marked.df %>%
                         select(-account.url)

  # summarise
  summary_entities <- highly_connected_coordinated_entities %>%
    dplyr::group_by(component) %>%
    dplyr::summarise(entities = n(),
              avg.subscriberCount = mean(account.avg.subscriberCount),
              cooRshare_ratio.avg = mean(coord.shares/(shares+coord.shares)),
              cooRscore.avg = mean(strength/degree),
              pageAdminTopCountry = unique(account.pageAdminTopCountry, na.rm = TRUE)[which.max(tabulate(match(account.pageAdminTopCountry, unique(account.pageAdminTopCountry))))],
              facebook_page = length(account.accountType[account.accountType=="facebook_page"]),
              facebook_group = length(account.accountType[account.accountType=="facebook_group"]),
              facebook_profile = length(account.accountType[account.accountType=="facebook_profile"]))

  if (newsguard_info){
    # query the the newsguardtech.com API
    domain_score <- data.frame(parent_domain = unique(ct_shares_marked.df$parent_domain), newsguard_score = NA)
    cat("\nGetting domains rating from NewsGuard (https://www.newsguardtech.com)...\n")
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
            write(paste("Unexpected http response code by the NewsGuard api", query$status_code, "on domain", parent_domain), file = "log.txt", append = TRUE)
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
      dplyr::group_by(component) %>%
      summarise(unique.full_domain = length(unique(full_domain)),
                unique.parent_domain = length(unique(parent_domain)),
                newsguard.parent_domain.score = mean(newsguard_score, na.rm = T),
                newsguard.parent_domain.rated = length(unique(parent_domain[!is.na(newsguard_score)]))) %>%
      mutate(newsguard.parent_domain.prop = newsguard.parent_domain.rated/unique.parent_domain)
  }
  else {
    summary_domains <- ct_shares_marked.df %>%
      dplyr::group_by(component) %>%
      dplyr::summarise(unique.full_domain = length(unique(full_domain)),
                       unique.parent_domain = length(unique(parent_domain)))
  }

  summary <- merge(summary_entities, summary_domains, by = "component")
  rm(summary_entities, summary_domains)

  summary <- summary %>%
    dplyr::rowwise() %>%
    dplyr::mutate(gini.full_domain = DescTools::Gini(prop.table(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "full_domain"]))),
           gini.parent_domain = DescTools::Gini(prop.table(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "parent_domain"])))) %>%
    dplyr::mutate(gini.full_domain = ifelse(is.nan(gini.full_domain), 1, gini.full_domain),
           gini.parent_domain = ifelse(is.nan(gini.parent_domain), 1, gini.parent_domain)) %>%
    dplyr::mutate(top.full_domain = paste(top_n(arrange(data.frame(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "full_domain"])), desc(Freq)),
                                         n=5, wt="Freq")$Var1, collapse = ", "),
           top.parent_domain = paste(top_n(arrange(data.frame(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "parent_domain"])), desc(Freq)),
                                           n=5, wt="Freq")$Var1, collapse = ", "))

  cat("Component summary done!\n")

  return(summary)
}
