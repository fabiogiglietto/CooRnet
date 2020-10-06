#' get_cluster_summary
#'
#' A function to get summary data by coordinated cluster
#'
#' @param output the output list resulting from the function get_coord_shares
#'
#' @return A data frame containing summary data by each coordinated cluster:
#' the average subscribers number of entities in a cluster,
#' the proportion of coordinated shares over the total shares (coorshare_ratio), the average coordinated score (avg_cooRscore),
#' a measure of dispersion (gini) in the distribution of domains coordinatedly shared by the cluster (0-1). Higher values correspond to an higher concentration (less different domains linked),
#' the top 5 coordinatedly shared domains (ranked by n. of shares),
#' the total number coordinatedly shared of domains
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
#'   cluster_summary <- get_cluster_summary(output)
#'
#'   # clustering the clusters rowwise mutate
#'   clusters <- hclust(dist(cluster_summary[, 2:4]))
#'   plot(clusters)
#'
#' @importFrom dplyr %>% group_by summarise mutate top_n arrange rowwise
#' @importFrom urltools suffix_extract domain
#' @importFrom DescTools Gini
#'
#' @export

get_cluster_summary <- function(output){

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
              avg.subscriberCount = mean(avg.account.subscriberCount),
              cooRshare_ratio.avg = mean(coord.shares/(shares+coord.shares)),
              cooRscore.avg = mean(strength/degree),
              pageAdminTopCountry = unique(account.pageAdminTopCountry, na.rm = TRUE)[which.max(tabulate(match(account.pageAdminTopCountry, unique(account.pageAdminTopCountry))))],
              facebook_page = length(account.accountType[account.accountType=="facebook_page"]),
              facebook_group = length(account.accountType[account.accountType=="facebook_group"]),
              facebook_profile = length(account.accountType[account.accountType=="facebook_profile"]))

  summary_domains <- ct_shares_marked.df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(unique.full_domain = length(unique(full_domain)),
              unique.parent_domain = length(unique(parent_domain)))

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

  cat("Done!")

  return(summary)
}
