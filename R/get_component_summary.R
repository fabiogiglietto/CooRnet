
#' get_component_summary
#'
#' A function to get summary data by coordinated component
#'
#' @param output the output list resulting from the function get_coord_shares
#'
#' @return A data frame containing summary data by each coordinated component:
#' the proportion of coordinated shares over the total shares (coorshare_ratio), the average coordinated score (avg_cooRscore), and a measure (gini) of how much the coordinated shares concern a more or less large number of domains
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
#' @importFrom dplyr %>% group_by summarise
#' @importFrom urltools suffix_extract domain
#' @importFrom DescTools Gini
#'
#' @export

get_component_summary <- function(output){

  ct_shares_marked.df <- output[[1]]
  highly_connected_coordinated_entities <- output[[3]]
  rm(output)

  ct_shares_marked.df <- subset(ct_shares_marked.df, ct_shares_marked.df$is_coordinated==TRUE) # subset to save resource by working on coordinated shares only

  # average gini-domains and hosts
  ct_shares_marked.df$full_domain <- urltools::domain(ct_shares_marked.df$expanded) # eg. www.foxnews.com, video.foxnews.com, nation.foxnews.com
  ct_shares_marked.df$parent_domain <- paste(urltools::suffix_extract(ct_shares_marked.df$full_domain)$domain, urltools::suffix_extract(ct_shares_marked.df$full_domain)$suffix, sep = ".")

  ct_shares_marked.df <- merge(x=ct_shares_marked.df,
                               y=highly_connected_coordinated_entities[,c("name", "component")],
                               by.x = "account.url",
                               by.y = "name")

  summary <- highly_connected_coordinated_entities %>% #
    group_by(component) %>%
    summarise(entities = n(),
              cooRshare_ratio.avg = mean(coord.shares/(shares+coord.shares)),
              cooRscore.avg = mean(strength/degree))

  summary <- summary %>%
    rowwise() %>%
    mutate(gini.full_domain = DescTools::Gini(prop.table(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "full_domain"]))),
           gini.parent_domain = DescTools::Gini(prop.table(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "parent_domain"])))) %>%
    mutate(gini.full_domain = ifelse(is.nan(gini.full_domain), 1, gini.full_domain),
           gini.parent_domain = ifelse(is.nan(gini.parent_domain), 1, gini.parent_domain)) %>%
    mutate(top.full.domain = paste(top_n(arrange(data.frame(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "full_domain"])), desc(Freq)), 5)$Var1, collapse = ", "),
           top.parent.domain = paste(top_n(arrange(data.frame(table(ct_shares_marked.df[ct_shares_marked.df$component==component, "parent_domain"])), desc(Freq)), 5)$Var1, collapse = ", "))


  return(summary)
}
