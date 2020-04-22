
#' get_component_summary
#'
#' A function to get summary data by coordinated component
#'
#' @param output the output list resulting from the function get_coord_shares
#'
#' @return A data frame containing summary data by each coordinated component:
#' the proportion of coordinated shares over the total shares (coorshare_ratio), the average coordinated score (avg_cooRscore), and a measure (gini) of how much the coordinated shares concern a more or less large number of domains
#'
#' @details The gini values is computed by using the Gini coefficient on the proportions of unique domains each component shared.
#' The Gini coefficient is a measure of the degree of concentration (inequality) of a variable in a distribution.
#' It ranges between 0 and 1: the more nearly equal the distribution, the lower its Gini index. When a component shared just one domain, the value of the variable is set to 1.
#'
#' The coorshare_ratio is a measure of how much coordinated is a component.
#' Its value is calculated by dividing the number of coordinated shares of each coordinated entities by the number of entities with which it has shared links in a coordinated way (its degree),
#' and then calculating the average by component of these values.
#'
#' @examples
#'   # get the top ten posts containing URLs shared by each network component and by engagement
#'   component_summary <- get_component_summary(output)
#'
#'   # clustering the components
#'   clusters <- hclust(dist(component_summary[, 2:4]))
#'   plot(clusters)
#'
#' @export

get_component_summary <- function(output){

  require(dplyr)
  require(urltools)
  require(DescTools)

  colnames(output[[3]])[1] <- "account.url"
  output[[1]] <- merge(output[[1]], output[[3]][,c("account.url", "degree", "component", "strength")], by = "account.url", all.x=T)

  # coordinated vs non coordinated shares ratio
  summary1 <- output[[1]] %>%
    group_by(component) %>%
    summarize(iscoord = length(is_coordinated[is_coordinated==TRUE]),
              isNOTcoord = length(is_coordinated[is_coordinated==FALSE])) %>%
    mutate(coorshare_ratio = iscoord/(iscoord+isNOTcoord)) %>%
    select(component, coorshare_ratio)

  # average cooRscore
  summary2 <- output[[3]] %>%
    mutate(cooRscore = strength/degree) %>%
    group_by(component) %>%
    summarize(avg_cooRscore = mean(cooRscore))

  # average gini-domains
  output[[1]]$domain <- suffix_extract(domain(output[[1]]$expanded))$host
  output[[1]]$domain <- gsub("www.", "", output[[1]]$domain)

  ginidomains <- function(dataset, component) {
    Gini(prop.table(table(dataset[dataset$component==component, "domain"])))
  }

  dfgini <- subset(output[[1]], !is.na(output[[1]]$component))

  for (i in 1:nrow(dfgini)){
    dfgini$gini[i] <- ginidomains(dfgini, component = dfgini$component[i])
    if (is.nan(dfgini$gini[i])) dfgini$gini[i] <- 1
  }

  dfgini <- unique(dfgini[,c("component", "gini")])

  component_summary <- merge(summary1, summary2, by = "component")
  component_summary <- merge(component_summary, dfgini, by = "component")

  return(component_summary)
}
