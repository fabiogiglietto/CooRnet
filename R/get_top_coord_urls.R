#' get_top_coord_urls
#'
#' A function to get the top n URLs shared in a coordinated way
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param order_by name of the column used to order the top news. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"
#' @param component return the top URLs by network component (TRUE, default) or just the top URLs (FALSE)
#' @param top number of the top URLs to be retrieved
#'
#' @return A data frame (grouped_df) containing the top URLs shared in a coordinated way by the highly coordinated entities, with a set of attributes
#'
#' @examples{
#' # get the top ten URLs shared in a coordinated way by each network component, by engagement
#' df <- get_top_coord_urls(output, order_by = "engagement", component = TRUE, top=10)
#'
#' # get the top ten URLs shared in a coordinated way, by engagement
#' df <- get_top_news(output, order_by = "engagement", top=10)
#'
#' @export

get_top_coord_urls <- function(output, order_by = "engagement", component=TRUE, top=10){

  require(dplyr) # 0.8.3

  coord_shares_urls <- output[[1]][output[[1]]$iscoordinated==TRUE & output[[1]]$account.url %in% output[[3]]$name,]
  coord_shares_urls$engagement <- apply(coord_shares_urls[,c("statistics.actual.likeCount",
                                                             "statistics.actual.shareCount",
                                                             "statistics.actual.commentCount",
                                                             "statistics.actual.loveCount",
                                                             "statistics.actual.wowCount",
                                                             "statistics.actual.hahaCount",
                                                             "statistics.actual.sadCount",
                                                             "statistics.actual.angryCount")], 1, sum)
  colnames(output[[3]])[1] <- "account.url"
  coord_shares_urls <- merge(coord_shares_urls, output[[3]][,c("account.url","component")], by="account.url", all.x=T)
  coord_shares_urls <- coord_shares_urls[,c("account.url","date","title","description","message","link","postUrl",
                                            "account.name","account.handle","account.subscriberCount","expanded",
                                            "statistics.actual.likeCount", "statistics.actual.shareCount",
                                            "statistics.actual.commentCount", "statistics.actual.loveCount",
                                            "statistics.actual.wowCount", "statistics.actual.hahaCount",
                                            "statistics.actual.sadCount","statistics.actual.angryCount",
                                            "engagement","component")]

  if(component==TRUE){

  suppressMessages(top_urls <-
      coord_shares_urls %>%
      group_by(component, expanded) %>%
      summarize(n_shares = n(),
                eng = sum(!!sym(order_by))) %>%
      top_n(top) %>%
      arrange(.by_group=T))

    colnames(top_urls)[4] <- order_by

    urls_account_id <- unique(coord_shares_urls[, c("expanded", "account.name", "account.url")])

    top_urls$account.names <- NA
    top_urls$account.urls <- NA

    for (i in 1:nrow(top_urls)){
      for (j in 1:nrow(urls_account_id)){
        if (urls_account_id$expanded[j] == top_urls$expanded[i]) {
          urls_account_id_names_sub <- unique(urls_account_id$account.name[urls_account_id$expanded == urls_account_id$expanded[j]])
          top_urls$account.names[i] <- paste(urls_account_id_names_sub, collapse = ", ")
        }

        if (urls_account_id$expanded[j] == top_urls$expanded[i]) {
          urls_account_id_urls_sub <- unique(urls_account_id$account.url[urls_account_id$expanded == urls_account_id$expanded[j]])
          top_urls$account.urls[i] <- paste(urls_account_id_urls_sub, collapse = ", ")
        }
      }
    }

    return(top_urls)
  }

  if(component==FALSE){

    suppressMessages(top_urls <-
      coord_shares_urls %>%
      group_by(expanded) %>%
      summarize(n_shares = n(),
                eng = sum(!!sym(order_by))) %>%
      top_n(top) %>%
      arrange(desc(eng)))

    colnames(top_urls)[3] <- order_by

    urls_account_id <- unique(coord_shares_urls[, c("expanded", "account.name", "account.url")])

    top_urls$account.names <- NA
    top_urls$account.urls <- NA

    for (i in 1:nrow(top_urls)){
      for (j in 1:nrow(urls_account_id)){
        if (urls_account_id$expanded[j] == top_urls$expanded[i]) {
          urls_account_id_names_sub <- unique(urls_account_id$account.name[urls_account_id$expanded == urls_account_id$expanded[j]])
          top_urls$account.names[i] <- paste(urls_account_id_names_sub, collapse = ", ")
        }

        if (urls_account_id$expanded[j] == top_urls$expanded[i]) {
          urls_account_id_urls_sub <- unique(urls_account_id$account.url[urls_account_id$expanded == urls_account_id$expanded[j]])
          top_urls$account.urls[i] <- paste(urls_account_id_urls_sub, collapse = ", ")
        }
      }
    }

    return(top_urls)
  }

}
