#' get_top_coord_shares
#'
#' A function to get the top n posts containing URLs shared in a coordinated way
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param order_by name of the column used to order the top news. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"
#' @param component return the top posts grouped by network component (TRUE, default) or by account (FALSE)
#' @param top number of the top news to be retrieved
#' }
#' @return A data frame (grouped_df) containing the top URLs shared in a coordinated way by the highly coordinated entities, with a set of attributes
#'
#' @examples
#'   # get the top ten posts containing URLs shared by each network component and by engagement
#'   df <- get_top_news(output, order_by = "engagement", top=10)
#'
#'   # get the top ten posts containing URLs shared in a coordinated way, by each entity and by engagement
#'   df <- get_top_news(output, order_by = "engagement", top=10)
#'
#' @export

get_top_coord_shares <- function(output, order_by = "engagement", component=TRUE, top=10){

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

  top_shares <-
    coord_shares_urls %>%
    arrange(component, desc(!!sym(order_by))) %>%
    group_by(component) %>%
    slice(1:top)

  return(top_shares)

  }

  if(component!=TRUE){
    top_shares <-
        coord_shares_urls %>%
        arrange(account.url, desc(!!sym(order_by))) %>%
        group_by(account.url) %>%
        slice(1:top) %>%
        arrange(component)

      return(top_shares)

      }
  }

