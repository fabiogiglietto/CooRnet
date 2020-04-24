#' get_top_coord_shares
#'
#' A function to get the top n posts by engagement containing URLs shared in a coordinated way
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param order_by name of the column used to order the top shares. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"
#' @param component return the top posts grouped by network component (TRUE, default) or just the top posts by engagement (FALSE)
#' @param top number of the top shares to be retrieved
#'
#' @return A data frame (grouped_df) containing the posts with the highest engagement shared in a coordinated way by the highly coordinated entities, with a set of attributes
#'
#' @examples
#'   # get the top ten posts containing URLs shared by each network component and by engagement
#'   df <- get_top_news(output, order_by = "engagement", top=10)
#'
#'   # get the top ten posts containing URLs shared in a coordinated way, by engagement
#'   df <- get_top_news(output, order_by = "engagement", top=10)
#'
#' @import dplyr
#'
#' @export

get_top_coord_shares <- function(output, order_by = "engagement", component=TRUE, top=10) {

  ct_shares_marked.df <- output[[1]]
  highly_connected_coordinated_entities <- output[[3]]
  rm(output)

  urls <- ct_shares_marked.df %>%
    filter(is_coordinated==TRUE & account.url %in% highly_connected_coordinated_entities$name) %>%
    left_join(highly_connected_coordinated_entities[, c("name", "component")], by = c("account.url" = "name")) %>%
    rowwise() %>%
    mutate(engagement = sum(statistics.actual.likeCount,
                               statistics.actual.shareCount,
                               statistics.actual.commentCount,
                               statistics.actual.loveCount,
                               statistics.actual.wowCount,
                               statistics.actual.hahaCount,
                               statistics.actual.sadCount,
                               statistics.actual.angryCount),
              statistics.actual.likeCount = sum(statistics.actual.likeCount),
              statistics.actual.shareCount = sum(statistics.actual.shareCount),
              statistics.actual.commentCount = sum(statistics.actual.commentCount),
              statistics.actual.wowCount = sum(statistics.actual.wowCount),
              statistics.actual.hahaCount = sum(statistics.actual.hahaCount),
              statistics.actual.likeCount = sum(statistics.actual.likeCount),
              statistics.actual.sadCount = sum(statistics.actual.sadCount),
              statistics.actual.angryCount = sum(statistics.actual.angryCount)) %>%
    select(c("account.url","date","title","description","message","link","postUrl",
             "account.name","account.handle","account.subscriberCount","expanded",
             "statistics.actual.likeCount", "statistics.actual.shareCount",
             "statistics.actual.commentCount", "statistics.actual.loveCount",
             "statistics.actual.wowCount", "statistics.actual.hahaCount",
             "statistics.actual.sadCount","statistics.actual.angryCount",
             "engagement","component")) %>%
    as.data.frame()

  if(component==TRUE) {
    urls <- urls %>%
      arrange(component, !!sym(order_by)) %>%
      group_by(component) %>%
      mutate(rank = rank(desc(!!sym(order_by)), ties.method = "first")) %>%
      filter(rank <= top)
  }
  else {
    urls <- urls %>%
      top_n(top, wt=!!sym(order_by)) %>%
      arrange(-!!sym(order_by)) %>%
      as_tibble()
  }

  return(urls)
}

