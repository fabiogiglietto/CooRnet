#' get_top_coord_urls
#'
#' A function to get the top n URLs shared in a coordinated way
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param order_by name of the column used to order the top news. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"
#' @param component return the top URLs by network component (TRUE, default) or just the top URLs (FALSE)
#' @param top number of the top URLs to be retrieved
#'
#' @return A data frame (grouped_df) containing the top URLs shared in a coordinated way by the highly coordinated entities, with shares and engagement statistics, list of entities and components that shared the link
#'
#' @examples
#' # get the top ten URLs shared in a coordinated way by each network component, by engagement
#' df <- get_top_coord_urls(output, order_by = "engagement", component = TRUE, top=10)
#'
#' # get the top ten URLs shared in a coordinated way, by engagement
#' df <- get_top_news(output, order_by = "engagement", top=10)
#'
#' @import dplyr
#'
#' @export

get_top_coord_urls <- function(output, order_by = "engagement", component=TRUE, top=10){

  ct_shares_marked.df <- output[[1]]
  highly_connected_coordinated_entities <- output[[3]]
  rm(output)

  urls <- ct_shares_marked.df %>%
    group_by(expanded) %>%
    summarise(shares = n(),
              engagement = sum(statistics.actual.likeCount,
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
    rowwise %>%
    mutate(cooR.shares = length(which(ct_shares_marked.df$expanded == expanded & ct_shares_marked.df$is_coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name)))  %>%
    filter(cooR.shares > 0) %>%
    mutate(account.url = paste(unique(ct_shares_marked.df$account.url[ct_shares_marked.df$expanded==expanded]), collapse=", "),
           cooR.account.url = paste(unique(ct_shares_marked.df$account.url[ct_shares_marked.df$expanded==expanded & ct_shares_marked.df$is_coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name]), collapse=", "),
           cooR.account.url.list = list(unique(ct_shares_marked.df$account.url[ct_shares_marked.df$expanded==expanded & ct_shares_marked.df$is_coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name])),
           account.name = paste(unique(shQuote(ct_shares_marked.df$account.name[ct_shares_marked.df$expanded==expanded])), collapse=", "),
           cooR.account.name = paste(unique(shQuote(ct_shares_marked.df$account.name[ct_shares_marked.df$expanded==expanded & ct_shares_marked.df$is_coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name])), collapse=", "),
           components = paste(unique(highly_connected_coordinated_entities$component[highly_connected_coordinated_entities$name %in% unlist(cooR.account.url.list)]), collapse = ", ")) %>%
    select(-cooR.account.url.list)

  if(component==TRUE) {
    urls <- urls %>%
      arrange(components, !!sym(order_by)) %>%
      group_by(components) %>%
      mutate(rank = rank(!!sym(order_by), ties.method = "first")) %>%
      filter(rank <= top)
  }
  else {
    urls <- urls %>%
      arrange(-!!sym(order_by)) %>%
      top_n(top)
    }

  return(urls)
  }
