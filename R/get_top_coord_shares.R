#' get_top_coord_shares
#'
#' A function to get the top n posts by engagement containing URLs shared in a coordinated way
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param order_by name of the column used to order the top shares. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"
#' @param mongo_url string: address of the MongoDB server in standard URI Format. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_database string: name of the MongoDB database where the shares have been saved. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_cluster logical: set to TRUE if you are using a MongoDB cluster instead of standalone instance (default FALSE)
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
#' @importFrom dplyr filter left_join rowwise mutate select arrange group_by top_n
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom plyr desc
#'
#' @export

get_top_coord_shares <- function(output,
                                 order_by = "engagement",
                                 mongo_url = NULL,
                                 mongo_database = NULL,
                                 mongo_cluster = TRUE,
                                 component=TRUE,
                                 top=10) {

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

  urls <- ct_shares_marked.df %>%
    dplyr::filter(is.coordinated==TRUE & account$url %in% highly_connected_coordinated_entities$name) %>%
    dplyr::mutate(account.url = account$url,
                  account.name = account$name,
                  account.handle = account$handle,
                  account.subscriberCount = account$subscriberCount) %>%
    dplyr::left_join(highly_connected_coordinated_entities[, c("name", "component")], by = c("account.url" = "name")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(engagement = sum(statistics$actual$likeCount,
                               statistics$actual$shareCount,
                               statistics$actual$commentCount,
                               statistics$actual$loveCount,
                               statistics$actual$wowCount,
                               statistics$actual$hahaCount,
                               statistics$actual$sadCount,
                               statistics$actual$angryCount),
              statistics.actual.likeCount = sum(statistics$actual$likeCount),
              statistics.actual.shareCount = sum(statistics$actual$shareCount),
              statistics.actual.commentCount = sum(statistics$actual$commentCount),
              statistics.actual.wowCount = sum(statistics$actual$wowCount),
              statistics.actual.hahaCount = sum(statistics$actual$hahaCount),
              statistics.actual.loveCount = sum(statistics$actual$loveCount),
              statistics.actual.sadCount = sum(statistics$actual$sadCount),
              statistics.actual.angryCount = sum(statistics$actual$angryCount)) %>%
    dplyr::select(c("account.url","date","title","description","message","postUrl",
             "account.name","account.handle","account.subscriberCount","expanded",
             "statistics.actual.likeCount", "statistics.actual.shareCount",
             "statistics.actual.commentCount", "statistics.actual.loveCount",
             "statistics.actual.wowCount", "statistics.actual.hahaCount",
             "statistics.actual.sadCount","statistics.actual.angryCount",
             "engagement","component")) %>%
    as.data.frame()

  if(component==TRUE) {
    urls <- urls %>%
      dplyr::arrange(component, !!sym(order_by)) %>%
      dplyr::group_by(component) %>%
      dplyr::mutate(rank = rank(plyr::desc(!!sym(order_by)), ties.method = "first")) %>%
      dplyr::filter(rank <= top) %>%
      dplyr::arrange(component, rank)
  }
  else {
    urls <- urls %>%
      dplyr::top_n(top, wt=!!sym(order_by)) %>%
      dplyr::arrange(-!!sym(order_by)) %>%
      tibble::as_tibble()
  }

  return(urls)
}

