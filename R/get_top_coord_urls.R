#' get_top_coord_urls
#'
#' A function to get the top n URLs shared in a coordinated way
#'
#' @param output the output list resulting from the function get_coord_shares
#' @param order_by name of the column used to order the top news. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"
#' @param mongo_url string: address of the MongoDB server in standard URI Format. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_database string: name of the MongoDB database where the shares have been saved. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_cluster logical: set to TRUE if you are using a MongoDB cluster instead of standalone instance (default FALSE)
#' @param component return the top URLs by network component (TRUE, default) or just the top URLs (FALSE)
#' @param top number of the top URLs to be retrieved
#'
#' @return A data frame (grouped_df) containing the top URLs shared in a coordinated way by the highly coordinated entities, with shares and engagement statistics, list of entities and components that shared the link
#'
#' @examples
#' # get the top ten URLs shared in a coordinated way by each network component, by engagement
#' df <- get_top_coord_urls(output, order_by = "engagement", component = TRUE, top=10)
#'
#' @importFrom dplyr filter n left_join rowwise mutate select arrange group_by top_n
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom plyr desc
#' @importFrom jsonlite flatten
#'
#' @export

get_top_coord_urls <- function(output,
                               order_by = "engagement",
                               mongo_url = NULL,
                               mongo_database = NULL,
                               mongo_cluster = TRUE,
                               component=TRUE,
                               top=10){


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
    ct_shares_marked.df <- jsonlite::flatten(ct_shares_marked.df)
  }
  else ct_shares_marked.df <- output[[1]]

  names(ct_shares_marked.df) <- gsub("_", ".", names(ct_shares_marked.df))
  highly_connected_coordinated_entities <- output[[3]]
  rm(output)

  urls <- ct_shares_marked.df %>%
    dplyr::group_by(expanded) %>%
    dplyr::summarise(shares = dplyr::n(),
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
              statistics.actual.loveCount = sum(statistics.actual.loveCount),
              statistics.actual.wowCount = sum(statistics.actual.wowCount),
              statistics.actual.hahaCount = sum(statistics.actual.hahaCount),
              statistics.actual.loveCount = sum(statistics.actual.loveCount),
              statistics.actual.sadCount = sum(statistics.actual.sadCount),
              statistics.actual.angryCount = sum(statistics.actual.angryCount)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cooR.shares = length(which(ct_shares_marked.df$expanded == expanded & ct_shares_marked.df$is.coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name)))  %>%
    dplyr::filter(cooR.shares > 0) %>%
    dplyr::mutate(account.url = paste(unique(ct_shares_marked.df$account.url[ct_shares_marked.df$expanded==expanded]), collapse=","),
           cooR.account.url = paste(unique(ct_shares_marked.df$account.url[ct_shares_marked.df$expanded==expanded & ct_shares_marked.df$is.coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name]), collapse=","),
           cooR.account.url.list = list(unique(ct_shares_marked.df$account.url[ct_shares_marked.df$expanded==expanded & ct_shares_marked.df$is.coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name])),
           account.name = paste(unique(shQuote(ct_shares_marked.df$account.name[ct_shares_marked.df$expanded==expanded])), collapse=","),
           cooR.account.name = paste(unique(shQuote(ct_shares_marked.df$account.name[ct_shares_marked.df$expanded==expanded & ct_shares_marked.df$is.coordinated==TRUE & ct_shares_marked.df$account.url %in% highly_connected_coordinated_entities$name])), collapse=","),
           components = paste(unique(highly_connected_coordinated_entities$component[highly_connected_coordinated_entities$name %in% unlist(cooR.account.url.list)]), collapse = ",")) %>%
    dplyr::select(-cooR.account.url.list) %>%
    as.data.frame()

  if(component==TRUE) {
    urls <- urls %>%
      dplyr::arrange(components, !!sym(order_by)) %>%
      dplyr::group_by(components) %>%
      dplyr::mutate(rank = rank(plyr::desc(!!sym(order_by)), ties.method = "first")) %>%
      dplyr::filter(rank <= top) %>%
      dplyr::arrange(components, rank)
  }
  else {
    urls <- urls %>%
      dplyr::top_n(top, wt=!!sym(order_by)) %>%
      dplyr::arrange(-!!sym(order_by)) %>%
      tibble::as_tibble()
    }

  return(urls)
  }
