#' get_coord_shares_mongo
#'
#' Given a dataset of CrowdTangle shares on a MongoDB database and a time threshold, this function detects networks of entities (pages, accounts and groups) that performed coordinated link sharing behavior
#'

#' @param mongo_database string: the name of the MongoDB used to host the collections.
#' @param ct_shares.df an open connection to an open ct_shares connection created by get_ctshares
#' @param coordination_interval a threshold in seconds that defines a coordinated share. Given a dataset of CrowdTangle shares, this threshold is automatically estimated by the estimate_coord_interval interval function. Alternatively it can be manually passed to the function in seconds
#' @param mongo_url string: address of the MongoDB server in standard URI Format. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_collection string: name of the MongoDB collection where the shares have been saved. Set to NULL to avoid using mongo (default NULL)
#' @param mongo_cluster logical: set to TRUE if you are using a MongoDB cluster instead of standalone instance (default FALSE)
#' @param parallel enables parallel processing to speed up the process taking advantage of multiple cores (default FALSE). The number of cores is automatically set to all the available cores minus one
#' @param percentile_edge_weight defines the percentile of the edge distribution to keep in order to identify a network of coordinated entities. In other terms, this value determines the minimum number of times that two entities had to coordinate in order to be considered part of a network. (default 0.90)
#' @param clean_urls clean the URLs from the tracking parameters (default FALSE)
#' @param keep_ourl_only restrict the analysis to ct shares links matching the original URLs (default=FALSE)
#' @param return_df logical: set to TRUE to return a dataframe of ct_shares (default FALSE)
#' @param gtimestamps add timestamps of the fist and last coordinated shares on each node. Slow on large networks (default=FALSE)
#'
#' @return A list (results_list) containing four objects: 1. The input data.table (ct_shares.dt) of shares with an additional boolean variable (coordinated) that identifies coordinated shares, 2. An igraph graph (highly_connected_g) with networks of coordinated entities whose edges also contains a t_coord_share attribute (vector) reporting the timestamps of every time the edge was detected as coordinated sharing, 3. A dataframe with a list of coordinated entities (highly_connected_coordinated_entities) with respective name (the account url), number of shares performed, average subscriber count, platform, account name, if the account name changed, if the account is verified, account handle, degree and component number
#'
#' @examples
#' output <- get_coord_shares_mongo(mongo_database)
#'
#' output <- get_coord_shares_mongo(mongo_database = mongo_database, coordination_interval = coordination.interval, percentile_edge_weight=0.9, clean_urls=FALSE, keep_ourl_only=FALSE, gtimestamps=FALSE)
#'
#' # Extract the outputs
#' get_outputs(output)
#'
#' # Save the data frame of CrowdTangle shares marked with the “is_coordinated” column
#' write.csv(ct_shares_marked.df, file=“ct_shares_marked.df.csv”)
#'
#' # Save the graph in a Gephi readable format
#' library(igraph)
#' write.graph(highly_connected_g, file="highly_connected_g.graphml", format = "graphml")
#'
#' # Save the data frame with the information about the highly connected coordinated entities
#' write.csv(highly_connected_coordinated_entities, file=“highly_connected_coordinated_entities.csv”)
#'
#' @importFrom stats quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr mutate mutate select filter
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom tidytable unnest. bind_rows.
#' @importFrom mongolite mongo
#'
#' @export

get_coord_shares_mongo <- function(ct_shares.df=NULL,
                                   mongo_database=NULL,
                                   mongo_url=NULL,
                                   coordination_interval=NULL,
                                   parallel=FALSE,
                                   percentile_edge_weight=0.90,
                                   clean_urls=FALSE,
                                   keep_ourl_only=FALSE,
                                   mongo_cluster=TRUE,
                                   return_df = FALSE,
                                   gtimestamps=FALSE) {

  options(warn=-1)

  # Connect to the ct_shares_info collection in mongoDB database
  # if(is.null(ct_shares.df)) {
  if(is.null(mongo_url)) stop("Please provide the address of the MongoDB server used to store the posts that shared your URLs")
  if(is.null(mongo_database)) stop("Please provide a name for the MongoDB database used to store the posts that shared your URLs")

  # }
  # else {
  #  ct_shares_mdb <- ct_shares.df
  # }

  ct_shares_mdb <-  connect_mongodb_cluster("shares_info", mongo_database, mongo_url, mongo_cluster)
  # Check if ct_shares_mdb already existed. Otherwise the function will be closed since no available database already exists
  if (ct_shares_mdb$count() == 0) stop("Please provide a name of an already existing mongoDB database. To do so, use get_ctshares function before calling this function.")

  # Retrieve all the database within the specified collection
  ct_shares.df <- ct_shares_mdb$find('{}')

  # Clean urls
  if(clean_urls==TRUE){
    valid_schemes <- read.csv(file = valid_schemes_csv)
    ct_shares.df <- clean_urls(ct_shares.df, "expanded", valid_schemes)

    # # Find urls in the mongoDB database and aggregate in a dataframe
    # urls_df <- ct_shares_mdb$aggregate('[{"$group":{"_id":"$expanded","id_number": {"$addToSet": "$_id"}}}]', options = '{"allowDiskUse":true}')
    # names(urls_df) <- c("url","id_number_list")
    #
    # # Apply the new version of clear_urls in order to create a dataframe with cleaned URLs
    # cleaned_urls_df <- clean_urls_mongo(urls_df, "url")
    #
    # # Substitute URLs in the database if they are cleaned troughtout the cleaning procedure
    # # Occurences of " need to be substituted with \" within the update query
    # original_urls_list <- as.list(cleaned_urls_df$url)
    # cleaned_urls_list <- as.list(cleaned_urls_df$cleaned_url)
    #
    # for (i in 1:length(original_urls_list)){
    #
    #   original_url <- stringi::stri_enc_toutf8(sprintf('%s',original_urls_list[i]))
    #   cleaned_url <- stringi::stri_enc_toutf8(sprintf('%s',cleaned_urls_list[i]))
    #
    #   if (cleaned_url != "") ct_shares_mdb$update(sprintf('{"expanded" : "%s"}',original_url), sprintf('{"$set": {"expanded": "%s"}}',cleaned_url) , multiple = TRUE)
    #   else ct_shares_mdb$remove(sprintf('{"expanded": "%s"}', original_url))
    # }
    write("Analysis performed on cleaned URLs", file = "log.txt", append = TRUE)
  }

  URLs_mdb <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$expanded", "freq": {"$sum":1}}},{"$match": {"freq": {"$gt": 1}}}]',options = '{"allowDiskUse":true}'))
  names(URLs_mdb) <- c("URL", "ct_shares")
  # URLS_mdb$URL <- as.character(URLS_mdb$URL)

  # keep original URLs only?
  if(keep_ourl_only==TRUE){

    URLs_original <- ct_shares_mdb$find(query = '{"is_orig" : { "$eq" : true }}',
                                        fields = '{"expanded" : true}')

    if (nrow(URLs_original) < 2) stop("Can't execute with keep_ourl_only=TRUE. Not enough posts matching original URLs")
    else URLs_mdb <- subset(URLs_mdb, URLs_mdb$URL %in% URLs_original$expanded)

    write("Analysis performed on shares matching original URLs", file = "log.txt", append = TRUE)
    rm(URLs_original)
  }

  URLs_list <- URLs_mdb$URL

  ct_shares.df <- subset(ct_shares.df, ct_shares.df$expanded %in% URLs_list)

  # Iterate until the 'it' variable is NULL
  # keep only shares of URLs shared more than one time
  # while(!is.null(x <- it$one())){
  #   single_url <- x$expanded
  #   if (single_url %in% URLs_list) {
  #     url_row <- data.frame(x)
  #     names(url_row) <- gsub("_", ".", names(url_row))
  #     ct_shares.df <- dplyr::bind_rows(ct_shares.df, url_row)
  #   }
  # }

  # estimate the coordination interval if not specified by the users
  if(is.null(coordination_interval)){
    coordination_interval <- estimate_coord_interval(ct_shares.df, clean_urls = clean_urls, keep_ourl_only= keep_ourl_only)
    coordination_interval <- coordination_interval[[2]]
  }

  # use the coordination interval resulting from estimate_coord_interval
  if(is.list(coordination_interval)){
    coordination_interval <- coordination_interval[[2]]
  }

  # use the coordination interval set by the user
  if(is.numeric(coordination_interval)){
    if (coordination_interval == 0) {
      stop("The coordination_interval value can't be 0.
           \nPlease choose a value greater than zero or use coordination_interval=NULL to automatically calculate the interval")
    } else {

      coordination_interval <- paste(coordination_interval, "secs")

      if (file.exists("log.txt")) {
        write(paste("\nget_coord_shares script executed on:", format(Sys.time(), format = "%F %R %Z"),
                    "\ncoordination interval set by the user:", coordination_interval), file="log.txt", append=TRUE)
      } else {
        write(paste0("#################### CooRnet #####################\n",
                     "\nScript executed on:", format(Sys.time(), format = "%F %R %Z"),
                     "\ncoordination interval set by the user:", coordination_interval),
              file="log.txt")
      }
    }
  }

  ###############
  # Parallel ####
  ###############

  if(parallel==TRUE){

    # setup parallel backend
    cores <- parallel::detectCores()-1
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    doSNOW::registerDoSNOW(cl)

    # progress bar
    pb <- utils::txtProgressBar(max=nrow(URLs_mdb), style=3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    progress_bar <- list(progress=progress)

    # cycle trough all URLs to find entities that shared the same link within the coordination internal
    dat.summary <-
      foreach::foreach(i=seq(1:nrow(URLs_mdb)), .packages="dplyr", .options.snow=progress_bar) %dopar% {

        # show progress...
        utils::setTxtProgressBar(pb, pb$getVal()+1)

        url <- URLs_mdb$URL[i]
        dat.summary <- subset(ct_shares.df, ct_shares.df$expanded==url)

        if (length(unique(dat.summary$account$url)) > 1) {
          dat.summary <- dat.summary %>%
            dplyr::mutate(cut = cut(as.POSIXct(date), breaks = coordination_interval)) %>%
            dplyr::group_by(cut) %>%
            dplyr::mutate(count=n(),
                          account.url=list(account$url),
                          share_date=list(date),
                          url = url) %>%
            dplyr::select(cut, count, account.url, share_date, url) %>%
            dplyr::filter(count > 1) %>%   # subset the URLs shared by more than one entity
            unique()

          return(dat.summary)
        }
      }

    parallel::stopCluster(cl)

    dat.summary <- tidytable::bind_rows.(dat.summary)

    if(nrow(dat.summary)==0){
      stop("there are not enough shares!")
    }

    coordinated_shares <- tidytable::unnest.(dat.summary)

    rm(dat.summary, cores, cl, pb, progress, progress_bar)

    # mark the coordinated shares in the data set
    ct_shares.df$is_coordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                            ct_shares.df$date %in% coordinated_shares$share_date &
                                            ct_shares.df$account$url %in% coordinated_shares$account.url, TRUE, FALSE)

    highly_c_list <- build_coord_graph(ct_shares.df=ct_shares.df, coordinated_shares=coordinated_shares, percentile_edge_weight=percentile_edge_weight, timestamps=gtimestamps)

    highly_connected_g <- highly_c_list[[1]]
    highly_connected_coordinated_entities <- highly_c_list[[2]]
    q <- highly_c_list[[3]]
    rm(highly_c_list)

    uniqueURLs_shared <- unique(ct_shares.df[, c("expanded", "is_coordinated")])

    # write the log
    write(paste("\nnumber of unique URLs shared in coordinated way:", table(uniqueURLs_shared$is_coordinated)[2][[1]], paste0("(", round((table(uniqueURLs_shared$is_coordinated)[2][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\nnumber of unique URLs shared in non-coordinated way:", table(uniqueURLs_shared$is_coordinated)[1][[1]], paste0("(", round((table(uniqueURLs_shared$is_coordinated)[1][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\npercentile_edge_weight:", percentile_edge_weight, paste0("(minimum coordination repetition: ", q, ")"),
                "\nhighly connected coordinated entities:", length(unique(highly_connected_coordinated_entities$name)),
                "\nnumber of component:", length(unique(highly_connected_coordinated_entities$component))),
          file="log.txt", append=TRUE)

    if (return_df==TRUE) {
      cat("\nReturning dataframe in the output...")
      results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)
      return(results_list)}
    else{
      cat("\nSaving coordinated dataframe on MongoDB database...")
      coord_shares_mdb <- connect_mongodb_cluster("coord_shares_info", mongo_database, mongo_url, mongo_cluster)
      coord_shares_mdb$insert(ct_shares.df)
      results_list <- list(coord_shares_mdb,highly_connected_g, highly_connected_coordinated_entities)
      return(results_list)
    }
  }

  ###################
  # Non Parallel ####
  ###################

  if(parallel==FALSE){

    datalist <- list()

    # progress bar
    total <- nrow(URLs_mdb)
    pb <- txtProgressBar(max=total, style=3)

    for (i in 1:total) {

      utils::setTxtProgressBar(pb, pb$getVal()+1)

      url <- URLs_mdb$URL[i]
      # TO DO: modify with mongoDB
      dat.summary <- subset(ct_shares.df, ct_shares.df$expanded==url)

      if (length(unique(dat.summary$account$url)) > 1) {
        dat.summary <- dat.summary %>%
          dplyr::mutate(cut = cut(as.POSIXct(date), breaks = coordination_interval)) %>%
          dplyr::group_by(cut) %>%
          dplyr::mutate(count=n(),
                        account.url=list(account$url),
                        share_date=list(date),
                        url = url) %>%
          dplyr::select(cut, count, account.url, share_date, url) %>%
          dplyr::filter(count > 1) %>%
          unique()

        datalist <- c(list(dat.summary), datalist)
        rm(dat.summary)
      }
    }

    if(length(datalist)==0){
      stop("there are not enough coordinated shares!", call. = FALSE)
    }

    datalist <- tidytable::bind_rows.(datalist)

    coordinated_shares <- tidytable::unnest.(datalist)
    rm(datalist)

    # mark the coordinated shares in the data set
    ct_shares.df$is_coordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                            ct_shares.df$date %in% coordinated_shares$share_date &
                                            ct_shares.df$account$url %in% coordinated_shares$account.url, TRUE, FALSE)

    # call build_coord_graph
    highly_c_list <- build_coord_graph(ct_shares.df=ct_shares.df, coordinated_shares=coordinated_shares, percentile_edge_weight=percentile_edge_weight, timestamps=gtimestamps)

    highly_connected_g <- highly_c_list[[1]]
    highly_connected_coordinated_entities <- highly_c_list[[2]]
    q <- highly_c_list[[3]]
    rm(highly_c_list)

    uniqueURLs_shared <- unique(ct_shares.df[, c("expanded", "is_coordinated")])

    # write the log
    write(paste("\nnumber of unique URLs shared in coordinated way:", table(uniqueURLs_shared$is_coordinated)[2][[1]], paste0("(", round((table(uniqueURLs_shared$is_coordinated)[2][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\nnumber of unique URLs shared in non-coordinated way:", table(uniqueURLs_shared$is_coordinated)[1][[1]], paste0("(", round((table(uniqueURLs_shared$is_coordinated)[1][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\npercentile_edge_weight:", percentile_edge_weight, paste0("(minimum coordination repetition: ", q, ")"),
                "\nhighly connected coordinated entities:", length(unique(highly_connected_coordinated_entities$name)),
                "\nnumber of component:", length(unique(highly_connected_coordinated_entities$component))),
          file="log.txt", append=TRUE)

    if (return_df==TRUE) {
      cat("\nReturning dataframe in the output...")
      results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)
      return(results_list)}
    else{
      cat("\nSaving coordinated dataframe on MongoDB database...")
      coord_shares_mdb <- connect_mongodb_cluster("coord_shares_info", mongo_database, mongo_url, mongo_cluster)
      coord_shares_mdb$insert(ct_shares.df)
      results_list <- list(coord_shares_mdb, highly_connected_g, highly_connected_coordinated_entities)
      return(results_list)
    }
  }
}
