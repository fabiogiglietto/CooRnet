#' get_coord_shares
#'
#' Given a dataset of CrowdTangle shares and a time threshold, this function detects networks of entities (pages, accounts and groups) that performed coordinated link sharing behavior
#'
#' @param ct_shares.df the data.frame of link posts resulting from the function get_ctshares
#' @param coordination_interval a threshold in seconds that defines a coordinated share. Given a dataset of CrowdTangle shares, this threshold is automatically estimated by the estimate_coord_interval interval function. Alternatively it can be manually passed to the function in seconds
#' @param parallel enables parallel processing to speed up the process taking advantage of multiple cores (default FALSE). The number of cores is automatically set to all the available cores minus one
#' @param percentile_edge_weight defines the percentile of the edge distribution to keep in order to identify a network of coordinated entities. In other terms, this value determines the minimum number of times that two entities had to coordinate in order to be considered part of a network. (default 0.90)
#' @param clean_urls clean the URLs from the tracking parameters (default FALSE)
#' @param keep_ourl_only restrict the analysis to ct shares links matching the original URLs (default=FALSE)
#' @param gtimestamps add timestamps of the fist and last coordinated shares on each node. Slow on large networks (default=FALSE)
#'
#' @return A list (results_list) containing four objects: 1. The input data.table (ct_shares.dt) of shares with an additional boolean variable (coordinated) that identifies coordinated shares, 2. An igraph graph (highly_connected_g) with networks of coordinated entities whose edges also contains a t_coord_share attribute (vector) reporting the timestamps of every time the edge was detected as coordinated sharing, 3. A dataframe with a list of coordinated entities (highly_connected_coordinated_entities) with respective name (the account url), number of shares performed, average subscriber count, platform, account name, if the account name changed, if the account is verified, account handle, degree and component number
#'
#' @examples
#' output <- get_coord_shares(ct_shares.df)
#'
#' output <- get_coord_shares(ct_shares.df = ct_shares.df, coordination_interval = coordination.interval, percentile_edge_weight=0.9, clean_urls=FALSE, keep_ourl_only=FALSE, gtimestamps=FALSE)
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
#' @importFrom tidyr unnest
#' @importFrom stats quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr mutate mutate select filter bind_rows
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#'
#'
#' @export

get_coord_shares <- function(ct_shares.df, coordination_interval=NULL, parallel=FALSE, percentile_edge_weight=0.90, clean_urls=FALSE, keep_ourl_only=FALSE, gtimestamps=FALSE) {

  options(warn=-1)

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

  # keep original URLs only?
  if(keep_ourl_only==TRUE){
    ct_shares.df <- subset(ct_shares.df, ct_shares.df$is_orig==TRUE)
    if (nrow(ct_shares.df) < 2) stop("Can't execute with keep_ourl_only=TRUE. Not enough posts matching original URLs")
    write("Analysis performed on shares matching original URLs", file = "log.txt", append = TRUE)
  }

  # clean urls?
  if(clean_urls==TRUE){
    ct_shares.df <- clean_urls(ct_shares.df, "expanded")
    write("Analysis performed on cleaned URLs", file = "log.txt", append = TRUE)
  }

  # get a list of all the shared URLs
  URLs <- as.data.frame(table(ct_shares.df$expanded))
  names(URLs) <- c("URL", "ct_shares")
  URLs <- subset(URLs, URLs$ct_shares>1) # remove the URLs shared just 1 time
  URLs$URL <- as.character(URLs$URL)

  # keep only shares of URLs shared more than one time
  ct_shares.df <- subset(ct_shares.df, ct_shares.df$expanded %in% URLs$URL)

  ###############
  # Parallel ####
  ###############

  if(parallel==TRUE){

    # setup parallel backend
    cores <- parallel::detectCores()-1
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    doSNOW::registerDoSNOW(cl)

    # progress bar
    pb <- utils::txtProgressBar(max=nrow(URLs), style=3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    progress_bar <- list(progress=progress)

    # cycle trough all URLs to find entities that shared the same link within the coordination internal
    dat.summary <-
      foreach::foreach(i=seq(1:nrow(URLs)), .combine = dplyr::bind_rows, .packages="dplyr", .options.snow=progress_bar) %dopar% {

        # show progress...
        utils::setTxtProgressBar(pb, pb$getVal()+1)

        url <- URLs$URL[i]
        dat.summary <- subset(ct_shares.df, ct_shares.df$expanded==url)

        if (length(unique(dat.summary$account.url)) > 1) {
          dat.summary <- dat.summary %>%
            dplyr::mutate(cut = cut(as.POSIXct(date), breaks = coordination_interval)) %>%
            dplyr::group_by(cut) %>%
            dplyr::mutate(count=n(),
                   account.url=list(account.url),
                   share_date=list(date),
                   url = url) %>%
            dplyr::select(cut, count, account.url, share_date, url) %>%
            dplyr::filter(count > 1) %>%   # subset the URLs shared by more than one entity
            unique()

          return(dat.summary)
        }
      }

    parallel::stopCluster(cl)

    if(nrow(dat.summary)==0){
      stop("there are not enough shares!")
    }

    coordinated_shares <- tidyr::unnest(dat.summary, cols = c(account.url, share_date))

    rm(dat.summary, cores, cl, pb, progress, progress_bar)

    # mark the coordinated shares in the data set
    ct_shares.df$is_coordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                           ct_shares.df$date %in% coordinated_shares$share_date &
                                           ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)

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

    results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)

    return(results_list)
  }

  ###################
  # Non Parallel ####
  ###################

  if(parallel==FALSE){

    datalist <- list()

    # progress bar
    total <- nrow(URLs)
    pb <- txtProgressBar(max=total, style=3)

    for (i in 1:nrow(URLs)) {

      utils::setTxtProgressBar(pb, pb$getVal()+1)

      url <- URLs$URL[i]
      dat.summary <- subset(ct_shares.df, ct_shares.df$expanded==url)

      if (length(unique(dat.summary$account.url)) > 1) {
        dat.summary <- dat.summary %>%
          dplyr::mutate(cut = cut(as.POSIXct(date), breaks = coordination_interval)) %>%
          dplyr::group_by(cut) %>%
          dplyr::mutate(count=n(),
                 account.url=list(account.url),
                 share_date=list(date),
                 url = url) %>%
          dplyr::select(cut, count, account.url, share_date, url) %>%
          dplyr::filter(count > 1) %>%
          unique()

        datalist <- c(list(dat.summary), datalist)
        rm(dat.summary)
      }
    }

    df <- dplyr::bind_rows(datalist)

    if(nrow(df)==0){
      stop("there are not enough shares!")
    }

    coordinated_shares <- tidyr::unnest(df, cols = c(account.url, share_date))
    rm(datalist, df)

    # mark the coordinated shares in the data set
    ct_shares.df$is_coordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                           ct_shares.df$date %in% coordinated_shares$share_date &
                                           ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)

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

    results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)

    return(results_list)
  }
}
