#' estimate_coord_interval
#'
#' This function estimates a threshold in seconds that defines a coordinated link share. While it is common that multiple (pages/groups/account) entities share the same link, some tend to perform these actions in an unusually short period of time. Unusual is thus defined here as a function of the median co-share time difference. More specifically, the function ranks all co-shares by time-difference from first share and focuses on the behaviour of the quickest second share performing q\% (default 0.5) URLs. The value returned is the median time in seconds spent by these URLs to cumulate the p\% (default 0.1) of their total shares
#'
#' @param ct_shares.df a dataframe containing the Crowdtangle shares after running get_ctshares.R
#' @param mongo_database string: the name of the MongoDB used to host the collections.
#' @param mongo_url string: address of the MongoDB server in standard URI Format. Set to NULL to avoid using mongo (default NULL)
#' @param q parameter that controls the quantile of quickest URLs to be filtered. Default to 0.1 [0-1]
#' @param p parameter that controls the percentage of total shares to be reached. Default to 0.5 [0-1]
#' @param clean_urls clean up unnecessary url paramters and malformed urls, and keep just the URLs included in the original data set (default FALSE)
#' @param mongo_cluster logical: set to TRUE if you are using a MongoDB cluster instead of standalone instance (default FALSE)
#' @param keep_ourl_only restrict the analysis to ct shares links matching the original URLs (default=FALSE)
#'
#' @return a list containing two objects: summary statistics of q\% quickest second share performing URLs, and a time in seconds corresponding to the median time spent by these URLs to cumulate the p\% of their total shares
#' @examples
#' cord_int <- estimate_coord_interval(ct_shares.df = ct_shares.df, q=0.1, p=0.5, clean_urls=TRUE, keep_ourl_only=FALSE)
#' cord_int[[1]]
#' cord_int[[2]]
#'
#' @importFrom dplyr group_by mutate select arrange filter
#'
#' @export

estimate_coord_interval <- function(ct_shares.df=NULL,
                                    mongo_database,
                                    mongo_url=NULL,
                                    q=0.1,
                                    p=0.5,
                                    clean_urls=FALSE,
                                    mongo_cluster=TRUE,
                                    keep_ourl_only=FALSE) {

  if(p < 0 | p > 1){
    stop("The p value must be between 0 and 1")
  }

  if(q < 0 | q > 1){
    stop("The q value must be between 0 and 1")
  }

  # initialize logfile
  if (!file.exists("log.txt")) {
    write(paste("#################### CooRnet #####################",
                "\nestimate_coord_interval script executed on:", format(Sys.time(), format = "%F %R %Z")),
          file="log.txt")
  }
  else {
    write(paste("\nestimate_coord_interval script executed on:", format(Sys.time(), format = "%F %R %Z")),
          file="log.txt", append = TRUE)
  }

  if(is.null(ct_shares.df)){
    if(is.null(mongo_url)) stop("Please provide the address of the MongoDB server used to store the posts that shared your URLs")
    ct_shares_mdb <-  connect_mongodb_cluster("shares_info", mongo_database, mongo_url, mongo_cluster)
    # Check if ct_shares_mdb already existed. Otherwise the function will be closed since no available database already exists
    if (ct_shares_mdb$count() == 0) stop("Please provide a name of an already existing mongoDB database. To do so, use get_ctshares function before calling this function.")

    # Retrieve all the database within the specified collection
    ct_shares.df <- ct_shares_mdb$find('{}')
  }

  # Clean urls
  if(clean_urls==TRUE){
    ct_shares.df <- clean_urls(ct_shares.df, "expanded")
    write("Coordination interval estimated on cleaned URLs", file = "log.txt", append = TRUE)
  }

  ct_shares.df <- ct_shares.df[, c("platformId", "date", "expanded"),]

  # Get a list of all shared URLs
  URLs_mdb <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$expanded", "freq": {"$sum":1}}},{"$match": {"freq": {"$gt": 1}}}]',options = '{"allowDiskUse":true}'))
  names(URLs_mdb) <- c("URL", "ct_shares")

  # keep original URLs only?
  if(keep_ourl_only==TRUE){
    # ct_shares.df <- subset(ct_shares.df, ct_shares.df$is_orig==TRUE)
    # if (nrow(ct_shares.df) < 2) stop("Can't execute with keep_ourl_only=TRUE. Not enough posts matching original URLs")
    URLs_original <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$expanded", "freq": {"$sum":1}}},{"$match": {"is_orig": true}}]',options = '{"allowDiskUse":true}'))
    names(URLs_original) <- c("URL", "ct_shares")

    if (nrow(URLs_original) < 2) stop("Can't execute with keep_ourl_only=TRUE. Not enough posts matching original URLs")
    else URLs_mdb <- subset(URLs_mdb, URLs_mdb$URL %in% URLs_original$URL)
    write("Coordination interval estimated on shares matching original URLs", file = "log.txt", append = TRUE)
  }

  URLs_list <- URLs_mdb$URL

  ct_shares.df <- subset(ct_shares.df, ct_shares.df$expanded %in% URLs_list)

  ranked_shares <- ct_shares.df %>%
    dplyr::group_by(expanded) %>%
    dplyr::mutate(ct_shares_count=n(),
           first_share_date = min(date),
           rank = rank(date, ties.method = "first"),
           date = date,
           sec_from_first_share = difftime(date, first_share_date, units = "secs"),
           perc_of_shares = rank/ct_shares_count) %>%
    dplyr::select(expanded, ct_shares_count, first_share_date, rank, date, sec_from_first_share, perc_of_shares) %>%
    dplyr::arrange(expanded)

  rm(ct_shares.df)

  # find URLs with an unusual fast second share and keep the quickest
  rank_2 <- ranked_shares %>%
    dplyr::group_by(expanded) %>%
    dplyr::filter(rank==2) %>%
    dplyr::mutate(sec_from_first_share = min(sec_from_first_share)) %>%
    dplyr::select(expanded, sec_from_first_share) %>%
    unique()

  rank_2 <- subset(rank_2, rank_2$sec_from_first_share <= as.numeric(quantile(rank_2$sec_from_first_share, q)))

  # keep only the quickest URLs's shares
  ranked_shares <- subset(ranked_shares, ranked_shares$expanded %in% rank_2$expanded)

  ranked_shares_sub <- ranked_shares %>%
    dplyr::filter(perc_of_shares > p) %>%
    dplyr::mutate(sec_from_first_share = min(sec_from_first_share)) %>%
    dplyr::select(expanded, sec_from_first_share) %>%
    unique()

  summary_secs <- summary(as.numeric(ranked_shares_sub$sec_from_first_share))
  coordination_interval <- paste0(quantile(ranked_shares_sub$sec_from_first_share, p), " secs")

  # get results in case of median equal 0 secs
  if(coordination_interval == "0 secs") {
    coordination_interval <- "1 secs"

    coord_interval <- list(summary_secs, coordination_interval)

    write(paste0("\n",
                  "\nq (quantile of quickest URLs to be filtered): ", q,
                  "\np (percentage of total shares to be reached): ", p,
                  "\ncoordination interval from estimate_coord_interval: ", coordination_interval,
                  "\nWarning: with the specified parameters p and q the median was 0 secs. The coordination interval has been automatically set to 1 secs"), file="log.txt", append=TRUE)

    return(coord_interval)
  }

  else {
    # get results in case of median > 0 secs
    coord_interval <- list(summary_secs, coordination_interval)

    write(paste0("\nq (quantile of quickest URLs to be filtered): ", q,
                 "\np (percentage of total shares to be reached): ", p,
                 "\ncoordination interval from estimate_coord_interval: ", coordination_interval),
        file="log.txt",
        append=TRUE)

  return(coord_interval)
  }
}
