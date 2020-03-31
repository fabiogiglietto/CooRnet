#' estimate_coord_interval
#'
#' This function estimates a threshold in seconds that defines a coordinated link share. While it is common that multiple (pages/groups/account) entities share the same link, some tend to perform these actions in an unusually short period of time. Unusual is thus defined here as a function of the median co-share time difference. More specifically, the function ranks all co-shares by time-difference from first share and focuses on the behaviour of the quickest second share performing q\% (default 0.5) URLs. The value returned is the median time in seconds spent by these URLs to cumulate the p\% (default 0.1) of their total shares
#'
#' @param ct_shares.df a tibble resulting from the function get_ctshares
#' @param q parameter that controls the quantile of quickest URLs to be filtered. Default to 0.1 [0-1]
#' @param p parameter that controls the percentage of total shares to be reached. Default to 0.5 [0-1]
#' @param clean_urls clean up unnecessary url paramters and malformed urls
#'
#' @return A list containing two objects: summary statistics of q\% quickest second share performing URLs, and a time in seconds corresponding to the median time spent by these URLs to cumulate the p\% of their total shares}
#' @examples
#' cord_int <- estimate_coord_interval(df, q=0.1, p=0.5, clean_urls=TRUE)
#' cord_int[[1]]
#' cord_int[[2]]
#'
#' @export

estimate_coord_interval <- function(ct_shares.df, q=0.1, p=0.5, clean_urls=FALSE) {

  if(p < 0 | p > 1){
    stop("The p value must be between 0 and 1")
  }

  if(q < 0 | q > 1){
    stop("The q value must be between 0 and 1")
  }

  require(tidyr)      # 1.0.2
  require(dplyr)      # 0.8.3

  # unnest expanded url and clean-up
  ct_shares.df <- unnest_ctshares(ct_shares.df, clean_urls = clean_urls)
  rm(clean_urls)

  # remove unnecessary columns
  ct_shares.df <- ct_shares.df[, c("id", "date", "expanded"),]

  # get a list of all shared URLs
  URLs <- as.data.frame(table(ct_shares.df$expanded))
  names(URLs) <- c("URL", "ct_shares")
  URLs <- subset(URLs, URLs$ct_shares>1) # remove URLs shared only 1 time
  URLs$URL <- as.character(URLs$URL)

  # keep only shares of URLs shared more than one time to reduce workload
  #  ct_shares.df_onetime <- subset(ct_shares.df, !(ct_shares.df$expanded %in% URLs$URL))  ??????????
  ct_shares.df <- subset(ct_shares.df, ct_shares.df$expanded %in% URLs$URL)

  ct_shares.df <- ct_shares.df[order(ct_shares.df$date),] # sort by date

  ranked_shares <- ct_shares.df %>%
    group_by(expanded) %>%
    mutate(ct_shares_count=n(),
           first_share_date = min(date),
           rank = rank(date, ties.method = "first"), # so as to calculate the metrics
           date = date,
           sec_from_first_share = difftime(date, first_share_date, units = "secs"),
           perc_of_shares = rank/ct_shares_count) %>%
    select(expanded, ct_shares_count, first_share_date, rank, date, sec_from_first_share, perc_of_shares) %>%
    arrange(expanded)

  # cleanup
  rm(ct_shares.df)

  # find URLs with an unusual fast second share and keep the quickest
  rank_2 <- ranked_shares %>%
    group_by(expanded) %>%
    filter(rank==2) %>%
    mutate(sec_from_first_share = min(sec_from_first_share)) %>%
    select(expanded, sec_from_first_share) %>%
    unique()

  rank_2 <- subset(rank_2, rank_2$sec_from_first_share <= as.numeric(quantile(rank_2$sec_from_first_share, q)))

  ranked_shares <- subset(ranked_shares, ranked_shares$expanded %in% rank_2$expanded) # keep only shares of quick URLs

  ranked_shares_sub <- ranked_shares %>%
    filter(perc_of_shares > p) %>%               # > ??
    mutate(sec_from_first_share = min(sec_from_first_share)) %>%
    select(expanded, sec_from_first_share) %>%
    unique()

  summary_secs <- summary(as.numeric(ranked_shares_sub$sec_from_first_share))
  coordination_interval <- paste0(quantile(ranked_shares_sub$sec_from_first_share, p), " secs")

  # cleanup
  rm(ranked_shares, rank_2, ranked_shares_sub)

  coord_interval <- list(summary_secs, coordination_interval)

  if (file.exists("log.txt")) {
    write(paste("\nq (quantile of quickest URLs to be filtered):", q,
                "\np (percentage of total shares to be reached):", p,
                "\ncoordination interval from estimate_coord_interval:", coordination_interval), file="log.txt", append=TRUE)
  } else {
    write(paste("#################### CLSB - LOG FILE #####################\n",
                "\nq (quantile of quickest URLs to be filtered):", q,
                "\np (percentage of total shares to be reached):", p,
                "\ncoordination interval from estimate_coord_interval:", coordination_interval), file="log.txt")
  }

  return(coord_interval)
}
