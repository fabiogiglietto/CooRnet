estimate_coord_interval <- function(df, q=0.1, p=0.5) {

  if(p < 0 | p > 1){
    stop("The p value must be between 0 and 1")
  }

  if(q < 0 | q > 1){
    stop("The q value must be between 0 and 1")
  }

  require(tidyr)      # 1.0.2
  require(dplyr)      # 0.8.3

  # unnest expanded url and clean-up
  ct_shares.df <- unnest(df, cols = expandedLinks)
  ct_shares.df$original <- NULL

  # remove duplicates created by the unnesting
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]

  # get a list of all shared URLs
  URLs <- as.data.frame(table(ct_shares.df$expanded))
  names(URLs) <- c("URL", "ct_shares")
  URLs <- subset(URLs, URLs$ct_shares>1) # remove URLs shared only 1 time
  URLs$URL <- as.character(URLs$URL)

  # keep only shares of URLs shared more than one time to reduce workload
  ct_shares.df_onetime <- subset(ct_shares.df, !(ct_shares.df$expanded %in% URLs$URL))
  ct_shares.df <- subset(ct_shares.df, ct_shares.df$expanded %in% URLs$URL)

  ct_shares.df <- ct_shares.df[order(ct_shares.df$date),] # sort by date

  ranked_shares <- ct_shares.df %>%
    group_by(expanded) %>%
    mutate(ct_shares_count=n(),
           first_share_date = min(date),
           rank = rank(date),
           date = date) %>%
    select(expanded, ct_shares_count, first_share_date, rank, date)

  ranked_shares$sec_from_first_share <- difftime(ranked_shares$date,ranked_shares$first_share_date, units='secs') # seconds from first share
  ranked_shares$perc_of_shares <- ranked_shares$rank/ranked_shares$ct_shares_count # percentage of shares over total shares
  ranked_shares <- subset(ranked_shares, ranked_shares$ct_shares_count > 1) # remove URLs wiht less than 2 shares (can't be coordinated)

  rank_2 <- ranked_shares %>%
    filter(rank==2) %>%
    group_by(expanded) %>%
    mutate(sec_from_first_share = min(sec_from_first_share)) %>%
    select(expanded, sec_from_first_share)

  rank_2 <- subset(rank_2, rank_2$sec_from_first_share <= as.numeric(quantile(rank_2$sec_from_first_share, q))) # keep the quickest URLs

  ranked_shares <- subset(ranked_shares, ranked_shares$expanded %in% rank_2$expanded) # keep only shares of quick URLs

  ranked_shares_sub <- ranked_shares %>%
    filter(perc_of_shares > p) %>%
    group_by(expanded) %>%
    mutate(sec_from_first_share = min(sec_from_first_share)) %>%
    select(expanded, sec_from_first_share) %>%
    unique()

  summary_secs <- summary(as.numeric(ranked_shares_sub$sec_from_first_share))
  coordination_interval <- paste0(quantile(ranked_shares_sub$sec_from_first_share, p), " secs")

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
