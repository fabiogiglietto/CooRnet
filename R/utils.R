unnest_ctshares <- function(ct_shares.df) {
  require(tidyr)      # 1.0.2
  
  # unnest expanded url and clean-up
  ct_shares.df <- unnest(ct_shares.df, cols = expandedLinks)
  ct_shares.df$original <- NULL
  
  # remove duplicates created by the unnesting
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]
}
