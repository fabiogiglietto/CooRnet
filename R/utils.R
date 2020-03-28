clean_urls <- function(df, url){
  df <- df[!grepl("\\.\\.\\.$", df[[url]]),]
  length(df[[url]][grepl("\\.\\.\\.$", df[[url]])]) == 0

  df[[url]] <- gsub("\\?utm_.*", "", df[[url]])
  df[[url]] <- gsub("\\?ref.*", "", df[[url]])
  df[[url]] <- gsub("\\?fbclid.*", "", df[[url]])
  df[[url]] <- gsub("\\?rss.*", "", df[[url]])
  df[[url]] <- gsub("\\?ico.*", "", df[[url]])
  df[[url]] <- gsub("\\?recruiter.*", "", df[[url]])
  df[[url]] <- gsub("\\?sr_share_.*", "", df[[url]])
  df[[url]] <- gsub("\\?fb_rel.*", "", df[[url]])
  df[[url]] <- gsub("\\?social.*", "", df[[url]])
  df[[url]] <- gsub("\\?intcmp_.*", "", df[[url]])
  df[[url]] <- gsub("\\?xrs.*", "", df[[url]])
  df[[url]] <- gsub("\\?CMP.*", "", df[[url]])
  df[[url]] <- gsub("\\?tid.*", "", df[[url]])
  df[[url]] <- gsub("\\?ncid.*", "", df[[url]])
  df[[url]] <- gsub("&utm_.*", "", df[[url]])
  df[[url]] <- gsub("\\?ncid.*", "", df[[url]])
  df[[url]] <- gsub("\\?rbs&utm_hp_ref.*", "", df[[url]])
  df[[url]] <- gsub("/#\\..*", "", df[[url]])
  df[[url]] <- gsub("\\?mobile.*", "", df[[url]])
  df[[url]] <- gsub("&fbclid.*", "", df[[url]])
  df[[url]] <- gsub("/$", "", df[[url]])

  df[[url]] <- gsub(".*(http)", "\\1", df[[url]]) # delete all before "http"
  df[[url]][grepl("^http://127.0.0.1", df[[url]])] <- df[[url]][grepl("^http://127.0.0.1", df[[url]])]
  df <- df[grepl("http://|https://", df[[url]]),] # remove all the entries with the url that does not start with "http"

  return(df)
}

unnest_ctshares <- function(ct_shares.df, clean_urls=FALSE) {
  require(tidyr)      # 1.0.2
  
  # unnest expanded url and clean-up
  ct_shares.df <- unnest(ct_shares.df, cols = expandedLinks)
  ct_shares.df$original <- NULL
  
  # clean urls
  if(clean_urls==TRUE){
    ct_shares.df <- clean_urls(ct_shares.df, "expanded")
  }
  
  # remove duplicates created by the unnesting
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]
  return(ct_shares.df)
}
