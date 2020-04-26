clean_urls <- function(df, url){

  df <- df[!grepl("\\.\\.\\.$", df[[url]]),]

  paramters_to_clean <- paste0(c("\\?utm_.*",
                                 "\\?ref.*",
                                 "\\?fbclid.*",
                                 "\\?rss.*",
                                 "\\?ico.*",
                                 "\\?recruiter.*",
                                 "\\?sr_share_.*",
                                 "\\?fb_rel.*",
                                 "\\?social.*",
                                 "\\?intcmp_.*",
                                 "\\?xrs.*",
                                 "\\?CMP.*",
                                 "\\?tid.*",
                                 "\\?ncid.*",
                                 "&utm_.*",
                                 "\\?rbs&utm_hp_ref.*",
                                 "/#\\..*",
                                 "\\?mobile.*",
                                 "&fbclid.*",
                                 "/$",
                                 collapse = "|"))

  df[[url]] <- gsub(paramters_to_clean, "", df[[url]])

  df[[url]] <- gsub(".*(http)", "\\1", df[[url]]) # delete all before "http"
  df <- df[!grepl("^http://127.0.0.1", df[[url]]), ]
  df <- df[grepl("http://|https://", df[[url]]),] # remove all the entries with the url that does not start with "http"

  return(df)
}
