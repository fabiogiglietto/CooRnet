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
