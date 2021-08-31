#' @importFrom stringr str_replace
#' @importFrom urltools url_decode

clean_urls <- function(df, url){

  df <- df[!grepl("\\.\\.\\.$", df[[url]]),]
  df <- df[!grepl("/url?sa=t&source=web", df[[url]], fixed=TRUE),]

  paramters_to_clean <- paste("\\?utm_.*",
                              "feed_id.*",
                              "&_unique_id.*",
                              "\\?#.*",
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
                              ")",
                              "/$",
                              sep = "|")

  df[[url]] <- gsub(paramters_to_clean, "", df[[url]])
  df[[url]] <- gsub(paramters_to_clean, "", df[[url]])
  df[[url]] <- gsub(paramters_to_clean, "", df[[url]])

  df[[url]] <- gsub(".*(http)", "\\1", df[[url]]) # delete all before "http"
  df[[url]] <- gsub("\\/$", "", df[[url]]) # delete remaining trailing slash
  df[[url]] <- gsub("\\&$", "", df[[url]]) # delete remaining trailing &

  filter_urls <- c("^http://127.0.0.1", "https://www.youtube.com/watch", "https://www.youtube.com/", "http://www.youtube.com/",
                   "https://youtu.be", "https://m.youtube.com",
                   "https://m.facebook.com/story", "https://m.facebook.com/", "https://www.facebook.com/",
                   "https://chat.whatsapp.com", "http://chat.whatsapp.com", "http://wa.me", "https://wa.me",
                   "https://api.whatsapp.com/send", "https://api.whatsapp.com/", "https://play.google.com/store/apps/details",
                   "https://www.twitter.com/")

  df <- df[!grepl(paste(filter_urls, collapse = "|"), df[[url]]), ]

  df[[url]] <- urltools::url_decode(stringr::str_replace(df[[url]], 'https://www.facebook.com/login/?next=', ''))
  df <- df[grepl("http://|https://", df[[url]]),] # remove all the entries with the url that does not start with "http"

  df[[url]] <- stringr::str_replace(df[[url]], 'm.youtube.com', 'www.youtube.com')
  df[[url]] <- stringr::str_replace(df[[url]], 'youtu.be/', 'www.youtube.com/watch?v=')
  df[[url]] <- stringr::str_replace(df[[url]], '^(.*youtube\\.com/watch\\?).*(v=[^\\&]*).*', '\\1\\2') # cleanup YouTube URLs

  return(df)
}
