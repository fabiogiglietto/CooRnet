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

  # filter_urls <- c("^http://127.0.0.1", "^https://www.youtube.com/watch$", "^https://www.youtube.com/$", "^http://www.youtube.com/$",
  #                  "^https://youtu.be$", "^https://m.youtube.com$", "^https://m.facebook.com/story",
  #                  "^https://m.facebook.com/$", "^https://www.facebook.com/$", "^https://chat.whatsapp.com$",
  #                  "^http://chat.whatsapp.com$", "^http://wa.me$", "^https://wa.me$", "^https://api.whatsapp.com/send$",
  #                  "^https://api.whatsapp.com/$", "^https://play.google.com/store/apps/details$", "^https://www.twitter.com/$", "^https://www.twitter.com$",
  #                  "^https://instagram.com/accounts/login", "^https://www.instagram.com/accounts/login", "^https://t.me/joinchat$")

  filter_urls <- c("^http://127.0.0.1", "^https://chat.whatsapp.com$",
                   "^http://chat.whatsapp.com$", "^http://wa.me$", "^https://wa.me$", "^https://api.whatsapp.com/send$",
                   "^https://api.whatsapp.com/$", "^https://play.google.com/store/apps/details$",
                   "^https://instagram.com/accounts/login", "^https://www.instagram.com/accounts/login", "^https://t.me/joinchat$")

  df <- df[!grepl(paste(filter_urls, collapse = "|"), df[[url]]), ]

  df[[url]] <- urltools::url_decode(stringr::str_replace(df[[url]], 'https://www.facebook.com/login/?next=', ''))
  df <- df[grepl("http://|https://", df[[url]]),] # remove all the entries with the url that does not start with "http"

  df[[url]] <- stringr::str_replace(df[[url]], 'm.youtube.com', 'www.youtube.com')
  df[[url]] <- stringr::str_replace(df[[url]], 'youtu.be/', 'www.youtube.com/watch?v=')
  df[[url]] <- stringr::str_replace(df[[url]], '^(.*youtube\\.com/watch\\?).*(v=[^\\&]*).*', '\\1\\2') # cleanup YouTube URLs

  return(df)
}

#' query_link_enpoint
#'
#' A wrapper for CrowdTangle API Links Endpoint. Returns a dataframe of posts matching the given URL
#'
#' @param query.string a well formed query string for the link endpoint
#' @param sleep_time
#'
#' @return a data.frame of posts matching the given URL
#'
#' @details to start using the library you need to set the CrowdTangle API key.
#'   Open the environment variable file with file.edit("~/.Renviron"), write CROWDTANGLE_API_KEY = <YOUR_API_KEY>, save the file and restart your current R session to start using the CrowdTangle API
#'
#' @importFrom httr RETRY content http_type status_code
#' @importFrom jsonlite fromJSON
#'

query_link_enpoint <- function(query.string, sleep_time=10) {
  resp <- tryCatch(
    {
      httr::RETRY(verb = "GET", url = query.string, times=3, terminate_on=c(401), pause_base=sleep_time, pause_cap=10, pause_min=sleep_time)
    },
    error=function(cond) {
      print(paste(cond, "on call:", query.string))
      write(paste("\n", cond, "on call:", query.string), file = "log.txt", append = TRUE)
      return(NA)
    }
  )

  status <- httr::status_code(resp)

  tryCatch(
    {
      if (status == 200L) {

        if (httr::http_type(resp) != "application/json") {
          stop("API did not return json", call. = FALSE)
        }

        response.json <- httr::content(resp, as = "text", type="application/json", encoding = "UTF-8")
        parsed <- jsonlite::fromJSON(response.json, flatten = TRUE)
        return(parsed)
      }
      else if (status == 429L)
      {
        message("API rate limit hit, sleeping...")
        write(paste("API rate limit hit on call:", resp$url), file = "log.txt", append = TRUE)
        Sys.sleep(sleep_time)
        return(NA)
      }
      else if (status == 401L)
      {
        stop("Unauthorized, please check your API token...", call. = FALSE)
      }
      else
      {
        message(paste(resp$status, resp$url))
        write(paste("Unexpected http response code", resp$status, "on call ", resp$url), file = "log.txt", append = TRUE)
        return(NA)
      }
    },
    error=function(cond) {
      write(paste("Error:", message(cond), "on call:", resp$url), file = "log.txt", append = TRUE)
      return(NA)
    })
}
