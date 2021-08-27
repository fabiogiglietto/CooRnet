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
      httr::RETRY(verb = "GET", url = query.string, times=3, terminate_on=c(401), pause_base=sleep_time, pause_cap=60)
    },
    error=function(cond) {
      print(paste(cond, "on call:", query.string))
      write(paste(cond, "on call:", query.string), file = "log.txt", append = TRUE)
      return(NA)
    }
  )

  if (httr::status_code(resp) == 401) {
    stop(
      print("Unauthorized, please check your API token..."), call. = FALSE
    )
  }

  if (httr::status_code(resp) == 429) {
      print("API rate limit hit, sleeping...")
      write(paste("API rate limit hit on call:", resp$url), file = "log.txt", append = TRUE)
      Sys.sleep(sleep_time)
      return(NA)
  }

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  response.json <- httr::content(resp, as = "text", type="application/json", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(response.json, flatten = TRUE)
  return(parsed)
}
