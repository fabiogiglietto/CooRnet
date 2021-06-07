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
#' @importFrom httr GET content http_type status_code
#' @importFrom jsonlite fromJSON
#'

query_link_enpoint <- function(query.string, sleep_time=30) {

  resp <- httr::GET(URLencode(query.string))

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

 status <- httr::status_code(resp)

 tryCatch(
   {
     if (status == 200) {
       response.json <- httr::content(resp, as = "text", type="application/json", encoding = "UTF-8")
       response <- jsonlite::fromJSON(response.json, flatten = TRUE)
       return(response)
      }
     else if (status == 429)
       {
       print("API rate limit hit, sleeping...")
       Sys.sleep(sleep_time)
       }
      else if (status == 401)
      {
       stop("Unauthorized, please check your token...")
      }
     else if (status == 400)
     {
       response.json <- httr::content(resp, as = "text", type="application/json", encoding = "UTF-8")
       response <- jsonlite::fromJSON(response.json, flatten = TRUE)
       print(paste(status, ":", response$message, "on call:", resp$url))
       write(paste(status, ":", response$message, "on call:", resp$url,) file = "log.txt", append = TRUE)
     }
      else
        {
          print(paste(response$status, resp$url))
          write(paste("Unexpected http response code", response$status, "on call ", resp$url), file = "log.txt", append = TRUE)
        }
   },
          error=function(cond) {
            print(paste("Error:", message(cond), "on call:", resp$url))
            write(paste("Error:", message(cond), "on call:", resp$url), file = "log.txt", append = TRUE)
          })
 }
