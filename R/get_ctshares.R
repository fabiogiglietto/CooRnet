#' get_ctshares
#'
#' A function to get the URLs shares from CrowdTangle
#'
#' @param urls a dataframe with at least a column "url" containing the URLs, and a column "date" with their published date
#' @param url_column name of the column (placed inside quote marks) where the URLs are stored
#' @param date_column name of the column (placed inside quote marks) where the date of the URLs are stored
#' @param platforms default to "facebook,instagram". You can specify only "facebook" to search on Facebook, or only "instagram" to search on Instagram}
#' @param nmax max number of results for query (default 500 as per API limit)
#' @param sleep_time pause between queries to respect API rate limits. Default to 20 secs, it can be lowered or increased depending on the assigned API rate limit
#' @param clean_urls clean the URLs from tracking parameters (default FALSE)
#'
#' @return A tibble (tbl_df) containing the entities that shared the URLs and a number of variables returned by the \href{https://github.com/CrowdTangle/API/wiki/Links}{CrowdTangle API links endpoint}
#'
#' @details To start using the library you need to set the CrowdTangle API key.
#'   Open the environment variable file with file.edit("~/.Renviron"), write CROWDTANGLE_API_KEY = <YOUR_API_KEY>, save the file and restart your current R session to start using the CrowdTangle API
#'
#' @example
#'   df <- get_ctshares(urls, url_column=“url”, date_column=“date”, platforms="facebook,instagram", nmax=100, sleep_time=20, clean_urls=FALSE)
#'
#' @export

get_ctshares <- function(urls, url_column, date_column, platforms="facebook,instagram", nmax=500, sleep_time=20, clean_urls=FALSE) {

  require(httr)      # 1.4.1
  require(jsonlite)  # 1.6.9
  require(tidyr)     # 1.0.2

  # remove duplicated rows
  urls <- urls[!duplicated(urls),]

  # set column names
  colnames(urls)[colnames(urls)==url_column] <- "url"
  colnames(urls)[colnames(urls)==date_column] <- "date"

  # clean urls
  if(clean_urls==TRUE){
    urls <- clean_urls(urls, "url")
  }

  # create empty object to store results
  ct_shares.df <- NULL

  # set progress bar
  total <- nrow(urls)
  pb <- txtProgressBar(min = 0, max = total, width = 100, style = 3)

  # query the API
  for (i in 1:nrow(urls)) {

    # show progress...
    setTxtProgressBar(pb, pb$getVal()+1)

    # set date limits
    startDate <- as.POSIXct(urls$date[i], origin="1970-01-01", tz = "UTC")
    endDate <- startDate+604800 # one week after date_published

    link <- urls$url[i]

    query <- GET("https://api.crowdtangle.com/links",
                 query = list(
                   link = link,
                   platforms = platforms,
                   startDate  = gsub(" ", "T", as.character(startDate)),
                   endDate  = gsub(" ", "T", as.character(endDate)),
                   includeSummary = "false",
                   sortBy = "date",
                   token = Sys.getenv("CROWDTANGLE_API_KEY"),
                   count = nmax))


    tryCatch(
      {
        json <- httr::content(query, as = "text", encoding = "UTF-8")
        c <- fromJSON(json, flatten = TRUE)
        if (c$status == 200) {
          if (length(c$result$posts) > 0) {
            ct_shares.df <- rbind_pages(list(ct_shares.df, c$result$posts))
          }
        }
        else {
          print(paste(c$status, i))
        }
      },
      error=function(cond) {
        print(paste("Error:", message(cond), "on URL:", i))
      },
      finally={
        Sys.sleep(sleep_time)
      })
  }

  # remove possible inconsistent rows with entity URL "https://facebook.com/null"
  ct_shares.df <- ct_shares.df[ct_shares.df$account.url!="https://facebook.com/null",]

  # remove duplicated rows
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df),]

  write(paste("#################### CLSB - LOG FILE #####################",
              "\nnumber of URLs:", nrow(urls),
              "\nnumber of shares:", nrow(ct_shares.df)),
        file="log.txt")


  return(ct_shares.df)

}

