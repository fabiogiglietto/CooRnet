#' get_ctshares
#'
#' A function to get the URLs shares from CrowdTangle
#'
#' @param urls a dataframe with at least a column "url" containing the URLs, and a column "date" with their published date
#' @param url_column name of the column (placed inside quote marks) where the URLs are stored
#' @param date_column name of the column (placed inside quote marks) where the date of the URLs are stored
#' @param platforms default to "facebook,instagram". You can specify only "facebook" to search on Facebook, or only "instagram" to search on Instagram
#' @param nmax max number of results for query (default 500 as per API limit)
#' @param sleep_time pause between queries to respect API rate limits. Default to 20 secs, it can be lowered or increased depending on the assigned API rate limit
#' @param clean_urls clean the URLs from tracking parameters (default FALSE)
#' @param save_ctapi_output saves the original CT API output in ./rawdata/ct_shares.df.0.rds
#'
#' @return a data.frame of posts that shared the URLs and a number of variables returned by the \href{https://github.com/CrowdTangle/API/wiki/Links}{CrowdTangle API links endpoint} and the original data set of news
#'
#' @details to start using the library you need to set the CrowdTangle API key.
#'   Open the environment variable file with file.edit("~/.Renviron"), write CROWDTANGLE_API_KEY = <YOUR_API_KEY>, save the file and restart your current R session to start using the CrowdTangle API
#'
#' @examples
#'   df <- get_ctshares(urls, url_column=“url”, date_column=“date”, platforms="facebook,instagram", nmax=100, sleep_time=20, clean_urls=FALSE, save_ctapi_output=FALSE)
#'
#' @export

get_ctshares <- function(urls, url_column, date_column, platforms="facebook,instagram", nmax=500, sleep_time=20, clean_urls=FALSE, save_ctapi_output=FALSE) {

  require(httr)      # 1.4.1
  require(jsonlite)  # 1.6.9
  require(tidyr)     # 1.0.2
  require(dplyr)     # 0.8.3

  if(missing(url_column)) {
    stop("Please check that specified column exists")
  }

  if(missing(date_column)) {
    stop("Please check that specified column exists")
  }


  # initialize logfile
  write(paste("#################### CooRnet #####################",
              "\nget_ctshares script executed on:", format(Sys.time(), format = "%F %R %Z")),
        file="log.txt")

  # remove duplicated rows
  urls <- urls[!duplicated(urls),]

  # set column names
  colnames(urls)[colnames(urls)==url_column] <- "url"
  colnames(urls)[colnames(urls)==date_column] <- "date"

  # clean the URLs
  if(clean_urls==TRUE){
    urls <- clean_urls(urls, "url")
    write("Original URLs have been cleaned", file = "log.txt", append = TRUE)
  }

  ct_shares.df <- NULL

  # progress bar
  total <- nrow(urls)
  pb <- txtProgressBar(min = 0, max = total, width = 100, style = 3)

  # query the CrowdTangle API
  for (i in 1:nrow(urls)) {

    setTxtProgressBar(pb, pb$getVal()+1)

    # set date limits: one week after date_published
    startDate <- as.POSIXct(urls$date[i], origin="1970-01-01", tz = "UTC")
    endDate <- startDate+604800

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
          write(paste("Unexpected http response code", c$status, "on url with id", i), file = "log.txt", append = TRUE)
        }
      },
      error=function(cond) {
        print(paste("Error:", message(cond), "on URL:", i))
        write(paste("Error:", message(cond), "on URL:", i), file = "log.txt", append = TRUE)
      },
      finally={
        Sys.sleep(sleep_time)
      })
  }

  # save original API output
  if(save_ctapi_output==TRUE){
    suppressWarnings(dir.create("./rawdata"))
    saveRDS(ct_shares.df, "./rawdata/ct_shares.df.0.rds")
  }

  if (is.null(ct_shares.df)){
    stop("\nNo ct_shares were found!")
  }

  # remove possible inconsistent rows with entity URL equal "https://facebook.com/null"
  ct_shares.df <- ct_shares.df[ct_shares.df$account.url!="https://facebook.com/null",]

  # get rid of duplicates
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df),]

  ct_shares.df <- unnest(ct_shares.df, cols = expandedLinks)
  ct_shares.df$original <- NULL

  # remove duplicates created by the unnesting
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]

  # remove shares performed more than one week from first share
  ct_shares.df <- ct_shares.df %>%
    group_by(expanded) %>%
    filter(difftime(max(date), min(date), units = "secs") <= 604800)

  ct_shares.df$is_orig <- ct_shares.df$expanded %in% urls$url

  # write log
  write(paste("Original URLs:", nrow(urls),
              "\nCT shares:", nrow(ct_shares.df),
              "\nUnique URLs in CT shares:", length(unique(ct_shares.df$expanded)),
              "\nLink in CT shares matching original URLs:", as.numeric(table(ct_shares.df$account.verified)["TRUE"])),
        file = "log.txt",
        append = TRUE)

  rm(urls)

  return(ct_shares.df)
}
