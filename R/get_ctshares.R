#' get_ctshares
#'
#' A function to get the URLs shares from CrowdTangle
#'
#' @param urls a dataframe with at least a column "url" containing the URLs, and a column "date" with their published date
#' @param url_column name of the column (placed inside quote marks) where the URLs are stored (defaults to "url")
#' @param date_column name of the column (placed inside quote marks) where the date of the URLs are stored (defaults to "date")
#' @param platforms default to "facebook,instagram". You can specify only "facebook" to search on Facebook, or only "instagram" to search on Instagram
#' @param nmax max number of results for query (default 1000 as per
#' @param sleep_time pause between queries to respect API rate limits. Default to 30 secs, it can be lowered or increased depending on the assigned \href{https://help.crowdtangle.com/en/articles/3443476-api-cheat-sheet}{API rate limit}.
#' @param clean_urls clean the URLs from tracking parameters (default FALSE)
#' @param save_ctapi_output saves the original CT API output in ./rawdata/ct_shares.df.0.rds
#'
#' @return a data.frame of posts that shared the URLs and a number of variables returned by the \href{https://github.com/CrowdTangle/API/wiki/Links}{CrowdTangle API links endpoint} and the original data set of news
#'
#' @details to start using the library you need to set the CrowdTangle API key.
#'   Open the environment variable file with file.edit("~/.Renviron"), write CROWDTANGLE_API_KEY = <YOUR_API_KEY>, save the file and restart your current R session to start using the CrowdTangle API
#'
#' @examples
#'   df <- get_ctshares(urls, url_column=“url”, date_column=“date”, platforms="facebook,instagram", nmax=100, sleep_time=30, clean_urls=FALSE, save_ctapi_output=FALSE)
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr group_by filter %>%
#' @importFrom utils setTxtProgressBar txtProgressBar menu
#' @importFrom tidytable unnest. bind_rows.
#' @importFrom urltools url_encode
#'
#' @export

get_ctshares <- function(urls, url_column, date_column, platforms="facebook,instagram", nmax=1000, sleep_time=30, clean_urls=FALSE, save_ctapi_output=FALSE) {

  if(missing(url_column)) {
    url_column = "url"
  }

  if(missing(date_column)) {
    date_column = "date"
  }

  if(!url_column %in% colnames(urls))
  {
    stop(paste0("Can't find '", url_column, "' in urls dataframe" ))
  }

  if(!date_column %in% colnames(urls))
  {
    stop(paste0("Can't find '", date_column, "' in urls dataframe" ))
  }

  # initialize logfile
  write(paste("#################### CooRnet #####################",
              "\nget_ctshares script executed on:", format(Sys.time(), format = "%F %R %Z")),
        file="log.txt")

  # set column names
  colnames(urls)[colnames(urls)==url_column] <- "url"
  colnames(urls)[colnames(urls)==date_column] <- "date"

  # in case of duplicated urls, keeps only the one with the oldest date
  urls <- urls %>%
    group_by(url) %>%
    summarise(date = min(date)) %>%
    select(url, date)

  # clean the URLs
  if(clean_urls==TRUE){
    urls <- clean_urls(urls, "url")

    # in case of duplicated urls, keeps only the one with the oldest date
    urls <- urls %>%
      group_by(url) %>%
      summarise(date = min(date)) %>%
      select(url, date)

    write("Original URLs have been cleaned", file = "log.txt", append = TRUE)
  }

  ct_shares.df <- NULL
  datalist <- list()

  # progress bar
  total <- nrow(urls)
  pb <- utils::txtProgressBar(min = 0, max = total, width = 100, style = 3)

  # query the CrowdTangle API
  for (i in 1:nrow(urls)) {

    url_ct_shares.df <- NULL
    url_datalist <- list()

    utils::setTxtProgressBar(pb, pb$getVal()+1)

    # set date limits: one week after date_published
    startDate <- as.POSIXct(urls$date[i], origin="1970-01-01", tz = "UTC")
    endDate <- startDate+604800

    link <- urls$url[i]

    # build the querystring
    query.string <- paste0("https://api.crowdtangle.com/links?",
                           "link=", urltools::url_encode(url=link),
                           "&platforms=", platforms,
                           "&startDate=", gsub(" ", "T", as.character(startDate)),
                           "&endDate=", gsub(" ", "T", as.character(endDate)),
                           "&includeSummary=FALSE",
                           "&includeHistory=TRUE",
                           "&sortBy=date",
                           "&searchField=TEXT_FIELDS_AND_IMAGE_TEXT",
                           "&token=", Sys.getenv("CROWDTANGLE_API_KEY"),
                           "&count=", nmax)

    c <- query_link_enpoint(query.string, sleep_time)

    if (any(!is.na(c))) { # check if the call failed returning NA

      if (length(c$result$posts) != 0) {

        url_datalist <- c(list(c$result$posts), url_datalist)

        # paginate
        counter <- 1L
        while (counter <= 10 & !is.null(c$result$pagination$nextPage)) # stop after 10 iterations
        {
          c <- query_link_enpoint(c$result$pagination$nextPage, sleep_time)
          counter <- sum(counter, 1)

          if (any(!is.na(c))) {
            url_datalist <- c(list(c$result$posts), url_datalist)
          }
          else break}
      }

      if (length(url_datalist) != 0) {
        url_ct_shares.df <- tidytable::bind_rows.(url_datalist)
      }
      else {
        url_ct_shares.df <- NULL
      }

      if (!is.null(url_ct_shares.df)) {
        # keep only fields actually used by CooRnet
        url_ct_shares.df <- url_ct_shares.df %>%
          dplyr::select_if(names(.) %in% c("platformId",
                                           "platform",
                                           "date",
                                           "type",
                                           "expandedLinks",
                                           "description",
                                           "postUrl",
                                           "history",
                                           "id",
                                           "message",
                                           "title",
                                           "statistics.actual.likeCount",
                                           "statistics.actual.shareCount",
                                           "statistics.actual.commentCount",
                                           "statistics.actual.loveCount",
                                           "statistics.actual.wowCount",
                                           "statistics.actual.hahaCount",
                                           "statistics.actual.sadCount",
                                           "statistics.actual.angryCount",
                                           "statistics.actual.thankfulCount",
                                           "statistics.actual.careCount",
                                           "account.id",
                                           "account.name",
                                           "account.handle",
                                           "account.subscriberCount",
                                           "account.url",
                                           "account.platform",
                                           "account.platformId",
                                           "account.accountType",
                                           "account.pageCategory",
                                           "account.pageAdminTopCountry",
                                           "account.pageDescription",
                                           "account.pageCreatedDate",
                                           "account.verified"))

        datalist <- c(list(url_ct_shares.df), datalist)

      }
      rm(url_ct_shares.df, url_datalist, c)
    }
  }

  ct_shares.df <- tidytable::bind_rows.(datalist)
  rm(datalist)

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

  ct_shares.df <- tidytable::unnest.(ct_shares.df, expandedLinks, .drop = FALSE)
  ct_shares.df$original <- NULL

  # remove duplicates created by the unnesting
  ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]

  # remove shares performed more than one week from first share
  ct_shares.df <- ct_shares.df %>%
    dplyr::group_by(expanded) %>%
    dplyr::filter(difftime(max(date), min(date), units = "secs") <= 604800)

  # clean the expanded URLs
  if(clean_urls==TRUE){
    ct_shares.df <- clean_urls(ct_shares.df, "expanded")
  }

  ct_shares.df$is_orig <- ct_shares.df$expanded %in% urls$url

  # write log
  write(paste("Original URLs:", nrow(urls),
              "\nCT shares:", nrow(ct_shares.df),
              "\nUnique URLs in CT shares:", length(unique(ct_shares.df$expanded)),
              "\nLinks in CT shares matching original URLs:", as.numeric(table(ct_shares.df$is_orig)["TRUE"])),
        file = "log.txt",
        append = TRUE)

  rm(urls)

  return(ct_shares.df)
}
