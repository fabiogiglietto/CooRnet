#' get_ctshares
#'
#' A function to get the URLs shares from CrowdTangle
#'
#' @param urls a dataframe with at least a column "url" containing the URLs, and a column "date" with their published date
#' @param url_column string: name of the column (placed inside quote marks) where the URLs are stored (defaults to "url")
#' @param date_column string: name of the column (placed inside quote marks) where the date of the URLs are stored (defaults to "date")
#' @param platforms string: default to "facebook,instagram". You can specify only "facebook" to search on Facebook, or only "instagram" to search on Instagram
#' @param nmax integer: max number of results for query (default 1000 as per
#' @param sleep_time pause between queries to respect API rate limits. Default to 30 secs, it can be lowered or increased depending on the assigned \href{https://help.crowdtangle.com/en/articles/3443476-api-cheat-sheet}{API rate limit}.
#' @param clean_urls logical: clean the URLs from tracking parameters (default FALSE)
#' @param mongo_url string: address of the MongoDB server in standard URI Format (required, default empty string will raise an error)
#' @param mongo_database string: the name of the MongoDB used to host the collections. Name can be choosen by the user (required, default empty string  will raise an error)
#' @param verbose logical: show progress messages?
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
#' @importFrom dplyr select group_by filter %>% select_if
#' @importFrom utils setTxtProgressBar txtProgressBar menu URLencode
#' @importFrom tidytable unnest. bind_rows.
#' @importFrom mongolite mongo
#'
#' @export

get_ctshares <- function(urls, url_column, date_column, platforms="facebook,instagram", nmax=1000, mongo_url="", mongo_database = "", sleep_time=30, is_df_saved = FALSE, clean_urls=FALSE, verbose=FALSE) {

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

  # lowercase all the URLs
  urls$url <- tolower(urls$url)

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

  conn <- connect_mongodb_cluster("urls", mongo_database, mongo_url)

  # Insert a dataframe called urls as a collection. Before inserting, erase a previous collection with the same name
  if(conn$count() > 0) {
    message("URLs database already exists. Existing files may be erased, choose a new path if this is not intended.")
    invisible(readline(prompt="Press [Enter] to continue or [Esc] to exit"))
    conn$drop()
    # invisible(readline("Press q to exit or c to continue"))
    # b <- scan("stdin", character(), n=1)
    # if (b=="q") break
    # if (b=="c")
  }

  # Define another mongoDB connection for a new collection to store CrowdTangle shares

  ct_shares_mdb <- connect_mongodb_cluster("shares_info", mongo_database, mongo_url)

  # Insert a dataframe called urls as a collection. Before inserting, erase a previous collection with the same name
  if(ct_shares_mdb$count() > 0) {
    message("Crowdtangle shares database already exists. Existing files may be erased, choose a new path if this is not intended.")
    invisible(readline(prompt="Press [Enter] to continue or [Esc] to exit"))
    ct_shares_mdb$drop()
  }

  conn$insert(urls)

  # progress bar
  total <- nrow(urls)
  pb <- utils::txtProgressBar(min = 0, max = total, width = 100, style = 3)

  # Define an interation parameter to use for running over all the collection
  it <- conn$iterate('{}')

  # Iterate until the 'it' variable is NULL
  while(!is.null(x <- it$one())){

    # query the CrowdTangle API
    ct_shares.df <- NULL
    datalist <- list()

    utils::setTxtProgressBar(pb, pb$getVal()+1)

    # set date limits: one week after date_published
    startDate <- as.POSIXct(x$date, origin="1970-01-01", tz = "UTC")
    endDate <- startDate+604800

    link <- x$url

    # build the querystring
    query.string <- paste0("https://api.crowdtangle.com/links?",
                           "link=", utils::URLencode(link, reserved = TRUE),
                           "&platforms=", platforms,
                           "&startDate=", gsub(" ", "T", as.character(startDate)),
                           "&endDate=", gsub(" ", "T", as.character(endDate)),
                           "&includeSummary=FALSE",
                           "&includeHistory=FALSE", # history takes a lot of space on the db
                           "&sortBy=date",
                           "&searchField=TEXT_FIELDS_AND_IMAGE_TEXT",
                           "&token=", Sys.getenv("CROWDTANGLE_API_KEY"),
                           "&count=", nmax)

    # call the API, returns NA if call fails
    if (verbose) message("Calling: ", query.string, " (link: ", link, ")")
    c <- query_link_enpoint(query.string, sleep_time)

    if (any(!is.na(c))) { # check if the call failed returning NA

      if (length(c$result$posts) != 0) {

        datalist <- c(list(c$result$posts), datalist)

        # paginate
        counter <- 1L
        while (counter <= 10 & !is.null(c$result$pagination$nextPage)) # stop after 10 iterations
        {
          if (verbose) message("Calling: ", c$result$pagination$nextPage, " (link: ", link, "(", counter, "))")
          c <- query_link_enpoint(c$result$pagination$nextPage, sleep_time)
          counter <- sum(counter, 1)

          if (any(!is.na(c))) {
            datalist <- c(list(c$result$posts), datalist)
          }
          else break}
      }

      if (length(datalist) != 0) {
        ct_shares.df <- tidytable::bind_rows.(datalist)
      }
      else {
        ct_shares.df <- NULL
      }

      if (!is.null(ct_shares.df)) {
        # keep only fields actually used by CooRnet
        ct_shares.df <- ct_shares.df %>%
          dplyr::select_if(names(.) %in% c("platformId",
                                           "platform",
                                           "date",
                                           "type",
                                           "expandedLinks",
                                           "description",
                                           "postUrl",
                                           # "history",
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

        ct_shares.df <- tidytable::unnest.(ct_shares.df, expandedLinks, .drop = FALSE)
        ct_shares.df <- dplyr::select(ct_shares.df, -original)
        ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]

        ct_shares.df <- as.data.frame(ct_shares.df)
        ct_shares.df <- ct_shares.df %>%
                        dplyr::rowwise() %>%
                        dplyr::mutate(is_orig = ifelse(expanded %in% urls$url, TRUE, FALSE))
                        # dplyr::mutate(is_orig = if_else(conn$count(sprintf('{"url": "%s"}',expanded))>0, TRUE, FALSE))

        # n_original_url <- conn$count(sprintf('{"url": "%s"}',ct_shares.df$expanded))
        # ct_shares.df$is_orig <- ifelse(n_original_url > 0, TRUE, FALSE)

        ct_shares_mdb$insert(ct_shares.df) # save the shares in the database
        # saveRDS(object = ct_shares.df, file = paste0("./rawdata/", i, ".rds"))
      }
      rm(ct_shares.df, datalist, c)
    }
  }

  # remove possible inconsistent rows with entity URL equal "https://facebook.com/null"
  ct_shares_mdb$remove('{"account_url" : "https://facebook.com/null"}')

  # Iterate on aggregation for getting all duplicates of the same post_id
  it <- ct_shares_mdb$aggregate(
    '[{"$group":{"_id":"$id", "dups": {"$addToSet": "$_id"}, "count": {"$sum":1}}}, {"$match": {"count": {"$gt": 1}}}]',
    options = '{"allowDiskUse":true}',
    iterate = TRUE
  )

  final_dups_list <- list()

  # remove duplicated post_ids given that the mongoDB id is unique
  while(!is.null(x <- it$one())){
    final_dups_list <- append(final_dups_list,x$dups[-1])
  }

  for (post_id in final_dups_list) ct_shares_mdb$remove(sprintf('{"_id":{"$oid":"%s"}}',post_id))

  # the idea is that most of the data processing can happen directly on the database to allow analysis of large quantity of urls/shares
  # for now I just get the shares from the database in to a data frame here to make the rest of the code work

  if(clean_urls==TRUE){

    # Create a dataframe with all expanded URLs and a dummy variable to use the clean_urls function in utils.R
    # old_expanded_df <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$old_expanded"}}]',options = '{"allowDiskUse":true}'))
    # old_expanded_df$n_row <- seq.int(1, length(example$`_id`))

    # Get the list of all cleaned expanded URLs
    # new_expanded_df <- clean_urls(old_expanded_df, "_id")

    # ct_shares_df <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$expanded"}}]',options = '{"allowDiskUse":true}'))

    # Find urls in the mongoDB database and aggregate in a dataframe
    urls_df <- ct_shares_mdb$aggregate(
              '[{"$group":{"_id":"$expanded","id_number": {"$addToSet": "$_id"}}}]',
              options = '{"allowDiskUse":true}')
    names(urls_df) <- c("url","id_number_list")

    # Apply the new version of clear_urls in order to create a dataframe with cleaned URLs
    cleaned_urls_df <- clean_urls_mongo(urls_df, "url")

    # Erase URLs in the database if they are removed troughtout the cleaning procedure
    erased_ids_list <- as.list(cleaned_urls_df$id_number_list[which(cleaned_urls_df$cleaned_url == "")])
    final_erased_ids_list <- list()
    for (ids_list in erased_ids_list) final_erased_ids_list <- append(final_erased_ids_list,ids_list)
    for (url_id in final_erased_ids_list) ct_shares_mdb$remove(sprintf('{"_id":{"$oid":"%s"}}', url_id))

    # Substitute URLs in the database if they are cleaned troughtout the cleaning procedure
    # Occurences of " need to be substituted with \" within the update query
    original_urls_list <- as.list(cleaned_urls_df$url[which(cleaned_urls_df$cleaned_url != "")])
    original_urls_list <- gsub('"','\"',original_urls_list)
    cleaned_urls_list <- as.list(cleaned_urls_df$cleaned_url[which(cleaned_urls_df$cleaned_url != "")])

    for (i in 1:length(original_urls_list)){

      original_url <- original_urls_list[i]
      cleaned_url <- cleaned_urls_list[i]

      ct_shares_mdb$update(sprintf('{"expanded" : "%s"}',original_url), sprintf('{"$set": {"expanded": "%s"}}',cleaned_url) , multiple = TRUE)
    }

    write("Analysis performed on cleaned URLs", file = "log.txt", append = TRUE)
  }

  # remove shares performed more than one week from first share
  # ct_shares.df <- ct_shares.df %>%
  #  dplyr::group_by(expanded) %>%
  #  dplyr::filter(difftime(max(date), min(date), units = "secs") <= 604800)

  # Aggregate list of dates for each expanded url with more than a share
  it <- ct_shares_mdb$aggregate(
  '[{"$group":{"_id":"$expanded", "ids_list": {"$addToSet": "$_id"}, "date_list": {"$addToSet": "$date"}, "count": {"$sum":1}}}, {"$match": {"count": {"$gt": 1}}}]',
  options = '{"allowDiskUse":true}',
  iterate = TRUE)

  erased_ids_list <- list()

  while(!is.null(x <- it$one())){

    date_list <- strptime(x$date_list, "%Y-%m-%d %H:%M:%S")

    # id_list <- x$ids_list[order(date_list, decreasing=FALSE)]
    # date_list <- sort(date_list, decreasing=FALSE)

    is_week <- difftime(max(date_list), min(date_list), units="secs") <= 604800

    if (is_week) {
      ids_list<- x$ids_list
      erased_ids_list <- append(erased_ids_list, ids_list)
    }
  }

  for (url_id in erased_ids_list) ct_shares_mdb$remove(sprintf('{"_id":{"$oid":"%s"}}', url_id))

  if (is_df_saved==TRUE) {
    ct_shares.df <- ct_shares_mdb$find('{}')
    names(ct_shares.df) <- gsub("_", ".", names(ct_shares.df)) # patch to rename the fields to their original name (mongolite changes dot with underscore)
  }

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
