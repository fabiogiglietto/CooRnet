#' get_ctshares
#'
#' A function to get the URLs shares from CrowdTangle
#'
#' @param urls a dataframe with at least a column "url" containing the URLs, and a column "date" with their published date
#' @param url_column string: name of the column (placed inside quote marks) where the URLs are stored (defaults to "url")
#' @param date_column string: name of the column (placed inside quote marks) where the date of the URLs are stored (defaults to "date")
#' @param platforms string: default to "facebook,instagram". You can specify only "facebook" to search on Facebook, or only "instagram" to search on Instagram
#' @param nmax integer: max number of results for query (default 1000)
#' @param sleep_time pause between queries to respect API rate limits. Default to 30 secs, it can be lowered or increased depending on the assigned \href{https://help.crowdtangle.com/en/articles/3443476-api-cheat-sheet}{API rate limit}.
#' @param clean_urls logical: clean the URLs from tracking parameters (default FALSE)
#' @param mongo_url string: address of the MongoDB server in standard URI Format (required, default empty string will raise an error)
#' @param mongo_database string: the name of the MongoDB used to host the collections. Name can be choosen by the user (required, default empty string  will raise an error)
#' @param return_df logical: set to TRUE to return a dataframe of ct_shares (default FALSE)
#' @param mongo_cluster logical: set to TRUE if you are using a MongoDB cluster instead of standalone instance (default FALSE)
#' @param get_history logical: set to TRUE to retive history from CrowdTangle API (default FALSE)
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

get_ctshares <- function(urls,
                         url_column,
                         date_column,
                         platforms="facebook,instagram",
                         nmax=1000,
                         mongo_url="",
                         mongo_database = "",
                         sleep_time=30,
                         mongo_cluster = TRUE,
                         return_df = TRUE,
                         get_history = FALSE,
                         clean_urls=FALSE,
                         verbose=FALSE) {

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

  conn <- connect_mongodb_cluster("urls", mongo_database, mongo_url, mongo_cluster)

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

  ct_shares_mdb <- connect_mongodb_cluster("shares_info", mongo_database, mongo_url, mongo_cluster)

  # Insert a dataframe called shares_info as a collection. Before inserting, erase a previous collection with the same name
  if(ct_shares_mdb$count() > 0) {
    message("Crowdtangle shares database already exists. Existing files may be erased, choose a new path if this is not intended.")
    invisible(readline(prompt="Press [Enter] to continue or [Esc] to exit"))
    ct_shares_mdb$drop()
  }

  # create an indexes
  conn$index(add = '{"urls" : 1}')
  ct_shares_mdb$index(add = '{"expanded" : 1}')
  ct_shares_mdb$index(add = '{"platformId" : 1}')
  ct_shares_mdb$index(add = '{"expanded" : 1, "platformId" : 1}')

  # ct_shares_mdb$run('{"createIndexes": "shares_info","indexes":[{"key":{"platformId":1, "expanded": 1},"name": "platformId_expanded_index","unique": true}]}')

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
                                           ifelse(get_history, "history", ""),
                                           # "id", # same of platformId
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
        ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("platformId", "postUrl", "expanded")]),]
        ct_shares.df <- as.data.frame(ct_shares.df)

        if(clean_urls==TRUE){
          ct_shares.df <- clean_urls(ct_shares.df, "expanded")
          # write("Original URLs have been cleaned", file = "log.txt", append = TRUE)
          # ct_shares.df <- ct_shares.df %>%
          #                 dplyr::mutate(expanded = replace(expanded, expanded != "", cleaned_url))
          #
          #   ct_shares.df$cleaned_url <- NULL
        }

        if (nrow(ct_shares.df)>=1){
              ct_shares.df <- ct_shares.df %>%
                              dplyr::rowwise() %>%
                              dplyr::mutate(is_orig = ifelse(expanded %in% urls$url, TRUE, FALSE))
            # dplyr::mutate(is_orig = if_else(conn$count(sprintf('{"url": "%s"}',expanded))>0, TRUE, FALSE))

            # n_original_url <- conn$count(sprintf('{"url": "%s"}',ct_shares.df$expanded))
            # ct_shares.df$is_orig <- ifelse(n_original_url > 0, TRUE, FALSE)
                for (j in 1:nrow(ct_shares.df)) {
                      ct_shares_mdb$update(gsub('^.|.$', '', jsonlite::toJSON(ct_shares.df[j,])), upsert = TRUE)
                }
            }
      }
      rm(ct_shares.df, datalist, c)
    }
  }

  if (verbose) message("Done with getting ct_shares! Now cleaning up...")

  if (verbose) message("(1). Removing duplicates...")
  # remove possible inconsistent rows with entity URL equal "https://facebook.com/null"
  ct_shares_mdb$remove('{"account_url" : "https://facebook.com/null"}')

  # Iterate on aggregation for getting all duplicates of the same post_id

  ct_shares_mdb$aggregate('[{ "$group": {"_id":{"platformId":"$platformId", "expanded":"$expanded"},
                            "doc": {"$first": "$$ROOT"}}},{"$replaceRoot": {"newRoot": "$doc"}},
                            {"$out": "shares_info"}]',options='{ "allowDiskUse": true }')

  # the idea is that most of the data processing can happen directly on the database to allow analysis of large quantity of urls/shares
  # for now I just get the shares from the database in to a data frame here to make the rest of the code work

  # if(clean_urls==TRUE){
  #
  #   if (verbose) message("(2). Cleaning URLs...")
  #
  #   # Create a dataframe with all expanded URLs and a dummy variable to use the clean_urls function in utils.R
  #   # old_expanded_df <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$old_expanded"}}]',options = '{"allowDiskUse":true}'))
  #   # old_expanded_df$n_row <- seq.int(1, length(example$`_id`))
  #
  #   # Get the list of all cleaned expanded URLs
  #   # new_expanded_df <- clean_urls(old_expanded_df, "_id")
  #
  #   # ct_shares_df <- as.data.frame(ct_shares_mdb$aggregate('[{"$group":{"_id":"$expanded"}}]',options = '{"allowDiskUse":true}'))
  #
  #   # Find urls in the mongoDB database and aggregate in a dataframe
  #   urls_df <- ct_shares_mdb$aggregate(
  #     '[{"$group":{"_id":"$expanded","id_number": {"$addToSet": "$_id"}}}]',
  #     options = '{"allowDiskUse":true}')
  #   names(urls_df) <- c("url","id_number_list")
  #
  #   # Apply the new version of clear_urls in order to create a dataframe with cleaned URLs
  #   cleaned_urls_df <- clean_urls_mongo(urls_df, "url")
  #
  #   # # Erase URLs in the database if they are removed troughtout the cleaning procedure
  #   # erased_ids_list <- as.list(cleaned_urls_df$id_number_list[which(cleaned_urls_df$cleaned_url == "")])
  #   # final_erased_ids_list <- list()
  #   # for (ids_list in erased_ids_list) final_erased_ids_list <- append(final_erased_ids_list,ids_list)
  #   # for (url_id in final_erased_ids_list) ct_shares_mdb$remove(sprintf('{"_id":{"$oid":"%s"}}', url_id))
  #
  #   # Substitute URLs in the database if they are cleaned troughtout the cleaning procedure
  #   # Occurrences of " need to be substituted with \" within the update query
  #   original_urls_list <- as.list(cleaned_urls_df$url)
  #   # original_urls_list <- gsub('"','\"',original_urls_list)
  #   cleaned_urls_list <- as.list(cleaned_urls_df$cleaned_url[which(cleaned_urls_df$cleaned_url != "")])
  #
  #   for (i in 1:length(original_urls_list)){
  #
  #     original_url <- stringi::stri_enc_toutf8(sprintf('%s',original_urls_list[i]))
  #     cleaned_url <- stringi::stri_enc_toutf8(sprintf('%s',cleaned_urls_list[i]))
  #
  #     if (cleaned_url != "") ct_shares_mdb$update(sprintf('{"expanded" : "%s"}',original_url), sprintf('{"$set": {"expanded": "%s"}}',cleaned_url) , multiple = TRUE)
  #     else ct_shares_mdb$remove(sprintf('{"_id":{"$oid":"%s"}}', original_url))
  #   }
  #
  #   write("Analysis performed on cleaned URLs", file = "log.txt", append = TRUE)
  # }

  if (verbose) message("(3). Cleaning shares performed outside the time span of one week from first post...")

  # How to remove dates more than one week after the first share:
  # Step 1 - "Group" expanded link, date and original docs and compute the min date in the list
  # Step 1 - "Unwind" simultaneously dates and docs by matching with the same unwinding index both variables
  # Step 2 - "Redact" to keep only IDs of expanded link with dates before the first week
  # Step 3 - "Unset" the min_date variable
  # Step 4 - "ReplaceRoot" with original doc

  # # Aggregate list of dates for each expanded url with more than a share
  ct_shares_mdb$aggregate('[{"$group": {"_id":"$expanded", "date": {"$addToSet": "$date"},  "min_date": {"$min": "$date"}, "doc": {"$push": "$$ROOT"}}},
                           {"$unwind": {"path" : "$date", "includeArrayIndex" : "date_index", "preserveNullAndEmptyArrays" : true}},
                           {"$unwind": {"path" : "$doc", "includeArrayIndex" : "doc_index", "preserveNullAndEmptyArrays" : true}},
                           {"$project": {"_id":1, "date": 1, "min_date": 1, "doc": 1, "compare": { "$cmp": ["$date_index", "$doc_index"]}}},
                           {"$match": {"compare": 0}},
                           {"$project": {"_id":1, "date": 1, "min_date": 1, "doc": 1}},
                           {"$redact": {"$cond":
                           {"if": {"$gt":[{"$divide" : [{"$subtract": [{"$toDate": "$date"}, {"$toDate": "$min_date"}]}, 1000]},86400]},
                           "then": "$$PRUNE",
                           "else": "$$KEEP"}}},
                           {"$unset": "min_date"},
                           {"$replaceRoot": {"newRoot":"$doc"}},
                           {"$out": "shares_info"}]',
                           options='{ "allowDiskUse": true }')

  # write log
  write(paste("Original URLs:", nrow(urls),
              "\nCT shares:", ct_shares_mdb$count(),
              "\nUnique URLs in CT shares:", length(ct_shares_mdb$distinct("expanded")),
              "\nCT shares matching original URLs:", ct_shares_mdb$count('{"is_orig":true}')),
        file = "log.txt",
        append = TRUE)

  rm(urls)

  if (return_df==TRUE) {
    if (verbose) message("Returning a dataframe...")
    ct_shares.df <- data.frame()
    ct_shares.df <- ct_shares_mdb$find('{}')
    names(ct_shares.df) <- gsub("_", ".", names(ct_shares.df)) # patch to rename the fields to their original name (mongolite changes dot with underscore)
    return(ct_shares.df)
    # ct_shares_mdb$drop()
  }
  else {
    if (verbose) message("Returning an open MongoDB connection...")
    return(ct_shares_mdb)
  }
}
