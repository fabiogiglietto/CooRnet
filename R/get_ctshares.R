get_ctshares <- function(urls, url_column, date_column, platforms="facebook,instagram", nmax=500, sleep_time=20) {

  require(httr)      # 1.4.1
  require(jsonlite)  # 1.6.9
  require(tidyr)     # 1.0.2

  # remove duplicated rows
  urls <- urls[!duplicated(urls),]

  # set column names
  colnames(urls)[colnames(urls)==url_column] <- "url"
  colnames(urls)[colnames(urls)==date_column] <- "date"

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

