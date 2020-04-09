#' get_urls_from_ct_histdata
#'
#' Given a CSV created by the CrowdTangle Historical Data feature (preferbly filtered for link type posts), this function extract a unique list of URLs with a first shared date
#'
#' @param ct_histdata_csv a local or remote link to a CSV created by the CrowdTangle Historical Data
#'
#' @return A data.frame with a unique list of URLs with respective first seen date
#'
#' @examples
#' urls <- get_urls_from_ct_histdata(ct_histdata_csv="mylocaldata.csv")
#'
#' urls <- get_urls_from_ct_histdata(ct_histdata_csv="https://ct-download.s3.us-west-2.amazonaws.com/...")
#'
#' # Use the new urls dataset to call get_ct_shares function
#' ct_shares.dt <- get_ctshares(urls, url_column="url", date_column="date", save_ctapi_output=TRUE)
#'
#' @export

get_urls_from_ct_histdata <- function(ct_histdata_csv=NULL) {
  require(readr) # 1.2.0
  require(dplyr) # 0.8.3

  if(is.null(ct_histdata_csv)) {
    stop("The function requires a valid CSV local or remote link to run")
    }

cat("\nLoading CSV...")

df <- read_csv(col_types = cols(
  .default = col_skip(),
  Created = col_character(),
  Link = col_character(),
  `Final Link` = col_character()),
  file =  ct_histdata_csv)

df$url <- ifelse(is.na(df$`Final Link`), df$Link, df$`Final Link`) # keep expanded links only

urls <- df %>%
  group_by(url) %>%
  summarise(Created = min(Created))

names(urls) <- c("url", "date")
rm(df)
return(urls)
}
