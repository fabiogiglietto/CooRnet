# Check that it doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z]", x)

# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)

library(CooRnet)
library(readr)
library(urltools)

urls.ifcn <- read_csv("urls.csv") # Indian IFCN URLS NOV 11
urls.ifcn$domain <- urltools::domain(urls.ifcn$url)

# get facebook posts
fb_urls <- subset(urls.ifcn, urls.ifcn$domain %in% c("www.facebook.com", "m.facebook.com"))
fb_urls$platformID <- urltools::path(fb_urls$url)
fb_urls$platformID <- strsplit(fb_urls$platformID, "/", fixed = TRUE)

fb_urls <- fb_urls[!letters_only(fb_urls$platformID),]

for (i in 1:1054) {
  a <- unlist(fb_urls$platformID[i])[numbers_only(unlist(fb_urls$platformID[i]))]
  fb_urls$platformID[i] <- paste(unlist(a), collapse = "_")
}

for (i in 1056:nrow(fb_urls)) {
  a <- unlist(fb_urls$platformID[i])[numbers_only(unlist(fb_urls$platformID[i]))]
  fb_urls$platformID[i] <- paste(unlist(a), collapse = "_")
}

ct_shares.df <- NULL
datalist <- list()

for (j in 1090:nrow(fb_urls)) {
  # build the querystring
  query.string <- paste0("https://api.crowdtangle.com/post/", fb_urls$platformID[j],
                         "?token=", Sys.getenv("CROWDTANGLE_API_KEY"))
  resp <- httr::RETRY(verb = "GET", url = query.string, times=3, terminate_on=c(401))
  response.json <- httr::content(resp, as = "text", type="application/json", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(response.json, flatten = TRUE)

  if (length(parsed$result$posts) != 0) {
    datalist <- c(list(parsed$result$posts), datalist)
  }

  Sys.sleep(1)
}

saveRDS(datalist, "datalist.rds")

ct_shares.df <- tidytable::bind_rows.(datalist)
rm(datalist)

saveRDS(ct_shares.df, "ct_shares.df.rds")

# remove possible inconsistent rows with entity URL equal "https://facebook.com/null"
ct_shares.df <- ct_shares.df[ct_shares.df$account.url!="https://facebook.com/null",]

ct_shares.df <- tidytable::unnest.(ct_shares.df, expandedLinks, .drop = FALSE)
ct_shares.df$original <- NULL

# remove duplicates created by the unnesting
ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]

keep <- c("expanded", "date")

urls <- ct_shares.df %>%
  select(keep)

urls.ifcn <- urls.ifcn %>%
  select(-"domain")

# in case of duplicated urls, keeps only the one with the oldest date
urls <- urls %>%
  group_by(expanded) %>%
  summarise(date = min(date)) %>%
  select(url=expanded, date)

urls <- rbind(urls, urls.ifcn)

# in case of duplicated urls, keeps only the one with the oldest date
urls <- urls %>%
  group_by(url) %>%
  summarise(date = min(date)) %>%
  select(url, date)


ct_shares.df <- CooRnet::get_ctshares(urls = urls,
                                      nmax = 1000,
                                      mongo_url = "192.168.251.159",
                                      mongo_database = "MEAG_INDIA",
                                      mongo_cluster = FALSE,
                                      return_df = FALSE,
                                      get_history = FALSE,
                                      clean_urls = TRUE)

output <- CooRnet::get_coord_shares_mongo(mongo_database = "MEAG_INDIA",
                                          mongo_url = "192.168.251.159",
                                          coordination_interval = 25,
                                          parallel = FALSE,
                                          percentile_edge_weight = 0.97,
                                          clean_urls = FALSE,
                                          keep_ourl_only = FALSE,
                                          mongo_cluster = FALSE)

get_outputs(coord_shares_output = output)
