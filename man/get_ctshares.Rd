\name{get_ctshares}
\alias{get_ctshares}
\title{get_ctshares}
\usage{
get_ctshares(urls, url_column, date_column, platforms="facebook,instagram", nmax, sleep_time=20)}
\description{
A function to get the URLs shares from CrowdTangle}
\arguments{
\item{urls}{a dataframe with at least a column "url" containing the URLs, and a column "date" with their published date}
\item{url_column}{name of the column (placed inside quote marks) where the URLs are stored}
\item{date_column}{name of the column (placed inside quote marks) where the date of the URLs are stored}
\item{platforms}{default to "facebook,instagram". You can specify only "facebook" to search on Facebook, or only "instagram" to search on Instagram}
\item{nmax}{max number of results for query (max=500 as per API limit)}
\item{sleep_time}{pause between queries to respect API rate limits. Default to 20 secs, it can be lowered or increased depending on the assigned API rate limit}
}
\value{A tibble (tbl_df) containing the entities that shared the URLs and a number of variables returned by the \href{https://github.com/CrowdTangle/API/wiki/Links}{CrowdTangle API links endpoint}}
\details{To start using the library you need to set the CrowdTangle API key.
Open the environment variable file with file.edit("~/.Renviron"), write CROWDTANGLE_API_KEY = <YOUR_API_KEY>, save the file and restart your current R session to start using the CrowdTangle API}
\examples{
df <- get_ctshares(urls, url_column=“url”, date_column=“date”, platforms="facebook,instagram", nmax=100, sleep_time=20)
}
