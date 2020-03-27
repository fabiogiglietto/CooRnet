\name{clean_urls}
\alias{clean_urls}
\title{clean_urls}
\usage{
clean_urls(df, url)}
\description{
A function to clean the urls
}
\arguments{
\item{df}{a data frame with a column containing the URLs to clean}
\item{url}{the column with the urls to clean}
}
\value{A vector of cleaned urls}
\examples{
urls <- clean_urls(df, url="url")
}
