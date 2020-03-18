\name{get_top_news}
\alias{get_top_news}
\title{get_top_news}
\usage{
get_top_news(output, order_by = "engagement", component=TRUE, top=10)}
\description{
A function to get the top n news shared in a coordinated way
}
\arguments{
\item{output}{the output list resulting from the function get_coord_shares}
\item{order_by}{name of the column used to order the top news. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"}
\item{component}{return the top news grouped by network component (TRUE, default) or by account (FALSE)}
\item{top}{number of the top news to be retrieved}
}
\value{A data frame (grouped_df) containing the top news shared in a coordinated way by the highly coordinated entities, with a set of attributes}
\examples{
# get the top ten news shared by each network component, by engagement
df <- get_top_news(output, order_by = "engagement", top=10)

# get the top ten news shared by each entity, by engagement
df <- get_top_news(output, order_by = "engagement", top=10)
}
