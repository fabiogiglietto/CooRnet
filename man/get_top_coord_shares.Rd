\name{get_top_coord_shares}
\alias{get_top_coord_shares}
\title{get_top_coord_shares}
\usage{
get_top_coord_shares(output, order_by = "engagement", component=TRUE, top=10)}
\description{
A function to get the top n posts containing URLs shared in a coordinated way
}
\arguments{
\item{output}{the output list resulting from the function get_coord_shares}
\item{order_by}{name of the column used to order the top news. Default to "engagement". Other possible values are: "statistics.actual.likeCount", "statistics.actual.shareCount", "statistics.actual.commentCount", "statistics.actual.loveCount", "statistics.actual.wowCount", "statistics.actual.hahaCount", "statistics.actual.sadCount","statistics.actual.angryCount"}
\item{component}{return the top posts grouped by network component (TRUE, default) or by account (FALSE)}
\item{top}{number of the top news to be retrieved}
}
\value{A data frame (grouped_df) containing the top URLs shared in a coordinated way by the highly coordinated entities, with a set of attributes}
\examples{
# get the top ten posts containing URLs shared by each network component and by engagement
df <- get_top_news(output, order_by = "engagement", top=10)

# get the top ten posts containing URLs shared in a coordinated way, by each entity and by engagement
df <- get_top_news(output, order_by = "engagement", top=10)
}
