\name{get_outputs}
\alias{get_outputs}
\title{get_outputs}
\usage{
get_outputs(get_coord_shares_output, ct_shares.df = TRUE, highly_connected_g = TRUE, highly_connected_coordinated_entities = TRUE)
}
\description{
A function to get the outputs of the get_coord_shares function.
}
\arguments{
\item{get_coord_shares_output}{the list from the get_coord_shares function}
\item{ct_shares.df}{the input data frame (ct_shares.df) of shares with an additional boolean variable (coordinated) that identifies coordinated shares (default TRUE)}
\item{highly_connected_g}{a igraph graph (highly_connected_g) with networks of coordinated entities (default TRUE)}
\item{highly_connected_coordinated_entities}{a dataframe with a list of coordinated entities (highly_connected_coordinated_entities) with respective name (the account url), number of shares performed, average subscriber count, platform, account name, if the account is verified, account handle, degree and component number (default TRUE)}
}
\examples{
get_outputs(get_coord_shares_output)
}
