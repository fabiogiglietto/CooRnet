\name{get_coord_shares}
\alias{get_coord_shares}
\title{get_coord_shares}
\usage{
get_coord_shares(df, coordination_interval=NULL, parallel=TRUE, percentile_edge_weight=0.90)
}
\description{
Given a dataset of CrowdTangle shares and a time threshold, this function detects networks of entities (pages, accounts and groups) that performed coordinated link sharing behaviour}
\arguments{
\item{df}{a dataframe of social media shares in the format provided by \href{https://github.com/CrowdTangle/API/wiki/Links}{CrowdTangle API links endpoint}}
\item{coordination_interval}{a threshold in seconds that defines a coordinated share. Given a dataset of CrowdTangle shares, this threshold is automatically estimated by the estimate_coord_interval interval function. Alternatively it can be manually passed to the function in seconds}
\item{parallel}{enables parallel processing to speed up the process taking advantage of multiple cores (default TRUE). The number of cores is automatically set to all the available cores minus one}
\item{percentile_edge_weight}{defines the percentile of the edge distribution to keep in order to identify a network of coordinated entities. In other terms, this value determines the minimum number of times that two entities had to coordinate in order to be considered part of a network. (default 0.90)}
}
\value{A list (results_list) containing four objects: 1. The input data.table (ct_shares.dt) of shares with an additional boolean variable (coordinated) that identifies coordinated shares, 2. An igraph graph (highly_connected_g) with networks of coordinated entities, 3. A dataframe with a list of coordinated entities (highly_connected_coordinated_entities) with respective name (the account url), number of shares performed, average subscriber count, platform, account name, if the account is verified, account handle, degree and component number}
\examples{
output <- get_coord_shares(df)

output <- get_coord_shares(df = ct_shares.df, coordination_interval = coordination.interval, cores = detectCores(), threshold=0.9)

# Get the data frame of CrowdTangle shares marked with the “iscoordinated” column
ct_shares.dt <- as.data.frame(output[[1]])
# Save the data frame in csv format
save.csv(ct_shares.dt, file=“ct_shares.dt.csv”)

# Get the graph of highly connected entities
highly_connected_g <- output[[2]]
# To save the graph in a Gephi readable format
library(igraph)
write.graph(highly_connected_g, "./data/highly_connected_g.graphml", format = "graphml")

# Get the data frame containing the information about the highly connected coordinated entities
highly_connected_coordinated_entities <- as.data.frame(output[[3]])
# Save the data frame in csv format
save.csv(highly_connected_coordinated_entities, file=“highly_connected_coordinated_entities.csv”)
}
