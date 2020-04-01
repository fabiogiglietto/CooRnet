# CooRnet
*Fabio Giglietto, [Nicola Righetti](https://github.com/nicolarighetti), Luca Rossi*

## Overview
Given a set of URLs, this packages detects coordinated link sharing behavior (CLSB) and outputs the network of entities that performed such behaviour and their graph.

## What do we mean by coordinated link sharing behaviour?
CLSB refers to a specific coordinated activities performed by a network of Facebook pages, groups and verified public profiles (Facebook public entities) that repetedly shared on social media the same news articles in a very short time from each other.

To identify such networks, we designed, implemented and tested an algorithm that, detects sets of Facebook public entities which performed CLSB by (1) estimating a time threshold that identify URLs shares performed by multiple distinguished entities within an unusually short period of time (as compared to the entire dataset), and (2) grouping the entities that repeatedly shared the same news story within this coordination interval. The rationale is that while it may be common that several entities share the same URLs, it is unlikely, unless a consistent coordination exists, that this occurs within the time threshold and repeatedly.

See also references for a more detailed description and real-world applications.

## Installation
You can install CooRnet from GitHub.

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("fabiogiglietto/CooRnet")
```

## API key
This package requires a <A HREF="https://github.com/CrowdTangle/API">CrowdTangle API</A> key entry in your R environment file. The R environment file is loaded every time R is started/restarted.

The following steps show how to add the key to your R environment file.

1) Open your .Renviron file. The file is usually located in your home directory. If the file does not exist, just create one and name it .Renviron.
2) Add a new line and enter your API key in the following format: CROWDTANGLE_API_KEY=<YOUR_API_KEY>.
3) Save the file and restart your current R session to start using CooRnet.

## Usage
CooRnet requires a list of urls with respective publication date. Such list can be collected from various sources including CrowdTangle itself (e.g. Historical Data with a link post type filter), Twitter APIs (filtering for tweets with a url) and GDELT.

In this example, we use a CSV file exported from <A HREF="https://explorer.mediacloud.org/#/home">MediaCloud Explorer</A>.

``` r
df <- read_csv("rawdata/MediaCloud_output.csv") # file exported from MediaCloud

library(CooRnet)

# get public shares of MediaCloud URLs on Facebook and Instagram (assumes a valid CROWDTANGLE_API_KEY in Env).
ct_shares.df <- get_ctshares(df, url_column = "url", platforms = "facebook,instagram", date_column = "publish_date", sleep_time = 1, nmax = 100)

# get coordinated shares and networks
output <- get_coord_shares(df = ct_shares.df)

# creates the output objects in the environment
# 1. ct_shares_marked.df: a dataframe that contains all the retrieved social media shares plus an extra boolean variable (is_coordinated) that identify if the shares was coordinated.
# 2. highly_connected_g: igraph graph of coordinated networks of pages/groups/accounts
# 3. highly_connected_coordinated_entities: a dataframe that lists coordinated entities and corresponding component
get_outputs(output)
```

## References

Giglietto, F., Righetti, N., & Marino, G. (2019). Understanding Coordinated and Inauthentic Link Sharing Behavior on Facebook in the Run-up to 2018 General Election and 2019 European Election in Italy. https://doi.org/10.31235/osf.io/3jteh

Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. Information, Communication and Society, 1–25. https://doi.org/10.1080/1369118X.2020.1739732
