# CooRnet

## Overview
Given a set of URLs, this packages detects coordinated link sharing behavior on social media and outputs the network of entities that performed such behaviour.

## Installation
You can install CooRnet from GitHub.

``` r
# install.packages("devtools")

library(devtools)
devtools::install_github("fabiogiglietto/CooRnet")
```

## Usage

``` r
df <- read_csv("rawdata/MediaCloud_output.csv") # file exported from MediaCloud

library(CooRnet)

# get public shares of MediaCloud URLs on Facebook and Instagram (assumes a valid CROWDTANGLE_API_KEY in Env).
ct_shares.df <- get_ctshares(df, url_column = "url", platforms = "facebook,instagram", date_column = "publish_date", sleep_time = 1, nmax = 100)

# get coordinated shares and networks
output <- get_coord_shares(df = ct_shares.df)
get_outputs(output)



