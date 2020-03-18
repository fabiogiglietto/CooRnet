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

## API key
This package requires a <A HREF="https://github.com/CrowdTangle/API">CrowdTangle API</> key entry in your R environment file. The R environment file is loaded every time R is started/restarted.

The following steps show how to add the key to your R environment file.

1) Open your .Renviron file. The file is usually located in your home directory. If the file does not exist, just create one and name it .Renviron.
2) Add a new line and enter your API key in the following format: CROWDTANGLE_API_KEY=<YOUR_API_KEY>.
3) Save the file and restart your current R session to start using CooRnet.

## Usage

``` r
df <- read_csv("rawdata/MediaCloud_output.csv") # file exported from MediaCloud

library(CooRnet)

# get public shares of MediaCloud URLs on Facebook and Instagram (assumes a valid CROWDTANGLE_API_KEY in Env).
ct_shares.df <- get_ctshares(df, url_column = "url", platforms = "facebook,instagram", date_column = "publish_date", sleep_time = 1, nmax = 100)

# get coordinated shares and networks
output <- get_coord_shares(df = ct_shares.df)
get_outputs(output)



