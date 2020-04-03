![enter image description here](https://docs.google.com/drawings/d/e/2PACX-1vQ8V_WGl9RZ7rYzWA6sfXhbODgsZv4UwJGgS3sgoE3b_wXLh-3zJYRWtHwLyXveLZkmSW7C1iZAqC6w/pub?w=414&h=113)

*Fabio Giglietto, [Nicola Righetti](https://github.com/nicolarighetti), [Luca Rossi](https://github.com/lrossi79)*

## Overview
Given a set of URLs, this package detects coordinated link sharing behavior (CLSB) and outputs the network of entities that performed such behavior.

## What do we mean by coordinated link sharing behavior?
CLSB refers to a specific coordinated activity performed by a network of Facebook pages, groups and verified public profiles (Facebook public entities) that repeatedly shared the same news articles in a very short time from each other.

To identify such networks, we designed, implemented and tested an algorithm that detects sets of Facebook public entities which performed CLSB by (1) estimating a time threshold that identifies URLs shares performed by multiple distinguished entities within an unusually short period of time (as compared to the entire dataset), and (2) grouping the entities that repeatedly shared the same news story within this coordination interval. The rationale is that, while it may be common that several entities share the same URLs, it is unlikely, unless a consistent coordination exists, that this occurs within the time threshold and repeatedly.

See also <A HREF="#References">references</A> for a more detailed description and real-world applications.

## Installation
You can install CooRnet from GitHub.

``` r
# install.packages("devtools")

library("devtools"")
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

In this example, we use <A HREF="https://github.com/jandix/mediacloudr">mediacloudr</A>, an API Wrapper for the mediacloud.org API, to retrieve our initial list of news stories. Alternatively, is it also possible to use a CSV file exported from <A HREF="https://explorer.mediacloud.org/#/home">MediaCloud Explorer</A>.

``` r
library("mediacloudr")

# add API key to your R environment file.
# Open your .Renviron file. The file is usually located in your home directory. If the file does not exist, just create one and name it .Renviron.
# Add a new line and enter your API key in the following format: MEDIACLOUD_API_KEY=<YOUR_API_KEY>.
# Save the file and restart your current R session to start using mediacloudr.

df <- get_story_list(rows = 100,
                           fq = "(text:coronavirus OR text:'covid-19' OR text:'SARS-CoV-2') AND (tags_id_media:186572515 OR tags_id_media:186572435 OR tags_id_media:186572516 OR tags_id_media:162546808 OR tags_id_media:162546809) AND publish_date:[2020-03-02T00:00:00.000Z TO 2020-04-03T00:00:00.000Z]")

# Alternative using a file exported from MediaCloud Explorer and uploaded into r
# df <- read_csv("rawdata/MediaCloud_output.csv") # file exported from MediaCloud

library("CooRnet")

# get public shares of MediaCloud URLs on Facebook and Instagram (assumes a valid CROWDTANGLE_API_KEY in Env).
ct_shares.df <- get_ctshares(df, url_column = "url", date_column = "publish_date", platforms = "facebook,instagram", sleep_time = 1, nmax = 100, clean_urls = TRUE)

# get coordinated shares and networks
output <- get_coord_shares(df = ct_shares.df)

# creates the output objects in the environment
# 1. ct_shares_marked.df: a dataframe that contains all the retrieved social media shares plus an extra boolean variable (is_coordinated) that identify if the shares was coordinated.
# 2. highly_connected_g: igraph graph of coordinated networks of pages/groups/accounts
# 3. highly_connected_coordinated_entities: a dataframe that lists coordinated entities and corresponding component
get_outputs(output)
```
## Acknowledgements

CooRnet has been developed as part of [Patterns of Facebook Interactions around Insular and Cross-Partisan Media Sources in the Run-up of the 2018 Italian Election](https://sites.google.com/uniurb.it/mine/home) research project activities. 

The project is supported, in part, by a grant by a grant from Social Science Research Council. Data and tools provided by Facebook.

## References

 - Giglietto, F., Righetti, N., & Marino, G. (2019). Understanding
   Coordinated and Inauthentic Link Sharing Behavior on Facebook in the
   Run-up to 2018 General Election and 2019 European Election in Italy.
   https://doi.org/10.31235/osf.io/3jteh
 - Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes
   a village to manipulate the media: coordinated link sharing behavior   during 2018 and 2019 Italian elections. Information, Communication   and Society, 1–25. https://doi.org/10.1080/1369118X.2020.1739732
