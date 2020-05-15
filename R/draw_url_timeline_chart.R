#' draw_url_timeline_chart
#'
#' Given an id from 1 to 10, this function draws an interactive timeline depicting how a link was shared on Facebook by coordinated and non coordinated shares
#'
#' @param output the output list resulting by the get_coord_shares function
#' @param top_coord_urls the output of get_top_coord_urls. If NULL a new top_url is created (default=NULL)
#' @param top_url_id a number indicating which url to use to draw the chart
#'
#' @return A data frame (grouped_df) containing the posts with the highest engagement shared in a coordinated way by the highly coordinated entities, with a set of attributes
#'
#' @examples
#'   # get the top ten posts containing URLs shared by each network component and by engagement
#'   df <- get_top_news(output, order_by = "engagement", top=10)
#'
#'   # get the top ten posts containing URLs shared in a coordinated way, by engagement
#'   df <- get_top_news(output, order_by = "engagement", top=10)
#'
#' @import stringr
#' @import tidyr
#' @import dplyr
#' @import igraph
#' @import ggsci
#' @import ggplot2
#' @import plotly
#' @import lubridate
#'
#' @export

draw_url_timeline_chart <- function(output, top_coord_urls=NULL, top_url_id=10) {
  g <- output[[2]]
  ct_shares_marked.df <- output[[1]]

  cl <- components(g)
  gc <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))
  # V(gc)$cluster <- cluster_louvain(gc)$membership

  if (is.null(top_coord_urls)) {
    cat("\n\nBuilding a table of top shared URLs...")
    top_url <- CooRnet::get_top_coord_urls(output = output, order_by = "statistics.actual.shareCount", component = FALSE, top = 10)
  }
  else {top_url <- top_coord_urls}

  top_url_tab <- top_url
  top_url$cooR.account.name <- strsplit(top_url$cooR.account.name,",")
  top_url <- unnest(data = top_url,cols = cooR.account.name)
  top_url$cooR.account.name <- str_trim(top_url$cooR.account.name)
  top_url$cooR.account.name <- gsub(pattern = "\"",replacement = "",x = top_url$cooR.account.name)
  top_url <- top_url[top_url$cooR.account.name %in% V(g)$account.name,]

  t <- ct_shares_marked.df[ct_shares_marked.df$expanded == top_url$expanded[top_url_id],]

  t2 <- t[c("account.name","expanded","date","subscriberCount","statistics.actual.shareCount","is_coordinated")]
  t2 <- t2 %>% arrange(date)

  t2 <- t2 %>% group_by() %>% dplyr::mutate(cumsum=cumsum(statistics.actual.shareCount))
  t2$date <- as_datetime(t2$date)
  t2$subscriberCount <- t2$subscriberCount/100
  t2$is_coordinated <- factor(t2$is_coordinated, ordered =T, levels = c("TRUE","FALSE"))

  p <- ggplot(data = t2, aes(x=date,y = cumsum,label=account.name)) +
    geom_line(color="gray")+
    geom_point(mapping = aes(size=subscriberCount, color=is_coordinated),alpha = .5)+
    scale_colour_startrek()+
    scale_size(range = c(0, 20))+
    scale_x_datetime(limits = as_datetime(c(min(t2$date),min(t2$date)+24*60*60)))+
    #geom_text()+
    #geom_text_repel()+
    theme_minimal()+
    labs(title = paste("Link Sharing activity"),
       subtitle = paste("Entities sharing the link during the first 24 hours"),
       color="Coordination",
       size="Subscribers (x100)",
       x="Time",
       y="Total number of shares")

  ggplotly(p) %>% layout(title = "", legend = list(orientation = "v", y = -0.2, x = 0))
}
