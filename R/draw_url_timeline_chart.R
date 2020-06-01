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
#' @importFrom tidyr unnest
#' @importFrom dplyr group_by mutate arrange %>%
#' @importFrom stringr str_trim
#' @importFrom igraph components induced.subgraph V
#' @importFrom lubridate as_datetime
#' @importFrom ggplot2 ggplot geom_line geom_point scale_size scale_x_datetime theme_minimal labs aes
#' @importFrom plotly ggplotly layout
#' @importFrom ggsci scale_colour_startrek
#'
#' @export

draw_url_timeline_chart <- function(output, top_coord_urls=NULL, top_url_id=1) {
  g <- output[[2]]
  ct_shares_marked.df <- output[[1]]

  cl <- igraph::components(g)
  gc <- igraph::induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

  if (is.null(top_coord_urls)) {
    cat("\n\nBuilding a table of top shared URLs...")
    top_url <- CooRnet::get_top_coord_urls(output = output, order_by = "statistics.actual.shareCount", component = FALSE, top = 10)
  }
  else {top_url <- top_coord_urls}

  top_url_tab <- top_url
  top_url$cooR.account.name <- strsplit(top_url$cooR.account.name,",")
  top_url <- tidyr::unnest(data = top_url,cols = cooR.account.name)
  top_url$cooR.account.name <- stringr::str_trim(top_url$cooR.account.name)
  top_url$cooR.account.name <- gsub(pattern = "\"",replacement = "",x = top_url$cooR.account.name)
  top_url <- top_url[top_url$cooR.account.name %in% igraph::V(g)$account.name,]

  t <- ct_shares_marked.df[ct_shares_marked.df$expanded == top_url_tab$expanded[top_url_id],]

  t2 <- t[c("account.name", "expanded", "date", "subscriberCount", "statistics.actual.shareCount", "is_coordinated")]
  t2 <- t2 %>% dplyr::arrange(date)

  t2 <- t2 %>% dplyr::group_by() %>% dplyr::mutate(cumsum=cumsum(statistics.actual.shareCount))
  t2$date <- lubridate::as_datetime(t2$date)
  t2$subscriberCount <- t2$subscriberCount
  t2$is_coordinated <- factor(t2$is_coordinated, ordered =T, levels = c("TRUE","FALSE"))

  p <- ggplot2::ggplot(data = t2, ggplot2::aes(x=date,y = cumsum, label=account.name)) +
    ggplot2::geom_line(color="gray")+
    ggplot2::geom_point(mapping = ggplot2::aes(size=subscriberCount, color=is_coordinated),alpha = .5)+
    ggsci::scale_colour_startrek()+
    ggplot2::scale_size(range = c(0, 20))+
    ggplot2::scale_x_datetime(limits = as_datetime(c(min(t2$date),min(t2$date)+24*60*60)))+
    #geom_text()+
    #geom_text_repel()+
    ggplot2::theme_minimal()+
    ggplot2::labs(title = paste("Link Sharing activity"),
       subtitle = paste("Entities sharing the link during the first 24 hours"),
       color="Coordination",
       size="Subscribers",
       x="Time",
       y="Total number of shares")

  plotly::ggplotly(p) %>% layout(title = t2$title[top_url_id], legend = list(orientation = "h", y = -0.3, x = 0))
}
