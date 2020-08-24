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
#' @importFrom tidytable unnest.
#' @importFrom dplyr group_by mutate arrange %>% bind_rows filter ungroup select
#' @importFrom stringr str_trim
#' @importFrom igraph components induced.subgraph V
#' @importFrom lubridate as_datetime
#' @importFrom ggplot2 ggplot geom_line geom_point scale_size scale_x_datetime theme_minimal labs aes
#' @importFrom plotly ggplotly layout
#' @importFrom ggsci scale_colour_startrek
#' @importFrom imputeTS na_interpolation
#'
#' @export

draw_url_timeline_chart <- function(output, top_coord_urls=NULL, top_url_id=1) {
  g <- output[[2]]
  ct_shares_marked.df <- output[[1]]

  cl <- igraph::components(g)
  gc <- igraph::induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

  # check if top_coord_urls had been provided
  if (is.null(top_coord_urls)) {
    # nope
    cat("\n\nBuilding a table of top shared URLs...")
    top_url <- CooRnet::get_top_coord_urls(output = output, order_by = "statistics.actual.shareCount", component = FALSE, top = 10)
  }
  else {
    # yep
    top_url <- top_coord_urls
    }

  top_url_tab <- top_url
  top_url$cooR.account.name <- strsplit(top_url$cooR.account.name,",")
  top_url <- tidytable::unnest.(top_url, cooR.account.name)
  top_url$cooR.account.name <- stringr::str_trim(top_url$cooR.account.name)
  top_url$cooR.account.name <- gsub(pattern = "\"",replacement = "",x = top_url$cooR.account.name)
  top_url <- top_url[top_url$cooR.account.name %in% igraph::V(g)$account.name,]

  t <- ct_shares_marked.df[ct_shares_marked.df$expanded == top_url_tab$expanded[top_url_id],]

  t2 <- t[c("account.name", "expanded", "date", "subscriberCount", "statistics.actual.shareCount", "is_coordinated")]

  if("history" %in% colnames(t)) {
    cat("Analyzing historical engagement data...\n");
    t2 <- t[c("account.name", "expanded", "date", "subscriberCount", "history", "statistics.actual.shareCount", "is_coordinated")]

    url.history <- dplyr::bind_rows(t2$history, .id = "post_label")

    shares_cumsum <- url.history %>%
      # calculate shares difference between consecutive timesteps
      group_by(post_label) %>%
      arrange(timestep) %>%
      mutate(diff_shares = c(0, diff(actual.shareCount, lag = 1))) %>%
      ungroup() %>%
      mutate(date = as.POSIXct(date)) %>%
      arrange(date) %>%
      mutate(cum_shares = cumsum(diff_shares)) %>%
      filter(difftime(date, min(date), units = "secs") <= 604800) %>% # keep only one week hist
      select(date, cum_shares)
  }

  t2 <- t2 %>% dplyr::arrange(date)
  t2 <- t2 %>% dplyr::group_by() %>% dplyr::mutate(shares_cumsum=cumsum(statistics.actual.shareCount))
  t2$date <- lubridate::as_datetime(t2$date)
  t2$is_coordinated <- factor(t2$is_coordinated, ordered =T, levels = c("TRUE","FALSE"), labels = c("coordinated", "not coordinated"))

  if("history" %in% colnames(t)) {
    # add historicaly data
    temp <- t2["date"]
    temp <- dplyr::bind_rows(temp, shares_cumsum)
    temp <- dplyr::arrange(temp, date)

    # estimates shares at post time
    temp <- imputeTS::na_interpolation(temp)

    t2 <- merge(temp, t2[1:7], by = "date")
    rm(temp)
  }

  p <- ggplot2::ggplot(data = t2, ggplot2::aes(x=date, y=shares_cumsum, label=account.name)) +
    ggplot2::geom_line(color="gray")+
    ggplot2::geom_point(mapping = ggplot2::aes(size=subscriberCount, color=is_coordinated),alpha = .5)+
    ggsci::scale_colour_startrek()+
    ggplot2::scale_size(range = c(0, 20))+
    ggplot2::theme_minimal()+
    ggplot2::labs(title = paste0("Link sharing activity for ", t2$expanded[top_url_id], " (", t2$date[top_url_id], ")"),
                  color="COORDINATION",
                  size="SUBSCRIBERS",
                  x="Time (UTC)",
                  y="Total number of shares")

  plotly::ggplotly(p) %>% layout(legend = list(orientation = "h", y = 0.1, x = 0.8))
}
