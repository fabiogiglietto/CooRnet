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
#' @importFrom dplyr group_by mutate arrange %>% filter ungroup select
#' @importFrom lubridate as_datetime
#' @importFrom ggplot2 ggplot geom_line geom_point scale_size scale_x_datetime theme_minimal labs aes
#' @importFrom plotly ggplotly layout
#' @importFrom ggsci scale_colour_startrek
#' @importFrom imputeTS na_interpolation
#'
#' @export

draw_url_timeline_chart <- function(output, top_coord_urls=NULL, top_url_id=1) {
  ct_shares_marked.df <- output[[1]]
  coordinated_entities <- output[[3]]

  # check if top_coord_urls had been provided
  if (is.null(top_coord_urls)) {
    # nope
    cat("\n\nBuilding a table of top shared URLs...")
    top_coord_urls <- CooRnet::get_top_coord_urls(output = output, order_by = "statistics.actual.shareCount", group_by = "none", top = 10)
  }

  # keep only shares of choosen top urls
  t <- ct_shares_marked.df[ct_shares_marked.df$expanded == top_coord_urls$expanded[top_url_id],]

  if("history" %in% colnames(t)) {
    cat("Analyzing historical engagement data...\n");

    # keep only useful fields
    t <- t[c("account.name", "expanded", "date", "account.subscriberCount", "history", "statistics.actual.shareCount", "is_coordinated")]

    # extract url history
    url.history <- dplyr::bind_rows(t$history, .id = "post_label")

    shares_cumsum <- url.history %>%
      # calculate shares difference between consecutive timesteps
      group_by(post_label) %>%
      arrange(timestep) %>%
      mutate(diff_shares = ifelse(timestep == "0", actual.shareCount, c(0, diff(actual.shareCount, lag = 1)))) %>%
      ungroup() %>%
      mutate(date = as.POSIXct(date)) %>%
      arrange(date) %>%
      mutate(actual.Sharecount_cumsum = cumsum(diff_shares)) %>%
      filter(difftime(date, min(date), units = "secs") <= 604800) %>% # keep only one week hist
      select(date, actual.Sharecount_cumsum)
  }
  else {
    # keep only useful fields
    t <- t[c("account.name", "expanded", "date", "account.subscriberCount", "statistics.actual.shareCount", "is_coordinated")]
  }

  t <- t %>% dplyr::arrange(date)
  t <- t %>% dplyr::group_by() %>%
    dplyr::mutate(actual.Sharecount_cumsum=cumsum(statistics.actual.shareCount))
  t$date <- lubridate::as_datetime(t$date)
  t$is_coordinated <- factor(t$is_coordinated, ordered =T, levels = c("TRUE","FALSE"), labels = c("coordinated", "not coordinated"))

  if("history" %in% colnames(t)) {
    # add historicaly data
    temp <- t["date"]
    temp <- dplyr::bind_rows(temp, shares_cumsum)
    temp <- dplyr::arrange(temp, date)

    # estimates shares at post time
    temp$actual.Sharecount_cumsum[1] <- 0
    temp <- imputeTS::na_interpolation(temp)
    temp$actual.Sharecount_cumsum <- round(temp$actual.Sharecount_cumsum, 0)

    t <- merge(temp, t[1:7], by = "date")
    rm(temp)
  }

  p <- ggplot2::ggplot(data = t, ggplot2::aes(x=date, y=actual.Sharecount_cumsum, label=account.name)) +
    ggplot2::geom_line(color="gray")+
    ggplot2::geom_point(mapping = ggplot2::aes(size=account.subscriberCount, color=is_coordinated),alpha = .5)+
    ggsci::scale_colour_startrek()+
    ggplot2::scale_size(range = c(0, 20))+
    ggplot2::theme_minimal()+
    ggplot2::labs(title = paste0("Link sharing activity for ", t$expanded[top_url_id], " (", t$date[top_url_id], ")"),
                  color="COORDINATION",
                  size="SUBSCRIBERS",
                  x="Time (UTC)",
                  y="Total number of shares")

  plotly::ggplotly(p) %>% plotly::layout(legend = list(orientation = "h", y = 0.1, x = 0.8))
}
