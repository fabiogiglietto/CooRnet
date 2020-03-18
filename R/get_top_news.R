
get_top_news <- function(output, order_by = "engagement", component=TRUE, top=10){

  require(dplyr) # 0.8.3

  coord_shares_urls <- output[[1]][output[[1]]$iscoordinated==TRUE & output[[1]]$account.url %in% output[[3]]$name,]
  coord_shares_urls$engagement <- apply(coord_shares_urls[,c("statistics.actual.likeCount",
                                                             "statistics.actual.shareCount",
                                                             "statistics.actual.commentCount",
                                                             "statistics.actual.loveCount",
                                                             "statistics.actual.wowCount",
                                                             "statistics.actual.hahaCount",
                                                             "statistics.actual.sadCount",
                                                             "statistics.actual.angryCount")], 1, sum)
  colnames(output[[3]])[1] <- "account.url"
  coord_shares_urls <- merge(coord_shares_urls, output[[3]][,c("account.url","component")], by="account.url", all.x=T)
  coord_shares_urls <- coord_shares_urls[,c("account.url","date","title","description","message","link","postUrl",
                                            "account.name","account.handle","account.subscriberCount","expanded",
                                            "statistics.actual.likeCount", "statistics.actual.shareCount",
                                            "statistics.actual.commentCount", "statistics.actual.loveCount",
                                            "statistics.actual.wowCount", "statistics.actual.hahaCount",
                                            "statistics.actual.sadCount","statistics.actual.angryCount",
                                            "engagement","component")]

  order_by <- grep(order_by, colnames(coord_shares_urls))

  if(component==TRUE){

  top_news <-
    coord_shares_urls %>%
    arrange(component, desc(coord_shares_urls[,order_by])) %>%
    group_by(component) %>%
    slice(1:top)

  return(top_news)

  }

  if(component!=TRUE){
      top_news <-
        coord_shares_urls %>%
        arrange(account.url, desc(coord_shares_urls[,order_by])) %>%
        group_by(account.url) %>%
        slice(1:top) %>%
        arrange(component)

      return(top_news)

      }
  }

