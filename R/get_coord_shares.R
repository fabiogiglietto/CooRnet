get_coord_shares <- function(ct_shares.df, coordination_interval=NULL, parallel=FALSE, percentile_edge_weight=0.90, clean_urls=FALSE){
  
  require(tidyr)      # 1.0.2
  require(dplyr)      # 0.8.3
  require(igraph)     # 1.2.4.2
  
  options(warn=-1)
  
  # estimate coordination interval if not specified by the users
  if(is.null(coordination_interval)){
    coordination_interval <- estimate_coord_interval(ct_shares.df)
    coordination_interval <- coordination_interval[[2]]
    
    # unnest expanded url and clean-up
    ct_shares.df <- unnest_ctshares(ct_shares.df)
  }
  
  if(is.list(coordination_interval)){
    coordination_interval <- coordination_interval[[2]]
    
    # unnest expanded url and clean-up
    ct_shares.df <- unnest_ctshares(ct_shares.df)
  }
  
  if(is.numeric(coordination_interval)){
    
    coordination_interval <- paste(coordination_interval, "secs")
    
    # unnest expanded url and clean-up
    ct_shares.df <- unnest_ctshares(ct_shares.df, clean_urls = TRUE)
    
    if (file.exists("log.txt")) {
      write(paste("coordination interval set by the user:", coordination_interval), file="log.txt", append=TRUE)
    } else {
      write(paste("#################### CLSB - LOG FILE #####################\n",
                  "coordination interval set by the user:", coordination_interval),
            file="log.txt")
    }
  }
  
  # get a list of all shared URLs
  URLs <- as.data.frame(table(ct_shares.df$expanded))
  names(URLs) <- c("URL", "ct_shares")
  URLs <- subset(URLs, URLs$ct_shares>1) # remove URLs shared only 1 time
  URLs$URL <- as.character(URLs$URL)
  
  ###############
  # Parallel ####
  ###############
  
  if(parallel==TRUE){
    
    require(doSNOW)     # 1.0.18
    require(parallel)   # 3.6.3
    
    # setup parallel backend to use many processors
    cores <- detectCores()-1
    cl <- makeCluster(cores)
    registerDoSNOW(cl) # Register cores for Parallel Computing
    
    # set progress bar
    pb <- txtProgressBar(max=nrow(URLs), style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    progress_bar <- list(progress=progress)
    
    # cycle trough all URLs to find entities that shared the same link within the coordination internal
    dat.summary <-
      foreach(i=seq(1:nrow(URLs)), .combine = rbind, .packages="dplyr", .options.snow=progress_bar) %dopar% {
        
        # show progress...
        setTxtProgressBar(pb, pb$getVal()+1)
        
        url <- URLs$URL[i]
        temp <- subset(ct_shares.df, ct_shares.df$expanded==url)
        
        if (length(unique(temp$account.url)) > 1) {
          dat.summary <- temp %>%
            mutate(cut = cut(as.POSIXct(date), breaks = coordination_interval)) %>%
            group_by(cut) %>%
            mutate(count=n(),
                   account.url=list(account.url),
                   share_date=list(date),
                   url = url) %>%
            select(cut, count, account.url, share_date, url) %>%
            filter(count > 1) %>%   # subset URLs shared by more than one entity
            unique()
          
          return(dat.summary)
        }
      }
    
    stopCluster(cl)
    
    if(nrow(dat.summary)==0){
      stop("there are not enough shares!")
    }
    
    # unnest and return coordinated_shares in the environment
    coordinated_shares <- unnest(dat.summary, cols = c(account.url, share_date))
    rm(dat.summary)
    
    # mark coordinated shares
    ct_shares.df$iscoordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                           ct_shares.df$date %in% coordinated_shares$share_date &
                                           ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)
    
    
    highly_c_list <- build_coord_graph(ct_shares.df, coordinated_shares)
    
    highly_connected_g <- highly_c_list[[1]]
    highly_connected_coordinated_entities <- highly_c_list[[1]]
    rm(highly_c_list)
    
    uniqueURLs_shared <- unique(ct_shares.df[, c("expanded", "iscoordinated")])
    
    
    write(paste("number of unique URLs shared in coordinated way:", table(uniqueURLs_shared$iscoordinated)[2][[1]], paste0("(", round((table(uniqueURLs_shared$iscoordinated)[2][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\nnumber of unique URLs shared in non-coordinated way:", table(uniqueURLs_shared$iscoordinated)[1][[1]], paste0("(", round((table(uniqueURLs_shared$iscoordinated)[1][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\npercentile_edge_weight:", percentile_edge_weight, paste0("(quantile: ", q, ")"),
                "\nhighly connected coordinated entities:", length(unique(highly_connected_coordinated_entities$name)),
                "\nnumber of component:", length(unique(highly_connected_coordinated_entities$component))),
          file="log.txt", append=TRUE)
    
    
    results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)
    
    
    return(results_list)
    
  }
  
  ###################
  # Non Parallel ####
  ###################
  
  if(parallel==FALSE){
    
    # initialize an empty object for results
    datalist <- list()
    
    # progress bar
    total <- nrow(URLs)
    pb <- txtProgressBar(max=total, style=3)
    
    for (i in 1:nrow(URLs)) {
      # show progress...
      setTxtProgressBar(pb, pb$getVal()+1)
      
      url <- URLs$URL[i]
      temp <- subset(ct_shares.df, ct_shares.df$expanded==url)
      # subset temp (all shares of one URL) by time published by differnet entities in a coordination.interval
      
      if (length(unique(temp$account.url)) > 1) {
        dat.summary <- temp %>%
          mutate(cut = cut(as.POSIXct(date), breaks = coordination_interval)) %>%
          group_by(cut) %>%
          mutate(count=n(),
                 account.url=list(account.url),
                 share_date=list(date),
                 url = url) %>%
          select(cut, count, account.url, share_date, url) %>%
          filter(count > 1) %>%
          unique()
        
        datalist <- c(list(dat.summary), datalist)
      }
    }
    
    df <- bind_rows(datalist)
    
    
    if(nrow(df)==0){
      stop("there are not enough shares!")
    }
    
    # unnest and return coordinated_shares in the environment
    coordinated_shares <- unnest(df, cols = c(account.url, share_date))
    rm(datalist, df)
    
    # mark coordinated shares
    ct_shares.df$iscoordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                           ct_shares.df$date %in% coordinated_shares$share_date &
                                           ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)
    
    
    highly_c_list <- build_coord_graph(ct_shares.df, coordinated_shares)
    
    highly_connected_g <- highly_c_list[[1]]
    highly_connected_coordinated_entities <- highly_c_list[[1]]
    rm(highly_c_list)
    
    uniqueURLs_shared <- unique(ct_shares.df[, c("expanded", "iscoordinated")])
    
    write(paste("number of unique URLs shared in coordinated way:", table(uniqueURLs_shared$iscoordinated)[2][[1]], paste0("(", round((table(uniqueURLs_shared$iscoordinated)[2][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\nnumber of unique URLs shared in non-coordinated way:", table(uniqueURLs_shared$iscoordinated)[1][[1]], paste0("(", round((table(uniqueURLs_shared$iscoordinated)[1][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\npercentile_edge_weight:", percentile_edge_weight, paste0("(quantile: ", q, ")"),
                "\nhighly connected coordinated entities:", length(unique(highly_connected_coordinated_entities$name)),
                "\nnumber of component:", length(unique(highly_connected_coordinated_entities$component))),
          file="log.txt", append=TRUE)
    
    results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)
    
    return(results_list)
  }
}
