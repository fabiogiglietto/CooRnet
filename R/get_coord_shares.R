get_coord_shares <- function(ct_shares.df, coordination_interval=NULL, parallel=FALSE, percentile_edge_weight=0.90, clean_urls=FALSE){

  require(tidyr)      # 1.0.2
  require(dplyr)      # 0.8.3
  require(igraph)     # 1.2.4.2

  options(warn=-1)

  # estimate coordination interval if not specified by the users
  if(is.null(coordination_interval)){
    coordination_interval <- estimate_coord_interval(df)
    coordination_interval <- coordination_interval[[2]]

    # unnest expanded url and clean-up
    ct_shares.df <- unnest(ct_shares.df, cols = expandedLinks)
    ct_shares.df$original <- NULL

    # remove duplicates created by the unnesting
    ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]
  }

  if(is.list(coordination_interval)){
    coordination_interval <- coordination_interval[[2]]

    # unnest expanded url and clean-up
    ct_shares.df <- unnest(ct_shares.df, cols = expandedLinks)
    ct_shares.df$original <- NULL

    # remove duplicates created by the unnesting
    ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]
  }

  if(is.numeric(coordination_interval)){

    coordination_interval <- paste(coordination_interval, "secs")

    # unnest expanded url and clean-up
    ct_shares.df <- unnest(ct_shares.df, cols = expandedLinks)
    ct_shares.df$original <- NULL

    # clean urls
    if(clean_urls==TRUE){
      ct_shares.df <- clean_urls(ct_shares.df, "expanded")
    }

    # remove duplicates created by the unnesting
    ct_shares.df <- ct_shares.df[!duplicated(ct_shares.df[,c("id", "platformId", "postUrl", "expanded")]),]

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

    # mark coordinated shares
    ct_shares.df$iscoordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                           ct_shares.df$date %in% coordinated_shares$share_date &
                                           ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)


    ###########################################
    # Get coordinated entities and network ####
    ###########################################

    el <- coordinated_shares[,c(3,5,4)] # drop unnecesary columns
    el$account.url <- trimws(el$account.url) # remove white space from platform.id
    v1 <- data.frame(node=unique(el$account.url), type=1) # create a dataframe with nodes and type 0=url 1=page
    v2 <- data.frame(node=unique(el$url), type=0)
    v <- rbind(v1,v2)

    g2.bp <- graph.data.frame(el,directed = T, vertices = v) # makes the biap
    g2.bp <- igraph::simplify(g2.bp, remove.multiple = T, remove.loops = T ,edge.attr.comb = "min") # simply the bipartite netwrok to avoid problems with resulting edge weight in projected network
    full_g <- suppressWarnings(bipartite.projection(g2.bp, multiplicity = T)$proj2) # project page-page network

    all_account_info <- ct_shares.df %>%
      group_by(account.url) %>%
      summarize(shares = n(),
                avg.account.subscriberCount=mean(account.subscriberCount))

    # group the pages that changed names
    ct_shares.df <- ct_shares.df %>%
      group_by(account.url) %>%
      mutate(name.changed = ifelse(length(unique(account.name))>1, TRUE, FALSE),
             account.name = paste(unique(account.name), collapse = " | "))

    more.account.info <- ct_shares.df[, c("account.id", "account.name", "name.changed", "account.handle",
                                          "account.url", "account.platform", "account.platformId", "account.verified")]

    more.account.info <- unique(more.account.info)
    all_account_info <- merge(all_account_info, more.account.info, by="account.url")


    # add vertex attributes
    vertex.info <- subset(all_account_info, as.character(all_account_info$account.url) %in% V(full_g)$name)

    V(full_g)$shares <- sapply(V(full_g)$name, function(x) vertex.info$shares[vertex.info$account.url == x])
    V(full_g)$avg.account.subscriberCount <- sapply(V(full_g)$name, function(x) vertex.info$avg.account.subscriberCount[vertex.info$account.url == x])
    V(full_g)$account.platform <- sapply(V(full_g)$name, function(x) vertex.info$account.platform[vertex.info$account.url == x])
    V(full_g)$account.name <- sapply(V(full_g)$name, function(x) vertex.info$account.name[vertex.info$account.url == x])
    V(full_g)$account.verified <- sapply(V(full_g)$name, function(x) vertex.info$account.verified[vertex.info$account.url == x])
    V(full_g)$account.handle <- sapply(V(full_g)$name, function(x) vertex.info$account.handle[vertex.info$account.url == x])


    # timestamp of coordinated sharing as edge atribute

    full_g <-  set.edge.attribute(graph = full_g,name = "t_coord_share",value = 0)
    for (v in 1:length(shared)){
      timestamps <- incident(g2.bp,v = V(g2.bp)[V(g2.bp)$name==shared[v]$name])$timestamp
      n <- neighbors(g2.bp,v = V(g2.bp)[V(g2.bp)$name==shared[v]$name],mode = "in")$name
      edges <- expand.grid(n,n)
      edges <- edges[edges$Var1 != edges$Var2,]
      edges <- edges[!duplicated(t(apply(edges, 1, sort))),]
      if(nrow(edges) >0){
        e <- get.edge.ids(full_g, as.vector(t(edges)))
        for (i in 1:length(e)){
          if (E(full_g)[e][i]$t_coord_share != 0){E(full_g)[e][i]$t_coord_share <-  paste(E(full_g)[e][i]$t_coord_share,min(timestamps),sep = ";")}
          if (E(full_g)[e][i]$t_coord_share == 0){E(full_g)[e][i]$t_coord_share <-  min(timestamps)}


        }
      }

    }


    # keep only highly coordinated entities
    V(full_g)$degree <- degree(full_g)
    q <- quantile(E(full_g)$weight, percentile_edge_weight) # set the percentile_edge_weight number of repetedly coordinated link sharing to keep
    highly_connected_g <- induced_subgraph(graph = full_g, vids = V(full_g)[V(full_g)$degree > 0 ]) # filter for degree
    highly_connected_g <- subgraph.edges(highly_connected_g, eids = which(E(highly_connected_g)$weight >= q),delete.vertices = T) # filter for edge weight
    # find and annotate nodes-components
    V(highly_connected_g)$component <- components(highly_connected_g)$membership

    highly_connected_coordinated_entities <- igraph::as_data_frame(highly_connected_g, "vertices")
    rownames(highly_connected_coordinated_entities) <- 1:nrow(highly_connected_coordinated_entities)
    colnames(more.account.info)[5] <- "name"
    highly_connected_coordinated_entities <- merge(highly_connected_coordinated_entities, unique(more.account.info[, c("name", "name.changed")]), by="name", all.x=T)
    highly_connected_coordinated_entities <- highly_connected_coordinated_entities[, c(1:5,10,6:9)]

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

    # mark coordinated shares
    ct_shares.df$iscoordinated <- ifelse(ct_shares.df$expanded %in% coordinated_shares$url &
                                           ct_shares.df$date %in% coordinated_shares$share_date &
                                           ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)


    ###########################################
    # Get coordinated entities and network ####
    ###########################################

    el <- coordinated_shares[,c(3,5,4)] # drop unnecesary columns
    el$account.url <- trimws(el$account.url) # remove white space from platform.id
    v1 <- data.frame(node=unique(el$account.url), type=1) # create a dataframe with nodes and type 0=url 1=page
    v2 <- data.frame(node=unique(el$url), type=0)
    v <- rbind(v1,v2)

    g2.bp <- graph.data.frame(el,directed = T, vertices = v) # makes the binet
    E(g2.bp)$timestamp <- el$share_date
    g2.bp <- igraph::simplify(g2.bp, remove.multiple = T, remove.loops = T,edge.attr.comb="min") # simply the netwrok to avoid problems with resulting edge weight in projected network
    full_g <- suppressWarnings(bipartite.projection(g2.bp, multiplicity = T)$proj2) # project page-page network

    # get a list of unique entities (pages/groups/verified profiles/instagram accounts)
    all_account_info <- ct_shares.df %>%
      group_by(account.url) %>%
      summarize(shares = n(),
                avg.account.subscriberCount=mean(account.subscriberCount))

    # group the pages that changed names
    ct_shares.df <- ct_shares.df %>%
      group_by(account.url) %>%
      mutate(name.changed = ifelse(length(unique(account.name))>1, TRUE, FALSE),
             account.name = paste(unique(account.name), collapse = " | "))

    more.account.info <- ct_shares.df[, c("account.id", "account.name", "name.changed", "account.handle",
                                          "account.url", "account.platform", "account.platformId", "account.verified")]

    more.account.info <- unique(more.account.info)
    all_account_info <- merge(all_account_info, more.account.info, by="account.url")

    # add vertex attributes
    vertex.info <- subset(all_account_info, as.character(all_account_info$account.url) %in% V(full_g)$name)

    V(full_g)$shares <- sapply(V(full_g)$name, function(x) vertex.info$shares[vertex.info$account.url == x])
    V(full_g)$avg.account.subscriberCount <- sapply(V(full_g)$name, function(x) vertex.info$avg.account.subscriberCount[vertex.info$account.url == x])
    V(full_g)$account.platform <- sapply(V(full_g)$name, function(x) vertex.info$account.platform[vertex.info$account.url == x])
    V(full_g)$account.name <- sapply(V(full_g)$name, function(x) vertex.info$account.name[vertex.info$account.url == x])
    V(full_g)$account.verified <- sapply(V(full_g)$name, function(x) vertex.info$account.verified[vertex.info$account.url == x])
    V(full_g)$account.handle <- sapply(V(full_g)$name, function(x) vertex.info$account.handle[vertex.info$account.url == x])

    # timestamp of coordinated sharing as edge atribute
    full_g <-  set.edge.attribute(graph = full_g,name = "t_coord_share",value = 0)
    for (v in 1:length(shared)){
      timestamps <- incident(g2.bp,v = V(g2.bp)[V(g2.bp)$name==shared[v]$name])$timestamp
      n <- neighbors(g2.bp,v = V(g2.bp)[V(g2.bp)$name==shared[v]$name],mode = "in")$name
      edges <- expand.grid(n,n)
      edges <- edges[edges$Var1 != edges$Var2,]
      edges <- edges[!duplicated(t(apply(edges, 1, sort))),]
      if(nrow(edges) >0){
        e <- get.edge.ids(full_g, as.vector(t(edges)))
        for (i in 1:length(e)){
          if (E(full_g)[e][i]$t_coord_share != 0){E(full_g)[e][i]$t_coord_share <-  paste(E(full_g)[e][i]$t_coord_share,min(timestamps),sep = ";")}
          if (E(full_g)[e][i]$t_coord_share == 0){E(full_g)[e][i]$t_coord_share <-  min(timestamps)}


        }
      }

    }



    # keep only highly coordinated entities
    V(full_g)$degree <- degree(full_g)
    q <- quantile(E(full_g)$weight, percentile_edge_weight) # set the percentile_edge_weight number of repetedly coordinated link sharing to keep
    highly_connected_g <- induced_subgraph(graph = full_g, vids = V(full_g)[V(full_g)$degree > 0 ]) # filter for degree
    highly_connected_g <- subgraph.edges(highly_connected_g, eids = which(E(highly_connected_g)$weight >= q),delete.vertices = T) # filter for edge weight
    # find and annotate nodes-components
    V(highly_connected_g)$component <- components(highly_connected_g)$membership

    highly_connected_coordinated_entities <- igraph::as_data_frame(highly_connected_g, "vertices")
    rownames(highly_connected_coordinated_entities) <- 1:nrow(highly_connected_coordinated_entities)
    colnames(more.account.info)[5] <- "name"
    highly_connected_coordinated_entities <- merge(highly_connected_coordinated_entities, unique(more.account.info[, c("name", "name.changed")]), by="name", all.x=T)
    highly_connected_coordinated_entities <- highly_connected_coordinated_entities[, c(1:5,10,6:9)]

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
