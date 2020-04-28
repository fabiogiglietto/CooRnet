#' @importFrom dplyr group_by mutate %>% summarize
#' @importFrom igraph graph.data.frame simplify bipartite.projection induced_subgraph subgraph.edges degree V V<- E E<- components strength as_edgelist get.edge.ids neighbors

build_coord_graph <- function(ct_shares.df, coordinated_shares, percentile_edge_weight=0.90, timestamps=FALSE) {

  ###########################################
  # Get coordinated entities and network ####
  ###########################################

  cat("\nBuilding the graph...")

  el <- coordinated_shares[,c("account.url", "url", "share_date")] # drop unnecesary columns
  el$url[el$url %in% el$account.url] <- sapply(el$url[el$url %in% el$account.url], function(x) paste(as.character(x),"?coornet_type=URL",sep = "")) # remove white space from account.url and add a custom parameter to avoid issue with duplicated vertexed when the URL of an entity is also in the list of shared URLs
  v1 <- data.frame(node=unique(el$account.url), type=1) # create a dataframe with nodes and type 0=url 1=page
  v2 <- data.frame(node=unique(el$url), type=0)
  v <- rbind(v1,v2)

  g2.bp <- igraph::graph.data.frame(el, directed = T, vertices = v) # makes the biap
  g2.bp <- igraph::simplify(g2.bp, remove.multiple = T, remove.loops = T, edge.attr.comb = "min") # simply the bipartite network to avoid problems with resulting edge weight in projected network
  full_g <- suppressWarnings(igraph::bipartite.projection(g2.bp, multiplicity = T)$proj2) # project entity-entity network

  all_account_info <- ct_shares.df %>%
    group_by(account.url) %>%
    mutate(name.changed = ifelse(length(unique(account.name))>1, TRUE, FALSE),
           handle.changed = ifelse(length(unique(account.handle))>1, TRUE, FALSE),
           account.name = paste(unique(account.name), collapse = " | "),
           account.handle = paste(unique(account.handle), collapse = " | ")) %>%
    summarize(shares = n(),
              coord.shares = sum(is_coordinated==TRUE),
              avg.account.subscriberCount=mean(account.subscriberCount),
              account.id = first(account.id),
              account.name=first(account.name),
              name.changed=first(name.changed),
              handle.changed=first(handle.changed),
              account.handle=first(account.handle),
              account.platform=first(account.platform),
              account.platformId=first(account.platformId),
              account.verified=first(account.verified))

  rm(ct_shares.df, coordinated_shares)

  # add vertex attributes
  vertex.info <- subset(all_account_info, as.character(all_account_info$account.url) %in% V(full_g)$name)

  V(full_g)$shares <- sapply(V(full_g)$name, function(x) vertex.info$shares[vertex.info$account.url == x])
  V(full_g)$coord.shares <- sapply(V(full_g)$name, function(x) vertex.info$coord.shares[vertex.info$account.url == x])
  V(full_g)$avg.account.subscriberCount <- sapply(V(full_g)$name, function(x) vertex.info$avg.account.subscriberCount[vertex.info$account.url == x])
  V(full_g)$account.platform <- sapply(V(full_g)$name, function(x) vertex.info$account.platform[vertex.info$account.url == x])
  V(full_g)$account.name <- sapply(V(full_g)$name, function(x) vertex.info$account.name[vertex.info$account.url == x])
  V(full_g)$account.verified <- sapply(V(full_g)$name, function(x) vertex.info$account.verified[vertex.info$account.url == x])
  V(full_g)$account.handle <- sapply(V(full_g)$name, function(x) vertex.info$account.handle[vertex.info$account.url == x])
  V(full_g)$name.changed <- sapply(V(full_g)$name, function(x) vertex.info$name.changed[vertex.info$account.url == x])
  V(full_g)$handle.changed <- sapply(V(full_g)$name, function(x) vertex.info$handle.changed[vertex.info$account.url == x])

  # keep only highly coordinated entities
  V(full_g)$degree <- igraph::degree(full_g)
  q <- quantile(E(full_g)$weight, percentile_edge_weight) # set the percentile_edge_weight number of repetedly coordinated link sharing to keep
  highly_connected_g <- igraph::induced_subgraph(graph = full_g, vids = V(full_g)[V(full_g)$degree > 0 ]) # filter for degree
  highly_connected_g <- igraph::subgraph.edges(highly_connected_g, eids = which(E(highly_connected_g)$weight >= q),delete.vertices = T) # filter for edge weight

  if (timestamps==TRUE) {
    cat("\n\nAdding timestamps. Please be patient... :)")

    # timestamp of coordinated sharing as edge atribute

    EL <- as.data.frame(igraph::as_edgelist(highly_connected_g))
    EL$weight <- E(highly_connected_g)$weight
    EL$coord_shares <- 0
    names(EL) <- c("V1","V2","weight","coord_share")

    coord_shares <- list()

    EL <- as_tibble(EL)

    ts_on_edge <- function(x,output){
      shared_2 <- intersect(igraph::neighbors(graph = g2.bp,v = V(g2.bp)[name==x[1]],mode = "out"),
                            igraph::neighbors(graph = g2.bp,v = V(g2.bp)[name==x[2]],mode = "out"))
      cs <- rep(NA,length(shared_2))
      cs <- sapply(shared_2, function(i) E(g2.bp)[igraph::get.edge.ids(graph = g2.bp,directed = F,vp = c(i,V(g2.bp)[name==x[1]]))]$share_date)

      return(cs)
    }
    E(highly_connected_g)$t_coord_share <- apply(EL,1,ts_on_edge)
  }

  # find and annotate nodes-components
  V(highly_connected_g)$component <- igraph::components(highly_connected_g)$membership
  V(highly_connected_g)$degree <- igraph::degree(highly_connected_g) # re-calculate the degree on the subgraph
  V(highly_connected_g)$strength <- igraph::strength(highly_connected_g) # sum up the edge weights of the adjacent edges for each vertex


  highly_connected_coordinated_entities <- igraph::as_data_frame(highly_connected_g, "vertices")
  rownames(highly_connected_coordinated_entities) <- 1:nrow(highly_connected_coordinated_entities)
  #colnames(all_account_info)["name"] <- "name" # please use column name and not number

  highly_c_list <- list(highly_connected_g, highly_connected_coordinated_entities, q)

  cat("\nDone!")

  return(highly_c_list)
}
