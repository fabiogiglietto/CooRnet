#' @importFrom dplyr group_by mutate %>% summarize first
#' @importFrom igraph graph.data.frame simplify bipartite.projection induced_subgraph subgraph.edges degree V V<- E E<- components strength as_edgelist get.edge.ids neighbors
#' @importFrom jsonlite flatten

build_coord_graph <- function(ct_shares.df, coordinated_shares, percentile_edge_weight=0.90, timestamps=FALSE) {

  ###########################################
  # Get coordinated entities and network ####
  ###########################################

  cat("\nBuilding the graph...")

  el <- coordinated_shares[,c("account.url", "url", "share_date")] # drop unnecessary columns
  ct_shares.df <- jsonlite::flatten(ct_shares.df)

  if (any(el$url %in% el$account.url)) {
    el$url[el$url %in% el$account.url] <- sapply(el$url[el$url %in% el$account.url], function(x) paste(as.character(x),"?coornet_type=URL",sep = "")) # remove white space from account.url and add a custom parameter to avoid issue with duplicated vertex when the URL of an entity is also in the list of shared URLs
  }

  v1 <- data.frame(node=unique(el$account.url), type=1) # create a dataframe with nodes and type 0=url 1=page
  v2 <- data.frame(node=unique(el$url), type=0)
  v <- rbind(v1,v2)

  g2.bp <- igraph::graph.data.frame(el, directed = T, vertices = v) # makes the bipartite graph
  g2.bp <- igraph::simplify(g2.bp, remove.multiple = T, remove.loops = T, edge.attr.comb = "min") # simplify the bipartite network to avoid problems with resulting edge weight in projected network
  full_g <- suppressWarnings(igraph::bipartite.projection(g2.bp, multiplicity = T)$proj2) # project entity-entity network

  all_account_info <- ct_shares.df %>%
    dplyr::group_by(account.url) %>%
    dplyr::mutate(account.name.changed = ifelse(length(unique(account.name))>1, TRUE, FALSE), # deal with account.data that may have changed (name, handle, pageAdminTopCountry, pageDescription, pageCategory)
                  account.name = paste(unique(account.name), collapse = " | "),
                  account.handle.changed = ifelse(length(unique(account.handle))>1, TRUE, FALSE),
                  account.handle = paste(unique(account.handle), collapse = " | "),
                  account.pageAdminTopCountry.changed = ifelse(length(unique(account.pageAdminTopCountry))>1, TRUE, FALSE),
                  account.pageAdminTopCountry = paste(unique(account.pageAdminTopCountry), collapse = " | "),
                  account.pageDescription.changed = ifelse(length(unique(account.pageDescription))>1, TRUE, FALSE),
                  account.pageDescription = paste(unique(account.pageDescription), collapse = " | "),
                  account.pageCategory.changed = ifelse(length(unique(account.pageCategory))>1, TRUE, FALSE),
                  account.pageCategory = paste(unique(account.pageCategory), collapse = " | ")) %>%
    dplyr::summarize(shares = n(),
                     coord.shares = sum(is_coordinated==TRUE),
                     account.avg.subscriberCount=mean(account.subscriberCount),
                     account.platformId = dplyr::first(account.platformId),
                     account.name = dplyr::first(account.name), # name
                     account.name.changed = dplyr::first(account.name.changed),
                     account.handle.changed = dplyr::first(account.handle.changed), # handle
                     account.handle = dplyr::first(account.handle),
                     account.pageAdminTopCountry.changed= dplyr::first(account.pageAdminTopCountry.changed), # AdminTopCountry
                     account.pageAdminTopCountry= dplyr::first(account.pageAdminTopCountry),
                     account.pageDescription.changed = dplyr::first(account.pageDescription.changed), # PageDescription
                     account.pageDescription = dplyr::first(account.pageDescription),
                     account.pageCategory.changed = dplyr::first(account.pageCategory.changed), # PageCategory
                     account.pageCategory = dplyr::first(account.pageCategory),
                     account.pageCreatedDate = dplyr::first(account.pageCreatedDate), # DateCreated
                     account.platform = dplyr::first(account.platform),
                     account.platformId = dplyr::first(account.platformId),
                     account.verified = dplyr::first(account.verified),
                     account.accountType = dplyr::first(account.accountType))

  rm(ct_shares.df, coordinated_shares)

  # add vertex attributes
  vertex.info <- subset(all_account_info, as.character(all_account_info$account.url) %in% V(full_g)$name)

  V(full_g)$shares <- sapply(V(full_g)$name, function(x) vertex.info$shares[vertex.info$account.url == x])
  V(full_g)$coord.shares <- sapply(V(full_g)$name, function(x) vertex.info$coord.shares[vertex.info$account.url == x])
  V(full_g)$account.avg.subscriberCount <- sapply(V(full_g)$name, function(x) vertex.info$account.avg.subscriberCount[vertex.info$account.url == x])
  V(full_g)$account.platform <- sapply(V(full_g)$name, function(x) vertex.info$account.platform[vertex.info$account.url == x])
  V(full_g)$account.name <- sapply(V(full_g)$name, function(x) vertex.info$account.name[vertex.info$account.url == x])
  V(full_g)$account.platformId<- sapply(V(full_g)$name, function(x) vertex.info$account.platformId[vertex.info$account.url == x])
  V(full_g)$name.changed <- sapply(V(full_g)$name, function(x) vertex.info$account.name.changed[vertex.info$account.url == x])
  V(full_g)$account.handle <- sapply(V(full_g)$name, function(x) vertex.info$account.handle[vertex.info$account.url == x])
  V(full_g)$handle.changed <- sapply(V(full_g)$name, function(x) vertex.info$account.handle.changed[vertex.info$account.url == x])
  V(full_g)$account.verified <- sapply(V(full_g)$name, function(x) vertex.info$account.verified[vertex.info$account.url == x])
  V(full_g)$pageAdminTopCountry.changed <- sapply(V(full_g)$name, function(x) vertex.info$account.pageAdminTopCountry.changed[vertex.info$account.url == x])
  V(full_g)$account.pageAdminTopCountry <- sapply(V(full_g)$name, function(x) vertex.info$account.pageAdminTopCountry[vertex.info$account.url == x])
  V(full_g)$account.accountType <- sapply(V(full_g)$name, function(x) vertex.info$account.accountType[vertex.info$account.url == x])
  V(full_g)$account.pageCreatedDate <- sapply(V(full_g)$name, function(x) vertex.info$account.pageCreatedDate[vertex.info$account.url == x])
  V(full_g)$account.pageDescription <- sapply(V(full_g)$name, function(x) vertex.info$account.pageDescription[vertex.info$account.url == x])
  V(full_g)$account.pageDescription.changed <- sapply(V(full_g)$name, function(x) vertex.info$account.pageDescription.changed[vertex.info$account.url == x])
  V(full_g)$account.pageCategory <- sapply(V(full_g)$name, function(x) vertex.info$account.pageCategory[vertex.info$account.url == x])
  V(full_g)$account.pageCategory.changed <- sapply(V(full_g)$name, function(x) vertex.info$account.pageCategory.changed[vertex.info$account.url == x])

   # keep only highly coordinated entities
   V(full_g)$degree <- igraph::degree(full_g)
   q <- quantile(E(full_g)$weight, percentile_edge_weight) # set the percentile_edge_weight number of repetedly coordinated link sharing to keep
   highly_connected_g <- igraph::induced_subgraph(graph = full_g, vids = V(full_g)[V(full_g)$degree > 0 ]) # filter for degree
   highly_connected_g <- igraph::subgraph.edges(highly_connected_g, eids = which(E(highly_connected_g)$weight >= q),delete.vertices = T) # filter for edge weight

   if (timestamps==TRUE) {
     cat("\n\nAdding timestamps. Please be patient... :)")

     # timestamp of coordinated sharing as edge attribute

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
  V(highly_connected_g)$cluster <- igraph::cluster_louvain(highly_connected_g)$membership # add cluster to simplyfy the analysis of large components
  V(highly_connected_g)$degree <- igraph::degree(highly_connected_g) # re-calculate the degree on the subgraph
  V(highly_connected_g)$strength <- igraph::strength(highly_connected_g) # sum up the edge weights of the adjacent edges for each vertex

  highly_connected_coordinated_entities <- igraph::as_data_frame(highly_connected_g, "vertices")
  rownames(highly_connected_coordinated_entities) <- 1:nrow(highly_connected_coordinated_entities)
  #colnames(all_account_info)["name"] <- "name" # please use column name and not number

  highly_c_list <- list(highly_connected_g, highly_connected_coordinated_entities, q)

  cat("\nDone!")

  return(highly_c_list)
}
