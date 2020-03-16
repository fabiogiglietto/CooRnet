get_outputs <- function(get_coord_shares_output,
                        ct_shares.df = TRUE,
                        highly_connected_g = TRUE,
                        highly_connected_coordinated_entities = TRUE){

 if (ct_shares.df)
    ct_shares.df <<- as.data.frame(get_coord_shares_output[[1]])
 if (highly_connected_g)
    highly_connected_g <<- get_coord_shares_output[2][[1]]
 if (highly_connected_coordinated_entities)
   highly_connected_coordinated_entities <<- get_coord_shares_output[[3]]
}
