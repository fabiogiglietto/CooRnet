#' get_outputs
#'
#' A function to get the outputs of the get_coord_shares function
#'
#' @param coord_shares_output the list from the get_coord_shares function
#' @param ct_shares_marked.df a dataframe that contains all the retrieved social media shares plus an extra boolean variable (is_coordinated) that identify if the shares was coordinated (default TRUE)
#' @param highly_connected_g graph of coordinated networks of pages/groups/accounts (default TRUE)
#' @param highly_connected_coordinated_entities a dataframe that lists coordinated entities and corresponding component (default TRUE)
#'
#' @examples
#' get_outputs(get_coord_shares_output)
#'
#' @export

get_outputs <- function(coord_shares_output,
                        ct_shares_marked.df = TRUE,
                        highly_connected_g = TRUE,
                        highly_connected_coordinated_entities = TRUE){

 if (ct_shares_marked.df)
    ct_shares_marked.df <<- as.data.frame(coord_shares_output[[1]])
 if (highly_connected_g)
    highly_connected_g <<- coord_shares_output[2][[1]]
 if (highly_connected_coordinated_entities)
   highly_connected_coordinated_entities <<- coord_shares_output[[3]]
}
