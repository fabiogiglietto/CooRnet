#' get_outputs
#'
#' A function to get the outputs of the get_coord_shares function
#'
#' @param coord_shares_output the list from the get_coord_shares function
#' @param ct_shares_marked.df a dataframe that contains all the retrieved social media shares plus an extra boolean variable (is_coordinated) that identify if the shares was coordinated (default TRUE)
#' @param highly_connected_g graph of coordinated networks of pages/groups/accounts (default TRUE)
#' @param highly_connected_coordinated_entities a dataframe that lists coordinated entities and corresponding component (default TRUE)
#' @param component_summary the result of CooRnet::get_component_summary (default FALSE)
#' @param cluster_summary the result of CooRnet::get_component_summary (default FALSE)
#' @param top_coord_urls the result of CooRnet::get_top_coord_urls (default FALSE)
#' @param top_coord_shares the result of CooRnet::get_top_coord_shares (default FALSE)
#' @param gdrive_folder_id a Google Drive folder id where to uploads selected files (output.rds, highly_connected_g.graphml, highly_connected_coordinated_entities.csv, component_summary.csv, top_coord_urls.csv, top_coord_shares) files (default NULL)
#'
#' @examples
#' get_outputs(coord_shares_output=output)
#'
#' get_outputs(coord_shares_output=output, gdrive_folder_id="0BwwA4oUTeiV1TGRPeTVjaWRDY1E")
#'
#' @importFrom googledrive drive_upload as_id
#' @importFrom igraph write.graph
#' @importFrom utils write.csv
#'
#' @export

get_outputs <- function(coord_shares_output,
                        ct_shares_marked.df = TRUE,
                        highly_connected_g = TRUE,
                        highly_connected_coordinated_entities = TRUE,
                        component_summary = FALSE,
                        cluster_summary = FALSE,
                        top_coord_urls = FALSE,
                        top_coord_shares = FALSE,
                        gdrive_folder_id = NULL) {

    if (is.null(gdrive_folder_id)) {
        if (ct_shares_marked.df)
            # extracts ct_shares_marked.df to the main environment
            ct_shares_marked.df <<- as.data.frame(coord_shares_output[[1]])
        if (highly_connected_g)
            # extracts highly_connected_g to the main environment
            highly_connected_g <<- coord_shares_output[2][[1]]
        if (highly_connected_coordinated_entities)
            # extracts highly_connected_coordinated_entities to the main environment
            highly_connected_coordinated_entities <<- coord_shares_output[[3]]
        if (component_summary)
            # creates component_summary to the main environment
            component_summary <<- CooRnet::get_component_summary(output = coord_shares_output)
        if (cluster_summary)
            # creates cluster_summary to the main environment
            cluster_summary <<- CooRnet::get_cluster_summary(output = coord_shares_output)
        if (top_coord_urls)
            # creates top_coord_url to the main environment
            top_coord_urls <<- CooRnet::get_top_coord_urls(output = coord_shares_output)
        if (top_coord_shares)
            # creates top_coord_shares to the main environment
            top_coord_shares <<- CooRnet::get_top_coord_shares(output = coord_shares_output)
        }

    else {
        # uploads output.rds to Google folder
        tempFileCon <- tempfile("coord_shares_output", fileext = ".rds")
        saveRDS(coord_shares_output, file = tempFileCon, ascii = FALSE)
        googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

        if (ct_shares_marked.df)
            # extracts ct_shares_marked.df to the main environment
            ct_shares_marked.df <<- as.data.frame(coord_shares_output[[1]])

        if (highly_connected_g) {
            # extracts highly_connected_g to the main environment and uploads to Google folder
            highly_connected_g <- coord_shares_output[2][[1]]

            tempFileCon <- tempfile("highly_connected_g", fileext = ".graphml")
            igraph::write.graph(highly_connected_g, format = "graphml", file = tempFileCon)
            googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

            highly_connected_g <<- highly_connected_g
            }

        if (highly_connected_coordinated_entities) {
            # extracts highly_connected_coordinated_entities to the main environment and uploads to Google folder
            highly_connected_coordinated_entities <- coord_shares_output[[3]]

            tempFileCon <- tempfile("coordinated_entities", fileext = ".csv")
            utils::write.csv(highly_connected_coordinated_entities, file = tempFileCon)
            googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

            highly_connected_coordinated_entities <<- highly_connected_coordinated_entities
        }

        if (component_summary) {
            # creates component_summary to the main environment and uploads to Google folder
            component_summary <- CooRnet::get_component_summary(output = coord_shares_output)

            tempFileCon <- tempfile("comp.summary", fileext = ".csv")
            utils::write.csv(component_summary, file = tempFileCon)
            googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

            component_summary <<- component_summary
        }

        if (cluster_summary) {
            # creates component_summary to the main environment and uploads to Google folder
            cluster_summary <- CooRnet::get_cluster_summary(output = coord_shares_output)

            tempFileCon <- tempfile("cluster.summary", fileext = ".csv")
            utils::write.csv(cluster_summary, file = tempFileCon)
            googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

            cluster_summary <<- cluster_summary
        }

        if (top_coord_urls) {
            # creates top_coord_urls to the main environment and uploads to Google folder
            top_coord_urls <- CooRnet::get_top_coord_urls(output = coord_shares_output)

            tempFileCon <- tempfile("top_coord_urls", fileext = ".csv")
            utils::write.csv(top_coord_urls, file = tempFileCon)
            googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

            top_coord_urls <<- top_coord_urls
        }

        if (top_coord_shares) {
            # creates top_coord_shares to the main environment and uploads to Google folder
            top_coord_shares <- CooRnet::get_top_coord_shares(output = coord_shares_output)

            tempFileCon <- tempfile("top_coord_shares", fileext = ".csv")
            utils::write.csv(top_coord_shares, file = tempFileCon)
            googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))

            top_coord_shares <<- top_coord_shares
        }
    }
}
