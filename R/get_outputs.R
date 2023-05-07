#' get_outputs
#'
#' A function to get the outputs of the get_coord_shares function
#'
#' @param coord_shares_output the list from the get_coord_shares function
#' @param ct_shares_marked.df a dataframe that contains all the retrieved social media shares plus an extra boolean variable (is_coordinated) that identify if the shares were coordinated (default FALSE)
#' @param highly_connected_g graph of coordinated networks of pages/groups/accounts (default TRUE)
#' @param highly_connected_coordinated_entities a dataframe that lists coordinated entities and corresponding component (default TRUE)
#' @param component_summary the result of CooRnet::get_component_summary (default FALSE)
#' @param cluster_summary the result of CooRnet::get_component_summary (default FALSE)
#' @param top_coord_urls the result of CooRnet::get_top_coord_urls (default FALSE)
#' @param top_coord_shares the result of CooRnet::get_top_coord_shares (default FALSE)
#' @param gdrive_folder_id a Google Drive folder id where to upload selected files (output.rds, highly_connected_g.graphml, highly_connected_coordinated_entities.csv, component_summary.csv, top_coord_urls.csv, top_coord_shares) files (default NULL)
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
                        ct_shares_marked.df = FALSE,
                        highly_connected_g = TRUE,
                        highly_connected_coordinated_entities = TRUE,
                        component_summary = FALSE,
                        cluster_summary = FALSE,
                        top_coord_urls = FALSE,
                        top_coord_shares = FALSE,
                        gdrive_folder_id = NULL) {

  process_output <- function(output_name, output_value, file_ext, upload_to_drive) {
    if (upload_to_drive) {
      tempFileCon <- tempfile(output_name, fileext = file_ext)
      if (file_ext == ".csv") {
        utils::write.csv(output_value, file = tempFileCon)
      } else if (file_ext == ".graphml") {
        igraph::write.graph(output_value, format = "graphml", file = tempFileCon)
      } else {
        saveRDS(output_value, file = tempFileCon, ascii = FALSE)
      }
      googledrive::drive_upload(tempFileCon, googledrive::as_id(gdrive_folder_id))
    }
    assign(output_name, output_value, envir = .GlobalEnv)
  }

  upload_to_drive <- !is.null(gdrive_folder_id)

  if (ct_shares_marked.df) {
    process_output("ct_shares_marked.df", as.data.frame(coord_shares_output[[1]]), ".csv", upload_to_drive)
  }

  if (highly_connected_g) {
    process_output("highly_connected_g", coord_shares_output[[2]], ".graphml", upload_to_drive)
  }

  if (highly_connected_coordinated_entities) {
    process_output("highly_connected_coordinated_entities", coord_shares_output[[3]], ".csv", upload_to_drive)
  }

  if (component_summary) {
    component_summary_output <- CooRnet::get_component_summary(output = coord_shares_output, labels = TRUE)
    process_output("component_summary", component_summary_output, ".csv", upload_to_drive)
  }

  if (cluster_summary) {
    cluster_summary_output <- CooRnet::get_cluster_summary(output = coord_shares_output, labels = TRUE)
    process_output("cluster_summary", cluster_summary_output, ".csv", upload_to_drive)
  }

  if (top_coord_urls) {
    top_coord_urls_output <- CooRnet::get_top_coord_urls(output = coord_shares_output)
    process_output("top_coord_urls", top_coord_urls_output, ".csv", upload_to_drive)
  }

  if (top_coord_shares) {
    top_coord_shares_output <- CooRnet::get_top_coord_shares(output = coord_shares_output)
    process_output("top_coord_shares", top_coord_shares_output, ".csv", upload_to_drive)
  }
}

