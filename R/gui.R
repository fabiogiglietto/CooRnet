#' @import shiny
#' @import shinyjs
#' @import igraph
#' @import devtools
#' @import readr
#' @export
gui <- function(){

ui <- fluidPage(
  useShinyjs(),

  # App title ----
  titlePanel("Coornet GUI"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput("coordinationinterval",""),
      textInput("percentileedgeweight"," "),

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File with your URLs",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----

      selectInput("data_type", "Select Data Type",
      choices = c("cluster summary", "highly connected coordinated entities", "component summary", "top coord urls", "top coord shares")),
      actionButton("create_df", "Create Dataframe",disabled=TRUE),
      downloadButton("downloadData", label = "Download graph",disabled=TRUE),
      # Horizontal line ----
    ),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      tableOutput("contents")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    options(shiny.maxRequestSize = 15000 * 1024^2)
    finaldf <- reactiveValues(data = NULL)

    validateInput <- function(inputText) {
    if (!stringr::str_detect(inputText, "^[0-9]+[.]?[0-9]*$")) {
      return("The value must be a positive number!")
    }
  }

  observeEvent(input$coordinationinterval, {
    inputText <- input$coordinationinterval
    validationMsg <- validateInput(inputText)
    if (!is.null(validationMsg)) {
      avviso_html <- paste0(" (",validationMsg,")")
    }
    else{
      avviso_html <- ""
    }
    updateTextInput(inputId = "coordinationinterval", label = paste0("Insert Coordination Interval", avviso_html))
  })

  observeEvent(input$percentileedgeweight, {
    inputText <- input$percentileedgeweight
    validationMsg <- validateInput(inputText)
    if (!is.null(validationMsg)) {
      avviso_html <- paste0(" (",validationMsg,")")
    }
    else{
      avviso_html <- ""
    }
    updateTextInput(inputId = "percentileedgeweight", label = paste0("Insert Percentile Edge Weight", avviso_html))
  })





    observeEvent(input$file1, {
      withProgress(message = 'calculating...',value = 0,{
        urls <- read.csv(input$file1$datapath)
        incProgress(0.3,detail = paste0("I'm making API requests, please wait as long as necessary"))
        ct_shares.urls <- get_ctshares(urls,
                                        sleep_time = 1,
                                        get_history = FALSE,
                                        clean_urls = TRUE)
        incProgress(0.5,detail = paste0("I made API requests"))
        output_coornet <<- get_coord_shares(ct_shares.df = ct_shares.urls,
                                    coordination_interval = if(input$coordinationinterval == "") '60 secs' else paste(as.character(input$coordinationinterval),"secs"),
                                    parallel = FALSE,
                                    percentile_edge_weight = if(input$percentileedgeweight == "") 0.995 else as.double(input$percentileedgeweight),
                                    keep_ourl_only = TRUE,
                                    clean_urls = TRUE)
        incProgress(0.2,detail = paste0("I finished"))
        enable("create_df")
        enable("downloadData")
      })
    })
    observeEvent(input$create_df, {
    if (input$data_type == "cluster summary") {
      finaldf$data <- get_cluster_summary(output_coornet)
    } else if(input$data_type == "highly connected coordinated entities"){
      get_outputs(coord_shares_output = output_coornet,
                        ct_shares_marked.df = FALSE,
                        highly_connected_g = FALSE,
                        highly_connected_coordinated_entities = TRUE,
                        component_summary = FALSE,
                        cluster_summary = FALSE,
                        top_coord_urls = FALSE,
                        top_coord_shares = FALSE,
                        gdrive_folder_id = NULL)
      finaldf$data <- highly_connected_coordinated_entities
    }
    else if(input$data_type == "component summary"){
      get_outputs(coord_shares_output = output_coornet,
                        ct_shares_marked.df = FALSE,
                        highly_connected_g = FALSE,
                        highly_connected_coordinated_entities = FALSE,
                        component_summary = TRUE,
                        cluster_summary = FALSE,
                        top_coord_urls = FALSE,
                        top_coord_shares = FALSE,
                        gdrive_folder_id = NULL)
      finaldf$data <- component_summary
    }
    else if(input$data_type == "top coord urls"){
      get_outputs(coord_shares_output = output_coornet,
                        ct_shares_marked.df = FALSE,
                        highly_connected_g = FALSE,
                        highly_connected_coordinated_entities = FALSE,
                        component_summary = FALSE,
                        cluster_summary = FALSE,
                        top_coord_urls = TRUE,
                        top_coord_shares = FALSE,
                        gdrive_folder_id = NULL)
      finaldf$data <- top_coord_urls

    }
    else if(input$data_type == "top coord shares"){
            get_outputs(coord_shares_output = output_coornet,
                        ct_shares_marked.df = FALSE,
                        highly_connected_g = FALSE,
                        highly_connected_coordinated_entities = FALSE,
                        component_summary = FALSE,
                        cluster_summary = FALSE,
                        top_coord_urls = FALSE,
                        top_coord_shares = TRUE,
                        gdrive_folder_id = NULL)
      finaldf$data <- top_coord_shares
    }
  })

    output$contents <- renderTable({
      return(finaldf$data)
      }
  )
  output$downloadData <- downloadHandler(
    filename = "grafo.graphml",
    content = function(file) {
      get_outputs(coord_shares_output = output_coornet,
                        ct_shares_marked.df = FALSE,
                        highly_connected_g = TRUE,
                        highly_connected_coordinated_entities = FALSE,
                        component_summary = FALSE,
                        cluster_summary = FALSE,
                        top_coord_urls = FALSE,
                        top_coord_shares = TRUE,
                        gdrive_folder_id = NULL)

      grafohml <- highly_connected_g

      write.graph(grafohml, file, format = "graphml")
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
}
