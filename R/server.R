#' Dashboard Server
#'
#' Main server logic for the Artemis dashboard
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @export
dashboard_server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- shiny::reactiveValues(
    deals_data = data.frame(),
    losses_data = data.frame(),
    processed_deals = list(),
    processed_losses = list(),
    last_update = NULL,
    refresh_status = "No data loaded"
  )
  
  # Function to load pre-scraped data
  load_data <- function() {
    tryCatch({
      data_dir <- "/srv/shiny-server/data"
      
      values$refresh_status <- "Loading pre-scraped data..."
      
      # Load pre-processed data (RDS only)
      if (file.exists(file.path(data_dir, "deals_processed.rds"))) {
        values$processed_deals <- readRDS(file.path(data_dir, "deals_processed.rds"))
        # Extract raw data from processed data
        values$deals_data <- values$processed_deals$raw_data
      } else {
        values$processed_deals <- list(yearly_issued = data.frame(), yearly_inforce = data.frame(), summary_stats = data.frame(), raw_data = data.frame())
        values$deals_data <- data.frame()
      }
      
      if (file.exists(file.path(data_dir, "losses_processed.rds"))) {
        values$processed_losses <- readRDS(file.path(data_dir, "losses_processed.rds"))
        # Extract raw data from processed data
        values$losses_data <- values$processed_losses$raw_data
      } else {
        values$processed_losses <- list(yearly_events = data.frame(), yearly_losses = data.frame(), summary_stats = data.frame(), raw_data = data.frame())
        values$losses_data <- data.frame()
      }
      
      # Load scraping metadata
      if (file.exists(file.path(data_dir, "scrape_metadata.rds"))) {
        metadata <- readRDS(file.path(data_dir, "scrape_metadata.rds"))
        values$last_update <- metadata$scrape_time
        values$refresh_status <- paste("Data scraped during container build at", 
                                      format(metadata$scrape_time, "%Y-%m-%d %H:%M:%S"))
      } else {
        values$last_update <- Sys.time()
        values$refresh_status <- "Pre-scraped data loaded successfully"
      }
      
    }, error = function(e) {
      values$refresh_status <- paste("Error loading pre-scraped data:", e$message)
      # Initialize with empty data structures
      values$deals_data <- data.frame()
      values$losses_data <- data.frame()
      values$processed_deals <- list(yearly_issued = data.frame(), yearly_inforce = data.frame(), summary_stats = data.frame(), raw_data = data.frame())
      values$processed_losses <- list(yearly_events = data.frame(), yearly_losses = data.frame(), summary_stats = data.frame(), raw_data = data.frame())
    })
  }
  
  # Load data on startup
  shiny::observe({
    load_data()
  })
  
  # Refresh data functionality disabled - data is pre-scraped during build
  # No refresh buttons exist in the updated UI
  
  # Render Deal Directory components
  render_deal_value_boxes(values, output)
  render_deal_charts(values, input, output)
  render_deal_table(values, output)
  
  # Render Cat Bond Losses components
  render_loss_value_boxes(values, output)
  render_loss_charts(values, output)
  
  # Management and utility outputs
  output$refresh_status <- shiny::renderText({
    values$refresh_status
  })
  
  # Data quality table
  output$data_quality_table <- DT::renderDataTable({
    quality_data <- data.frame(
      Dataset = c("Deal Directory", "Cat Bond Losses"),
      Records = c(nrow(values$deals_data), nrow(values$losses_data)),
      `Last Updated` = c(
        ifelse(is.null(values$last_update), "Never", format(values$last_update, "%Y-%m-%d %H:%M:%S")),
        ifelse(is.null(values$last_update), "Never", format(values$last_update, "%Y-%m-%d %H:%M:%S"))
      ),
      Status = c(
        ifelse(nrow(values$deals_data) > 0, "OK", "No Data"),
        ifelse(nrow(values$losses_data) > 0, "OK", "No Data")
      ),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(quality_data, options = list(dom = 't', pageLength = 5))
  })
}
