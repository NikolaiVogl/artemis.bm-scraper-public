#' Artemis Scraper Class
#'
#' R6 class for scraping Artemis.bm website
#'
#' @importFrom R6 R6Class
#' @export
ArtemisScraper <- R6::R6Class("ArtemisScraper",
  public = list(
    #' @field base_url Base URL for Artemis.bm
    base_url = "https://www.artemis.bm",
    
    #' Scrape deal directory
    #' @return Data frame with deal information
    scrape_deal_directory = function() {
      tryCatch({
        url <- paste0(self$base_url, "/deal-directory/")
        
        # Add headers to mimic browser request
        response <- httr::GET(url, httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        ))
        
        if (httr::status_code(response) != 200) {
          stop("Failed to fetch deal directory page")
        }
        
        page <- rvest::read_html(httr::content(response, "text"))
        
        # Extract deal information (structure will depend on actual HTML)
        deals <- self$parse_deal_directory_html(page)
        
        return(deals)
        
      }, error = function(e) {
        message("Error scraping deal directory: ", e$message)
        return(data.frame())
      })
    },
    
    #' Scrape cat bond losses
    #' @return Data frame with loss information
    scrape_cat_bond_losses = function() {
      tryCatch({
        url <- paste0(self$base_url, "/cat-bond-losses/")
        
        response <- httr::GET(url, httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        ))
        
        if (httr::status_code(response) != 200) {
          stop("Failed to fetch cat bond losses page")
        }
        
        page <- rvest::read_html(httr::content(response, "text"))
        
        # Extract loss information
        losses <- self$parse_cat_bond_losses_html()
        
        return(losses)
        
      }, error = function(e) {
        message("Error scraping cat bond losses: ", e$message)
        return(data.frame())
      })
    },
    
    #' Parse deal directory HTML using Search Filter Pro AJAX endpoint
    #' @param page HTML page object (unused, kept for compatibility)
    #' @return Data frame with parsed deal data
    parse_deal_directory_html = function(page) {
      tryCatch({
        # Search Filter Pro AJAX endpoint
        data_url <- "https://www.artemis.bm/?sfid=59514&sf_action=get_data&sf_data=results"
    
        # Headers to simulate browser behavior
        headers <- httr::add_headers(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          `Referer` = "https://www.artemis.bm/deal-directory/",
          `Content-Type` = "application/x-www-form-urlencoded"
        )
    
        # POST request with form data
        response <- httr::POST(data_url, headers, body = "sf_data=results", encode = "form")
    
        # Check response
        if (httr::status_code(response) != 200) {
          stop("Failed to fetch data from Search Filter endpoint")
        }
    
        # Parse JSON response first
        json_content <- httr::content(response, "text", encoding = "UTF-8")
        json_data <- jsonlite::fromJSON(json_content)
        
        # Extract HTML content from JSON results
        if (!"results" %in% names(json_data)) {
          stop("No 'results' field in JSON response")
        }
        
        html_content <- json_data$results
        page_html <- rvest::read_html(html_content)
        
        # Extract table rows from the HTML content
        table_rows <- rvest::html_nodes(page_html, "table#table-deal tr")
        
        if (length(table_rows) <= 1) {
          message("No data rows found in table")
          return(data.frame())
        }
        
        # Extract header row to get column names
        header_row <- rvest::html_nodes(table_rows[1], "th")
        col_names <- rvest::html_text(header_row, trim = TRUE)
        
        # Extract data rows
        data_rows <- table_rows[-1] # Skip header row
        
        # Convert rows to data frame
        table_data <- lapply(data_rows, function(row) {
          cells <- rvest::html_nodes(row, "td")
          cell_values <- rvest::html_text(cells, trim = TRUE)
          
          # Ensure we have the right number of columns
          if (length(cell_values) == length(col_names)) {
            return(cell_values)
          } else {
            return(NULL)
          }
        })
        
        # Filter out NULL rows and convert to data frame
        table_data <- table_data[!sapply(table_data, is.null)]
        
        if (length(table_data) == 0) {
          message("No valid data rows found")
          return(data.frame())
        }
        
        # Convert to data frame
        df <- data.frame(do.call(rbind, table_data), stringsAsFactors = FALSE)
        names(df) <- tolower(gsub("[^a-zA-Z0-9]+", "_", col_names))
        
        # Clean up the data frame
        df[] <- lapply(df, function(x) trimws(x))
        
        return(df)
    
      }, error = function(e) {
        message("Error in parse_deal_directory_html: ", e$message)
        return(data.frame())
      })
    },
    
    #' Parse cat bond losses HTML from TablePress table
    #' @return Data frame with parsed loss data
    parse_cat_bond_losses_html = function() {
      tryCatch({
        # Direct page URL (TablePress embeds table in page)
        data_url <- "https://www.artemis.bm/cat-bond-losses/"
    
        # Headers to mimic a real browser
        headers <- httr::add_headers(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          `Referer` = "https://www.artemis.bm/"
        )
    
        # Send GET request
        response <- httr::GET(data_url, headers)
    
        # Check for success
        if (httr::status_code(response) != 200) {
          stop("Failed to fetch data from cat bond losses page")
        }
    
        # Parse HTML content
        html_content <- httr::content(response, "text", encoding = "UTF-8")
        page_html <- rvest::read_html(html_content)
        
        # Extract TablePress table
        table_rows <- rvest::html_nodes(page_html, "table#tablepress-2 tr")
        
        if (length(table_rows) <= 1) {
          message("No data rows found in TablePress table")
          return(data.frame())
        }
        
        # Extract header row to get column names
        header_row <- rvest::html_nodes(table_rows[1], "th")
        col_names <- rvest::html_text(header_row, trim = TRUE)
        
        # Extract data rows (skip header row)
        data_rows <- table_rows[-1]
        
        # Convert rows to data frame
        table_data <- lapply(data_rows, function(row) {
          cells <- rvest::html_nodes(row, "td")
          cell_values <- rvest::html_text(cells, trim = TRUE)
          
          # Ensure we have the right number of columns
          if (length(cell_values) == length(col_names)) {
            return(cell_values)
          } else {
            return(NULL)
          }
        })
        
        # Filter out NULL rows and convert to data frame
        table_data <- table_data[!sapply(table_data, is.null)]
        
        if (length(table_data) == 0) {
          message("No valid data rows found in cat bond losses table")
          return(data.frame())
        }
        
        # Convert to data frame
        df <- data.frame(do.call(rbind, table_data), stringsAsFactors = FALSE)
        names(df) <- tolower(gsub("[^a-zA-Z0-9]+", "_", col_names))
        
        # Clean up the data frame
        df[] <- lapply(df, function(x) trimws(x))
        
        return(df)
    
      }, error = function(e) {
        message("Error in parse_cat_bond_losses_html: ", e$message)
        return(data.frame())
      })
    }
  )
)

#' Create Artemis Scraper Instance
#'
#' Factory function to create an Artemis scraper
#'
#' @return An ArtemisScraper instance
#' @export
create_artemis_scraper <- function() {
  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("R6 package is required for the scraper")
  }
  return(ArtemisScraper$new())
}
