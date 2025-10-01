#!/usr/bin/env Rscript
# Pre-scraping script for Artemis.bm data
# This script runs during container build to scrape data and save it to files

# Create data directory if it doesn't exist
data_dir <- "/srv/shiny-server/data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

cat("Initializing Artemis scraper data preparation...\n")

# Check if we can load required packages
required_packages <- c("httr", "rvest", "jsonlite", "dplyr", "lubridate")
missing_packages <- c()

for (pkg in required_packages) {
  if (!suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("Missing packages for scraping:", paste(missing_packages, collapse = ", "), "\n")
  stop("Cannot proceed without required packages")
}

cat("All required packages available, attempting to scrape...\n")

# Source the package functions
source("/srv/shiny-server/R/scraper.R")
source("/srv/shiny-server/R/data_processing.R")
source("/srv/shiny-server/R/utils.R")

# Initialize scraper
scraper <- create_artemis_scraper()

# Attempt to scrape deal directory data
cat("Scraping deal directory...\n")
deals_data <- scraper$scrape_deal_directory()

if (nrow(deals_data) == 0) {
  stop("Deal directory scraping returned no data")
}

cat(sprintf("Successfully scraped %d deal records\n", nrow(deals_data)))

# Process and save processed data (RDS only)
processed_deals <- process_deal_data(deals_data)
saveRDS(processed_deals, file.path(data_dir, "deals_processed.rds"))

# Attempt to scrape cat bond losses data
cat("Scraping cat bond losses...\n")
losses_data <- scraper$scrape_cat_bond_losses()

if (nrow(losses_data) == 0) {
  stop("Cat bond losses scraping returned no data")
}

cat(sprintf("Successfully scraped %d loss records\n", nrow(losses_data)))

# Process and save processed losses data (RDS only)
processed_losses <- process_losses_data(losses_data)
saveRDS(processed_losses, file.path(data_dir, "losses_processed.rds"))

# Save scraping metadata
scrape_metadata <- list(
  scrape_time = Sys.time(),
  scrape_date = Sys.Date(),
  timezone = Sys.timezone(),
  note = "Data scraped successfully during container build"
)
saveRDS(scrape_metadata, file.path(data_dir, "scrape_metadata.rds"))

cat(sprintf("Data preparation completed at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("Data saved to /srv/shiny-server/data/\n")

# List created files
cat("Created files:\n")
files <- list.files(data_dir, full.names = FALSE)
for (f in files) {
  size <- file.size(file.path(data_dir, f))
  cat(sprintf("  %s (%d bytes)\n", f, size))
}