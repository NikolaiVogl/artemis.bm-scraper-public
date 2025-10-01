#' Process Deal Data
#'
#' Transform and analyze deal directory data for visualization
#'
#' @param deals_data Raw deals data frame
#' @return List with processed data components
#' @export
process_deal_data <- function(deals_data) {
  if (nrow(deals_data) == 0) {
    return(list(
      yearly_issued = data.frame(),
      yearly_inforce = data.frame(),
      summary_stats = data.frame(),
      raw_data = data.frame()
    ))
  }

  # Handle different data formats:
  # 1. Scraped data has: issuer, cedent, risks_perils_covered, size, date
  # 2. Sample data has: deal_name, issue_date, maturity_date, volume_usd, sponsor, type
  
  # Check if this is already processed sample data or raw scraped data
  if ("volume_usd" %in% names(deals_data) && "issue_date" %in% names(deals_data)) {
    # Already has the necessary columns (sample data format)
    deals_data <- deals_data %>%
      dplyr::mutate(
        issue_date = as.Date(issue_date),
        maturity_date = if("maturity_date" %in% names(deals_data)) {
          as.Date(maturity_date)
        } else {
          issue_date + lubridate::years(3)
        }
      )
  } else {
    # Raw scraped data - needs transformation
    # Map scraped column names to expected names for processing
    deals_data <- deals_data %>%
      dplyr::rename(
        issue_date = date,
        volume_size = size
      )
    
    # Clean and convert date and numeric fields
    deals_data <- deals_data %>%
      dplyr::mutate(
        # Parse the issue date (format like "Oct 2025", "Sep 2025")
        issue_date = lubridate::parse_date_time(paste("01", issue_date), orders = c("d b Y"), quiet = TRUE),
        # For maturity, we'll estimate 3 years from issue date as a reasonable default
        # since the original data doesn't include maturity dates
        maturity_date = issue_date + lubridate::years(3),
        # Extract numeric value from size (e.g., "$300m" -> 300000000)
        volume_usd = dplyr::case_when(
          grepl("\\$([0-9.]+)m", volume_size, ignore.case = TRUE) ~ 
            as.numeric(gsub("\\$([0-9.]+)m.*", "\\1", volume_size, ignore.case = TRUE)) * 1e6,
          grepl("\\$([0-9.]+)b", volume_size, ignore.case = TRUE) ~ 
            as.numeric(gsub("\\$([0-9.]+)b.*", "\\1", volume_size, ignore.case = TRUE)) * 1e9,
          TRUE ~ as.numeric(gsub("[^0-9.]", "", volume_size))
        )
      )
  }

  # Remove rows with missing or invalid dates
  deals_processed <- deals_data %>%
    dplyr::filter(!is.na(issue_date), !is.na(volume_usd), volume_usd > 0) %>%
    dplyr::mutate(
      issue_year = lubridate::year(issue_date),
      maturity_year = lubridate::year(maturity_date),
      volume_millions = volume_usd / 1e6
    )

  # Calculate yearly issued volumes
  yearly_issued <- deals_processed %>%
    dplyr::group_by(issue_year) %>%
    dplyr::summarise(
      total_volume = sum(volume_millions, na.rm = TRUE),
      deal_count = n(),
      .groups = 'drop'
    ) %>%
    dplyr::rename(year = issue_year)

  # Calculate yearly inforce volumes
  current_year <- lubridate::year(Sys.Date())
  years_range <- min(deals_processed$issue_year, na.rm = TRUE):current_year

  yearly_inforce <- do.call(rbind, lapply(years_range, function(yr) {
    inforce_deals <- deals_processed %>%
      dplyr::filter(issue_year <= yr & maturity_year > yr)

    data.frame(
      year = yr,
      total_volume = sum(inforce_deals$volume_millions, na.rm = TRUE),
      deal_count = nrow(inforce_deals)
    )
  }))

  # Summary statistics
  summary_stats <- deals_processed %>%
    dplyr::summarise(
      total_deals = n(),
      total_volume = sum(volume_millions, na.rm = TRUE),
      avg_deal_size = mean(volume_millions, na.rm = TRUE),
      min_year = min(issue_year, na.rm = TRUE),
      max_year = max(issue_year, na.rm = TRUE)
    )

  return(list(
    yearly_issued = yearly_issued,
    yearly_inforce = yearly_inforce,
    summary_stats = summary_stats,
    raw_data = deals_processed
  ))
}


#' Process Losses Data
#'
#' Transform and analyze cat bond losses data for visualization
#'
#' @param losses_data Raw losses data frame
#' @return List with processed data components
#' @export
process_losses_data <- function(losses_data) {
  if (nrow(losses_data) == 0) {
    return(list(
      yearly_events = data.frame(),
      yearly_losses = data.frame(),
      summary_stats = data.frame()
    ))
  }

  # Map scraped column names to expected names for processing
  # Current scraped columns: cat_bond, sponsor, orig_size, cause_of_loss, loss_amount, _time_to_payment, date_of_loss
  losses_data <- losses_data %>%
    dplyr::rename(
      event_name = cat_bond,
      event_date = date_of_loss,
      loss_amount_text = loss_amount,
      original_size = orig_size
    )

  # Clean and convert fields
  losses_processed <- losses_data %>%
    dplyr::mutate(
      # Parse date from various formats like "January 2025", "2023, 2024, 2025", "2024 / 2025 risk period"
      event_year = dplyr::case_when(
        # Extract year from patterns like "January 2025"
        grepl("\\b(20[0-9]{2})\\b", event_date) ~ as.numeric(stringr::str_extract(event_date, "20[0-9]{2}")),
        # For multi-year periods, use the most recent year
        grepl("20[0-9]{2}\\s*/\\s*20[0-9]{2}", event_date) ~ 
          as.numeric(stringr::str_extract(event_date, "20[0-9]{2}(?=\\s*$|\\s*risk)")),
        TRUE ~ NA_real_
      ),
      # Extract loss amounts - this is complex as it's mostly text descriptions
      # We'll create a simplified numeric indicator: 1 for any loss, 0 for none
      has_loss = ifelse(grepl("principal|loss|reduced|zero|affected", loss_amount_text, ignore.case = TRUE), 1, 0),
      # Extract original size in millions
      orig_size_millions = dplyr::case_when(
        grepl("\\$([0-9.]+)m", original_size, ignore.case = TRUE) ~ 
          as.numeric(gsub("\\$([0-9.]+)m.*", "\\1", original_size, ignore.case = TRUE)),
        grepl("\\$([0-9.]+)b", original_size, ignore.case = TRUE) ~ 
          as.numeric(gsub("\\$([0-9.]+)b.*", "\\1", original_size, ignore.case = TRUE)) * 1000,
        TRUE ~ NA_real_
      ),
      # Extract event type from cause of loss
      event_type = dplyr::case_when(
        grepl("hurricane|cyclone", cause_of_loss, ignore.case = TRUE) ~ "Hurricane",
        grepl("earthquake", cause_of_loss, ignore.case = TRUE) ~ "Earthquake", 
        grepl("wildfire|fire", cause_of_loss, ignore.case = TRUE) ~ "Wildfire",
        grepl("flood", cause_of_loss, ignore.case = TRUE) ~ "Flood",
        grepl("storm|convective", cause_of_loss, ignore.case = TRUE) ~ "Storm",
        grepl("winter", cause_of_loss, ignore.case = TRUE) ~ "Winter Storm",
        TRUE ~ "Other"
      )
    )

  # Remove rows with missing year
  losses_processed <- losses_processed %>%
    dplyr::filter(!is.na(event_year), event_year >= 2000, event_year <= lubridate::year(Sys.Date()) + 5)

  # Yearly event summary
  yearly_events <- losses_processed %>%
    dplyr::group_by(event_year) %>%
    dplyr::summarise(
      event_count = n(),
      bonds_at_risk = dplyr::n_distinct(event_name),
      total_size = sum(orig_size_millions, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::rename(year = event_year)

  # Yearly losses by event type
  yearly_losses <- losses_processed %>%
    dplyr::group_by(event_year, event_type) %>%
    dplyr::summarise(
      event_count = n(),
      loss_amount = sum(orig_size_millions, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::rename(year = event_year)

  # Summary statistics
  summary_stats <- losses_processed %>%
    dplyr::summarise(
      total_events = n(),
      total_bonds_affected = dplyr::n_distinct(event_name),
      total_loss = sum(orig_size_millions, na.rm = TRUE),
      avg_loss_per_event = mean(orig_size_millions, na.rm = TRUE),
      min_year = min(event_year, na.rm = TRUE),
      max_year = max(event_year, na.rm = TRUE)
    )

  return(list(
    yearly_events = yearly_events,
    yearly_losses = yearly_losses,
    summary_stats = summary_stats,
    raw_data = losses_processed
  ))
}
