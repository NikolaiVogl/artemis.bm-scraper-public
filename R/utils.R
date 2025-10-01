#' Format Currency
#'
#' Format numeric values as currency strings
#'
#' @param value Numeric value in millions (can be a vector)
#' @return Character string or vector formatted as currency
#' @export
format_currency <- function(value) {
  # Handle NULL input
  if (is.null(value)) {
    return("USD 0M")
  }
  
  # Vectorized operation
  result <- ifelse(is.na(value), "USD 0M",
                   ifelse(value >= 1000,
                          sprintf("USD %.1fB", value / 1000),
                          sprintf("USD %.0fM", value)))
  
  return(result)
}
