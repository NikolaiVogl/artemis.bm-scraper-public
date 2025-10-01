#' Render Deal Value Boxes
#'
#' Render value boxes for deal directory metrics
#'
#' @param values Reactive values object
#' @param output Shiny output object
#' @export
render_deal_value_boxes <- function(values, output) {
  output$total_deals <- shinydashboard::renderValueBox({
    count <- if(nrow(values$processed_deals$summary_stats) > 0) {
      values$processed_deals$summary_stats$total_deals
    } else { 0 }
    
    shinydashboard::valueBox(
      value = count,
      subtitle = "Total Deals",
      icon = shiny::icon("handshake"),
      color = "blue"
    )
  })
  
  output$total_volume <- shinydashboard::renderValueBox({
    volume <- if(nrow(values$processed_deals$summary_stats) > 0) {
      format_currency(values$processed_deals$summary_stats$total_volume)
    } else { "USD 0M" }
    
    shinydashboard::valueBox(
      value = volume,
      subtitle = "Total Volume",
      icon = shiny::icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$avg_deal_size <- shinydashboard::renderValueBox({
    avg_size <- if(nrow(values$processed_deals$summary_stats) > 0) {
      format_currency(values$processed_deals$summary_stats$avg_deal_size)
    } else { "USD 0M" }
    
    shinydashboard::valueBox(
      value = avg_size,
      subtitle = "Avg Deal Size",
      icon = shiny::icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$active_deals <- shinydashboard::renderValueBox({
    current_year <- lubridate::year(Sys.Date())
    active_count <- if(nrow(values$processed_deals$yearly_inforce) > 0) {
      current_data <- values$processed_deals$yearly_inforce %>%
        dplyr::filter(year == current_year)
      if(nrow(current_data) > 0) current_data$deal_count else 0
    } else { 0 }
    
    shinydashboard::valueBox(
      value = active_count,
      subtitle = "Active Deals",
      icon = shiny::icon("clock"),
      color = "purple"
    )
  })
}

#' Render Deal Charts
#'
#' Render charts for deal directory visualization
#'
#' @param values Reactive values object
#' @param input Shiny input object
#' @param output Shiny output object
#' @export
render_deal_charts <- function(values, input, output) {
  # Volume chart
  output$volume_chart <- plotly::renderPlotly({
    if(length(values$processed_deals) == 0 || is.null(values$processed_deals$yearly_issued) || input$volume_type == "") {
      return(plotly::plotly_empty())
    }
    
    data_to_plot <- if(input$volume_type == "issued") {
      values$processed_deals$yearly_issued
    } else {
      values$processed_deals$yearly_inforce
    }
    
    if(nrow(data_to_plot) == 0) {
      return(plotly::plotly_empty())
    }
    
    chart_title <- if(input$volume_type == "issued") "Volume Issued by Year" else "Volume In-Force by Year"
    
    p <- plotly::plot_ly(data_to_plot, x = ~year, y = ~total_volume, type = 'bar',
                 text = ~paste("Volume:", format_currency(total_volume), "<br>Deals:", deal_count),
                 hovertemplate = '<b>%{x}</b><br>Volume: %{text}<extra></extra>',
                 marker = list(color = if(input$volume_type == "issued") '#3498db' else '#e74c3c')) %>%
      plotly::layout(title = chart_title,
             xaxis = list(title = "Year"),
             yaxis = list(title = "Volume (USD Millions)"),
             showlegend = FALSE)
    
    return(p)
  })
  
  # Deal type chart
  output$deal_type_chart <- plotly::renderPlotly({
    if(nrow(values$processed_deals$raw_data) == 0) {
      return(plotly::plotly_empty())
    }
    
    # Check if 'type' column exists in the data
    if(!"risks_perils_covered" %in% names(values$processed_deals$raw_data)) {
      # No type information available, show a placeholder
      return(plotly::plotly_empty())
    }
    
    # Group by risks_perils_covered since that's what we have from the scraper
    type_summary <- values$processed_deals$raw_data %>%
      dplyr::group_by(risks_perils_covered) %>%
      dplyr::summarise(count = n(), volume = sum(volume_millions, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(dplyr::desc(count)) %>%
      dplyr::slice_head(n = 10) # Show top 10 risk types
    
    p <- plotly::plot_ly(type_summary, x = ~risks_perils_covered, y = ~count, type = 'bar',
                 text = ~paste("Count:", count, "<br>Volume:", format_currency(volume)),
                 hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>',
                 marker = list(color = '#2ecc71')) %>%
      plotly::layout(title = "Deal Count by Risk/Peril Type",
             xaxis = list(title = "Risk/Peril Type", tickangle = -45),
             yaxis = list(title = "Number of Deals"),
             showlegend = FALSE,
             margin = list(b = 150)) # Extra margin for rotated labels
    
    return(p)
  })
}

#' Render Deal Table
#'
#' Render data table for deal directory
#'
#' @param values Reactive values object
#' @param output Shiny output object
#' @export
render_deal_table <- function(values, output) {
  output$deals_table <- DT::renderDataTable({
    # Use processed data which has all the necessary columns
    if(is.null(values$processed_deals$raw_data) || nrow(values$processed_deals$raw_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't')))
    }
    
    # Start with processed data that has volume_usd and other fields
    processed_data <- values$processed_deals$raw_data
    
    # Build display data with available columns
    display_data <- processed_data %>%
      dplyr::mutate(
        volume_formatted = format_currency(volume_millions),
        issue_date_formatted = format(issue_date, "%Y-%m-%d"),
        maturity_date_formatted = format(maturity_date, "%Y-%m-%d")
      )
    
    # Select columns that exist - handle optional columns like sponsor, type, deal_name
    available_cols <- names(display_data)
    
    # Map scraped column names to display names
    select_cols <- c()
    col_names <- c()
    
    if("issuer" %in% available_cols) {
      select_cols <- c(select_cols, "issuer")
      col_names <- c(col_names, "Issuer")
    }
    
    if("issue_date_formatted" %in% available_cols) {
      select_cols <- c(select_cols, "issue_date_formatted")
      col_names <- c(col_names, "Issue Date")
    }
    
    if("maturity_date_formatted" %in% available_cols) {
      select_cols <- c(select_cols, "maturity_date_formatted")
      col_names <- c(col_names, "Maturity Date")
    }
    
    if("volume_formatted" %in% available_cols) {
      select_cols <- c(select_cols, "volume_formatted")
      col_names <- c(col_names, "Volume")
    }
    
    if("cedent" %in% available_cols) {
      select_cols <- c(select_cols, "cedent")
      col_names <- c(col_names, "Cedent")
    }
    
    if("risks_perils_covered" %in% available_cols) {
      select_cols <- c(select_cols, "risks_perils_covered")
      col_names <- c(col_names, "Risks/Perils")
    }
    
    # Select available columns
    display_data <- display_data %>%
      dplyr::select(dplyr::all_of(select_cols))
    
    DT::datatable(display_data, 
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = col_names)
  })
}

#' Render Loss Value Boxes
#'
#' Render value boxes for cat bond loss metrics
#'
#' @param values Reactive values object
#' @param output Shiny output object
#' @export
render_loss_value_boxes <- function(values, output) {
  output$total_events <- shinydashboard::renderValueBox({
    count <- if(nrow(values$processed_losses$summary_stats) > 0) {
      values$processed_losses$summary_stats$total_events
    } else { 0 }
    
    shinydashboard::valueBox(
      value = count,
      subtitle = "Total Events",
      icon = shiny::icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$total_losses <- shinydashboard::renderValueBox({
    losses <- if(nrow(values$processed_losses$summary_stats) > 0) {
      format_currency(values$processed_losses$summary_stats$total_loss)
    } else { "USD 0M" }
    
    shinydashboard::valueBox(
      value = losses,
      subtitle = "Total Losses",
      icon = shiny::icon("chart-line-down"),
      color = "red"
    )
  })
  
  output$avg_loss <- shinydashboard::renderValueBox({
    avg_loss <- if(nrow(values$processed_losses$summary_stats) > 0) {
      format_currency(values$processed_losses$summary_stats$avg_loss_per_event)
    } else { "USD 0M" }
    
    shinydashboard::valueBox(
      value = avg_loss,
      subtitle = "Avg Loss/Event",
      icon = shiny::icon("calculator"),
      color = "orange"
    )
  })
  
  output$bonds_affected <- shinydashboard::renderValueBox({
    bonds <- if(nrow(values$processed_losses$summary_stats) > 0) {
      values$processed_losses$summary_stats$total_bonds_affected
    } else { 0 }
    
    shinydashboard::valueBox(
      value = bonds,
      subtitle = "Bonds Affected",
      icon = shiny::icon("shield-alt"),
      color = "yellow"
    )
  })
}

#' Render Loss Charts
#'
#' Render charts for cat bond loss visualization
#'
#' @param values Reactive values object
#' @param output Shiny output object
#' @export
render_loss_charts <- function(values, output) {
  # Events chart
  output$events_chart <- plotly::renderPlotly({
    if(nrow(values$processed_losses$yearly_events) == 0) {
      return(plotly::plotly_empty())
    }
    
    data_to_plot <- values$processed_losses$yearly_events
    
    if(nrow(data_to_plot) == 0) {
      return(plotly::plotly_empty())
    }
    
    p <- plotly::plot_ly(data_to_plot, x = ~year) %>%
      plotly::add_trace(y = ~event_count, name = 'Events', type = 'bar', yaxis = 'y', 
                marker = list(color = '#e74c3c')) %>%
      plotly::add_trace(y = ~bonds_at_risk, name = 'Bonds at Risk', type = 'scatter', 
                mode = 'lines+markers', yaxis = 'y2',
                line = list(color = '#f39c12')) %>%
      plotly::layout(title = "Annual Events and Bonds at Risk",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Events", side = 'left'),
             yaxis2 = list(title = "Bonds at Risk", side = 'right', overlaying = 'y'),
             legend = list(x = 0.7, y = 1))
    
    return(p)
  })
  
  # Losses chart
  output$losses_chart <- plotly::renderPlotly({
    if(nrow(values$processed_losses$yearly_losses) == 0) {
      return(plotly::plotly_empty())
    }
    
    data_to_plot <- values$processed_losses$yearly_losses
    
    if(nrow(data_to_plot) == 0) {
      return(plotly::plotly_empty())
    }
    
    p <- plotly::plot_ly(data_to_plot, x = ~year, y = ~loss_amount, color = ~event_type,
                 type = 'scatter', mode = 'lines+markers') %>%
      plotly::layout(title = "Loss Amounts by Event Type",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Loss Amount (USD Millions)"))
    
    return(p)
  })
}

#' Render Loss Table
#'
#' Render data table for cat bond losses
#'
#' @param values Reactive values object
#' @param output Shiny output object
#' @export
render_loss_table <- function(values, output) {
  output$losses_table <- DT::renderDataTable({
    if(nrow(values$losses_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't')))
    }
    
    display_data <- values$losses_data %>%
      dplyr::mutate(
        loss_formatted = format_currency(loss_amount_usd / 1000000),
        event_date = format(event_date, "%Y-%m-%d")
      ) %>%
      dplyr::select(event_name, event_date, loss_formatted, affected_bonds, event_type)
    
    DT::datatable(display_data, 
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Event Name", "Date", "Loss Amount", "Affected Bonds", "Event Type"))
  })
}
