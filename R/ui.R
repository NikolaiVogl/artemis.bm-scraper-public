#' Dashboard UI
#'
#' Create the Shiny dashboard user interface
#'
#' @return A shinydashboard page object
#' @export
dashboard_ui <- function() {
  shinydashboard::dashboardPage(
    # Dashboard Header
    shinydashboard::dashboardHeader(title = "Artemis.bm Analytics Dashboard"),
    
    # Dashboard Sidebar
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Deal Directory", tabName = "deals", icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("Cat Bond Losses", tabName = "losses", icon = shiny::icon("exclamation-triangle")),
        shinydashboard::menuItem("Data Information", tabName = "refresh", icon = shiny::icon("info-circle")),
        shiny::br()
      )
    ),
    
    # Dashboard Body
    shinydashboard::dashboardBody(
      # Custom CSS
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .content-wrapper, .right-side {
            background-color: #f4f4f4;
          }
          .box {
            box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          }
          .value-box-value {
            font-size: 2.5em !important;
          }
        "))
      ),
      
      shinydashboard::tabItems(
        # Deal Directory Tab
        shinydashboard::tabItem(tabName = "deals",
          shiny::fluidRow(
            # Summary boxes
            shinydashboard::valueBoxOutput("total_deals", width = 3),
            shinydashboard::valueBoxOutput("total_volume", width = 3),
            shinydashboard::valueBoxOutput("avg_deal_size", width = 3),
            shinydashboard::valueBoxOutput("active_deals", width = 3)
          ),
          
          shiny::fluidRow(
            # Main chart
            shinydashboard::box(
              title = "Volume Analysis", status = "primary", solidHeader = TRUE,
              width = 8, height = "500px",
              div(
                style = "margin-bottom: 15px;",
                shiny::radioButtons("volume_type", "Display Type:",
                           choices = list("Issued Volume" = "issued", 
                                        "In-Force Volume" = "inforce"),
                           selected = "issued",
                           inline = TRUE)
              ),
              plotly::plotlyOutput("volume_chart", height = "400px")
            ),
            
            # Deal type breakdown
            shinydashboard::box(
              title = "Deal Types", status = "info", solidHeader = TRUE,
              width = 4, height = "500px",
              plotly::plotlyOutput("deal_type_chart", height = "400px")
            )
          ),
          
          shiny::fluidRow(
            # Data table
            shinydashboard::box(
              title = "Deal Directory Data", status = "success", solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput("deals_table")
            )
          )
        ),
        
        # Cat Bond Losses Tab
        shinydashboard::tabItem(tabName = "losses",
          shiny::fluidRow(
            # Summary boxes
            shinydashboard::valueBoxOutput("total_events", width = 3),
            shinydashboard::valueBoxOutput("total_losses", width = 3),
            shinydashboard::valueBoxOutput("avg_loss", width = 3),
            shinydashboard::valueBoxOutput("bonds_affected", width = 3)
          ),
          
          shiny::fluidRow(
            # Event counts chart
            shinydashboard::box(
              title = "Annual Event Analysis", status = "warning", solidHeader = TRUE,
              width = 6, height = "500px",
              plotly::plotlyOutput("events_chart", height = "400px")
            ),
            
            # Loss amounts chart
            shinydashboard::box(
              title = "Loss Amounts by Type", status = "danger", solidHeader = TRUE,
              width = 6, height = "500px",
              plotly::plotlyOutput("losses_chart", height = "400px")
            )
          )
        ),
        
        # Data Information Tab
        shinydashboard::tabItem(tabName = "refresh",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Data Information", status = "primary", solidHeader = TRUE,
              width = 12,
              shiny::h3("Data Sources"),
              shiny::p("This dashboard displays pre-scraped data from:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::tags$a("Artemis Deal Directory", 
                              href = "https://www.artemis.bm/deal-directory/", 
                              target = "_blank")),
                shiny::tags$li(shiny::tags$a("Artemis Cat Bond Losses", 
                              href = "https://www.artemis.bm/cat-bond-losses/", 
                              target = "_blank"))
              ),
              shiny::br(),
              
              shiny::div(
                class = "alert alert-info",
                style = "background-color: #d9edf7; border-color: #bce8f1; color: #31708f; padding: 15px; border-radius: 4px;",
                shiny::h4("📋 Static Data Notice", style = "margin-top: 0;"),
                shiny::p("Data in this dashboard is scraped during the container build process to improve performance and reduce server load. The data represents a snapshot from when the container was built and will not change during runtime."),
                shiny::p("For the most up-to-date data, a new container build is required.")
              ),
              
              shiny::h4("Data Status"),
              shiny::verbatimTextOutput("refresh_status"),
              shiny::br(),
              
              shiny::h4("Data Quality"),
              DT::dataTableOutput("data_quality_table")
            )
          )
        )
      )
    )
  )
}