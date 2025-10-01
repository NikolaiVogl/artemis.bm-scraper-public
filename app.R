# Artemis.bm Scraper Dashboard
# Main Shiny Application

# Load the package (in development mode)
pkgload::load_all()

# Define UI
ui <- dashboard_ui()

# Define server logic
server <- dashboard_server

# Run the application 
shinyApp(ui = ui, server = server)
