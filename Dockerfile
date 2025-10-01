# Use official R base image with Shiny
FROM rocker/shiny:4.3.1

# Set maintainer
LABEL maintainer="artemis.bm-scraper"

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server

RUN R -e "install.packages(c('shiny','shinydashboard','htmltools','DT','plotly','dplyr','tidyr','rvest','httr','jsonlite','lubridate','stringr','ggplot2','scales','R6','pkgload','magrittr'), repos='https://cloud.r-project.org')"

# Copy application files
COPY . .

# Set proper permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Switch to shiny user before running pre-scrape to avoid root-owned files
USER shiny

# Make pre-scrape script executable and run it during build
RUN chmod +x /srv/shiny-server/scripts/pre_scrape_data.R && \
    cd /srv/shiny-server && Rscript scripts/pre_scrape_data.R

# Expose port
EXPOSE 3838

# Set environment variables for Azure Web Apps
ENV SHINY_LOG_STDERR=1

# Command to run the application
# Azure Web Apps will set the PORT environment variable
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '3838')))"]

