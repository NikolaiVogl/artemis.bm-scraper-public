# Artemis.bm Scraper Dashboard

A modular R Shiny dashboard application that scrapes data from [Artemis.bm](https://www.artemis.bm) and provides interactive visualizations for catastrophe bond and insurance-linked securities (ILS) market data.

## Features

### Data Sources
- **Deal Directory**: Scrapes data from https://www.artemis.bm/deal-directory/
- **Cat Bond Losses**: Scrapes data from https://www.artemis.bm/cat-bond-losses/

### Dashboard Views
1. **Deal Directory Analytics**
   - Volume issued by year
   - Toggle between issued volume and in-force volume
   - Deal type breakdown
   - Summary statistics and data tables

2. **Cat Bond Losses Analytics**
   - Annual event counts and bonds at risk
   - Loss amounts by event type over time
   - Summary statistics and loss event details

### Key Capabilities
- **Interactive Charts**: Built with Plotly for rich interactivity
- **Responsive Design**: Works on desktop and mobile devices

### Prerequisites
- R (version 4.0 or higher)
- Required R packages (automatically installed via DESCRIPTION file)

### Setup
1. Clone the repository:
   ```bash
   git clone https://github.com/NikolaiVogl/artemis.bm-scraper.git
   cd artemis.bm-scraper
   ```

2. Install R dependencies (using devtools/remotes):
   ```r
   # Install devtools if not already installed
   install.packages("devtools")
   
   # Install package dependencies
   devtools::install_deps()
   ```

3. Run the application:
   ```r
   # Load the package in development mode
   pkgload::load_all()
   
   # Or run directly
   shiny::runApp()
   ```

The dashboard will be available at a random port (typically starting with `http://localhost:` followed by a port number like 3838, 4000, etc.) unless you specify a port. Shiny will print the URL in the console. To specify a specific port:

```r
shiny::runApp(port = 3838)
```

### Docker Development

For containerized development:

1. **Build the Docker image**:
   ```bash
   docker build -t artemis-bm-scraper .
   ```

2. **Run the container**:
   ```bash
   docker run --rm -p 3838:3838 artemis-bm-scraper
   ```

The application will be available at `http://localhost:3838`.
