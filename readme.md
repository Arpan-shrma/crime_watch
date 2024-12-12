# Canada Crime Watch Dashboard

## Dashboard
Assess the dashboard from this link.
[Canada Crime watch](https://arpan-sharma.shinyapps.io/crime_watch/)


## Overview

The **Canada Crime Watch Dashboard** is an interactive Shiny web application designed to provide insights into crime statistics across Canada. It allows users to explore trends, analyze regional crime rates, and gain insights into demographic and geographic crime data.

## Features

- **Filters:** Narrow down analysis by year, geographic level (national, provincial, or city), crime category, and more.
- **Dashboard View:** Summarized metrics, crime trends, and distribution visualizations.
- **Geographic Analysis:** Explore crime rates across provinces and cities using interactive maps and regional statistics.
- **Crime Analysis:** View breakdowns of crime categories, including clearance rates.
- **Trends:** Examine year-over-year changes and identify the fastest increasing or decreasing crime types.
- **Demographics:** Compare youth vs. adult involvement across crime categories.
- **Location Comparison:** Compare crime rates across selected provinces or cities.
- **Data Explorer:** Access detailed crime data, apply filters, and download datasets for further analysis.

## How to Use

1. **Filters:**
   - Use the sidebar to select filters such as years, geographic level, provinces, cities, and crime types.
   - Use the "Reset Filters" button to clear all selections.

2. **Tabs:**
   - **Dashboard:** View key statistics, trends, and crime distributions.
   - **Geographic Analysis:** Use the map and regional charts to explore crime patterns geographically.
   - **Crime Analysis:** Analyze crime categories and clearance rates.
   - **Trends:** Discover crime trends and view year-over-year changes.
   - **Demographics:** Compare youth vs. adult crime statistics.
   - **Location Comparison:** Compare crime rates between selected provinces or cities.
   - **Data Explorer:** View and export raw crime data.

3. **Export Data:**
   - Use the "Data Explorer" tab to filter and download crime data in CSV or Excel format.

## Prerequisites

To run this dashboard locally, ensure you have the following installed:

- R (version 4.0 or later)
- RStudio (optional)
- R packages:
  - `shiny`
  - `shinydashboard`
  - `data.table`
  - `plotly`
  - `leaflet`
  - `DT`
  - `scales`
  - `viridis`
  - `leaflet.extras`
  - `shinycssloaders`
  - `sf`

Install all required packages by running:
```R
install.packages(c("shiny", "shinydashboard", "data.table", "plotly", "leaflet", "DT", "scales", "viridis", "leaflet.extras", "shinycssloaders", "sf"))
```
## Installation

Follow these steps to set up and run the Canada Crime Watch Dashboard on your local machine:

### 1. Clone the Repository
First, clone this repository to your local machine using the command below:
```bash
git clone https://github.com/your-repo/canada-crime-watch-dashboard.git
cd canada-crime-watch-dashboard
```

### 2. Install R and RStudio
Ensure you have R (version 4.0 or later) and RStudio installed on your machine. You can download them here:
- [Download R](https://cran.r-project.org/)
- [Download RStudio](https://www.rstudio.com/products/rstudio/download/)

### 3. Install Required R Packages
Install the necessary R packages by running the following command in your R or RStudio console:

```R
install.packages(c("shiny", "shinydashboard", "data.table", "plotly", "leaflet", "DT", "scales", "viridis", "leaflet.extras", "shinycssloaders", "sf"))
```

### 4. Run the application
Once all dependencies are installed, you can launch the Shiny application. Open your R console, navigate to the cloned directory, and run:

```bash
shiny::runApp()
```


## Contributors

Made by [Arpan Sharma](https://github.com/Arpan-shrma) and [Harsh Tiwari](https://github.com/HarshTiwari1710)


## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

You are free to use, modify, and distribute this software, provided that the original license is included with any substantial copies of the software. (**Note**: If the link to dashboard is not working contact collaborators)

