# Use the official Rocker Shiny image as base
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'readr', 'DT', 'shinythemes', 'RColorBrewer', 'writexl', 'plotly', 'shinyjs', 'ggplot2', 'viridis', 'tibble', 'tidyr'), repos='https://cran.rstudio.com/')"

# Remove default shiny apps
RUN rm -rf /srv/shiny-server/*

# Copy your app files
COPY app.R /srv/shiny-server/
COPY final_ispr_men_table.csv /srv/shiny-server/
COPY variable_abbreviations.csv /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server

# Expose port 3838
EXPOSE 3838

# Run the shiny server
CMD ["/usr/bin/shiny-server"]