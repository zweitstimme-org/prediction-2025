# Use an official R base image from Docker Hub
FROM rocker/r-ver:4.4.0

# Set the working directory inside the container
WORKDIR /app

# Install system dependencies for Plumber and other R packages
RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libsodium-dev \
&& rm -rf /var/lib/apt/lists/*
  
  # Install plumber and any other necessary R packages
  RUN R -e "install.packages('plumber')"

# Copy the Plumber API script into the container
COPY plumber.R /app/
  
# Copy the required files into the container
# COPY output/forecasts/forecast_api.rds /app/files/
# COPY output/fig/figure_forecast.png /app/files/
# COPY output/fig/figure_forecast.pdf /app/files/

# Expose the port for the API
EXPOSE 8073

# Start the Plumber API
CMD ["R", "-e", "library(plumber); pr('/app/plumber.R') %>% pr_run(host = '0.0.0.0', port=8073)"]