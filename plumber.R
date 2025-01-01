# plumber.R
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*") # "https://zweitstimme.org")
  plumber::forward()
}

# Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

# Serve a PDF file
#* @serializer contentType list(type="application/pdf")
#* @get /pdf
function(res) {
  pdf_path <- "/app/files/figure_forecast.pdf"
  
  # Check if file exists
  if (!file.exists(pdf_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Read the file and return as raw content
  pdf_content <- readBin(pdf_path, "raw", file.info(pdf_path)$size)
  
  # Set the content type
  res$setHeader("Content-Type", "application/pdf")
  
  res$body <- pdf_content
  res
}



# Serve the PNG file
#* @serializer contentType list(type="image/png")
#* @get /figure
function(res) {
  # Path to the PNG file
  png_path <- "/app/files/figure_forecast.png"
  
  # Check if the file exists
  if (!file.exists(png_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Read the PNG file as raw binary content
  png_content <- readBin(png_path, "raw", file.info(png_path)$size)
  
  # Set the content type as image/png
  res$setHeader("Content-Type", "image/png")
  
  # Set the response body to the binary content
  res$body <- png_content
  res
}




# Serve the PNG file
#* @serializer contentType list(type="image/png")
#* @get /figure_districts
function(res) {
  # Path to the PNG file
  png_path <- "/app/files/figure_forecast_districts.png"
  
  # Check if the file exists
  if (!file.exists(png_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Read the PNG file as raw binary content
  png_content <- readBin(png_path, "raw", file.info(png_path)$size)
  
  # Set the content type as image/png
  res$setHeader("Content-Type", "image/png")
  
  # Set the response body to the binary content
  res$body <- png_content
  res
}





# Serve forecasts as JSON
# Including 83% and 95% intervals
#* @serializer unboxedJSON
#* @get /forecast
function(res) {
  rds_path <- "/app/files/forecast_api.rds"
  
  # Check if the file exists
  if (!file.exists(rds_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Load the RDS file
  forecast_data <- readRDS(rds_path)
  
  # Manually convert the data to JSON and specify UTF-8 encoding
  json_data <- jsonlite::toJSON(forecast_data, pretty = TRUE, auto_unbox = TRUE, 
                                encode = "UTF-8")
  
  # Set the content type and encoding
  res$setHeader("Content-Type", "application/json; charset=utf-8")
  
  # Return JSON data directly
  res$body <- json_data
  
  res
}

# Serve district forecasts as JSON
# Including 83% intervals
#* @serializer unboxedJSON
#* @get /forecast_districts
function(res) {
  rds_path <- "/app/files/forecast_districts.rds"
  
  # Check if the file exists
  if (!file.exists(rds_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Load the RDS file
  forecast_data <- readRDS(rds_path)
  
  # Manually convert the data to JSON and specify UTF-8 encoding
  json_data <- jsonlite::toJSON(forecast_data, pretty = TRUE, auto_unbox = TRUE, 
                                encode = "UTF-8")
  
  # Set the content type and encoding
  res$setHeader("Content-Type", "application/json; charset=utf-8")
  
  # Return JSON data directly
  res$body <- json_data
  
  res
}


# Serve the last update timestamp
#* @serializer unboxedJSON
#* @get /last_updated
function(res) {
  rds_path <- "/app/files/last_updated.rds"
  
  # Check if the file exists
  if (!file.exists(rds_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Load the timestamp from the RDS file
  last_updated <- readRDS(rds_path)
  
  # Return the timestamp as JSON
  return(list(last_updated = last_updated))
}

# Serve forecast draws as JSON
# 10,000 draws from the forecast distribution
#* @serializer unboxedJSON
#* @get /draws
function(res) {
  rds_path <- "/app/files/forecast_draws.rds"
  
  # Check if the file exists
  if (!file.exists(rds_path)) {
    res$status <- 404
    return(list(error = "File not found"))
  }
  
  # Load the RDS file
  forecast_data <- readRDS(rds_path)
  
  # Manually convert the data to JSON and specify UTF-8 encoding
  json_data <- jsonlite::toJSON(forecast_data, pretty = TRUE, auto_unbox = TRUE, 
                                encode = "UTF-8")
  
  # Set the content type and encoding
  res$setHeader("Content-Type", "application/json; charset=utf-8")
  
  # Return JSON data directly
  res$body <- json_data
  
  res
}

# This endpoint serves the HTML file directly
#* @get /interactive
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/forecast.html"
  
  # Check if the file exists
  if (file.exists(html_file_path)) {
    # Serve the HTML file
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")  # Read HTML file and set body
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}



# This endpoint serves the HTML file directly
# It is a mobile version of interactive
#* @get /interactive_mobile
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/forecast_mobile.html"
  
  # Check if the file exists
  if (file.exists(html_file_path)) {
    # Serve the HTML file
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")  # Read HTML file and set body
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}


# This endpoint serves the HTML file directly
#* @get /interactive_trend
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/forecast_trend.html"
  
  # Check if the file exists
  if (file.exists(html_file_path)) {
    # Serve the HTML file
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")  # Read HTML file and set body
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}



# Serve hurdle.png
#* @get /hurdle
function(req, res) {
  png_file_path <- "/app/files/hurdle.png"
  
  if (file.exists(png_file_path)) {
    res$setHeader("Content-Type", "image/png")
    res$body <- readBin(png_file_path, "raw", file.info(png_file_path)$size)
    return(res)
  } else {
    res$status <- 404
    return(list(message = "PNG file not found"))
  }
}

# Serve majorities.png
#* @get /majorities
function(req, res) {
  png_file_path <- "/app/files/majorities.png"
  
  if (file.exists(png_file_path)) {
    res$setHeader("Content-Type", "image/png")
    res$body <- readBin(png_file_path, "raw", file.info(png_file_path)$size)
    return(res)
  } else {
    res$status <- 404
    return(list(message = "PNG file not found"))
  }
}

# Serve winner.png
#* @get /winner
function(req, res) {
  png_file_path <- "/app/files/winner.png"
  
  if (file.exists(png_file_path)) {
    res$setHeader("Content-Type", "image/png")
    res$body <- readBin(png_file_path, "raw", file.info(png_file_path)$size)
    return(res)
  } else {
    res$status <- 404
    return(list(message = "PNG file not found"))
  }
}
