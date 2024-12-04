# plumber.R


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Serve a PDF file
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



#* Serve the PNG file
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

#* Serve forecasts as JSON
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


#* Serve the last update timestamp
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

#* Serve forecasts as JSON
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

