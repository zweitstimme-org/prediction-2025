
#* Enable CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#* Save a quiz submission
#* @post /quiz
function(req) {
  # Parse the JSON body
  submission <- fromJSON(req$postBody)
  
  # Get current working directory for debugging
  current_dir <- getwd()
  print(paste("Current working directory:", current_dir))
  
  # Create a unique filename using timestamp and sanitized name
  timestamp <- format(now(), "%Y%m%d_%H%M%S")
  safe_name <- gsub("[^[:alnum:]]", "_", submission$name)
  
  # Use absolute path
  quiz_dir <- "/app/files/quiz"
  filename <- file.path(quiz_dir, paste0(timestamp, "_", safe_name, ".json"))
  
  print(paste("Attempting to write to:", filename))
  
  # Create quiz directory if it doesn't exist
  if (!dir.exists(quiz_dir)) {
    dir.create(quiz_dir, recursive = TRUE)
    print(paste("Created directory:", quiz_dir))
  }
  
  # Write the submission to a file
  tryCatch({
    write(toJSON(submission, pretty = TRUE), filename)
    print(paste("Successfully wrote to:", filename))
    list(
      message = "Submission saved successfully",
      status = 200,
      path = filename  # Return the path for debugging
    )
  }, error = function(e) {
    print(paste("Error writing file:", e$message))
    list(
      error = "Failed to save submission",
      details = e$message,
      path = filename,
      status = 500
    )
  })
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
  rds_path <- "/app/files/prediction_data_districts.rds"
  
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
#* @get /forecast_districts_zeit
function(res) {
  rds_path <- "/app/files/prediction_data_districts_zeit.rds"
  
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





# Serve predicted probabilities for vacant seats as JSON
#* @serializer unboxedJSON
#* @get /pred_vacant
function(res) {
  rds_path <- "/app/files/pred_vacant.rds"
  
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



# Serve predicted probabilities as JSON
#* @serializer unboxedJSON
#* @get /pred_probabilities
function(res) {
  rds_path <- "/app/files/pred_probabilities.rds"
  
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


# Serve forecast draws as JSON
# 10,000 draws from the forecast distribution
#* @serializer unboxedJSON
#* @get /draws
function(res) {
  rds_path <- "/app/files/forecast_party_vote_draws.rds"
  
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
#* @get /interactive_districts_share
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/map_value.html"
  
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
#* @get /interactive_districts_probability
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/map_probability.html"
  
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
#* @get /interactive_vacant
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/map_vacant.html"
  
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




#* @get /map_zs_afd
function(req, res) {
  html_file_path <- "/app/files/map_zs_afd.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}


#* @get /map_zs_cdu
function(req, res) {
  html_file_path <- "/app/files/map_zs_cdu.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}

#* @get /map_zs_fdp
function(req, res) {
  html_file_path <- "/app/files/map_zs_fdp.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}

#* @get /map_zs_gru
function(req, res) {
  html_file_path <- "/app/files/map_zs_gru.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}

#* @get /map_zs_spd
function(req, res) {
  html_file_path <- "/app/files/map_zs_spd.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}

#* @get /map_zs_lin
function(req, res) {
  html_file_path <- "/app/files/map_zs_lin.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}

#* @get /map_zs_bsw
function(req, res) {
  html_file_path <- "/app/files/map_zs_bsw.html"
  if (file.exists(html_file_path)) {
    res$setHeader("Content-Type", "text/html")
    res$body <- paste(readLines(html_file_path), collapse = "\n")
    return(res)
  } else {
    res$status <- 404
    return(list(message = "HTML file not found"))
  }
}


# This endpoint serves the HTML file directly
#* @get /map_citizen_forecast_winner
function(req, res) {
  # Specify the path to your saved HTML file
  html_file_path <- "/app/files/map_citizen_forecast_winner.html"
  
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



#* Save quiz results for open participants
#* @post /quiz-open
function(req, res) {
  # Parse the JSON body
  data <- req$body
  