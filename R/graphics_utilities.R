
# Right now only png supported
#' @noRd
get_image_rtf <- function(file_path, width, height, units) {
  
  if (!units %in% c("inches", "cm")) {
    stop("Invalid units.") 
  }
  
  # Get conversion factor to twips
  if (units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  # Construct rtf codes
  ret <- paste0("{\\pict\\pngblip\\picwgoal",round(width*conv),"\\pichgoal",round(height*conv)," \n")
  
  # Convert image to byte codes
  ret <- paste0(ret, get_image_bytes(file_path), "\n}")

  
  return(ret)
}

#' @noRd
get_image_bytes <- function(file_path) {
  
  
  if (!file.exists(file_path))
    stop(paste("Image file not found:", file_path))

  max.bytes<-50000000

  # Read in bytes
  dat<-readBin(file_path, what="raw", size=1, 
               signed=TRUE, endian="little", n=max.bytes);

  # Split single vector into list of 80 character vectors
  sdat <- split(dat, ceiling(seq_along(dat)/40))

  # Collapse each 80 character vector into a string
  lns <- sapply(sdat, paste, collapse = "")

  # Append a line return to each line and collapse into single string
  ret <- paste0(lns, "\n", collapse = "")

  
  return(ret)

}

      
#' @noRd
get_image_html <- function(file_path, report_path, plt, units) {
  

  dr <- file.path(dirname(report_path), "images")
  if (!file.exists(dr)) {
    dir.create(dr) 
  }
    
  pth <- file.path(dr, basename(file_path))

  
  res <- file.copy(file_path, pth)
  if (all(res == TRUE)) {
    file.remove(file_path) 
  }
  
  u <- units
  if (u == "inches")
    u <- "in"
  
  
  ret <- paste0("<img src=\"./images/", basename(pth), "\"", 
                " style=\"height:", plt$height, u, ";",
                " width:", plt$width, u, ";\">")
  
  
  return(ret)
}
