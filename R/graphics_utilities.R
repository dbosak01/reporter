
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
  
  blp <- "\\pngblip"
  if (grepl(".emf", file_path, fixed = TRUE))
    blp <- "\\emfblip"
  
  # Construct rtf codes
  ret <- paste0("{\\pict", blp, "\\picwgoal",round(width*conv),"\\pichgoal",round(height*conv)," \n")
  
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

#' @import stringi      
#' @noRd
get_image_html <- function(file_path, report_path, plt, units) {
  

  dr <- file.path(dirname(report_path), "images")
  if (!file.exists(dr)) {
    dir.create(dr) 
  }
  
  fl <-  paste0( gsub(".html", "", basename(report_path), fixed = TRUE), "-", 
                 stri_rand_strings(1, 4, pattern = "[A-Z0-9]"), ".jpg")
    
  pth <- file.path(dr, fl)

  
  res <- file.copy(file_path, pth)
  if (all(res == TRUE)) {
    file.remove(file_path) 
  }
  
  u <- units
  if (u == "inches")
    u <- "in"
  
  ph <- round(plt$height * .99, 3)
  pw <- round(plt$width * .99, 3)
  
  ret <- paste0("<img src=\"./images/", basename(pth), "\"", 
                " style=\"height:", ph, u, ";",
                " width:", pw, u, ";\">")
  
  
  return(ret)
}
