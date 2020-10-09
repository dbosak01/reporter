
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
  
  ret <- paste0("{\\pict\\pngblip\\picwgoal",round(width*conv),"\\pichgoal",round(height*conv)," \n")
  ret <- paste0(ret, get_image_bytes(file_path), "\n} \\par\\ql")
  
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

          
#          
# img <- "./man/images/cat.png"
# 
# dat <- get_image_rtf(img, 3, 3, "inches")
# writeLines(dat, "bytes.txt")          
          
          
