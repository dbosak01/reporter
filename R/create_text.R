
# Create Text Constructor     ---------------------------------------------

#' @title Create a text specification
#' @description Function to create a text specification that can be 
#' added as content to a report.
#' @param txt The text to create.
#' @param blank_row Whether to create blanks above or below the object.  Valid
#' value are 'above', 'below', 'both', or 'none'.
#' @param width The width of the text in the specified units of measure.
#' @param align How to align the text within the text area.  Valid values
#' are 'left', 'right', 'center', or 'centre'.
#' @return The text specification.
#' @export
create_text <- function(txt, width = NULL, align = "left", 
                        blank_row = "below") {
  
  ret <- structure(list(), class = c("text_spec", "list"))
  
  ret$text <- txt
  ret$align <- align
  ret$width <- width
  ret$blank_row <- blank_row
  
  return(ret)
  
}


# Create Text Pages -------------------------------------------------------

#' @description A function to output strings for plain text content
#' @details Basic logic is to wrap any text to the available line width, 
#' then then just dump it out.  All formatting is left to the user.
#' @param rs The Report Spec
#' @param txt The text content to output
#' @import stringi
#' @noRd
create_text_pages_text <- function(rs, txt) {
  
  rws <- get_text_body(txt$text, rs$line_size, rs$body_line_count)
  
  # Get last page 
  lpg <- rws[[length(rws)]]

  # Append empty strings to fill up body
  blnks <- rep("", rs$body_line_count - length(lpg))
  
  rws[[length(rws)]] <- c(lpg, blnks)
  
  return(rws)
}

#' @noRd
get_text_body <- function(txt, line_width, line_count) {
  
  # Wrap the text 
  a <- stri_wrap(unlist(
    strsplit(txt, split = "\n", fixed = TRUE)), 
    width = line_width, normalize = FALSE)
  
  ret <- list()  # Page list
  tmp <- c()     # 1 page of content
  for (i in seq_along(a)) {
    if (length(tmp) < line_count) {
      
      # Append to existing page
      tmp[length(tmp) + 1] <- a[i]
      
    } else {
      
      # Start a new page
      ret[[length(ret) + 1]] <- tmp
      tmp <- c(a[i])
    }
  }
  
  # Deal with last page
  if (length(tmp) > 0 ) {
    
    # Add last page
    ret[[length(ret) + 1]] <- tmp
    
  }
  
  return(ret)
  
}
