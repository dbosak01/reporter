
# Create Text Constructor     ---------------------------------------------

#' @title Create a text specification
#' @description Function to create a text specification that can be 
#' added as content to a report.
#' @param txt The text to create.
#' @param width The width of the text in the specified units of measure.
#' @param align How to align the text within the text area.  Valid values
#' are 'left', 'right', 'center', or 'centre'.
#' @return The text specification.
#' @export
create_text <- function(txt, width = NULL, align = "left") {
  
  ret <- structure(list(), class = c("text_spec", "list"))
  
  ret$text <- txt
  ret$align <- align
  ret$width <- width
  
  return(ret)
  
}


# Create Text Pages -------------------------------------------------------

#' @description A function to output strings for plain text content
#' @details Basic logic is to wrap any text to the available line width, 
#' then then just dump it out.  All formatting is left to the user.
#' @param rs The Report Spec
#' @param txt The text content to output
#' @param lpg_rows Last page rows.
#' @import stringi
#' @noRd
create_text_pages_text <- function(rs, txt, lpg_rows, content_blank_row) {
  
  w <- rs$line_size
  if (!is.null(txt$width))
    w <- round(txt$width / rs$char_width)
  
  h <- rs$body_line_count 
  
  rws <- get_text_body(txt, w, h, lpg_rows, content_blank_row)
  
  return(rws)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_text_body <- function(txt, line_width, line_count, lpg_rows, 
                          content_blank_row) {
  
  # Get titles and footnotes
  ttls <- get_titles(txt$titles, line_width) 
  ftnts <- get_footnotes(txt$footnotes, line_width) 
  
  # Wrap the text 
  s <- stri_wrap(unlist(
    strsplit(txt$text, split = "\n", fixed = TRUE)), 
    width = line_width, normalize = FALSE)
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
      a <- ""
  
  
  # Add blank below content if requested
  b <- NULL
  if (content_blank_row %in% c("both", "below"))
    b <- ""
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls, s, ftnts, b)
  
  # Page list
  ret <- list()  
  
  # Create tmp variable for 1 page of content
  tmp <- c()
  
  # Offset the first page with remaining rows from the 
  # last page of the previous content
  offset <- lpg_rows 
  
  # Assign content to pages
  for (i in seq_along(rws)) {
    if (length(tmp) < (line_count - offset)) {
      
      # Append to existing page
      tmp[length(tmp) + 1] <- rws[i]
      
    } else {
      
      # Start a new page
      ret[[length(ret) + 1]] <- format(tmp, width = line_width, 
                                       justify = get_justify(txt$align))
      tmp <- rws[i]
      
      # Set to zero on second page and leave it that way
      offset <- 0  
    }
  }
  
  # Deal with last page
  if (length(tmp) > 0 ) {
    
    # Add last page
    ret[[length(ret) + 1]] <- format(tmp, width = line_width, 
                                     justify = get_justify(txt$align))
    
  }
  
  return(ret)
  
}
