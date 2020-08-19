
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
#' @param lpg_rows Last page rows.
#' @import stringi
#' @noRd
create_text_pages_text <- function(rs, txt, lpg_rows) {
  
  w <- rs$line_size
  if (!is.null(txt$width))
    w <- round(txt$width / rs$char_width)
  
  ttls <- get_titles(txt$titles, w) 
  ftnts <- get_footnotes(txt$footnotes, w) 
  
  h <- rs$body_line_count 
  
  rws <- get_text_body(txt$text, w, h, 
                       txt$align, lpg_rows, 
                       length(ttls))
  
  if (length(rws) > 0) {
    
    # Append titles to the beginning of the first page
    rws[[1]] <- c(ttls, rws[[1]])
    

    # Problem if addition of footnotes puts the height
    # over the available line count.
    # If not, append to end of last page.
    # If so, create a new page to get overflow.
    last_page <- rws[[length(rws)]]
    if (length(last_page) + length(ftnts) <= h)
      rws[[length(rws)]] <- c(last_page, ftnts)
    else {
      d <- length(last_page) + length(ftnts) - h
      rws[[length(rws)]] <- c(last_page, ftnts[1:d])
      rws[[length(rws) + 1]] <-  ftnts[(d + 1):length(ftnts)]
    }

  }
  
  return(rws)
}
#' Create list of vectors of strings for each page 
#' @noRd
get_text_body <- function(txt, line_width, line_count, align, lpg_rows,
                          title_count) {
  
  # Wrap the text 
  a <- stri_wrap(unlist(
    strsplit(txt, split = "\n", fixed = TRUE)), 
    width = line_width, normalize = FALSE)
  
  ret <- list()  # Page list
  tmp <- c()     # 1 page of content
  
  # Offset the first page with remaining rows from the 
  # last page of the previous content
  # plus any titles for the text content
  offset <- lpg_rows + title_count
  
  # print("offset text")
  # print(offset)
  
  for (i in seq_along(a)) {
    if (length(tmp) < (line_count - offset)) {
      
      # Append to existing page
      tmp[length(tmp) + 1] <- a[i]
      
    } else {
      
      # Start a new page
      ret[[length(ret) + 1]] <- format(tmp, width = line_width, 
                                       justify = get_justify(align))
      tmp <- c(a[i])
      
      # Set to zero on second page and leave it that way
      offset <- 0  
    }
  }
  
  # Deal with last page
  if (length(tmp) > 0 ) {
    
    # Add last page
    ret[[length(ret) + 1]] <- format(tmp, width = line_width, 
                                     justify = get_justify(align))
    
  }
  
  return(ret)
  
}
