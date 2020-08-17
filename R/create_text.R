
# Create Text Functions ---------------------------------------------------

#' @description A function to output strings for plain text content
#' @details Basic logic is to wrap any text to the available line width, 
#' then then just dump it out.  All formatting is left to the user.
#' @param rs The Report Spec
#' @param txt The text content to output
#' @import stringi
#' @noRd
create_text <- function(rs, txt) {
  
  # Wrap the text 
  a <- stri_wrap(unlist(
    strsplit(txt, split = "\n", fixed = TRUE)), 
    width = rs$line_size, normalize = FALSE)
  
  ret <- list()  # Page list
  tmp <- c()     # 1 page of content
  for (i in seq_along(a)) {
    if (length(tmp) < rs$body_line_count) {
      
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
    
    # Append empty strings to fill up body
    tmp <- append(tmp, rep("", rs$body_line_count - length(tmp)))
    
    # Add last page
    ret[[length(ret) + 1]] <- tmp
    
  }
  
  return(ret)
}
