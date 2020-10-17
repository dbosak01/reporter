
# Create Text Constructor     ---------------------------------------------

#' @title Create text content
#' @description Function to create a text specification that can be 
#' added as content to a report.  The function creates an S3 object of class
#' 'text_spec'.  The \code{create_text} function can be used to include 
#' analysis on a statistical report.
#' @details 
#' To add plain text to a report, use the \code{create_text} function.  The 
#' function allows you to set a width and alignment for the text.  The
#' function will preserve any other formatting you apply to the text.  See
#' the \code{\link{add_content}} function to control page breaking and 
#' blanks spaces above or below the text.  
#' 
#' The text specification also accepts titles and footnotes.  See the 
#' \code{\link{titles}} and \code{\link{footnotes}} functions for further 
#' details.
#' 
#' @param txt The text to create.
#' @param width The width of the text in the specified units of measure.  If 
#' no width is specified, the full page width will be used.
#' @param align How to align the text on the page.  Valid values
#' are 'left', 'right', 'center', or 'centre'.  Default is 'left'.
#' @return The text specification.
#' @family text
#' @seealso 
#' \code{\link{titles}} to add a title block to the text,  
#' \code{\link{footnotes}} to add footnotes, and \code{\link{add_content}} 
#' to add the text object to a report.
#' @examples 
#' library(rptr)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Create dummy text
#' dt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
#'   "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
#'   "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
#'   "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
#'   "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
#'   "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
#'   "qui officia deserunt mollit anim id est laborum.")
#' 
#' # Create the text object
#' txt <- create_text(dt) %>% 
#'   titles("Text Content 1.0", "Sample Text Report") %>% 
#'   footnotes("* Cicero, 1st century BCE")
#' 
#' # Create the report object
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   add_content(txt) 
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write the report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #                                Text Content 1.0
#' #                               Sample Text Report
#' # 
#' # Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
#' # incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
#' # nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
#' # Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
#' # eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
#' # in culpa qui officia deserunt mollit anim id est laborum.
#' # 
#' # * Cicero, 1st century BCE
#' #
#' @export
create_text <- function(txt, width = NULL, align = "left") {
  
  if (!"character" %in% class(txt))
    stop("value must be of class 'character'")
  
  if (is.null(align))
    align = "left"
  
  if (!align %in% c("left", "right", "center", "centre"))
    stop(paste0("align value invalid.  Valid values are 'left', 'right',",
                "'center', or 'centre'."))
  
  if (!is.null(width)) {
    
    if (!is.numeric(width))
      stop("width must be a number.")
    
    if (width <= 0)
      stop("width must be greater than zero.")
    
  }
  
  ret <- structure(list(), class = c("text_spec", "list"))
  
  ret$text <- txt
  ret$align <- align
  ret$width <- width
  
  return(ret)
  
}


# Utilities ---------------------------------------------------------------




#' @title Prints the text spec
#' @description A function to print the text spec.
#' The \strong{print} function will print the text spec in summary 
#' form.  To view all parameters, set the \code{verbose} parameter to TRUE.
#' @param x The text spec.
#' @param ... Additional parameters to pass to the underlying print function.
#' @param verbose Whether to print in verbose form.  Default is FALSE.
#' @seealso 
#' \code{\link{create_text}} function to create a text specification.
#' @return The text spec, invisibly.
#' @family text
#' @examples 
#' txt <- create_text("Lorem ipsum dolor sit amet, consectetur...",
#'                    align = "left", width = 3)
#' txt
#'
#' # A text specification: 6 words
#' # - text: Lorem ipsum dolor sit amet, consectetur...
#' # - width: 3
#' # - align: left
#' @import crayon
#' @export
print.text_spec <- function(x, ..., verbose = FALSE){
  
  
  if (verbose == TRUE) {
    
    print(unclass(x))
    
  } else {
    
    
    grey60 <- make_style(grey60 = "#999999")

    # Print header
    cat(grey60("# A text specification: "))
    
    # Print 100 characters of text 
    if (!is.null(x$text)) {
      
      wc <- lengths(strsplit(x$text, " "))
      cat(grey60(wc %+% " words\n"))
      
      cat("- text: ")
      if (nchar(x$text) > 100)
        cat(paste0(substr(x$text, 1, 100), "..."))
      else 
        cat(x$text)
      
      cat("\n")
            
    } else {
      
     cat("\n") 
    }
    
    if (!is.null(x$width)) 
      cat(paste0("- width: ", x$width, "\n"))
    
    if (!is.null(x$align)) 
      cat(paste0("- align: ", x$align, "\n"))
    
    # Print  titles
    if (!is.null(x$titles)) {
      
      ttlcnt <- 1
      # There can be more than 1 title block
      for (i in seq_along(x$titles)) {
        
        for (j in seq_along(x$titles[[i]]$titles)) {
          cat("- title " %+% as.character(ttlcnt) %+% ": '" 
              %+% substring(x$titles[[i]]$titles[[j]], 1) %+% "'\n")
          ttlcnt <- ttlcnt + 1
        }
        
      }
    }
    
    # Print footnotes
    if (!is.null(x$footnotes)) {
      
      ftncnt <- 1
      # There can be more than 1 footnote block
      for (i in seq_along(x$footnotes)) {
        
        for (j in seq_along(x$footnotes[[i]]$footnotes)) {
          cat("- footnote " %+% as.character(ftncnt) %+% ": '" 
              %+% substring(x$footnotes[[i]]$footnotes[[j]], 1) %+% "'\n")
          ftncnt <- ftncnt + 1
        }
        
      }
    }
    
    
  }
  
  invisible(x)
}


# Write Functions -------------------------------------------------------

#' @description A function to output strings for plain text content
#' @details Basic logic is to wrap any text to the available line width, 
#' then then just dump it out.  All formatting is left to the user.
#' @param rs The Report Spec
#' @param cntnt The text content to output
#' @param lpg_rows Last page rows.
#' @import stringi
#' @noRd
create_text_pages_text <- function(rs, cntnt, lpg_rows) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  txt <- cntnt$object
  
  # Default width to the overall line size
  w <- rs$line_size
  
  # If user supplies a width, override default
  if (!is.null(txt$width))
    w <- round(txt$width / rs$char_width)
  
  rws <- get_text_body(txt, w, rs$body_line_count, lpg_rows, cntnt$blank_row)
  
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
  #print(paste("Offset:", offset))
  
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
    

    # If page is not empty
    if (max(nchar(trimws(tmp))) > 0) {
    
      # Add last page
      ret[[length(ret) + 1]] <- format(tmp, width = line_width, 
                                       justify = get_justify(txt$align))
    }
    
  }
  
  return(ret)
  
}
