
# Create Text Constructor     ---------------------------------------------

#' @title Create text content
#' @description Function to create a text specification that can be 
#' added as content to a report.   The text content can be used to include 
#' analysis on a statistical report.  A text specification is an S3 
#' object of class 'text_spec'. 
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
#' @param align How to align the text within the content area.  Valid values
#' are 'left', 'right', 'center', or 'centre'.  Default is 'left'.
#' @return The text specification.
#' @family text
#' @seealso 
#' \code{\link{titles}} to add a title block to the text,  
#' \code{\link{footnotes}} to add footnotes, and \code{\link{add_content}} 
#' to add the text object to a report.
#' @examples 
#' library(reporter)
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
      cat(grey60(paste0(as.character(wc), " words\n")))
      
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
    
    
    print_title_header(x$title_hdr)

    print_titles(x$titles)
    
    print_footnotes(x$footnotes)
    
    
  }
  
  invisible(x)
}


# Write Text Functions -------------------------------------------------------

#' @description A function to output strings for plain text content
#' @details Basic logic is to wrap any text to the available line width, 
#' then then just dump it out.  All formatting is left to the user.
#' @param rs The Report Spec
#' @param cntnt The text content to output
#' @param lpg_rows Last page rows.

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
  
  rws <- get_text_body(rs, txt, w, rs$body_line_count, lpg_rows, cntnt$blank_row)
  
  return(rws)
}



#' Create list of vectors of strings for each page 
#' @import stringi
#' @noRd
get_text_body <- function(rs, txt, line_width, line_count, lpg_rows, 
                          content_blank_row) {
  
  # Get titles and footnotes
  ttls <- get_titles(txt$titles, line_width, rs$uchar) 
  ftnts <- get_footnotes(txt$footnotes, line_width, rs$uchar) 
  ttl_hdr <- get_title_header(txt$title_hdr, line_width, rs$uchar)
  
  # Wrap the text 
  s <- stri_wrap(unlist(
    strsplit(txt$text, split = "\n", fixed = TRUE)), 
    width = line_width, normalize = FALSE)
  
  
  # Make sure rows are the same length
  s <- pad_any(s, line_width + 1, 
               get_justify(txt$align))
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
      a <- ""
  
  
  # Add blank below content if requested
  b <- NULL
  if (content_blank_row %in% c("both", "below"))
    b <- ""
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls, ttl_hdr, s, ftnts, b)
  
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
      ret[[length(ret) + 1]] <- pad_any(tmp, line_width, 
                                        get_justify(txt$align))
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
      ret[[length(ret) + 1]] <- pad_any(tmp, line_width, 
                                       get_justify(txt$align))
    }
    
  }
  
  return(ret)
  
}


# Write RTF Functions -------------------------------------------------------


#' @noRd
create_text_pages_rtf <- function(rs, cntnt, lpg_twips, content_blank_row) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  txt <- cntnt$object
  
  w <- rs$content_size[["width"]] 
  
  
  # If user supplies a width, override default
  if (!is.null(txt$width))
    w <- txt$width
  
  rws <- get_text_body_rtf(rs, txt, w, rs$body_line_count, 
                           lpg_twips, content_blank_row)
  
  return(rws)
  
  
}

#' Create list of vectors of strings for each page 
#' @import stringi
#' @noRd
get_text_body_rtf <- function(rs, txt, width, line_count, lpg_twips, 
                              content_blank_row, conv) {

  bh <- rs$body_size[["height"]]
  bw <- rs$body_size[["width"]]
  lw <- rs$content_size[["width"]]
  conv <- rs$twip_conversion
  
  # Get report titles and footnotes
  rttls <- get_titles_rtf(rs$titles, lw, rs)
  rftnts <- get_titles_rtf(rs$footnotes, lw, rs)
  rttl_hdr <- get_title_header_rtf(rs$title_hdr, lw, rs)
  
  # Get content titles and footnotes
  ttls <- get_titles_rtf(txt$titles, width, rs) 
  ftnts <- get_footnotes_rtf(txt$footnotes, width, rs) 
  ttl_hdr <- get_title_header_rtf(txt$title_hdr, width, rs)
  
  hgt <- bh - rttls$twips - rftnts$twips - rttl_hdr$twips 
  hgt <- hgt - ttls$twips - ftnts$twips - ttl_hdr$twips 
  
  
  txtpgs <- get_text_pages_rtf(rs, txt$text, hgt, width, lpg_twips)
  
  if (txt$align == "right") 
    algn <- "\\qr"
  else if (txt$align %in% c("center", "centre"))
    algn <- "\\qc"
  else 
    algn <- "\\ql"
  
  ret <- list()
  
  for (i in seq_along(txtpgs)) {
    
    pg <- txtpgs[[i]]
    
    s <- paste0( algn, " ", pg, "\\line\\pard")


    # Add blank above content if requested
    a <- NULL
    if (i == 1 & content_blank_row %in% c("both", "above"))
      a <- "\\line"
    
    
    # Add blank below content if requested
    b <- NULL
    if (i == length(txtpgs) & content_blank_row %in% c("both", "below"))
      b <- "\\line"
    else if (length(txtpgs) > 1 & i != length(txtpgs))
      b <- rs$page_break_rtf
    
    spcs <- NULL
    
    
    # Combine titles, blanks, body, and footnotes
    ret[[length(ret) + 1]] <- c(a, rttls$rtf, rttl_hdr$rtf,
                              ttls$rtf, s, spcs, ftnts$rtf, rftnts$rtf, b)
    
    
  }

  # ret <- rws
  # # Page list
  # #ret <- list()  
  # 
  # # Create tmp variable for 1 page of content
  # tmp <- c()
  
  # # Offset the first page with remaining rows from the 
  # # last page of the previous content
  # offset <- lpg_rows 
  # #print(paste("Offset:", offset))
  # 
  # # Assign content to pages
  # for (i in seq_along(rws)) {
  #   if (length(tmp) < (line_count - offset)) {
  #     
  #     # Append to existing page
  #     tmp[length(tmp) + 1] <- rws[i]
  #     
  #   } else {
  #     
  #     # Start a new page
  #     ret[[length(ret) + 1]] <- pad_any(tmp, line_width, 
  #                                       get_justify(txt$align))
  #     tmp <- rws[i]
  #     
  #     # Set to zero on second page and leave it that way
  #     offset <- 0  
  #   }
  # }
  # 
  # # Deal with last page
  # if (length(tmp) > 0 ) {
  #   
  #   
  #   # If page is not empty
  #   if (max(nchar(trimws(tmp))) > 0) {
  #     
  #     # Add last page
  #     ret[[length(ret) + 1]] <- pad_any(tmp, line_width, 
  #                                       get_justify(txt$align))
  #   }
  #   
  # }
  
  
  
  return(ret)
  
}

#' @description height in twips, width in unit of measure.
#' @noRd
get_text_pages_rtf <- function(rs, txt, height, width, lpg_twips) {
  
  lh <- rs$line_height
  offst <- floor(lpg_twips / lh)
  lns <- floor(height / lh) 
  print(height)
  print(lh)
  print(lns)
  
  tpgs <- split_text_rtf(txt, lns, width, rs$font, rs$font_size, rs$units, offst)
  

  
  return(tpgs)
}


#' @description lines is the number of lines per page or cell before breaking.
#' Width is the width of the page or cell.
#' @noRd
split_text_rtf <- function(txt, lines, width, font, font_size, units, offset = 0) {
  
  pgs <- c()
  lnlngth <- 0
  ln <- c()
  cnt <- 0
  lns <- c()
  
  # Split text into words
  wrds <- strsplit(txt, " ")[[1]]
  
  # Set font
  f <- "mono"
  if (tolower(font) == "arial")
    f <- "sans"
  else if (tolower(font) == "times")
    f <- "serif"
  
  # Set font and size
  par(family = f, ps = font_size)

  # Get lengths for all words plus space after
  lngths <-  (strwidth(wrds, units = units) + strwidth(" ", units = units)) * 1.03

  # Loop through words and add up lines
  for (i in seq_along(wrds)) {
    
    lnlngth <- lnlngth + lngths[i] 
    if (lnlngth <= width)
      ln <- append(ln, wrds[i])
    else {
      cnt <- cnt + 1

      # If cnt exceeds allowed lines per page, start a new page
      if (cnt <= lines - offset) {
        lns <- append(lns, paste(ln, collapse = " "))
        ln <- wrds[i]
        lnlngth <- lngths[i]
      } else {
        pgs <- append(pgs, paste(lns, collapse = " "))

        lns <- paste(ln, collapse = " ")
        ln <- wrds[i]
        lnlngth <- lngths[i]
        # After first page, set this to zero.
        offset <- 0
        cnt <- 1
      }

    }
    
    
  }
  
  if (length(lns) > 0 | length(ln) > 0) {
    lns <- append(lns, paste(ln, collapse = " "))
    
    pgs <- append(pgs, paste(lns, collapse = " ")) 
  }
  
  return(pgs)
  
}

