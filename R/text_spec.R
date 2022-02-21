
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
#' @param borders Whether and where to place a border. Valid values are 'top',
#' 'bottom', 'left', 'right', 'all', 'none', and 'outside'.  
#' Default is 'none'.  The 'left', 'right', and 'outside' 
#' border specifications only apply to RTF reports.
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
create_text <- function(txt, width = NULL, align = "left", borders = "none") {
  
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
  
  if (!all(borders %in% c("top", "bottom", "left", "right", 
                          "all", "none", "outside")))
    stop(paste("Borders parameter invalid.  Valid values are", 
               "'top', 'bottom', 'left', 'right', 'all', ",
               "'none', or 'outside'."))
  
  ret <- structure(list(), class = c("text_spec", "list"))
  
  ret$text <- txt
  ret$align <- align
  ret$width <- width
  ret$borders <- borders
  
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
  ttls <- get_titles(txt$titles, line_width, rs$line_size, 
                     rs$uchar, rs$char_width) 
  ftnts <- get_footnotes(txt$footnotes, line_width,  rs$line_size, 
                         rs$uchar, rs$char_width) 
  ttl_hdr <- get_title_header(txt$title_hdr, line_width, rs$line_size, 
                              rs$uchar, rs$char_width)
  
  # Wrap the text 
  s <- stri_wrap(unlist(
    strsplit(txt$text, split = "\n", fixed = TRUE)), 
    width = line_width, normalize = FALSE)
  
  
  # Make sure rows are the same length
  s <- pad_any(s, line_width, 
               get_justify(txt$align))
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
      a <- ""
  
  # Add top border if requested
  tbrdr <- NULL
  if (any(txt$borders %in% c("top", "all", "outside")))
    tbrdr <-  paste0(rep(rs$uchar, line_width), collapse = "")
  
  # Add bottom border if requested
  bbrdr <- NULL
  if (any(txt$borders %in% c("bottom", "all", "outside")))
    bbrdr <- paste0(rep(rs$uchar, line_width), collapse = "")
  
  # Add blank below content if requested
  b <- NULL
  if (content_blank_row %in% c("both", "below"))
    b <- ""
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls, ttl_hdr, tbrdr, s, bbrdr)
  
  # Set to true for now.  Need to fix text paging below.
  wrap_flag <- FALSE
  
  # Get footnotes
  ftnts <- get_page_footnotes_text(rs, txt, line_width, lpg_rows, 
                                   length(rws), wrap_flag, content_blank_row)
  # Append footnotes 
  rws <- c(rws, ftnts)
  
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
create_text_pages_rtf <- function(rs, cntnt, lpg_rows, content_blank_row) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  txt <- cntnt$object
  
  # Default to content width
  w <- rs$content_size[["width"]] 

  # If user supplies a width, override default
  if (!is.null(txt$width))
    w <- txt$width
  
  res <- get_text_body_rtf(rs, txt, w, rs$body_line_count, 
                           lpg_rows, content_blank_row, cntnt$align)
  

  return(res)
  
  
}

#' Create list of vectors of strings for each page 
#' @import stringi
#' @noRd
get_text_body_rtf <- function(rs, txt, width, line_count, lpg_rows, 
                              content_blank_row, talgn) {


  # Get content titles and footnotes
  ttls <- get_titles_rtf(txt$titles, width, rs, talgn) 
  ttl_hdr <- get_title_header_rtf(txt$title_hdr, width, rs, talgn)
  ftnts <- get_footnotes_rtf(txt$footnotes, width, rs, talgn) 
  
  t <- sum(ttls$lines, ftnts$lines, ttl_hdr$lines)
  hgt <- rs$body_line_count - t

  # Break text content into pages if necessary
  tpgs <- split_text(txt$text, hgt, width, rs$font, 
                 rs$font_size, rs$units, lpg_rows)
  
  # Capture rtf pages and line counts
  txtpgs <- tpgs$text
  lns <- tpgs$lines
  
  # Calculate text width in twips
  w <- round(width * rs$twip_conversion)
  
  # Get content alignment codes
  if (talgn == "right") 
    tgn <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    tgn <- "\\trqc"
  else 
    tgn <- "\\trql"
  
  # Get text alignment codes
  if (txt$align == "right") 
    algn <- "\\qr"
  else if (txt$align %in% c("center", "centre"))
    algn <- "\\qc"
  else 
    algn <- "\\ql"

  # Get cell border codes
  b <- get_cell_borders(1, 1, 1, 1, txt$borders)  
  
  # Prepare row header and footer
  rwhd <- paste0("\\trowd\\trgaph0", tgn, b, "\\cellx", w, algn, " ")
  rwft <- paste0("\\cell\\row")
  
  ret <- list()
  cnt <- c()
  
  # Gather rtf and line counts for each page
  for (i in seq_along(txtpgs)) {
    
    if (i == length(txtpgs))
      wrap_flag <- FALSE
    else 
      wrap_flag <- TRUE
    
    pg <- txtpgs[[i]]
    
    # Put line ending on all but last line
    if (length(pg) > 1) {
      s <- paste0(pg[seq(1, length(pg) - 1)], "\\line ")
      s <- c(s, pg[length(pg)])
    } else
      s <- pg

    # Add blank above content if requested
    a <- NULL
    if (i == 1 & content_blank_row %in% c("both", "above"))
      a <- "\\par"
  
    # Deal with cell padding.  Don't count this in line count.
    cp <- paste0("\\li", rs$cell_padding, "\\ri", rs$cell_padding)
    

    # Sum up lines
    cnts <- sum(length(a),  ttls$lines, ttl_hdr$lines, lns[[i]])
    
    # Get footnotes
    ftnts <- get_page_footnotes_rtf(rs, txt, width, lpg_rows, cnts,
                                    wrap_flag, content_blank_row, talgn)
    
    # On LibreOffice, have to protect the table from the title width or
    # the table row will inherit the title row width. Terrible problem.
    tpt <- "{\\pard\\fs1\\sl0\\par}"
    if (any(txt$borders %in% c("all", "top", "outside"))) {
      if (ttls$border_flag | rs$page_template$titles$border_flag |  
          rs$page_template$title_hdr$border_flag)
        tpt <- ""
    }
    
    # Prevent infection of widths on LibreOffice.
    bpt <- "{\\pard\\fs1\\sl0\\par}"
    if (any(txt$borders %in% c("all", "top", "outside"))) {
      if (!is.null(ftnts)) {
        if (ftnts$border_flag)
          bpt <- ""
      }
      
      if (!is.null(rs$page_template$footnotes)) {
        if (rs$page_template$footnotes$border_flag)
          bpt <- ""
      }
    }
    
    # Combine titles, blanks, body, and footnotes
    rws <- c(a, cp, ttls$rtf, ttl_hdr$rtf, tpt, cp, rwhd, s, rwft, bpt)
    
    ret[[length(ret) + 1]] <- c(rws, cp, ftnts$rtf)
    cnt[[length(cnt) + 1]] <- sum(cnts, ftnts$lines)
    
  }
  
  res <- list(rtf = ret, lines = cnt)

  return(res)
  
}


#' @description lines is the number of lines per page or cell before breaking.
#' Width is the width of the page or cell.
#' @noRd
split_text <- function(txt, lines, width, font, 
                           font_size, units, offset = 0) {
  
  pgs <- c()
  lnlngth <- 0
  ln <- c()
  cnt <- 0
  lns <- c()
  cnts <- c()
  wdths <- c()
  wdth <- c()
  
  # Split text into words
  wrds <- strsplit(txt, " ", fixed = TRUE)[[1]]
  
  # Set font
  f <- "mono"
  if (tolower(font) == "arial")
    f <- "sans"
  else if (tolower(font) == "times")
    f <- "serif"
  
  lngths <- c()
  
  
  lngths <- (get_text_width(wrds, units = units, font = font, font_size = font_size) + 
     get_text_width(" ", units = units, font = font, font_size = font_size)) * 1.03
  
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
        wdth[length(wdth) + 1] <- lnlngth - lngths[i] 
        ln <- wrds[i]
        lnlngth <- lngths[i]
      } else {
        
        # Assign current lines and counts
        pgs[[length(pgs) + 1]] <- lns
        cnts[[length(cnts) + 1]] <- length(lns)
        wdths[[length(wdths) + 1]] <- wdth
        wdth <- lnlngth  - lngths[i] 
        
        # Assign overflow to next page
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
    cnt <- cnt + 1
    if (cnt <= lines - offset) {
      lns <- append(lns, paste(ln, collapse = " "))
      wdth[length(wdth) + 1] <- lnlngth
      
    } else {
      pgs[[length(pgs) + 1]] <- lns
      cnts[[length(cnts) + 1]] <- length(lns)
      wdths[[length(wdths) + 1]] <- wdth
      
      lns <-  paste(ln, collapse = " ")
      wdth[length(wdth) + 1] <- lnlngth
    }
    pgs[[length(pgs) + 1]] <- lns
    cnts[[length(cnts) + 1]] <- length(lns)
    wdths[[length(wdths) + 1]] <- wdth
  }
  
  res <- list(text = pgs, 
              lines = cnts,
              widths = wdths)
  
  return(res)
  
}


# Write HTML Functions -------------------------------------------------------


#' @noRd
create_text_pages_html <- function(rs, cntnt, lpg_rows, content_blank_row) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  txt <- cntnt$object
  
  # Default to content width
  w <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(txt$width))
    w <- txt$width
  
  res <- get_text_body_html(rs, txt, w, rs$body_line_count, 
                           lpg_rows, content_blank_row, cntnt$align)
  
  
  return(res)
  
  
}

#' Create list of vectors of strings for each page 
#' @import stringi
#' @noRd
get_text_body_html <- function(rs, txt, width, line_count, lpg_rows, 
                              content_blank_row, talgn) {
  
  
  # Get content titles and footnotes
  ttls <- get_titles_html(txt$titles, width, rs, talgn) 
  ttl_hdr <- get_title_header_html(txt$title_hdr, width, rs, talgn)
  
  ftnts <- get_footnotes_html(txt$footnotes, width, rs, talgn, FALSE) 
  
  exclude_top <- NULL
  if (ttls$border_flag | ttl_hdr$border_flag)
    exclude_top <- "top"
  
  t <- sum(ttls$lines, ftnts$lines, ttl_hdr$lines)
  hgt <- line_count - t
  
  # Break text content into pages if necessary
  tpgs <- split_text(txt$text, hgt, width, rs$font, 
                         rs$font_size, rs$units, lpg_rows)
  
  # Capture rtf pages and line counts
  txtpgs <- tpgs$text
  lns <- tpgs$lines
  
  u <- rs$units
  if (u == "inches")
    u <- "in"
  
  # Calculate text width in twips
  w <- paste0("width:", round(width, 3), u, ";")
  
  
  # Get text alignment codes
  if (txt$align == "right") 
    algn <- "text-align:right;"
  else if (txt$align %in% c("center", "centre"))
    algn <- "text-align:center;"
  else 
    algn <- "text-align:left;"
  
  # Get cell border codes
  b <- get_cell_borders_html(1, 1, 1, 1, txt$borders, exclude = exclude_top)  
  
  # Prepare row header and footer
  rwhd <- paste0("<table ", 
                 " style=\"", w, algn, "\">\n<tr><td style=\"", b, "\">")
  rwft <- paste0("</td></tr>\n</table>\n")
  
  ret <- list()
  cnt <- c()
  
  # Gather rtf and line counts for each page
  for (i in seq_along(txtpgs)) {
    
    if (i == length(txtpgs))
      wrap_flag <- FALSE
    else 
      wrap_flag <- TRUE
    
    pg <- txtpgs[[i]]
    
    # Put line ending on all but last line
    if (length(pg) > 1) {
      s <- paste0(pg[seq(1, length(pg) - 1)], "<br>\n")
      s <- c(s, pg[length(pg)])
    } else
      s <- pg
    
    # Add blank above content if requested
    a <- NULL
    if (i == 1 & content_blank_row %in% c("both", "above"))
      a <- "<br>\n"
    
    
    # Sum up lines
    cnts <- sum(length(a),  ttls$lines, ttl_hdr$lines, lns[[i]])
    
    # Get footnotes
    if ("bottom" %in% get_outer_borders(txt$borders))
      ftnts <- get_page_footnotes_html(rs, txt, width, lpg_rows, cnts,
                                       wrap_flag, content_blank_row, talgn, TRUE)
    else 
      ftnts <- get_page_footnotes_html(rs, txt, width, lpg_rows, cnts,
                                    wrap_flag, content_blank_row, talgn, FALSE)
    
    
    # Combine titles, blanks, body, and footnotes
    rws <- c(a, ttls$html, ttl_hdr$html, 
             rwhd, s, rwft)
    
    ret[[length(ret) + 1]] <- c(rws, ftnts$html)
    cnt[[length(cnt) + 1]] <- sum(cnts, ftnts$lines)
    
  }
  
  res <- list(html = ret, lines = cnt)
  
  return(res)
  
}


# Write PDF Functions -------------------------------------------------------


#' @noRd
create_text_pages_pdf <- function(rs, cntnt, lpg_rows, content_blank_row) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  txt <- cntnt$object
  
  # Default to content width
  w <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(txt$width))
    w <- txt$width
  
  ys <- sum(rs$page_template$titles$points, rs$page_template$title_hdr$points,
            rs$page_template$page_header$points)

  
  res <- get_text_body_pdf(rs, txt, w, rs$body_line_count, 
                           lpg_rows, content_blank_row, 
                           cntnt$align, ystart = ys)
  
  
  return(res)
  
  
}

#' Create list of vectors of strings for each page 
#' @import stringi
#' @noRd
get_text_body_pdf <- function(rs, txt, width, line_count, lpg_rows, 
                              content_blank_row, talgn, ystart = 0) {
  
  lh <- rs$line_height
  bh <- rs$border_height
  bs <- rs$border_spacing
  conv <- rs$point_conversion
  
  # Get content titles and footnotes
  ttls <- get_titles_pdf(txt$titles, width, rs, talgn, 
                         ystart = ystart) 
  ttl_hdr <- get_title_header_pdf(txt$title_hdr, width, rs, talgn, 
                                  ystart = ystart)
  ftnts <- get_footnotes_pdf(txt$footnotes, width, rs, talgn) 
  
  # Nice that if titles are taller because of the font size,
  # this will account for it.
  p <- sum(ttls$points, ftnts$points, ttl_hdr$points)
  t <- ceiling(p/lh)

  hgt <- line_count - t
  
  # If last page already filled up, start a new page
  if (ceiling(lpg_rows) >= floor(hgt))
    lpg_rows <- 0
  
  if (content_blank_row %in% c("above", "all"))
    lpg_rows <- lpg_rows + 1
  
  # Break text content into pages if necessary
  tpgs <- split_text(txt$text, hgt, width, rs$font, 
                     rs$font_size, rs$units, lpg_rows)
  
  # Capture pages, line counts, and widths
  txtpgs <- tpgs$text
  lns <- tpgs$lines
  wdths <- tpgs$widths
  
  # Calculate text width in points
  w <- round(width * rs$point_conversion)
  
  # Get content alignment codes
  if (talgn == "right") {
    lb <- rs$content_size[["width"]] - width
    rb <- rs$content_size[["width"]]
  } else if (talgn %in% c("center", "centre")) {
    lb <- (rs$content_size[["width"]] - width) / 2
    rb <- width + lb
  } else {
    lb <- 0
    rb <- width
  }
  
  fp_ttls <- list(pdf = NULL, lines = 0)
  fp_ttl_hdr <- list(pdf = NULL, lines = 0)

  ret <- list()
  cnt <- c()
  pnt <- c()

  # Gather pdf and line counts for each page
  for (i in seq_along(txtpgs)) {
    
    cnts <- t
    pnts <- p
    rws <- list()
    
    if (i == length(txtpgs))
      wrap_flag <- FALSE
    else 
      wrap_flag <- TRUE
    
    pg <- txtpgs[[i]]
    pw <- wdths[[i]]
    
    if (lpg_rows > 0 & i == 1) {
      
      # Get starting y coordinate
      yline <- sum(ystart, (lpg_rows * lh))
      
      if (content_blank_row %in% c("above", "both")) {
        yline <- yline + lh
        cnts <- cnts + 1 
      }
      
      fp_ttls <- get_titles_pdf(txt$titles, width, rs, talgn, 
                             ystart = yline) 
      fp_ttl_hdr <- get_title_header_pdf(txt$title_hdr, width, rs, talgn, 
                                      ystart = yline)
      

      yline <- sum(yline, fp_ttls$points, fp_ttl_hdr$points)
      
      
    } else {
      
      fp_ttls <- list(lines = 0)
      fp_ttl_hdr <- list(lines = 0)
    
      # Get starting y coordinate
      yline <- sum(ystart, ttls$points, ttl_hdr$points)
    
    }
    
    
    if (ttls$border_flag | ttl_hdr$border_flag)
      ypos <- yline - lh + 1
    else 
      ypos <- yline - lh + bs
    
    for (ln in seq_along(pg)) {
      
      rws[[length(rws) + 1]] <- page_text(pg[ln], rs$font_size, 
                                          xpos = get_points(lb, 
                                                            rb,
                                                            pw[ln],  
                                                            units = rs$units,
                                                            align = txt$align),
                                          ypos = yline)
      yline <- yline + lh
      cnts <- cnts + 1
    }
    
    yend <- yline - lh + bs
    
    #yline <- ceiling(ypos + (plt$height * rs$point_conversion)) 
    brdrs <- strip_borders(txt$borders)
    
    # Top border
    if (any(brdrs %in% c("all", "outside", "top"))) {
      
      rws[[length(rws) + 1]] <- page_hline(lb * conv, 
                                           ypos, 
                                           (rb - lb) * conv) 
      
    }
    
    border_flag <- FALSE
    # Bottom border
    if (any(brdrs %in% c("all", "outside", "bottom"))) {
      
      rws[[length(rws) + 1]] <- page_hline(lb * conv, 
                                           yend , 
                                           (rb - lb) * conv) 
      border_flag <- TRUE
      
    }
    
    # Left border
    if (any(brdrs %in% c("all", "outside", "left"))) {
      
      
      rws[[length(rws) + 1]] <- page_vline(lb * conv, 
                                           ypos, 
                                           yend - ypos) 
      
    }
    
    # Right border
    if (any(brdrs %in% c("all", "outside", "right"))) {
      
      
      rws[[length(rws) + 1]] <- page_vline(rb * conv, 
                                           ypos, 
                                           yend - ypos) 
      
    }
    
    # rs, spec, spec_width, lpg_rows, ystart,
    # wrap_flag, content_blank_row, talgn,
    # brdr_flag = FALSE
    
    if (border_flag)
      ys <- yline + bs
    else 
      ys <- yline + 1
    
    # Get footnotes
    ftnts <- get_page_footnotes_pdf(rs, txt, width, lpg_rows, ys,
                                    wrap_flag, content_blank_row, talgn,
                                    brdr_flag = border_flag)
    
    # Add blank above content if requested
    if (i ==length(txtpgs) & content_blank_row %in% c("both", "below")) {
      cnts <- cnts + 1
      yline <- yline + lh

    }

    pnts <- pnts + (cnts * lh)
    
    
    # Add remaining page content.
    if (fp_ttls$lines > 0) {
      rws<- append(rws, fp_ttls$pdf)
    } else {
      if (ttls$lines > 0) {
        rws<- append(rws, ttls$pdf)
      } 
    }
    if (fp_ttl_hdr$lines > 0) {
      rws <- append(rws, fp_ttl_hdr$pdf)
    } else {
      if (ttl_hdr$lines > 0) {
        rws <- append(rws, ttl_hdr$pdf)
      }
    }


    if (ftnts$lines > 0) {
        rws <- append(rws, ftnts$pdf)
    }
    
    ret[[length(ret) + 1]] <- rws
    cnt[length(cnt) + 1]  <- cnts
    pnt[length(pnt) + 1]  <- pnts
  }
  
  res <- list(pdf = ret, 
              lines = cnt,
              points = pnt)
  
  return(res)
  
}




# Write DOCX Functions -------------------------------------------------------


#' @noRd
create_text_pages_docx <- function(rs, cntnt, lpg_rows, content_blank_row) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  txt <- cntnt$object
  
  # Default to content width
  w <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(txt$width))
    w <- txt$width
  
  res <- get_text_body_docx(rs, txt, w, rs$body_line_count, 
                            lpg_rows, content_blank_row, cntnt$align)
  
  
  return(res)
  
  
}

#' Create list of vectors of strings for each page 
#' @import stringi
#' @noRd
get_text_body_docx <- function(rs, txt, width, line_count, lpg_rows, 
                               content_blank_row, talgn) {
  
  conv <- rs$twip_conversion
  
  # Get content titles and footnotes
  ttls <- get_titles_docx(txt$titles, width, rs, talgn) 
  ttl_hdr <- get_title_header_docx(txt$title_hdr, width, rs, talgn)
  
  ftnts <- get_footnotes_docx(txt$footnotes, width, rs, talgn, FALSE) 
  
  exclude_top <- NULL
  if (ttls$border_flag | ttl_hdr$border_flag)
    exclude_top <- "top"
  
  t <- sum(ttls$lines, ftnts$lines, ttl_hdr$lines)
  hgt <- line_count - t
  
  # Break text content into pages if necessary
  tpgs <- split_text(txt$text, hgt, width, rs$font, 
                     rs$font_size, rs$units, lpg_rows)
  
  # Capture rtf pages and line counts
  txtpgs <- tpgs$text
  lns <- tpgs$lines
  
  u <- rs$units
  if (u == "inches")
    u <- "in"
  
  # Get text alignment codes
  if (txt$align == "right")
    algn <- "right"
  else if (txt$align %in% c("center", "centre"))
    algn <- "center"
  else
    algn <- "left"
  
  
  tw <- round(width * conv)
  
  # Get indents for alignment
  ta <- get_indent_docx(talgn, rs$line_size, tw, 
                        rs$base_indent, txt$borders, conv)
   
  # Get cell border codes
  b <- get_table_borders_docx(txt$borders, exclude = exclude_top)  
   
  # Prepare row header and footer
  rwhd <- paste0("<w:tbl>\n", '<w:tblPr>',
                 '<w:tblW w:w="', tw, '"/>', 
                 ta,
                 b,
                 "</w:tblPr>",
                 "<w:tr><w:tc>",
                 '<w:tcPr><w:tcW w:w="', tw, '"/></w:tcPr>',
                 '<w:p>',
                 '<w:pPr><w:jc w:val="', algn, '"/></w:pPr>')
  rwft <- paste0("</w:p></w:tc></w:tr>\n</w:tbl>\n", rs$table_break)
  
  ret <- list()
  cnt <- c()
  
  # Gather docx and line counts for each page
  for (i in seq_along(txtpgs)) {
    
    if (i == length(txtpgs))
      wrap_flag <- FALSE
    else 
      wrap_flag <- TRUE
    
    pg <- txtpgs[[i]]
    
    # Put line ending on all but last line
    # if (length(pg) > 1) {
    #   s <- paste0(pg[seq(1, length(pg) - 1)], "<br>\n")
    #   s <- c(s, pg[length(pg)])
    # } else
      s <- run(pg)
    
    # Add blank above content if requested
    a <- NULL
    if (i == 1 & content_blank_row %in% c("both", "above"))
      a <- "<w:p/>\n"
    
    
    # Sum up lines
    cnts <- sum(length(a),  ttls$lines, ttl_hdr$lines, lns[[i]])
    
    # Get footnotes
    if ("bottom" %in% get_outer_borders(txt$borders))
      ftnts <- get_page_footnotes_docx(rs, txt, width, lpg_rows, cnts,
                                       wrap_flag, content_blank_row, talgn, TRUE)
    else 
      ftnts <- get_page_footnotes_docx(rs, txt, width, lpg_rows, cnts,
                                       wrap_flag, content_blank_row, talgn, FALSE)
    
    
    # Combine titles, blanks, body, and footnotes
    rws <- c(a, ttls$docx, ttl_hdr$docx, 
             rwhd, s, rwft)
    
    ret[[length(ret) + 1]] <- c(rws, ftnts$docx)
    cnt[[length(cnt) + 1]] <- sum(cnts, ftnts$lines)
    
  }
  
  res <- list(docx = ret, lines = cnt)
  
  return(res)
  
}




