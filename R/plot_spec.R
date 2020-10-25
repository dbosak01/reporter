
# Create Plot Constructor     ---------------------------------------------


#' @title Create plot content
#' @description Function to create a plot specification that can be 
#' added as content to a report. The \code{create_plot} function can 
#' be used to include charts, graphs, and figures on a statistical report.  
#' The function only supports plot objects returned by 
#' \code{\link[ggplot2]{ggplot}}.  It does not support the Base R 
#' \code{\link[base]{plot}} function. 
#' @details 
#' To add a plot to a report, use the \code{create_plot} function.  The 
#' function allows you to set a width and height for the plot.  The
#' function will preserve any other geometries you apply to the plot.  See
#' the \code{\link{add_content}} function to control page breaking and 
#' blanks spaces above or below the plot.    
#' 
#' The plot specification also accepts titles and footnotes.  See the 
#' \code{\link{titles}} and \code{\link{footnotes}} functions for further 
#' details.
#' 
#' @param x The plot to create.  Specifically, this parameter should be 
#' set to an object returned from a call to \code{\link[ggplot2]{ggplot}}.
#' @param height The height of the plot in the specified units of measure. 
#' @param width The width of the plot in the specified units of measure. 
#' @return The plot specification.
#' @family plot
#' @seealso 
#' \code{\link{titles}} to add a title block to the plot,  
#' \code{\link{footnotes}} to add footnotes, and \code{\link{add_content}} 
#' to add the plot object to a report.
#' @examples
#' library(rptr)
#' library(ggplot2)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.pdf")
#' 
#' # Create ggplot
#' p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
#'
#' # Create plot object
#' plt <- create_plot(p, height = 4, width = 8)
#'
#' rpt <- create_report(tmp, output_type = "PDF") %>%
#'   page_header("Client", "Study: XYZ") %>% 
#'   titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
#'   set_margins(top = 1, bottom = 1) %>%
#'   add_content(plt) %>%
#'   footnotes("* Motor Trend, 1974") %>% 
#'   page_footer("Time", "Confidential", "Page [pg] of [tpg]")
#' 
#' # Write out report
#' write_report(rpt)
#'
#' # Uncomment to view PDF file
#' # shell.exec(tmp)
#' @export
create_plot <- function(x, height = NULL, width = NULL) {
  
  
  if (!"ggplot" %in% class(x))
    stop("plot object must be of class 'ggplot'")
  
  ret <- structure(list(), class = c("plot_spec", "list"))
  
  ret$plot <- x
  ret$height <- height 
  ret$width <- width

  
  return(ret)
}



# Utilities ---------------------------------------------------------------

#' @title Prints the plot spec
#' @description A function to print the plot spec.
#' The \strong{print} function will print the plot spec in summary 
#' form.  To view all parameters, set the \code{verbose} parameter to TRUE.
#' @param x The plot spec.
#' @param ... Additional parameters to pass to the underlying print function.
#' @param verbose Whether to print in verbose form.  Default is FALSE.
#' @seealso 
#' \code{\link{create_plot}} function to create a plot specification.
#' @return The plot spec, invisibly.
#' @family plot
#' @examples 
#' txt <- create_text("Lorem ipsum dolor sit amet, consectetur...")
#' print(txt)
#'
#' # A text specification:
#' # - text: data.frame 'mtcars' 32 rows 11 cols
#' @import crayon
#' @export
print.plot_spec <- function(x, ..., verbose = FALSE){
  
  
  if (verbose == TRUE) {
    
    # If verbose mode is indicated, print values as a list
    for (nm in names(x)) {
      
      cat("$", nm, "\n", sep = "")
      if (nm == "plot") {
        
        print(unclass(x[[nm]]), ...)
      }
      else  {
        
        print(x[[nm]], ...)
      }
      cat("\n")
    }
    
    
  } else {
    
    
    grey60 <- make_style(grey60 = "#999999")
    
    
    cat(grey60("# A plot specification: \n"))
    
    if (!is.null(x$plot)) {
      
      dat <- x$plot[["data"]]

      cat("- data: ")
      cat(paste0(nrow(dat), " rows, ", ncol(dat), " cols\n"))

      cat(paste0("- layers: ", length(x$plot[["layers"]]), "\n"))
      
    } 
    
      
    cat(paste0("- height: ", x$height, "\n"))
    cat(paste0("- width: ", x$width, "\n"))
    
    if (!is.null(x$page_by)) {
      cat(paste0("- page by: ", x$page_by$var, "\n"))
      
    }
    
    print_title_header(x$title_hdr)
    
    print_titles(x$titles)
    
    print_footnotes(x$footnotes)
    
  }
  
  invisible(x)
}


# Write Functions -------------------------------------------------------

#' @description A function to output strings for plot content
#' @details Basic logic is to write the plot as a png file to a temporary
#' location, then put a token in the text that references the temp location.
#' Later code will pick up the image and insert it into the report.  Blank
#' lines are generated to fill up the page based on a calculation using
#' the plot height and width.
#' @param rs The Report Spec
#' @param cntnt The text content to output
#' @param lpg_rows Last page rows.
#' @noRd
create_plot_pages_text <- function(rs, cntnt, lpg_rows, tmp_dir) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  # Get plot spec 
  plt <- cntnt$object
  
  
  # Get data in case we need it for page by
  raw <- plt$plot$data
  
  # Determine if there is a page by
  pgby <- NULL
  if (!is.null(rs$page_by))
    pgby <- rs$page_by
  if (!is.null(plt$page_by))
    pgby <- plt$page_by
  
  if (!is.null(pgby)) {
    dat_lst <- split(raw, raw[[pgby$var]])
  } else {
    dat_lst <- list(raw) 
  }
  
  u <- ifelse(rs$units == "inches", "in", rs$units)
  p <- plt$plot
  ret <- list()
  cntr <- 1
  
  for (dat in dat_lst) {
  
    tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".png")
    
    p$data <- dat
    
    pgval <- NULL
    if (!is.null(pgby)) {
      # print(pgby$var)
      # print(dat)
      pgval <- dat[1, c(pgby$var)]
    }
    
    # Save plot to temp file
    ggplot2::ggsave(tmp_nm, p, width =  plt$width, height = plt$height, 
           dpi = 300, units = u)
    
    # Get rtf page bodies
    # Can return multiple pages if it breaks across pages
    # Not sure what that means for a plot, but that is the logic
    pgs <- get_plot_body(plt, tmp_nm, cntnt$align, rs,
                         lpg_rows, cntnt$blank_row, pgby, pgval)
    
    # Within a content creation function, assumed that it will take 
    # care of filling out blanks for each page.  Only the last page
    # can have empty rows.
    for (pg in pgs) {
      rem <- rs$body_line_count - length(pg)
      if (rem > 0 & cntr < length(dat_lst))
        blnks <- rep("", rem)
      else 
        blnks <- c()
      
      # Combine all pages with all previous pages
      ret <- c(ret, list(c(pg, blnks)))
    }
    
    cntr <- cntr + 1
  
  }
  
  return(ret)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_plot_body <- function(plt, plot_path, align, rs,
                          lpg_rows, content_blank_row, pgby, pgval) {
  
  # Get titles and footnotes
  w <- ceiling(plt$width / rs$char_width)
  ttls <- get_titles(plt$titles, w) 
  ftnts <- get_footnotes(plt$footnotes, w) 
  pgbys <- get_page_by(pgby, w, pgval)
  
  pltpth <- gsub("\\", "/", plot_path, fixed = TRUE)
  
  
  s <- c(paste0("```", pltpth, "|", plt$height, 
                  "|", plt$width, "|", align, "```"))
  
  h <- ceiling(plt$height / rs$line_height) + 1  # adjustment needed?

  fill <- rep("```fill```", h) 
  s <- c(s, fill)
  
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
    a <- ""
  
  
  # Add blank below content if requested
  b <- NULL
  if (content_blank_row %in% c("both", "below"))
    b <- ""
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls, pgbys, s, ftnts, b)
  
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
    if (length(tmp) < (rs$body_line_count - offset)) {
      
      # Append to existing page
      tmp[length(tmp) + 1] <- rws[i]
      
    } else {
      
      # Start a new page
      ret[[length(ret) + 1]] <- format(tmp, width = rs$line_size, 
                                       justify = get_justify(align))
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
      ret[[length(ret) + 1]] <- format(tmp, width = rs$line_width, 
                                       justify = get_justify(align))
    }
    
  }
  
  return(ret)
  
}
