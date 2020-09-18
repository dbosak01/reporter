
# Create Plot Constructor     ---------------------------------------------


#' @title Create plot content
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
#' @param x The plot to create.
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
#' @import ggplot2
#' @export
create_plot <- function(x, height = NULL, width = NULL) {
  
  
  ret <- structure(list(), class = c("plot_spec", "list"))
  
  ret$plot <- x
  ret$height <- height 
  ret$width <- width

  
  return(ret)
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
create_plot_pages_text <- function(rs, cntnt, lpg_rows, tmp_dir) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  plt <- cntnt$object
  
  tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".png")
  
  u <- ifelse(rs$units == "inches", "in", rs$units)
  
  # print("Temp plot name:")
  # print(tmp_nm)
  
  ggsave(tmp_nm, plt$plot, width =  plt$width, height = plt$height, 
         dpi = 300, units = u)
  

  ret <- get_plot_body(plt, tmp_nm, cntnt$align, rs,
                       lpg_rows, cntnt$blank_row)
  
  
  return(ret)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_plot_body <- function(plt, plot_path, align, rs,
                          lpg_rows, content_blank_row) {
  
  # Get titles and footnotes
  w <- ceiling(plt$width / rs$char_width)
  ttls <- get_titles(plt$titles, w) 
  ftnts <- get_footnotes(plt$footnotes, w) 
  
  
  s <- c(paste0("{{", plot_path, "|", plt$height, 
                  "|", plt$width, "|", align, "}}"))
  
  h <- ceiling(plt$height / rs$line_height) + 1  # adjustment needed?

  fill <- rep("{{fill}}", h) 
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