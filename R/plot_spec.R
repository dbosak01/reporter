
# Create Plot Constructor     ---------------------------------------------


#' @title Create plot content
#' @description Function to create a plot specification that can be 
#' added as content to a report. The \code{create_plot} function can 
#' be used to include charts, graphs, and figures on a statistical report.  
#' The function supports plot objects returned by 
#' \code{\link[ggplot2]{ggplot}} or \code{\link[survminer]{ggsurvplot}}.  
#' It does not support the Base R 
#' \code{plot} function. 
#' @details 
#' To add a plot to a report, use the \code{create_plot} function.  The 
#' function allows you to set a width and height for the plot.  The
#' function will preserve any other geometries you apply to the plot.  See
#' the \code{\link{add_content}} function to control page breaking and 
#' blanks spaces above or below the plot.    
#' 
#' A plot specification accepts a \code{\link{page_by}} function.  If a page by
#' is applied to the plot, the plot data will be subset by the page by 
#' variable, and re-run for each subset.
#' 
#' The plot specification also accepts titles and footnotes.  See the 
#' \code{\link{titles}} and \code{\link{footnotes}} functions for further 
#' details.
#' 
#' As of \strong{reporter} version 1.2.9, the \code{create_plot} function 
#' also accepts a path to a JPEG stored on the file system instead of a 
#' plot object.  This
#' functionality was added to allow the user to create figures from other
#' plotting packages.  If you pass an image path, the image will be inserted
#' into the report at the location specified.  
#' 
#' @param x The plot to create.  Specifically, this parameter should be 
#' set to an object returned from a call to \code{\link[ggplot2]{ggplot}}
#' or \code{\link[survminer]{ggsurvplot}}.  This parameter also accepts 
#' a path to a JPEG file.  If a path is specified, the image will be appended
#' to the report at the point the content object is added.
#' @param height The height of the plot in the specified units of measure. 
#' @param width The width of the plot in the specified units of measure. 
#' @param borders Whether and where to place a border. Valid values are 'top',
#' 'bottom', 'left', 'right', 'all', 'none', and 'outside'.  
#' Default is 'none'.  The 'left', 'right', and 'outside' 
#' border specifications only apply to RTF reports.
#' @return The plot specification.
#' @family plot
#' @seealso 
#' \code{\link{titles}} to add a title block to the plot,  
#' \code{\link{footnotes}} to add footnotes, and \code{\link{add_content}} 
#' to add the plot object to a report.
#' @examples
#' library(reporter)
#' library(ggplot2)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.rtf")
#' 
#' # Create ggplot
#' p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
#'
#' # Create plot object
#' plt <- create_plot(p, height = 4, width = 8)
#'
#' rpt <- create_report(tmp, output_type = "RTF") %>%
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
#' # Uncomment to view RTF file
#' # shell.exec(tmp)
#' @export
create_plot <- function(x, height, width, borders = "none") {
  
  
  if (!any(class(x) %in% c("gg", "ggplot", "ggcoxzph", "ggsurv", "character")))
    stop("plot object must be of type 'ggplot', 'ggsurv', or a JPG file path.")
  
  
  if (!all(borders %in% c("top", "bottom", "left", "right", 
                          "all", "none", "outside")))
    stop(paste("Borders parameter invalid.  Valid values are", 
               "'top', 'bottom', 'left', 'right', 'all', ",
               "'none', or 'outside'."))
  
  ret <- structure(list(), class = c("plot_spec", "list"))
  
  ret$plot <- x
  ret$height <- height 
  ret$width <- width
  ret$borders <- borders

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


# Write Text Functions -------------------------------------------------------

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
  
  p <- plt$plot
  
  
  if (any(class(p) %in% c("character"))) {
    
    tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
    
    file.copy(p, tmp_nm, overwrite = TRUE)
    
    ret <- list()
    pgs <- get_plot_body(plt, tmp_nm, cntnt$align, rs,
                         lpg_rows, cntnt$blank_row, NULL, NULL, 
                         FALSE)
    
    for (pg in pgs) {
      
      ret <- c(ret, list(pg))
    }
  
    
  } else {
  
    # Get data in case we need it for page by
    raw <- p$data
    
    # Determine if there is a page by
    pgby <- NULL
    if (!is.null(rs$page_by))
      pgby <- rs$page_by
    if (!is.null(plt$page_by))
      pgby <- plt$page_by
    
    if (!is.null(pgby)) {
      if (!pgby$var %in% names(raw))
        stop("Page by variable not found in plot data.")
      
      dat_lst <- split(raw, raw[[pgby$var]])
    } else {
      dat_lst <- list(raw) 
    }
    
    u <- ifelse(rs$units == "inches", "in", rs$units)
  
    ret <- list()
    cntr <- 1
    
    for (dat in dat_lst) {
    
      tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
      
      p$data <- dat
      
      pgval <- NULL
      if (!is.null(pgby)) {
        # print(pgby$var)
        # print(dat)
        pgval <- dat[1, c(pgby$var)]
      }
      
      # Save plot to temp file
      if (any(class(p) %in% c("ggcoxzph", "ggsurv"))) {
  
        # Deal with survival plots
        ggplot2::ggsave(tmp_nm, gridExtra::arrangeGrob(grobs = p), 
                        width =  plt$width, height = plt$height, 
                        dpi = 300, units = u )
        
        
      } else {
  
        # Any other type of plots
        ggplot2::ggsave(tmp_nm, p, width =  plt$width, height = plt$height, 
               dpi = 300, units = u)
      }
      
      # Get rtf page bodies
      # Can return multiple pages if it breaks across pages
      # Not sure what that means for a plot, but that is the logic
      pgs <- get_plot_body(plt, tmp_nm, cntnt$align, rs,
                           lpg_rows, cntnt$blank_row, pgby, pgval, 
                           cntr < length(dat_lst))
      
      # Within a content creation function, assumed that it will take 
      # care of filling out blanks for each page.  Only the last page
      # can have empty rows.
      for (pg in pgs) {
  
        ret <- c(ret, list(pg))
      }
      
      cntr <- cntr + 1
    
    }
  
  }
  
  return(ret)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_plot_body <- function(plt, plot_path, align, rs,
                          lpg_rows, content_blank_row, pgby, pgval, wrap_flag) {
  
  # Get titles and footnotes
  w <- ceiling(plt$width / rs$char_width)
  ttls <- get_titles(plt$titles, w, rs$line_size, rs$uchar, rs$char_width) 
  ttl_hdr <- get_title_header(plt$title_hdr, w, rs$line_size, rs$uchar, 
                              rs$char_width)
  
  pgbys <- get_page_by(pgby, w, pgval)
  
  pltpth <- gsub("\\", "/", plot_path, fixed = TRUE)
  
  
  s <- c(paste0("```", pltpth, "|", plt$height, 
                  "|", plt$width, "|", align, "```"))
  
  h <- ceiling(plt$height / rs$row_height) + 1  # adjustment needed? Appears so.
  w <- ceiling(plt$width / rs$char_width) 

  fill <- rep("```fill```", h) 
  s <- c(s, fill)
  
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
    a <- ""
  
  # Add top border if requested
  tbrdr <- NULL
  if (any(plt$borders %in% c("top", "all")))
    tbrdr <-  paste0(rep(rs$uchar, w), collapse = "")
  
  # Add bottom border if requested
  bbrdr <- NULL
  if (any(plt$borders %in% c("bottom", "all", "outside")))
    bbrdr <-  paste0(rep(rs$uchar, w), collapse = "")
  
  # Add blank below content if requested
  b <- NULL
  if (content_blank_row %in% c("both", "below", "outside"))
    b <- ""
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls, ttl_hdr, pgbys, tbrdr, s, bbrdr)
  
  
  ftnts <- get_page_footnotes_text(rs, plt, w, lpg_rows, length(rws),
                                   wrap_flag, content_blank_row)
  
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
    if (length(tmp) < (rs$body_line_count - offset)) {
      
      # Append to existing page
      tmp[length(tmp) + 1] <- rws[i]
      
    } else {
      
      # Start a new page
      ret[[length(ret) + 1]] <- pad_any(tmp, rs$line_size, 
                                        get_justify(align))
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
      ret[[length(ret) + 1]] <- pad_any(tmp,  rs$line_size, 
                                        get_justify(align))
    }
    
  }
  
  return(ret)
  
}



# Write RTF Functions -----------------------------------------------------


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
create_plot_pages_rtf <- function(rs, cntnt, lpg_rows, tmp_dir) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  pgs <- list()
  cnts <- c()
  
  # Get plot spec 
  plt <- cntnt$object
  
  
  p <- plt$plot
  
  
  if (any(class(p) %in% c("character"))) {
    
    tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
    
    file.copy(p, tmp_nm, overwrite = TRUE)
    
    # Get rtf page bodies
    res <- get_plot_body_rtf(plt, tmp_nm, cntnt$align, rs,
                             lpg_rows, cntnt$blank_row, NULL, NULL, 
                             FALSE)
    
    pgs[[length(pgs) + 1]] <- res$rtf
    cnts[[length(cnts) + 1]] <- res$lines
  
    
    
  } else {
    
    
    # Get data in case we need it for page by
    raw <- plt$plot$data
    
    # Determine if there is a page by
    pgby <- NULL
    if (!is.null(rs$page_by))
      pgby <- rs$page_by
    if (!is.null(plt$page_by))
      pgby <- plt$page_by
    
    if (!is.null(pgby)) {
      if (!pgby$var %in% names(raw))
        stop("Page by variable not found in plot data.")
      
      dat_lst <- split(raw, raw[[pgby$var]])
    } else {
      dat_lst <- list(raw) 
    }
    
    u <- ifelse(rs$units == "inches", "in", rs$units)
    p <- plt$plot
    ret <- list()
    cntr <- 1

    
    for (dat in dat_lst) {
      
      tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
      
      p$data <- dat
      
      pgval <- NULL
      if (!is.null(pgby)) {
        # print(pgby$var)
        # print(dat)
        pgval <- dat[1, c(pgby$var)]
      }
      
      # Save plot to temp file
      if (any(class(p) %in% c("ggcoxzph", "ggsurv"))) {
        
        # Deal with survival plots
        ggplot2::ggsave(tmp_nm, gridExtra::arrangeGrob(grobs = p), 
                        width =  plt$width, height = plt$height, 
                        dpi = 300, units = u )
        
        
      } else if (any(class(p) %in% c("character"))) {
        
        
        file.copy(p, tmp_nm, overwrite = TRUE)
        
        
      } else {
        
        # Any other type of plots
        ggplot2::ggsave(tmp_nm, p, width =  plt$width, height = plt$height, 
                        dpi = 300, units = u)
      }
      
      # Get rtf page bodies
      res <- get_plot_body_rtf(plt, tmp_nm, cntnt$align, rs,
                           lpg_rows, cntnt$blank_row, pgby, pgval, 
                           cntr < length(dat_lst))
      
      pgs[[length(pgs) + 1]] <- res$rtf
      cnts[[length(cnts) + 1]] <- res$lines
      
      cntr <- cntr + 1
      
    }
  
  }
  
  ret <- list(rtf = pgs,
              lines = cnts)
  
  return(ret)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_plot_body_rtf <- function(plt, plot_path, talign, rs,
                          lpg_rows, content_blank_row, pgby, pgval, wrap_flag) {
  
  # Default to content width
  wth <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(plt$width))
    wth <- plt$width
  


  # Get titles and footnotes
  ttls <- get_titles_rtf(plt$titles, wth, rs, talign) 
  ttl_hdr <- get_title_header_rtf(plt$title_hdr, wth, rs, talign)
  pgbys <- get_page_by_rtf(pgby, wth, pgval, rs, talign)
  
  # Get image RTF codes
  img <- get_image_rtf(plot_path, plt$width, plt$height, rs$units)
  
  # Assign table alignment codes
  if (talign == "left") {
    talgn <- "\\trql" 
  } else if (talign == "right") {
    talgn <- "\\trqr"
  } else  {
    talgn <- "\\trqc"
  }
  
  algn <- "\\qc" 

  # Convert width to twips
  w <- round(wth * rs$twip_conversion)
  
  # Get border codes
  b <- get_cell_borders(1, 1, 1, 1, plt$borders)
  
  # Concat all header codes
  hd <- paste0("\\sl0\\trowd\\trgaph0", talgn, b, "\\cellx", w, algn, " \n")
  
  ft <- paste0("\\cell\\row\n\\ql", rs$font_rtf, rs$spacing_multiplier)
  
  
  
  # Concat RTF codes for image
  img <- paste0(hd, img, ft)
  imght <- round((plt$height * rs$twip_conversion) / rs$line_height)
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
    a <- "\\par"
  
  

  
  # Get sum of all items to this point
  lns <- sum(length(a), ttls$lines, ttl_hdr$lines, pgbys$lines, imght)
  
  # Get footnotes, filler, and content blank line
  ftnts <- get_page_footnotes_rtf(rs, plt, wth, lpg_rows, lns,
                                   wrap_flag, content_blank_row, talign)
  
  
  # On LibreOffice, have to protect the table from the title width or
  # the table row will inherit the title row width. Terrible problem.
  tpt <- "{\\pard\\fs1\\sl0\\par}"
  if (any(plt$borders %in% c("all", "top", "outside"))) {
    if (ttls$border_flag | rs$page_template$titles$border_flag |  
        rs$page_template$title_hdr$border_flag)
      tpt <- ""
  }
  
  # Prevent infection of widths on LibreOffice.
  bpt <- "{\\pard\\fs1\\sl0\\par}"
  if (any(plt$borders %in% c("all", "top", "outside"))) {
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
  rws <- c(a, ttls$rtf, ttl_hdr$rtf, pgbys$rtf, tpt, img, bpt)
  
  # Combine everything
  rws <- c(rws, ftnts$rtf)
  lns <- sum(lns, ftnts$lines)
  

  # Page list
  ret <- list(rtf = rws,
              lines = lns)  
  
  
  return(ret)
  
}


# Write HTML Functions -----------------------------------------------------


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
create_plot_pages_html <- function(rs, cntnt, lpg_rows, tmp_dir) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  pgs <- list()
  cnts <- c()
  
  # Get plot spec 
  plt <- cntnt$object
  
  p <- plt$plot
  
  
  if (any(class(p) %in% c("character"))) {
    
    tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
    
    file.copy(p, tmp_nm, overwrite = TRUE)

    # Get rtf page bodies
    res <- get_plot_body_html(plt, tmp_nm, cntnt$align, rs,
                              lpg_rows, cntnt$blank_row, NULL, NULL, 
                              FALSE)
    
    pgs[[length(pgs) + 1]] <- res$html
    cnts[[length(cnts) + 1]] <- res$lines
    
    
  } else {
    

    # Get data in case we need it for page by
    raw <- plt$plot$data
    
    # Determine if there is a page by
    pgby <- NULL
    if (!is.null(rs$page_by))
      pgby <- rs$page_by
    if (!is.null(plt$page_by))
      pgby <- plt$page_by
    
    if (!is.null(pgby)) {
      if (!pgby$var %in% names(raw))
        stop("Page by variable not found in plot data.")
      
      dat_lst <- split(raw, raw[[pgby$var]])
    } else {
      dat_lst <- list(raw) 
    }
    
    u <- ifelse(rs$units == "inches", "in", rs$units)
    p <- plt$plot
    ret <- list()
    cntr <- 1

    
    for (dat in dat_lst) {
      
      tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
      
      p$data <- dat
      
      pgval <- NULL
      if (!is.null(pgby)) {
        # print(pgby$var)
        # print(dat)
        pgval <- dat[1, c(pgby$var)]
      }
      
      # Save plot to temp file
      if (any(class(p) %in% c("ggcoxzph", "ggsurv"))) {
        
        # Deal with survival plots
        ggplot2::ggsave(tmp_nm, gridExtra::arrangeGrob(grobs = p), 
                        width =  plt$width, height = plt$height, 
                        dpi = 300, units = u )
        
        
      } else {
        
        # Any other type of plots
        ggplot2::ggsave(tmp_nm, p, width =  plt$width, height = plt$height, 
                        dpi = 300, units = u)
      }
      
      # Get rtf page bodies
      res <- get_plot_body_html(plt, tmp_nm, cntnt$align, rs,
                               lpg_rows, cntnt$blank_row, pgby, pgval, 
                               cntr < length(dat_lst))
      
      pgs[[length(pgs) + 1]] <- res$html
      cnts[[length(cnts) + 1]] <- res$lines
      
      cntr <- cntr + 1
      
    }
  }
  
  ret <- list(html = pgs,
              lines = cnts)
  
  return(ret)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_plot_body_html <- function(plt, plot_path, talign, rs,
                              lpg_rows, content_blank_row, pgby, pgval, wrap_flag) {
  
  # Default to content width
  wth <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(plt$width))
    wth <- plt$width
  

  # Get titles and footnotes
  ttls <- get_titles_html(plt$titles, wth, rs, talign) 
  ttl_hdr <- get_title_header_html(plt$title_hdr, wth, rs, talign)
  
  exclude_top <- NULL
  if (ttls$border_flag == TRUE | ttl_hdr$border_flag == TRUE) {
    exclude_top <- "top"
  
    pgbys <- get_page_by_html(pgby, wth, pgval, rs, talign, TRUE)
  } else {
    
    pgbys <- get_page_by_html(pgby, wth, pgval, rs, talign, FALSE)
  }
  
  if (is.null(exclude_top)) {
    if (pgbys$border_flag)
      exclude_top <- "top"
    
  }
  
  # Get image RTF codes
  img <- get_image_html(plot_path, rs$modified_path, plt, rs$units)

  
  # algn <- "\\qc" 
  u <- rs$units
  if (u == "inches")
    u <- "in"
  
  # Convert width to twips
  w <- paste0("width:", round(wth, 3), u, ";")
  
  # Get border codes
  b <- get_cell_borders_html(1, 1, 1, 1, plt$borders, exclude = exclude_top)
  
  # Concat all header codes
  hd <- paste0("<table style =\"", w, "\">\n", 
               "<tr><td style=\"", b, "\">\n")
  
  ft <- "</td></tr></table>\n"

  
  # Concat RTF codes for image
  img <- paste0(hd, img, ft)
  
  # ** Do something with this **
  imght <- floor(plt$height / rs$line_height)
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
    a <- "<br>"
  
  
  # Get sum of all items to this point
  lns <- sum(length(a), ttls$lines, ttl_hdr$lines, pgbys$lines, imght)
  
  if ("bottom" %in% get_outer_borders(plt$borders))
    extp <- TRUE
  else 
    extp <- FALSE 
  
  # Get footnotes, filler, and content blank line
  ftnts <- get_page_footnotes_html(rs, plt, wth, lpg_rows, lns,
                                  wrap_flag, content_blank_row, talign, extp)
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls$html, ttl_hdr$html, pgbys$html, img)
  
  
  # Combine everything
  rws <- c(rws, ftnts$html)
  lns <- sum(lns, ftnts$lines)
  
  
  # Page list
  ret <- list(html = rws,
              lines = lns)  
  
  
  return(ret)
  
}




# Write PDF Functions -----------------------------------------------------


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
create_plot_pages_pdf <- function(rs, cntnt, lpg_rows, tmp_dir) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")
  
  pgs <- list()
  cnts <- c()
  pnts <- c()
  
  # Get plot spec 
  plt <- cntnt$object
  p <- plt$plot
  
  if (any(class(p) %in% c("character"))) {
    
    tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
    
    file.copy(p, tmp_nm, overwrite = TRUE)
    
    ys <- sum(rs$page_template$titles$points, rs$page_template$title_hdr$points,
              rs$page_template$page_header$points, (lpg_rows * rs$line_height))
    
    # Get pdf page bodies
    res <- get_plot_body_pdf(plt, tmp_nm, cntnt$align, rs, row_offset,
                             cntnt$blank_row, NULL, NULL, 
                             FALSE, ystart = ys)
    
    pgs[[length(pgs) + 1]] <- res$pdf
    cnts[length(cnts) + 1] <- res$lines
    pnts[length(pnts) + 1] <- res$lines * rs$line_height
    
    
  } else {
  
    # Get data in case we need it for page by
    raw <- plt$plot$data
    
    # Determine if there is a page by
    pgby <- NULL
    if (!is.null(rs$page_by))
      pgby <- rs$page_by
    if (!is.null(plt$page_by))
      pgby <- plt$page_by
    
    if (!is.null(pgby)) {
      if (!pgby$var %in% names(raw))
        stop("Page by variable not found in plot data.")
      
      dat_lst <- split(raw, raw[[pgby$var]])
    } else {
      dat_lst <- list(raw) 
    }
    
    u <- ifelse(rs$units == "inches", "in", rs$units)
    p <- plt$plot
    ret <- list()
    cntr <- 1

    row_offset <- lpg_rows
    
    for (dat in dat_lst) {
      
      if (cntr > 1)
        row_offset <- 0
      
      tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
      
      p$data <- dat
      
      pgval <- NULL
      if (!is.null(pgby)) {
        # print(pgby$var)
        # print(dat)
        pgval <- dat[1, c(pgby$var)]
      }
      
      # Save plot to temp file
      if (any(class(p) %in% c("ggcoxzph", "ggsurv"))) {
        
        # Deal with survival plots
        ggplot2::ggsave(tmp_nm, gridExtra::arrangeGrob(grobs = p), 
                        width =  plt$width, height = plt$height, 
                        dpi = 300, units = u )
        
        
      } else {
        
        # Any other type of plots
        ggplot2::ggsave(tmp_nm, p, width =  plt$width, height = plt$height, 
                        dpi = 300, units = u)
      }
      ys <- sum(rs$page_template$titles$points, rs$page_template$title_hdr$points,
                rs$page_template$page_header$points, (row_offset * rs$line_height))
      
      # Get pdf page bodies
      res <- get_plot_body_pdf(plt, tmp_nm, cntnt$align, rs, row_offset,
                               cntnt$blank_row, pgby, pgval, 
                               cntr < length(dat_lst), ystart = ys)
      
      pgs[[length(pgs) + 1]] <- res$pdf
      cnts[length(cnts) + 1] <- res$lines
      pnts[length(pnts) + 1] <- res$lines * rs$line_height
      cntr <- cntr + 1
      
  }
  }
  
  ret <- list(pdf = pgs,
              lines = cnts, 
              points = pnts)
  
  return(ret)
}

#' Create list of page_text or page_image objects for a single page 
#' @noRd
get_plot_body_pdf <- function(plt, plot_path, talign, rs,
                              lpg_rows, content_blank_row, pgby, pgval, 
                              wrap_flag, ystart = 0) {
  cnt <- 0
  lh <- rs$line_height
  conv <- rs$point_conversion
  bh <- rs$border_height
  
  # Default to content width
  wth <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(plt$width))
    wth <- plt$width
  
  
  # Add blank above content if requested
  aln <- 0
  if (content_blank_row %in% c("both", "above")) {
    aln <- 1
    ystart <- ystart + lh
  }
  
  # Get titles and footnotes
  ttls <- get_titles_pdf(plt$titles, wth, rs, talign, ystart = ystart) 
  ttl_hdr <- get_title_header_pdf(plt$title_hdr, wth, rs, 
                                  talign, ystart = ystart)
  
  if (ttls$border_flag | ttl_hdr$border_flag) {
    pgbys <- get_page_by_pdf(pgby, wth, pgval, rs, talign, 
                           ystart = sum(ystart, ttls$points, ttl_hdr$points) - 2)
  
  } else {
    
    pgbys <- get_page_by_pdf(pgby, wth, pgval, rs, talign, 
                             ystart = sum(ystart, ttls$points, ttl_hdr$points))
  }
  
  # Get image PDF codes
  #img <- get_image_pdf(plot_path, plt$width, plt$height, rs$units)
  
  if (talign == "right") {
    lb <- rs$content_size[["width"]] - wth
    rb <- rs$content_size[["width"]]
  } else if (talign %in% c("center", "centre")) {
    lb <- (rs$content_size[["width"]] - wth) / 2
    rb <- wth + lb
  } else {
    lb <- 0
    rb <- wth
  }
  # 
  # algn <- "\\qc" 
  
  # Convert width to twips
  w <- round(wth * rs$point_conversion)

  if ((ttls$border_flag | ttl_hdr$border_flag) & pgbys$points > 0)
    ypos <- sum(ystart, ttls$points, ttl_hdr$points, pgbys$points) - 2
  else 
    ypos <- sum(ystart, ttls$points, ttl_hdr$points, pgbys$points) 
  
  lnstrt <- ceiling(ypos / rs$line_height)
  

  imght <- round((plt$height * rs$point_conversion) / lh)
  

  rws <- list()
  rws[[length(rws) + 1]] <- page_image(plot_path,
                             height = plt$height,
                             width = plt$width,
                             align = talign,
                             line_start = lnstrt + aln)

  
  yline <- ceiling(ypos + (plt$height * rs$point_conversion)) 
  brdrs <- strip_borders(plt$borders)
  
  ylpos <- ypos - lh + bh - 1
  
  # Top border
  if (any(brdrs %in% c("all", "outside", "top"))) {
    
    rws[[length(rws) + 1]] <- page_hline(lb * conv, 
                                         ylpos, 
                                         (rb - lb) * conv) 
    
  }
  
  # Bottom border
  if (any(brdrs %in% c("all", "outside", "bottom"))) {
    
    rws[[length(rws) + 1]] <- page_hline(lb * conv, 
                                         ylpos, 
                                         (rb - lb) * conv) 
    
  }
  
  # Left border
  if (any(brdrs %in% c("all", "outside", "left"))) {
    
    
    rws[[length(rws) + 1]] <- page_vline(lb * conv, 
                                         ylpos, 
                                         yline - ypos) 
    
  }
  
  # Right border
  if (any(brdrs %in% c("all", "outside", "right"))) {
    
    
    rws[[length(rws) + 1]] <- page_vline(rb * conv, 
                                         ylpos, 
                                         yline - ypos) 
    
  }
  
  

  
  # Get footnotes, filler, and content blank line
  ftnts <- get_page_footnotes_pdf(rs, plt, wth, lpg_rows, yline - bh - 1,
                                  wrap_flag, content_blank_row, talign)
  
  bln <- 0
  if (content_blank_row %in% c("both", "below")) {
    bln <-  1
  }
  
  # Add remaining page content.
  # This needs to be done now so everything is on the page
  # Different than all other output types.
  if (ttls$lines > 0) {
    rws<- append(rws, ttls$pdf)
  }
  if (ttl_hdr$lines > 0) {
    rws <- append(rws, ttl_hdr$pdf)
  }
  if (pgbys$lines > 0) {
    rws <- append(rws, pgbys$pdf)
  }
  if (ftnts$lines > 0) {
    rws <- append(rws, ftnts$pdf)
  }
  

  # Get sum of all items to this point
  lns <- sum(aln, ttls$lines, ttl_hdr$lines, pgbys$lines, 
             imght, ftnts$lines, bln)
  
  # Page list
  ret <- list(pdf = rws,
              lines = lns,
              points =  ylpos - ystart)  
  
  
  return(ret)
  
}



# Write DOCX Functions -----------------------------------------------------


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
create_plot_pages_docx<- function(rs, cntnt, lpg_rows, tmp_dir) {
  
  if (!"report_spec" %in% class(rs))
    stop("Report spec expected for parameter rs")
  
  if (!"report_content" %in% class(cntnt))
    stop("Report Content expected for parameter cntnt")

  pgs <- list()
  cnts <- c()
  imgs <- list()
  
  relIndex <- rs$relIndex
  
  # Get plot spec 
  plt <- cntnt$object
  p <- plt$plot
  
  
  if (any(class(p) %in% c("character"))) {
    
    tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
    
    file.copy(p, tmp_nm, overwrite = TRUE)
    
    # Get rtf page bodies
    res <- get_plot_body_docx(plt, tmp_nm, cntnt$align, rs,
                              lpg_rows, cntnt$blank_row, NULL, NULL, 
                              FALSE, relIndex)
    
    pgs[[length(pgs) + 1]] <- res$docx
    cnts[[length(cnts) + 1]] <- res$lines
    
    relID <- paste0("rId", relIndex)
    imgs[[relID]] <- tmp_nm
    relIndex <- relIndex + 1
    
  } else {
    
    
    # Get data in case we need it for page by
    raw <- plt$plot$data
    
    # Determine if there is a page by
    pgby <- NULL
    if (!is.null(rs$page_by))
      pgby <- rs$page_by
    if (!is.null(plt$page_by))
      pgby <- plt$page_by
    
    if (!is.null(pgby)) {
      if (!pgby$var %in% names(raw))
        stop("Page by variable not found in plot data.")
      
      dat_lst <- split(raw, raw[[pgby$var]])
    } else {
      dat_lst <- list(raw) 
    }
    
    u <- ifelse(rs$units == "inches", "in", rs$units)
    p <- plt$plot
    ret <- list()
    cntr <- 1
    
    
    for (dat in dat_lst) {
      
      tmp_nm <- tempfile(tmpdir = tmp_dir, fileext = ".jpg")
      
      p$data <- dat
      
      pgval <- NULL
      if (!is.null(pgby)) {
        # print(pgby$var)
        # print(dat)
        pgval <- dat[1, c(pgby$var)]
      }
      
      # Save plot to temp file
      if (any(class(p) %in% c("ggcoxzph", "ggsurv"))) {
        
        # Deal with survival plots
        ggplot2::ggsave(tmp_nm, gridExtra::arrangeGrob(grobs = p), 
                        width =  plt$width, height = plt$height, 
                        dpi = 300, units = u )
        
        
      } else {
        
        # Any other type of plots
        ggplot2::ggsave(tmp_nm, p, width =  plt$width, height = plt$height, 
                        dpi = 300, units = u)
      }
      
      # Get rtf page bodies
      res <- get_plot_body_docx(plt, tmp_nm, cntnt$align, rs,
                                lpg_rows, cntnt$blank_row, pgby, pgval, 
                                cntr < length(dat_lst), relIndex)
      
      pgs[[length(pgs) + 1]] <- res$docx
      cnts[[length(cnts) + 1]] <- res$lines
      
      relID <- paste0("rId", relIndex)
      imgs[[relID]] <- tmp_nm
      relIndex <- relIndex + 1
      
      cntr <- cntr + 1
      
    }
  }
  
  ret <- list(docx = pgs,
              lines = cnts,
              images = imgs)
  
  return(ret)
}

#' Create list of vectors of strings for each page 
#' @noRd
get_plot_body_docx <- function(plt, plot_path, talign, rs,
                               lpg_rows, content_blank_row, pgby, pgval, 
                               wrap_flag, rID) {
  
  conv <- rs$twip_conversion
  
  # Default to content width
  wth <- rs$content_size[["width"]] 
  
  # If user supplies a width, override default
  if (!is.null(plt$width))
    wth <- plt$width
  
  
  # Get titles and footnotes
  ttls <- get_titles_docx(plt$titles, wth, rs, talign,
                          content_brdrs = plt$borders) 
  ttl_hdr <- get_title_header_docx(plt$title_hdr, wth, rs, talign)
  
  exclude_top <- NULL
  if (ttls$border_flag == TRUE | ttl_hdr$border_flag == TRUE) {
    exclude_top <- "top"
    
    pgbys <- get_page_by_docx(pgby, wth, pgval, rs, talign, TRUE)
  } else {
    
    pgbys <- get_page_by_docx(pgby, wth, pgval, rs, talign, FALSE)
  }
  
  if (is.null(exclude_top)) {
    if (pgbys$border_flag)
      exclude_top <- "top"
    
  }
  
  algn <- talign
  if (talign == "centre")
    algn <- "center"
  
  # algn <- "\\qc" 
  # This height and width is for the image scaling.
  u <- rs$units
  iconv <- 914400  # Image unit conversion. Not even sure what units this is.
  if (u == "inches") {
    hgth <- (plt$height - .2) * iconv 
    wdth <- (plt$width - .2) * iconv 
    
  } else {
    hgth <- (cin(plt$height) - .2) * iconv 
    wdth <- (cin(plt$width) - .2) * iconv
    
  }
  
  # Get image RTF codes
  img <- get_image_docx(rID, algn, hgth, wdth)
  
  
  # Get indent codes for alignment
  ta <- get_indent_docx(talign, rs$line_size, round(wth * conv), 
                        rs$base_indent, plt$borders, conv)
  
  grd <- get_col_grid(c(img = round(wth * conv)), 1)
  

  # Get border codes
  tb <- get_table_borders_docx(plt$borders)
  
  tcb <- paste('<w:tcBorders>', 
               ifelse(any(plt$borders %in% c("all", "outside", "top")),
							'<w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>', ""),
							ifelse(any(plt$borders %in% c("all", "outside", "bottom")),
							'<w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>', ""),
						  '</w:tcBorders>')
  
  # Concat all header codes
  hd <- paste0("<w:tbl>",  "<w:tblPr>",
               '<w:tblStyle w:val="TableGrid"/>',
               rs$cell_margin,
               '<w:tblW w:w="', round(wth * conv),'"',
               ' w:type="dxa"/>', ta, tb,
               "</w:tblPr>\n", grd,
               "<w:tr><w:tc>\n",
               '<w:tcPr><w:tcW w:w="', round(wth * conv),'"/>', tcb,'</w:tcPr>')
  
  ft <- paste0("</w:tc></w:tr></w:tbl>\n")
  
  
  # Concat RTF codes for image
  img <- paste0(hd, img, ft)
  
  # ** Do something with this **
  imght <- floor(plt$height / rs$line_height)
  
  # Add blank above content if requested
  a <- NULL
  if (content_blank_row %in% c("both", "above"))
    a <- rs$blank_row
  
  
  # Get sum of all items to this point
  lns <- sum(length(a), ttls$lines, ttl_hdr$lines, pgbys$lines, imght)
  
  if ("bottom" %in% get_outer_borders(plt$borders))
    extp <- TRUE
  else 
    extp <- FALSE 
  
  # Get footnotes, filler, and content blank line
  ftnts <- get_page_footnotes_docx(rs, plt, wth, lpg_rows, lns,
                                   wrap_flag, content_blank_row, talign, extp,
                                   content_brdrs = plt$borders)
  
  # Combine titles, blanks, body, and footnotes
  rws <- c(a, ttls$docx, ttl_hdr$docx, pgbys$docx, img)
  
  
  # Combine everything
  rws <- c(rws, ftnts$docx)
  lns <- sum(lns, ftnts$lines)
  
  
  # Page list
  ret <- list(docx = rws,
              lines = lns)  
  
  
  return(ret)
  
}


get_image_docx <- function(id, algn, hgth, wdth) {
 
  
 ret <- paste0('<w:p>',
               '<w:pPr><w:jc w:val="', algn, '"/></w:pPr>',
    '<w:r>
    <w:rPr>
    <w:noProof/>
    </w:rPr>
    <w:drawing>
    <wp:inline distT="0" distB="0" distL="0" distR="0" 
       wp14:anchorId="53259B4E" wp14:editId="79F9BC2E">
      <wp:extent cx="', wdth, '" cy="', hgth, '"/>
        <wp:effectExtent l="0" t="0" r="0" b="0"/>
          <wp:docPr id="', id, '" name="Picture ', id, '"/>
            <wp:cNvGraphicFramePr>
            <a:graphicFrameLocks
          xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" noChangeAspect="1"/>
            </wp:cNvGraphicFramePr>
            <a:graphic
          xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">
            <a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
            <pic:pic
          xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
            <pic:nvPicPr>
            <pic:cNvPr id="', id, '" name="Picture ', id, '"/>
            <pic:cNvPicPr/>
            </pic:nvPicPr>
            <pic:blipFill>
            <a:blip r:embed="rId', id, '" cstate="print">
            <a:extLst>
            <a:ext uri="{28A0092B-C50C-407E-A947-70E740481C1C}">
            <a14:useLocalDpi
          xmlns:a14="http://schemas.microsoft.com/office/drawing/2010/main" val="0"/>
            </a:ext>
            </a:extLst>
            </a:blip>
            <a:stretch>
            <a:fillRect/>
            </a:stretch>
            </pic:blipFill>
            <pic:spPr>
            <a:xfrm>
            <a:off x="0" y="0"/>
            <a:ext cx="', wdth, '" cy="', hgth, '"/>
            </a:xfrm>
            <a:prstGeom prst="rect">
            <a:avLst/>
            </a:prstGeom>
            </pic:spPr>
            </pic:pic>
            </a:graphicData>
            </a:graphic>
            </wp:inline>
            </w:drawing>
            </w:r>
            </w:p>')
 
 #ret <- paste0('<w:p><w:r><w:t>Hello</w:t></w:r></w:p>')
 
 
 return(ret)
  
}

