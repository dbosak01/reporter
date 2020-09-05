

# Report Spec Constructor -----------------------------------------------


#' @title
#' Create a report
#'
#' @description
#' Creates a report shell to which you may add content.
#' 
#' @details
#' This function is the constructor for the report object.  The report
#' object contains information needed to create a report. The object is
#' defined as an S3 object, and has a class of 'report_spec'.
#' 
#' The report object holds information concerning report page size, font, 
#' titles, footnotes, page header, page footer, margins, and other options.  
#' Use the \code{\link{add_content}} function to add content to the report.  
#' The report may be written to a file using the \code{\link{write_report}} 
#' function. 
#' 
#' See the \code{\link{create_table}} and \code{\link{create_text}} functions
#' to create content for the report.
#'
#' @param file_path The output path of the desired report. Either a full path or
#' a relative path is acceptable.  This parameter is not required to create the
#' report_spec object, but will be required to print the report.
#' @param output_type The report output type.  Currently, the only valid value 
#' is "text".  Default is "text".  Will eventually support "RTF", "PDF", and 
#' "DOCX".
#' @param orientation The page orientation of the desired report.  Valid values
#' are "landscape" or "portrait".  The default page orientation is "landscape".
#' @param uom Specifies the units of measurement.  This setting will 
#' indicate the units for columns widths, margins, paper size, and other 
#' measurements. Valid values are "inches" or "cm".  Default value is "inches".
#' @param paper_size The expected paper size on which the report may be 
#' printed.  The \code{paper_size} will determine how much text can fit on
#' one page.  Valid values are "letter", "legal", "A4", and "RD4".  Default is 
#' "letter".
#' @return A new report_spec S3 object.
#' @family report
#' @seealso 
#' \itemize{
#'   \item \code{\link{titles}} to add titles to the report.
#'   \item \code{\link{footnotes}} to add footnotes to the report.
#'   \item \code{\link{page_header}} to add a page header to the report. 
#'   \item \code{\link{page_footer}} to add a page_footer to the report. 
#'   \item \code{\link{add_content}} to add content to the report.
#'   \item \code{\link{options_fixed}} to set options for text output.
#'   \item \code{\link{add_content}} to add content to the report.
#'   \item \code{\link{write_report}} to write the report to the file system.
#' }
#' @examples
#' # Create temp file path
#' fp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Create the report object
#' rpt <- create_report(fp) 
#' 
#' # Add title
#' rpt <- titles(rpt, "MTCARS sample report")
#' 
#' # Add content 
#' rpt <- add_content(rpt, create_table(mtcars)) 
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(fp))
#' @export
create_report <- function(file_path = "", output_type = "text", 
                          orientation ="landscape", uom = "inches",
                          paper_size = "letter") {

  x <- structure(list(), class = c("report_spec", "list"))

  # Trap missing or invalid output_type parameter
  if (!output_type %in% c("text")) {
    
    stop(paste0("output_type parameter on create_report() ",
                "function is invalid: '", output_type,
                "'\n\tValid values are: 'text'."))
  }
  
  # Trap missing or invalid orientation parameter.
  if (!orientation %in% c("landscape", "portrait")) {

    stop(paste0("orientation parameter on ",
                "create_report() function is invalid: '", orientation,
                "'\n\tValid values are: 'landscape' or 'portrait'."))
  }
  
  # Trap missing or invalid uom parameter.
  if (!uom %in% c("inches", "cm")) {
    
    stop(paste0("uom parameter on ",
                "create_report() function is invalid: '", uom,
                "'\n\tValid values are: 'inches' or 'cm'."))
  }
  
  # Trap missing or invalid paper_size parameter.
  if (!paper_size %in% c("letter", "legal", "A4", "RD4")) {
    
    stop(paste0("paper_size parameter on ",
                "create_report() function is invalid: '", paper_size,
                "'\n\tValid values are: 'letter', 'legal', 'A4', 'RD4'."))
  }
  

  # Populate report_spec fields
  x$file_path <- file_path
  x$output_type <- output_type
  x$orientation <- orientation
  x$content <- list()           # Initialize content list
  x$uom <- uom                  # Unit of measure
  x$paper_size <- paper_size
  x$page_size <- get_page_size(paper_size, uom)
  x$pages <- 1                  # Track # of pages in report

  
  if (output_type == "text") {
    
    # Set default options for text
    # This sets line_height and char_width
    # which are needed for all conversions from 
    # uom to text
    x <- options_fixed(x)
    
  } 
    
  # else if (output_type == "docx") {
  #   
  #   # Set default options for docx
  #   #x <- options_docx(x)
  #   
  # }

  # Set default margins
  x <- set_margins(x)

  return(x)

}


# Options -----------------------------------------------------------------

#' @description This is a lookup table to get standard settings for various 
#' editors.
#' @noRd
editor_settings <- read.table(header = TRUE, text = '
                    editor          cpi     cpcm     lpi     lpcm
                    editplus    14.6565    5.769   6.857   2.7027
                    notepad          12   4.7619  5.6805   2.2305
                    notepad++        12   4.7619   6.531   2.5862
                    word         11.497   4.5454  6.1146      2.4
                    wordpad      10.909   4.3165  6.1146      2.4
                               ') 


#' @title
#' Set options for a report with a variable width font
#'
#' @description
#' This function sets the options for a report of output type 
#' 'rtf', 'docx', or 'pdf'.
#'
#' @param x The report spec.
#' @param font_name The font name to use on the report.  The specified font
#' will be used on the entire report.  Valid values are "Courier New", "Arial",
#' "Calibri", or "Times New Roman".  The default font is "Courier New".
#' @param font_size The font size.  Valid value is 10.
#' @return The updated report spec.
#' @family report
#' @examples
#' # Here is an example
#' # This function commented out for now, until variable width fonts are available.
#' @noRd  
options_variable <- function(x, font_name="Courier New", font_size=10) {
  
  # Trap missing or invalid font_name parameter.
  if (!font_name %in% c("Courier New", "Times New Roman", "Arial", "Calibri")) {
    
    stop(paste0("ERROR: font_name parameter on create_report() ",
                "function is invalid: '", font_name,
                "'\n\tValid values are: 'Arial', 'Calibri', 'Courier New', ",
                "and 'Times New Roman'."))
  }
  
  x$font_size <-font_size
  x$font_name <- font_name
  x$font_family <- get_font_family(font_name)
  
  return(x)
}

#' @title
#' Set options for a report with a fixed width font
#'
#' @description
#' This function sets the options for a report of output type 'text'.
#' 
#' @details The \code{options_fixed} function sets the characters per 
#' unit of measure (\strong{cpuom}) and lines per unit of measure
#' (\strong{lpuom}) settings for the report.  These settings determine how 
#' many characters and lines will fit within one unit of measure (uom), as 
#' specified on the \code{\link{create_report}} function.  These settings are
#' important to ensure the report content stays within available page size 
#' and margins.  Because every text editor allows a different number of 
#' characters and lines on a page, these settings must be adjusted depending
#' on the editor.  The function provides a shortcut \code{editor} parameter
#' to directly specify a common editor.  If this parameter is specified, the
#' function will set the characters per unit of measure and lines per
#' unit of measure for you.  If the editor is not available in the 
#' \code{editor} parameter selections, you must set the \strong{cpuom} and 
#' \strong{lpuom} parameters manually.  To determine your \strong{cpuom}
#' and \strong{lpuom}, see the help for \code{\link{write_registration_file}}.
#'
#' @param x The report spec.
#' @param editor The expected text editor to use for printing.  Assigning
#' this parameter will set the \strong{cpuom} and \strong{lpuom} parameters
#' appropriately for the editor.  Valid values are 'notepad',
#' 'word', 'wordpad', 'notepad++', and 'editplus'.  If the editor parameter 
#' is used, any settings for \strong{cpuom} and \strong{lpuom} will be 
#' ignored.
#' @param cpuom Characters per unit of measure of printed text.    
#' if uom is inches, 
#' default is 12, which equals a 10pt font.  This value will be used to 
#' determine how many characters can fit on a line.  
#' @param lpuom Lines per unit of measure of the printed text. Default for 
#' inches is 6, which is average for a 10pt font. This value 
#' will be used to determine the number of lines that can fit on a page. 
#' @return The updated report spec.
#' @seealso \code{\link{create_report}} to create a report and set the unit
#' of measure, \code{\link{write_registration_file}} to determine the 
#' characters and lines per unit of measure manually.
#' @family report
#' @examples
#' # Create temp file path
#' fp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Create the report object
#' rpt <- create_report(fp) 
#' 
#' # Add title
#' rpt <- titles(rpt, "MTCARS sample report")
#' 
#' # Add content and set options editor parameter to notepad++.
#' # The editor option can optimize the settings for a specific text editor.
#' rpt <- add_content(rpt, create_table(mtcars)) 
#' rpt <- options_fixed(rpt, editor = "notepad++")
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(fp))
#' @export
options_fixed <- function(x, editor = NULL, cpuom = NULL, lpuom = NULL) {
  
  if (is.null(editor)) {
    # Trap missing or invalid cpuom parameter.
    if (is.null(cpuom))
      x$cpuom <- if (x$uom == "inches") 12 else 4.687
    else if (!(cpuom >= 8 & cpuom <= 14)) {
      
      stop(paste0("ERROR: cpi parameter on create_report() ",
                  "function is invalid: '", cpuom,
                  "'\n\tValue must be between 0 and 15."))
    }
    else
      x$cpuom <- cpuom
      
    # Trap missing or invalid lpuom parameter.
    if (is.null(lpuom))
      x$lpuom <- if (x$uom == "inches") 6 else 2.55
    else if (!(lpuom > 0 & lpuom <= 10)) {
      
      stop(paste0("ERROR: lpuom parameter on create_report() ",
                  "function is invalid: '", lpuom,
                  "'\n\tValue must be between 0 and 10."))
    }
    else
      x$lpuom <- lpuom
      


  } else {
    
    if (!tolower(editor) %in% c("notepad", "word", "wordpad", "notepad++",
                                "editplus")) {
      
     stop(paste("editor parameter invalid.  Valid values are:", 
                "'notepad', 'word', 'wordpad', 'notepad++','editplus'"))
    }
    
    e <- editor_settings[editor_settings$editor == tolower(editor), ]
    
    x$editor <- editor
    
    # Set columns per unit of measure
    # and lines per unit of measure
    if (x$uom == "inches") {
      x$cpuom <- e$cpi
      x$lpuom <- e$lpi
    } else {
      x$cpuom <- e$cpcm
      x$lpuom <- e$lpcm
    }
    
    # print(paste("cpuom:", x$cpuom))
    # print(paste("lpuom:", x$lpuom))
  }
  
  x$char_width <- 1 / x$cpuom
  x$line_height <- 1 / x$lpuom
    
  
  return(x)
  
}

#' @title
#' Set page margins
#' @description Set the page margins on the report spec object.
#' @details
#' The margins will be used for the entire report.  Units for the margins
#' are specified using the \strong{uom} parameter on the 
#' \code{\link{create_report}} function.  Available units are 'inches' and 'cm'.
#' When the unit of measure is inches, default margins are 1 inch on left and 
#' right, and .5 inches on top and bottom.  When the unit of measure is 
#' centimeters, default margins are 2.54 cm on left and right, and 1.27 cm
#' on top and bottom.
#' 
# The \strong{min_margin} parameter is used to set the minimum margin allowed
# by the printer.  This value will be subtracted from the margin settings 
# when the blank_margins option is used.
# 
#' @param x The report spec object.
#' @param top The top margin.
#' @param bottom The bottom margin.
#' @param left The left margin.
#' @param right The right margin.
# @param min_margin The printer minimum margin.
# @param blank_margins When this option is TRUE, \strong{rptr} will use blank 
# spaces and blank rows to create left and top margins, rather than rely 
# on the editor to set margins.  When used, editor margins
# should be set to zero.  Valid values are TRUE and FALSE. Default is
# TRUE.
#' @return The report_spec with margins set as desired.
#' @family report
#' @examples
#' # Here is an example
# create_report("mtcars.docx", orientation="portrait") %>%
# set_margins(margin_top = 1, margin_bottom =1)  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# file.show("mtcars.docx")
#' @export
set_margins <- function(x, top=NULL, bottom=NULL,
                           left=NULL, right=NULL 
                           #min_margin = NULL , blank_margins = TRUE
                           ) {

  if (!is.null(top)) {
    if (is.na(top) | top < 0 | !is.numeric(top)){
      stop("ERROR: invalid value for top")
    } 
    else
      x$margin_top = top
  }
  else
    x$margin_top = if (x$uom == "inches") .5 else 1.27
  
  if (!is.null(bottom)) {
    if (is.na(bottom) | bottom < 0| !is.numeric(bottom)){
      stop("ERROR: invalid value for bottom")
    }
    else
      x$margin_bottom = bottom
  }
  else
    x$margin_bottom = if (x$uom == "inches") .5 else 1.27
  
  if (!is.null(left)) {
    if (is.na(left) | left < 0| !is.numeric(left)){
      stop("ERROR: invalid value for left")
    }
    else
      x$margin_left = left
  }
  else
    x$margin_left = if (x$uom == "inches") 1 else 2.54
  
  if (!is.null(right)) {
    if (is.na(right) | right < 0| !is.numeric(right)){
      stop("ERROR: invalid value for right")
    }
    else 
      x$margin_right = right
  }
  else
    x$margin_right = if (x$uom == "inches") 1 else 2.54

  # if (!is.null(min_margin)) {
  #   if (is.na(min_margin) | min_margin < 0 | !is.numeric(min_margin)){
  #     stop("ERROR: invalid value for min_margin")
  #   }
  #   else 
  #     x$min_margin = min_margin
  # }
  # else
  #   x$min_margin = if (x$uom == "inches") .394 else 1
  
  #x$blank_margins <- blank_margins

  return(x)
}


# Page Template Items -----------------------------------------------



#' @title
#' Add a page header to the report
#'
#' @description
#' This function adds a page header to the report.  The page header will appear
#' on each page of the report.
#'
#' @details
#' The page header may contain text on the left or right. Use the appropriate
#' parameters to specify the desired text.  Multiple text values for each side
#' may be specified as a vector of strings.
#' @param x The report spec object.
#' @param left The left page header text.  May be a single string or a vector
#' of strings.
#' @param right The right page header text.  May be a single string or a vector
#' of strings.
#' @param blank_row Whether to create a blank row below the page header.
#' Valid values are 'below' and 'none'.  Default is 'none'.
#' @return The modified report specification.
#' @family report
#' @examples
#' # Here is an example
# create_report("mtcars.docx", orientation="portrait") %>%
# page_header(left = "Cars Data", right = "Study ABC")  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
page_header <- function(x, left="", right="", blank_row = "none"){

  if (length(left) > 5 | length(right) > 5){
    stop("ERROR: Header string count exceeds limit of 5 strings per side.")
  }
  
  if (is.null(blank_row))
    blank_row <- "none"
  
  if (!blank_row %in% c("below", "none")) {
    stop(paste("blank_row parameter value invalid:", blank_row, "\n", 
      "Valid values are 'below' and 'none'."))
  }
  
  x$page_header_left <- left
  x$page_header_right <- right
  x$page_header_blank_row <- blank_row

  return(x)
}


#' @title
#' Add titles to the report
#'
#' @description
#' This function adds one or more to the object.  If added to a report, 
#' the titles will be added to
#' the page template, and thus appear on each page of the report.
#'
#' @details
#' The titles function accepts a set of strings of the desired title text.
#' The titles may be aligned center, left or right using the align parameter.
#' The titles may be assigned to the report, a table or a text specification.
#'
#' @param x The object to assign titles to.  Valid objects are a report,
#' a table, or a text specification.
#' @param ... A set of title strings.
#' @param align The position to align the titles.  Valid values are 'left', 
#' 'right', 'center' or 'centre'.
#' @param blank_row Where to place a blank row.  Valid values are 'above',
#' 'below', 'both', or 'none'.
#' @return The modified report.
#' @family report
#' @examples
#' # Here is a comment
# create_report("mtcars.docx", orientation="portrait") %>%
# titles("Cars Table Title", "All Cars")  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
titles <- function(x, ..., align = "center", blank_row = "below"){

  # Create title structure
  ttl <- structure(list(), class = c("title_spec", "list"))
  
  if (length(c(...)) > 10)
    stop("Limit of 10 titles reached.") 

  # Assign attributes
  ttl$titles <-  c(...)
  ttl$blank_row <- blank_row
  ttl$align <- align

  x$titles[[length(x$titles) + 1]] <- ttl
  
  return(x)

}


#' @title
#' Add footnotes to the report
#'
#' @description
#' This function adds one or more footnotes to the report.  The footnotes will
#' be added to the page template, and thus appear on each page of the report.
#'
#' @details
#' The footnotes function accepts a set of strings of the desired footnote text.
#' The footnotes may be aligned center, left or right using the align parameter.
#' The footnotes may be located in the footer, page body, or table according to
#' the location parameter. The user is responsible for adding desired symbols.
#' Footnote symbols will not be generated automatically.
#' @param x The object to assign footnotes to.
#' @param ... A set of footnotes strings.
#' @param align The position to align the titles.  Valid values are: "left",
#' "right", "center", or "centre".
#' @param blank_row Whether to print a blank row above or below the footnote.
#' Valid values are 'above', 'below', 'both', or 'none'.  Default is 'above'.
#' @return The modified report.
#' @family report
#' @examples
#' # Here are some examples.
# create_report("mtcars.docx", orientation="portrait") %>%
# titles("Cars Table Title", "All Cars")  %>%
# footnotes("Source: 1974 Motor Trend US magazine") %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
footnotes <- function(x, ..., align = "left", blank_row = "above"){

  # Create title structure
  ftn <- structure(list(), class = c("footnote_spec", "list"))
  
  ft <- c(...)
  
  if (length(ft) > 25){
    stop("footnotes function is limited to a maximum of 25 footnotes.")
  }

  ftn$footnotes <- ft
  ftn$blank_row <- blank_row
  ftn$align <- align
  
  x$footnotes[[length(x$footnotes) + 1]] <- ftn

  return(x)

}

#' @title
#' Add a page footer to the report
#'
#' @description
#' This function adds a page footer to the report.  The page footer will appear
#' on each page of the report.
#'
#' @details
#' The page footer may contain text on the left, right, or center.
#' Use the appropriate parameters to specify the desired text.  
#' 
#' There are two special tokens to generate page numbers: [pg] and [tpg]. 
#' Use [pg] to indicate the current page number.  Use [tpg] to indicate the
#' total number of pages in the report.  These token may be place anywhere 
#' in the page header or page footer. 
#'
#' @param x The report spec object.
#' @param left The left page footer text.  May be a single string or a vector
#' of strings.
#' @param center The center page footer text.  May be a single string or a
#' vector of strings.
#' @param right The right page footer text.  May be a single string or a vector
#' of strings.
#' @param blank_row Whether to create a blank row above the page footer.
#' Valid values are 'above' and 'none'.  Default is 'above'.
#' @return The modified report.
#' @family report
#' @seealso \code{\link{page_header}} to add a page header to the report, 
#' \code{\link{titles}} to add titles, and \code{\link{footnotes}} to
#' add footnotes.
#' @examples
#' library(magrittr)
#' 
#' # Create table 
#' tbl <- create_table(mtcars) %>% 
#'        titles("MTCARS Sample Data") %>% 
#'        footnotes("* From Motor Trend, 1973")
#' 
#' # Create temp report file name
#' tmp <- file.path(tempdir(), "mtcars.txt")
#'         
#' # Create report 
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'        page_header(left = "Cars Data", right = "Sample Report")  %>%
#'        page_footer(left = Sys.time(), right = "Page [pg] of [tpg]")  %>%
#'        add_content(tbl)
#'
#' # Write out the report        
#' write_report(rpt)
#' 
#' # Send report to console 
#' writeLines(readLines(tmp))
#' @export
page_footer <- function(x, left="",  center="", right="", blank_row = "above"){

  if (length(left) > 5 | length(right) > 5 | length(center) > 5){
    stop("ERROR: Footer string count exceeds limit of 5 strings per section.")
  }
  
  if (is.null(blank_row))
    blank_row <- "none"
  
  if (!blank_row %in% c("none", "above"))
    stop("Invalid value for blank_row.  Valid values are 'above' or 'none'.")
  
  x$page_footer_left <- left
  x$page_footer_right <- right
  x$page_footer_center <- center
  x$page_footer_blank_row <- blank_row

  return(x)
}




# Functions ---------------------------------------------------------------


#' @title Add content to a report
#' @description This function adds an object to the report content list. 
#' @details 
#' For a text report, 
#' valid objects are free text or a table_spec.  Objects will be
#' appended to the report in order they are added.  By default, a page break
#' is added after the content.
#' @param x The report_spec to append content to.
#' @param object The object to append.
#' @param page_break Whether to add a page break after the object. 
#' Valid values are TRUE or FALSE.  You can manipulate the page_break 
#' parameter to add multiple objects to the same page.  
#' @param align How to align the content.  Valid values are 'left', 'right',
#' 'center', and 'centre'.
#' @param blank_row Whether to put a blank row above or below the content.
#' Valid values are 'above', 'below', 'both', or 'none'.
#' @return The modified report_spec.
#' @family report
#' @examples
#' # Create temp file path
#' fp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Create the report object
#' rpt <- create_report(fp) 
#' 
#' # Add title
#' rpt <- titles(rpt, "MTCARS sample report")
#' 
#' # Add content 
#' rpt <- add_content(rpt, create_table(mtcars), page_break = FALSE)
#' rpt <- add_content(rpt, create_text("* NOTE: Car information from 1971.")) 
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(fp))
#' @export
add_content <- function(x, object, page_break=TRUE, align = "center",
                        blank_row = "below") {
  
  if (!page_break %in% c(TRUE, FALSE)) {
   stop(paste("Page break value invalid.",
              "Valid values are TRUE and FALSE."))
  }
  
  cont <- structure(list(), class = c("report_content", "list"))
  
  cont$object <- object
  cont$page_break <- page_break
  cont$blank_row <- blank_row
  cont$align <- align
  cont$pages <- list()

  # Add object to the content list
  x$content[[length(x$content) + 1]] <- cont


  return(x)
}


#' @title
#' Write a report to the file system
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#' @details 
#' The function renders the report in the requested format, and writes it
#' to the location specified in the report \code{file_path} parameter.
#' @param x The report object to write.
#' @return The report spec, with settings modified during writing.
#' @family report
#' @export
write_report <- function(x) {
  
  
  if (!"report_spec" %in% class(x)) {
    
    stop(paste0("report object missing or invalid."))
  }
  
  
  # Trap missing or invalid output_type parameter
  if (x$file_path == "") {
    
    stop(paste0("report file_path missing or invalid."))
  }

  ret <- ""

  if (x$output_type == "text") {
    ret <- write_report_text(x)
  } else {
   stop(paste("Output type currently not supported:", x$output_type))
  }
  # } else if (x$output_type == "docx") {
  #   ret <- write_report_docx(x, ...)
  # }
  
  invisible(ret)
}




# Write Registration File -------------------------------------------------

#' @title 
#' Write a registration file 
#' 
#' @description 
#' This function will create a registration file to help determine
#' the correct \code{cpi} and \code{lpi} for editor/printer.  
#' The \code{cpi} and \code{lpi} are 
#' used in \code{output_type = "text"} to determine available space on
#' the page.   
#' @param file_path The full or relative file name and path to create.
#' @export
write_registration_file <- function(file_path) {
  
  
  f <- file(file_path, open="w")
  
  ln1 <- "0--------+---------+---------+---------+---------+---------+"
  
  writeLines(ln1, con = f)
  
  ln2 <- "-       10        20        30        40        50        60"
  
  writeLines(ln2, con = f)
  
  ln3 <- c("-", "-", "-", "-", "-", "-", "-", "+ 10")
  
  writeLines(ln3, con = f)
  
  ln4 <- c("-", "-","-", "-", "-", "-", "-", "-", "-", "+ 20")
  
  writeLines(ln4, con = f)
  
  ln5 <- c("-", "-","-", "-", "-", "-", "-", "-", "-", "+ 30")
  
  writeLines(ln5, con = f)
  
  ln6 <- c("-", "-","-", "-", "-", "-", "-", "-", "-", "+ 40")
  
  writeLines(ln6, con = f)
  
  ln7 <- c("-", "-","-", "-", "-", "-", "-", "-", "-", "+ 50")
  
  writeLines(ln7, con = f)
  
  ln8 <- c("-", "-","-", "-", "-", "-", "-", "-", "-", "+ 60")
  
  writeLines(ln8, con = f)
  
  close(f)
}




