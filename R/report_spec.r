

# Report Spec Constructor -----------------------------------------------


#' @title
#' Create a report
#'
#' @description
#' Creates a report shell to which you may add titles, footnotes, content, etc.
#' 
#' @details
#' This function is the constructor for the report object.  The report
#' object contains information needed to create a report. The object is
#' defined as an S3 object, and has a class of 'report_spec'.
#' 
#' The report object holds information concerning report page size, orientation, 
#' titles, footnotes, page header, page footer, margins, and other options.  
#' Use the \code{\link{add_content}} function to add content to the report.  
#' The report may be written to a file using the \code{\link{write_report}} 
#' function. 
#' 
#' @section Report family of functions:
#' The report is the primary container for report specifications.  The
#' following functions add additional specifications to the report object 
#' initialized with \code{create_report}.
#' \itemize{
#'   \item \code{\link{titles}} to add titles to the report.
#'   \item \code{\link{footnotes}} to add footnotes to the report.
#'   \item \code{\link{title_header}} to add a title header to the report.
#'   \item \code{\link{page_header}} to add a page header to the report. 
#'   \item \code{\link{page_footer}} to add a page_footer to the report. 
#'   \item \code{\link{add_content}} to add content to the report.
#'   \item \code{\link{options_fixed}} to set options for fixed-width output.
#'   \item \code{\link{add_content}} to add content to the report.
#'   \item \code{\link{write_report}} to write the report to the file system.
#' }
#' 
#' The report family of functions are pipe-friendly.  After creating the 
#' report, you may pipe the object to any of the above functions to append
#' additional options.
#' 
#' Note that PDF output has some limitations not found in TXT and RTF output.
#' See \link{NotesOnPDF} for additional information.
#'
#' @param file_path The output path of the desired report. Either a full path or
#' a relative path is acceptable.  This parameter is not required to create the
#' report_spec object, but will be required to write the report.  In addition, 
#' the file extension is not required.  If the file extension is not supplied,
#' the \code{\link{write_report}} function will add a file extension based
#' on the \code{output_type} specified.
#' @param output_type The report output type.  Default is "TXT".  Valid
#' values are "TXT", "RTF", and "PDF".
# @param font_type The font type to use on the report. The default value 
# 'fixed'.  A font type of 'fixed' will use a fixed-width,
# monospace font such as Courier.  Currently, a font type of 'fixed' is the 
# only option available.  Future versions will include variable-width fonts
# such as Arial and Times New Roman.  To set options for font type 'fixed', 
# used the \code{\link{options_fixed}} function.
#' @param orientation The page orientation of the desired report.  Valid values
#' are "landscape" or "portrait".  The default page orientation is "landscape".
#' @param units Specifies the units of measurement.  This setting will 
#' indicate the units for columns widths, margins, paper size, and other 
#' measurements. Valid values are "inches" or "cm" (centimeters).  
#' Default value is "inches".
#' @param paper_size The expected paper size on which the report may be 
#' printed.  The \code{paper_size} will determine how much text can fit on
#' one page.  Valid values are "letter", "legal", "A4", and "RD4".  Default is 
#' "letter".
#' @param missing How to display missing values in the report.  Default is
#' to replace them with an empty string, which removes them from the report.
#' To display missing values as is, set the missing parameter to NULL.  To
#' replace missing values with a character string (such as ".", "-", or "<NA>")
#' pass the desired character string to the missing parameter.
#' @return A new report_spec object.
#' @family report
#' @seealso \code{\link{create_table}}, \code{\link{create_text}}, and
#' \code{\link{create_plot}} functions
#' to create content for the report.
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "airquality.txt")
#' 
#' # Prepare Data
#' dat <- airquality[sample(1:153, 15), ]
#' dat$Month <-  as.Date(paste0("1973-", dat$Month, "-01"))
#' 
#' # Define table
#' tbl <- create_table(dat, show_cols = c("Month", "Day", "Wind", "Temp", "Ozone")) %>% 
#'   titles("Table 9.6", "Air Quality Sample Report") %>% 
#'   column_defaults(width = .5) %>% 
#'   define(Month, format = "%B", align = "left", width = 1) %>% 
#'   define(Temp, format = "%.0f") %>% 
#'   footnotes("* New York, May to September 1973")
#' 
#' # Define report 
#' rpt <- create_report(tmp, orientation = "portrait",  missing = "-") %>% 
#'   add_content(tbl) 
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write the report to the console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #                      Table 9.6
#' #              Air Quality Sample Report
#' #      
#' #      Month           Day   Wind   Temp  Ozone
#' #      ----------------------------------------
#' #      July              8    6.3     92     97
#' #      July              9    5.7     92     97
#' #      August            1    6.9     81     39
#' #      July             23   11.5     82      -
#' #      June              9   13.8     90     71
#' #      July             12   14.3     73     10
#' #      July              4   10.9     84      -
#' #      May              31    7.4     76     37
#' #      September        30   11.5     68     20
#' #      June             25      8     75      -
#' #      June             28   11.5     80      -
#' #      August           18    7.4     76     23
#' #      June             20   10.3     76     13
#' #      July              1    4.1     84    135
#' #      May              23    9.7     61      4
#' #      
#' #      * New York, May to September 1973
#' @export
create_report <- function(file_path = "", output_type = "TXT", 
                          orientation ="landscape", units = "inches",
                          paper_size = "letter", missing = "") {#,
                         # font = "fixed", font_size = NULL) {
  font = "fixed"
  font_size = NULL

  x <- structure(list(), class = c("report_spec", "list"))

  # Trap missing or invalid output_type parameter
  if (!toupper(output_type) %in% c("TXT", "PDF", "RTF")) {
    
    stop(paste0("output_type parameter on create_report() ",
                "function is invalid: '", output_type,
                "'\n\tValid values are: 'TXT', 'PDF', 'RTF'."))
  } else {
    
    output_type <- toupper(output_type) 
  }
  
  # Trap missing or invalid orientation parameter.
  if (!orientation %in% c("landscape", "portrait")) {

    stop(paste0("orientation parameter on ",
                "create_report() function is invalid: '", orientation,
                "'\n\tValid values are: 'landscape' or 'portrait'."))
  }
  
  # Trap missing or invalid units parameter.
  if (!units %in% c("inches", "cm")) {
    
    stop(paste0("units parameter on ",
                "create_report() function is invalid: '", units,
                "'\n\tValid values are: 'inches' or 'cm'."))
  }
  
  # if (!units %in% c("inches", "cm", "char")) {
  #   
  #   stop(paste0("units parameter on ",
  #               "create_report() function is invalid: '", units,
  #               "'\n\tValid values are: 'inches', 'cm', or 'char'."))
  # }
  
  
  # Trap missing or invalid paper_size parameter.
  if (!paper_size %in% c("letter", "legal", "A4", "RD4")) {
    
    stop(paste0("paper_size parameter on ",
                "create_report() function is invalid: '", paper_size,
                "'\n\tValid values are: 'letter', 'legal', 'A4', 'RD4'."))
  }
  
  # Trap missing or invalid font parameter
  if (is.null(font))
    font <- "fixed"
  else {
    if (!tolower(font) %in% c("fixed", "courier", "arial", "times"))
      stop(paste0("font value invalid.  ", 
                  "Valid values are 'Courier', 'Arial', 'Times', and 'fixed'."))
  }
  
  # Trap invalid font_size parameter
  if (!is.null(font_size)) {
    if (!font_size %in% c(8, 10, 12)) {
      stop("font_size parameter invalid.  Valid values are 8, 10, and 12.") 
    }
  }
    
  # Populate report_spec fields
  x$file_path <- file_path
  x$output_type <- output_type
  x$orientation <- orientation
  x$content <- list()           # Initialize content list
  x$units <- units              # Unit of measure
  x$paper_size <- paper_size
  x$page_size <- get_page_size(paper_size, units)
  x$pages <- 0                  # Track # of pages in report
  x$column_widths <- list()      # Capture table column widths for reference
  x$missing <- missing
  x$font <- font      
  x$font_size <- font_size

  
  if (output_type %in% c("TXT", "PDF", "RTF")) {
    
    # Set default options for text
    # This sets line_height and char_width
    # which are needed for all conversions from 
    # units to text
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


#' @title
#' Set options for a report (variable width font)
#'
#' @description
#' This function sets the options for a report of output type 
#' 'RTF', or 'PDF' when the \code{font_type} parameter on 
#' \code{\link{create_report}} function is set to "variable".
#'
#' @param x The report spec.
#' @param font_name The font name to use on the report.  The specified font
#' will be used on the entire report.  Valid values are "Courier", "Courier New", 
#' "Arial", "Calibri", or "Times New Roman".  The default font is "Courier New".
#' @param font_size The font point size.  Valid values are 10 or 12.
#' @return The updated report spec.
#' @family report
#' @examples
#' # Here is an example
#' # This function commented out for now, until variable width fonts are available.
#' @noRd  
NULL
# options_variable <- function(x, font_name="Courier New", font_size=10) {
#   
#   # Trap missing or invalid font_name parameter.
#   if (!font_name %in% c("Courier New", "Times New Roman", "Arial", "Calibri")) {
#     
#     stop(paste0("font_name parameter on create_report() ",
#                 "function is invalid: '", font_name,
#                 "'\n\tValid values are: 'Arial', 'Calibri', 'Courier New', ",
#                 "and 'Times New Roman'."))
#   }
#   
#   x$font_size <-font_size
#   x$font_name <- font_name
#   x$font_family <- get_font_family(font_name)
#   
#   return(x)
# }


#' @description This is a lookup table to get standard settings for various 
#' editors.
#' @noRd
editor_settings <- read.table(header = TRUE, text = '
                    editor          cpi     cpcm     lpi     lpcm    mmi   mmcm
                    editplus       14.2     5.75     6.1     2.62      0      0
                    notepad          12     4.75    5.65     2.22      0      0
                    notepad++        12     4.75    6.50     2.55  0.393      1
                    word           11.2      4.4       6     2.35      0      0
                    wordpad        10.8      4.2       6     2.35      0      0
                    pdf12            12     4.70       5    2.000  .1967     .5
                    pdf10       14.2222     5.58    6.10      2.4  .1967     .5
                    pdf8           17.5     6.88    7.55     2.95  .1967     .5
                    rtf12            10   3.9473     5.3     2.05      0      0
                    rtf10            12   4.7619    6.38      2.5      0      0
                    rtf8             15      5.9    7.95     3.05      0      0
                               ') 


#' @title
#' Set options for a fixed-width report
#'
#' @description
#' This function sets the options for a report  
#' with a fixed width font. 
#' 
#' @details The \code{options_fixed} function sets options for reports 
#' with a fixed-width, monospace font.  These reports are based off a 
#' text report, but may be output as type "RTF" or "PDF".  
#' 
#' @section Text Reports:
#' The \code{options_fixed} function sets
#' the characters per 
#' unit of measure (\code{cpuom}) and lines per unit of measure
#' (\code{lpuom}) settings for the report.  These settings determine how 
#' many characters and lines will fit within one unit of measure (uom), as 
#' specified on the \code{\link{create_report}} function.  These settings are
#' important to ensure the report content stays within the available page size 
#' and margins.  Because every editor allows a different number of 
#' characters and lines on a page, these settings must be adjusted depending
#' on the editor.  
#' 
#' The \code{options_fixed} function provides a shortcut 
#' \code{editor} parameter
#' to directly specify a popular editor.  If this parameter is specified, the
#' function will set the characters per unit of measure and lines per
#' unit of measure for you.  If the editor is not available in the 
#' \code{editor} parameter selections, for best results, you should 
#' set the \code{cpuom} and 
#' \code{lpuom} parameters manually.  To determine your \code{cpuom}
#' and \code{lpuom}, see the help for \code{\link{write_registration_file}}.
#' 
#' Alternatively, using the \code{options_fixed} function, 
#' you may set the \code{line_size} and \code{line_count} directly.  Note that
#' the \code{line_size} and \code{line_count} may be different for different
#' output types and editors. 
#'
#' The \code{min_margin} parameter is used to set the minimum margin allowed
#' by the editor.  This value will be subtracted from the margin settings 
#' when the \code{blank_margins} option is used. It is useful for 
#' editors that do not calculate margins from the edge of the page.
#' 
#' As some editors do not support Unicode characters, it may be necessary 
#' to change the character used for the header and spanning header underlines.
#' The default character is a Unicode #U00AF macron.  The macron is sometimes
#' referred to as an "overline", since it is located at the top of the 
#' character area.  If your editor does not support Unicode, the macron
#' will not be displayed properly.  In this case, change the underline character
#' to a dash ("-") or an underscore ("_") using the \code{uchar} parameter.
#' 
#' @section RTF and PDF Reports:
#' For RTF and PDF reports, most of the parameters on the \code{options_fixed}
#' function do not apply.  For RTF and PDF reports, these parameters will
#' be set automatically, and cannot be changed.  
#' 
#' Some of the \code{options_fixed} function apply only to RTF and PDF.
#' In particular, the \code{font_size} parameter applies only to RTF and PDF
#' reports.  Valid font size options are 8, 10, and 12.
#' 
#' @param x The report spec.
#' @param editor The expected text editor to use for printing text reports.  
#' Assigning this parameter will set the \code{cpuom} and \code{lpuom} 
#' parameters appropriately for the text editor.  Valid values are 'notepad',
#' 'word', 'wordpad', 'notepad++', and 'editplus'.  If the editor parameter 
#' is used, any settings for \code{cpuom} and \code{lpuom} will be 
#' ignored. It is not necessary to set this parameter for RTF and PDF reports.
#' @param cpuom Characters per unit of measure of printed text.    
#' If units is inches, the default is 12.  If units is centimeters (cm), the 
#' default is 4.687.  This value will be used to 
#' determine how many characters can fit on a line.  
#' @param lpuom Lines per unit of measure of the printed text. Default for 
#' inches is 6. The default for centimeters (cm) is 2.55.  This value 
#' will be used to determine the number of lines that can fit on a page. 
#' @param min_margin The editor minimum margin.  This parameter normally
#' defaults to 0, but may be set for some types of editors.  
#' @param blank_margins When this option is TRUE, \strong{reporter} will use blank 
#' spaces and blank rows to create left and top margins, rather than rely 
#' on the editor to set margins.  When used, editor margins
#' should be set to zero.  Valid values are TRUE and FALSE. Default is
#' FALSE.  This option is only valid for \code{output_type = 'TXT'}.
#' @param font_size The size of the font in points.  Default is 10pt.  This
#' option is only valid for output types RTF and PDF.  Valid values are 8, 10, 
#' and 12.
#' @param line_size The number of characters that will fit on a line.  Normally,
#' the \code{line_size} is calculated based on the page size, font size, and cpuom.
#' You can override the calculated value by setting the \code{line_size}
#' directly.  
#' @param line_count The number of lines that will fit on page.  Normally,
#' the \code{line_count} is calculated based on the page size, font size, and lpuom.
#' You can override the calculated value by setting the \code{line_count}
#' directly.
#' @param uchar The character to use for underlines on the table 
#' header and spanning headers.  Default is a Unicode macron character #U00AF.
#' You may use a dash or underscore if your editor does not support
#' Unicode.  The \code{uchar} is forced to a dash for PDF output, 
#' as the LaTeX converter does not support the macron character.
#' @return The updated report spec.
#' @seealso \code{\link{create_report}} to create a report and set the unit
#' of measure, \code{\link{write_registration_file}} to determine the 
#' characters and lines per unit of measure manually.
#' @family report
#' @encoding UTF-8
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create a temporary file
#' tmp <- file.path(tempdir(), "bod.txt")
#'
#' # Define table
#' tbl <- create_table(BOD, width = 2.5) %>% 
#'   titles("Table 3.6", "BOD* Sample Report") %>% 
#'   define(Time, format = "Day %s", align = "left") %>% 
#'   define(demand, format = "%2.1f mg/l", label = "Demand") %>% 
#'   footnotes("* Biochemical Oxygen Demand")
#'        
#' # Define report #1 - No blank margins
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #           Table 3.6
#' #       BOD* Sample Report
#' # 
#' # Time                  Demand
#' # ----------------------------
#' # Day 1               8.3 mg/l
#' # Day 2              10.3 mg/l
#' # Day 3              19.0 mg/l
#' # Day 4              16.0 mg/l
#' # Day 5              15.6 mg/l
#' # Day 7              19.8 mg/l
#' # 
#' # * Biochemical Oxygen Demand
#' 
#' 
#' # Define report #2 - blank margins
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   options_fixed(blank_margins = TRUE) %>% 
#'   set_margins(top = .5, left = 1) %>% 
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # 
#' # 
#' # 
#' #                       Table 3.6
#' #                   BOD* Sample Report
#' # 
#' #              Time                  Demand
#' #              ----------------------------
#' #              Day 1               8.3 mg/l
#' #              Day 2              10.3 mg/l
#' #              Day 3              19.0 mg/l
#' #              Day 4              16.0 mg/l
#' #              Day 5              15.6 mg/l
#' #              Day 7              19.8 mg/l
#' # 
#' #              * Biochemical Oxygen Demand
#' @export
options_fixed <- function(x, editor = NULL, cpuom = NULL, lpuom = NULL,
                          min_margin = NULL, blank_margins = FALSE,
                          font_size = NULL, line_size = NULL, line_count = NULL,
                          uchar = "\U00AF") {
  
  # Set font_size defaults
  if (is.null(x$font_size) & is.null(font_size)) {
    font_size <- 10
  } else if (!is.null(x$font_size) & is.null(font_size)) {
    font_size <- x$font_size
  }

  if (!"report_spec" %in% class(x)) {
    stop("Input object must be of class 'report_spec'.") 
  }
  
  if (x$output_type == "TXT") {
    if (is.null(editor)) {
      # Trap missing or invalid cpuom parameter.
      if (is.null(cpuom)) {
        if (x$units == "inches")
          x$cpuom <- 12 
        else if (x$units == "cm")
          x$cpuom <- 4.687
        else if (x$units == "char")
          x$cpuom <- 1
      } else if (!(cpuom >= 1 & cpuom <= 15)) {
        
        stop(paste0("cpi parameter on create_report() ",
                    "function is invalid: '", cpuom,
                    "'\n\tValue must be between 0 and 15."))
      }
      else
        x$cpuom <- cpuom
        
      # Trap missing or invalid lpuom parameter.
      if (is.null(lpuom)) {
        if (x$units == "inches")
          x$lpuom <-  6 
        else if (x$units == "cm")
          x$lpuom <- 2.55
        else if (x$units == "char")
          x$lpuom <- 1
      } else if (!(lpuom > 0 & lpuom <= 10)) {
        
        stop(paste0("lpuom parameter on create_report() ",
                    "function is invalid: '", lpuom,
                    "'\n\tValue must be between 0 and 10."))
      }
      else
        x$lpuom <- lpuom
        
      if (!is.null(min_margin)) {
        if (is.na(min_margin) | min_margin < 0 | !is.numeric(min_margin)){
          stop("ERROR: invalid value for min_margin")
        }
        else
          x$min_margin = min_margin
      } else
        x$min_margin = 0
  
    } else {
      
      if (!tolower(editor) %in% c("notepad", "word", "wordpad", "notepad++",
                                  "editplus")) {
        
       stop(paste("editor parameter invalid.  Valid values are:", 
                  "'notepad', 'word', 'wordpad', 'notepad++','editplus'"))
      }
      
      e <- editor_settings[editor_settings$editor == tolower(editor), ]
      
      x$editor <- editor
      
      # Set characters per unit of measure
      # and lines per unit of measure
      if (x$units == "inches") {
        x$cpuom <- e$cpi
        x$lpuom <- e$lpi
        x$min_margin <- e$mmi
      } else if (x$units == "cm") {
        x$cpuom <- e$cpcm
        x$lpuom <- e$lpcm
        x$min_margin <- e$mmcm
      } else if (x$units == "char") {
        
        x$cpuom <- 1
        x$lpuom <- 1
        x$min_margin <- round(e$mmi / 12)
      }
      
      # print(paste("cpuom:", x$cpuom))
      # print(paste("lpuom:", x$lpuom))
    }
    
    
    x$uchar <- uchar
    x$blank_margins <- blank_margins
  
  } else if (x$output_type == "PDF") {
    
    
    if (font_size == 12)
      e <- editor_settings[editor_settings$editor == "pdf12", ]
    else if (font_size == 10)
      e <- editor_settings[editor_settings$editor == "pdf10", ]
    else if (font_size == 8)
      e <- editor_settings[editor_settings$editor == "pdf8", ]
    else 
      stop("Invalid font_size setting.  Valid values are 8, 10 and 12")
    
    # Set cpuom and lpuom
    if (x$units == "inches") {
      x$cpuom <- e$cpi
      x$lpuom <- e$lpi
      x$min_margin <- e$mmi
    } else if (x$units == "cm") {
      x$cpuom <- e$cpcm
      x$lpuom <- e$lpcm
      x$min_margin <- e$mmcm
    } else if (x$units == "char") {
      
      x$cpuom <- 1
      x$lpuom <- 1
      x$min_margin <- round(e$mmi / 12)
    }
    
    if (uchar == "\U00AF")
      x$uchar <- "-"
    else 
      x$uchar <- uchar
    
    x$blank_margins <- FALSE
    
  } else if (x$output_type == "RTF") {

    
    if (font_size == 12)
      e <- editor_settings[editor_settings$editor == "rtf12", ]
    else if (font_size == 10)
      e <- editor_settings[editor_settings$editor == "rtf10", ]
    else if (font_size == 8)
      e <- editor_settings[editor_settings$editor == "rtf8", ]
    else 
      stop("Invalid font_size setting.  Valid values are 8, 10 and 12")
    
    # Set cpuom and lpuom
    if (x$units == "inches") {
      x$cpuom <- e$cpi
      x$lpuom <- e$lpi
      x$min_margin <- e$mmi
    } else if (x$units == "cm") {
      x$cpuom <- e$cpcm
      x$lpuom <- e$lpcm
      x$min_margin <- e$mmcm
    } else if (x$units == "char") {
      x$cpuom <- 1
      x$lpuom <- 1
      x$min_margin <- round(e$mmi / 12)
    }
    
    x$blank_margins <- FALSE
    
    if (uchar == "\U00AF") {
      if (Sys.info()[["sysname"]] != "Windows")
        x$uchar <- "-"
      else 
        x$uchar <- uchar
    } else 
      x$uchar <- uchar
    
  }
  
  if (!is.null(line_size)) {
    if (!is.numeric(line_size))
      stop("line_size must be a number.")
    if (line_size <= 0)
      stop("line_size must be greater than zero.")
  }
  
  if (!is.null(line_count)) {
    if (!is.numeric(line_count))
      stop("line_count must be a number.")
    if (line_count <= 0)
      stop("line_count must be greater than zero.")
  }

  x$font_size <- font_size
  x$char_width <- 1 / x$cpuom
  x$line_height <- 1 / x$lpuom
  x$user_line_size <- line_size
  x$user_line_count <- line_count
  
  return(x)
  
}

#' @title
#' Set page margins
#' @description Sets the page margins for the report.  The units for this 
#' parameter can be inches or centimeters, depending on the units of measure 
#' specified on the \code{\link{create_report}} function.  
#' @details
#' The margins set with \code{set_margins} will be used for the entire report.  
#' Units for the margins
#' are specified by the \code{units} parameter on the 
#' \code{\link{create_report}} function.  Available units are 'inches' and 'cm'.
#' When the unit of measure is inches, default margins are 1 inch on the left and 
#' right, and .5 inches on top and bottom.  When the unit of measure is 
#' centimeters, default margins are 2.54 cm on left and right, and 1.27 cm
#' on top and bottom.
#'
#' Note that when using output type of TXT, and not using the 
#' \code{blank_margins} option, setting the margins only reduces
#' the area available for content on a page.  You must still set the actual
#' margins on the available editor to match those specified in 
#' \code{set_margins}.  Any mismatch may result in content not fitting properly
#' on the page. For best results, set the right and bottom margins to zero 
#' to allow for slight overflow without causing a page break or wrapping lines.
#' @param x The report spec object.
#' @param top The top margin.
#' @param bottom The bottom margin.
#' @param left The left margin.
#' @param right The right margin.
#' @return The report_spec with margins set as desired.
#' @family report
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create a temporary file
#' tmp <- file.path(tempdir(), "bod.txt")
#'
#' # Define table
#' tbl <- create_table(BOD, width = 2.5) %>% 
#'   titles("Table 3.6", "BOD¹ Sample Report") %>% 
#'   define(Time, format = "Day %s", align = "left") %>% 
#'   define(demand, format = "%2.1f mg/l", label = "Demand") %>% 
#'   footnotes("¹ Biochemical Oxygen Demand")
#'        
#' # Define report #1 - No blank margins
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #           Table 3.6
#' #       BOD* Sample Report
#' # 
#' # Time                  Demand
#' # ----------------------------
#' # Day 1               8.3 mg/l
#' # Day 2              10.3 mg/l
#' # Day 3              19.0 mg/l
#' # Day 4              16.0 mg/l
#' # Day 5              15.6 mg/l
#' # Day 7              19.8 mg/l
#' # 
#' # * Biochemical Oxygen Demand
#' 
#' 
#' # Define report #2 - blank margins
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   options_fixed(blank_margins = TRUE) %>% 
#'   set_margins(top = .5, left = 1) %>% 
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # 
#' # 
#' # 
#' #                       Table 3.6
#' #                   BOD* Sample Report
#' # 
#' #              Time                  Demand
#' #              ----------------------------
#' #              Day 1               8.3 mg/l
#' #              Day 2              10.3 mg/l
#' #              Day 3              19.0 mg/l
#' #              Day 4              16.0 mg/l
#' #              Day 5              15.6 mg/l
#' #              Day 7              19.8 mg/l
#' # 
#' #              * Biochemical Oxygen Demand
#' @export
set_margins <- function(x, top=NULL, bottom=NULL,
                           left=NULL, right=NULL) {

  if (!is.null(top)) {
    if (is.na(top) | top < 0 | !is.numeric(top)){
      stop("ERROR: invalid value for top")
    } 
    else
      x$margin_top = top
  }
  else {
    if (x$units == "inches")
      x$margin_top <- .5 
    else if (x$units == "cm")
      x$margin_top <- 1.27
    # else if (x$units == "char")
    #   x$margin_top <- set_char_margins(x, "top")
  }
  
  if (!is.null(bottom)) {
    if (is.na(bottom) | bottom < 0| !is.numeric(bottom)){
      stop("ERROR: invalid value for bottom")
    }
    else
      x$margin_bottom = bottom
  }
  else {
    if (x$units == "inches")
      x$margin_bottom <- .5 
    else if (x$units == "cm")
      x$margin_bottom <- 1.27
    # else if (x$units == "char")
    #   x$margin_bottom <- set_char_margins(x, "bottom")
  }
  
  if (!is.null(left)) {
    if (is.na(left) | left < 0| !is.numeric(left)){
      stop("ERROR: invalid value for left")
    }
    else
      x$margin_left = left
  }
  else {
    if (x$units == "inches")
      x$margin_left <-  1 
    else if (x$units == "cm")
      x$margin_left <- 2.54
    # else if (x$units == "char")
    #   x$margin_left <- set_char_margins(x, "left")
    
  }
  
  if (!is.null(right)) {
    if (is.na(right) | right < 0| !is.numeric(right)){
      stop("ERROR: invalid value for right")
    }
    else 
      x$margin_right = right
  }
  else {
    if (x$units == "inches")
      x$margin_right <- 1 
    else if (x$units == "cm")
      x$margin_right <- 2.54
    # else if (x$units == "char")
    #   x$margin_right <- set_char_margins(x, "right")
  }


  return(x)
}

# Not used now.  May be used in future.
# @noRd
# set_char_margins <- function(rs, margin) {
#   
#   ret <- NULL
#   if (!is.null(rs$font_size)) {
#     if (rs$font_size == 8) {
#       if (margin %in% c("right", "left"))
#         ret <- 15
#       else if (margin %in% c("top", "bottom"))
#         ret <- 4
#     }  
#     if (rs$font_size == 10) {
#       if (margin %in% c("right", "left"))
#         ret <- 12
#       else if (margin %in% c("top", "bottom"))
#         ret <- 3
#     }  
#     if (rs$font_size == 12) {
#       if (margin %in% c("right", "left"))
#         ret <- 10
#       else if (margin %in% c("top", "bottom"))
#         ret <- 3
#     }  
#   }
#   
#   return(ret)
# }

# Page Template Items -----------------------------------------------



#' @title
#' Add a page header
#'
#' @description
#' This function adds a page header to the report.  The page header will appear
#' at the top of each page of the report.  
#'
#' @details
#' The page header may contain text on the left or right. Use the appropriate
#' parameters to specify the desired text.  Only one page header is allowed
#' on a report. The page header will be repeated on every page of the report.
#' Multiple text values for each side
#' may be specified as a vector of strings.  
#' 
#' If the width of the page header
#' string exceeds the available space, an error will be generated. There is 
#' also a limit of 5 page header strings per each side.
#' 
#' There are two special tokens to generate page numbers: [pg] and [tpg]. 
#' Use [pg] to indicate the current page number.  Use [tpg] to indicate the
#' total number of pages in the report.  These tokens may be placed anywhere 
#' in the page header or page footer. 
#' 
#' Each header string must fit within the available space.  The \strong{reporter}
#' package will not wrap headers.  If a header string does not fit within the 
#' available space, an error will be generated.  In these situations, either
#' shorten the header string or split it into multiple headers that each fit 
#' within the available space. 
#' 
#' @param x The report object.
#' @param left The left page header text.  May be a single string or a vector
#' of strings.
#' @param right The right page header text.  May be a single string or a vector
#' of strings.
#' @param blank_row Whether to create a blank row below the page header.
#' Valid values are 'below' and 'none'.  Default is 'none'.
#' @return The modified report specification.
#' @family report
#' @examples
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' dat <- data.frame(name = rownames(mtcars[1:10, ]), mtcars[1:10, 1:5], 
#'                   stringsAsFactors = FALSE)
#' 
#' # Create the report object
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   page_header("Client: Motor Trend", "Study: Cars") %>% 
#'   titles("MTCARS Sample Report") %>% 
#'   add_content(create_table(dat)) %>% 
#'   page_footer(Sys.time(), right = "Page [pg] of [tpg]")
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # Client: Motor Trend                                                Study: Cars
#' #                              MTCARS Sample Report
#' # 
#' #                name                 mpg cyl   disp   hp  drat
#' #                ----------------------------------------------
#' #                Mazda RX4             21   6    160  110   3.9
#' #                Mazda RX4 Wag         21   6    160  110   3.9
#' #                Datsun 710          22.8   4    108   93  3.85
#' #                Hornet 4 Drive      21.4   6    258  110  3.08
#' #                Hornet Sportabout   18.7   8    360  175  3.15
#' #                Valiant             18.1   6    225  105  2.76
#' #                Duster 360          14.3   8    360  245  3.21
#' #                Merc 240D           24.4   4  146.7   62  3.69
#' #                Merc 230            22.8   4  140.8   95  3.92
#' #                Merc 280            19.2   6  167.6  123  3.92
#' # 
#' # ...
#' # 
#' # 2020-10-17 11:53:51                                                Page 1 of 1
#' @export
page_header <- function(x, left="", right="", blank_row = "none"){

  if (!"report_spec" %in% class(x))
    stop("Page header can only be assigned to an object of class 'report_spec'")
  
  if (length(left) > 5 | length(right) > 5){
    stop("Header string count exceeds limit of 5 strings per side.")
  }
  
  if (is.null(blank_row))
    blank_row <- "none"
  
  if (!blank_row %in% c("below", "none")) {
    stop(paste("blank_row parameter value invalid:", blank_row, "\n", 
      "Valid values are 'below' and 'none'."))
  }
  
  if (!is.null(x$title_hdr))
    stop("Cannot add both a page header and a title header.")
  
  x$page_header_left <- left
  x$page_header_right <- right
  x$page_header_blank_row <- blank_row

  return(x)
}


#' @title
#' Adds a title header block 
#'
#' @description
#' This function adds a title header to an object.  A title header is a special
#' type of title layout that has titles on the left and header information
#' on the right.  
#' @details
#' The \code{title_header} function accepts a set of strings of the desired 
#' title text, and a vector of header strings. The titles will appear on the 
#' left of the title header, and the header strings on the right. To
#' specify multiple titles for the block, pass them to the function 
#' as separate strings.
#' 
#' Title headers may be assigned to a report, a table, a text specification, 
#' or a plot. If assigned to the report, the title header will appear 
#' at the top of the page, and be repeated for every page of the report.  
#' If the title header is assigned to  
#' content, the titles will appear above the content, and be repeated if the 
#' content breaks to the next page.  
#' 
#' One title header function accepts up to 10 titles. Blank rows above or below 
#' the title block may be controlled using the 
#' \code{blank_row} parameter.
#'
#' Each title string must fit within the available space.  The \strong{reporter}
#' package will not wrap titles.  If a title does not fit within the 
#' available space, an error will be generated.  In these situations, either
#' shorten the title or split it into multiple titles that each fit within the
#' available space. 
#' 
#' @param x The object to assign titles to.  Valid objects are a report,
#' table, text, or plot specification.
#' @param ... A set of title strings.
#' @param right A set of header strings to be shown on the right side of the
#' title header.  Pass the header strings as a vector of strings.
#' @param blank_row Where to place a blank row.  Valid values are 'above',
#' 'below', 'both', or 'none'.  Default is 'below'.
#' @param borders Whether and where to place a border. Valid values are 'top',
#' 'bottom', 'all', or 'none'.  Default is 'none'.  
#' @return The modified report.
#' @family report
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create a temporary file
#' tmp <- file.path(tempdir(), "expenses.txt")
#' 
#' # Prepare data
#' dat <- data.frame(category = rownames(USPersonalExpenditure),
#'                   USPersonalExpenditure, stringsAsFactors = FALSE)
#' 
#' # Define table
#' tbl <- create_table(dat) %>% 
#'   title_header("Table 1.0", "US Personal Expenditures from 1940 - 1960",
#'                right = c("Page [pg] of [tpg]", "World Almanac")) %>% 
#'   column_defaults(from = X1940, to = X1960, width = .6, format = "$%.2f") %>%
#'   define(category, label = "Category") %>% 
#'   define(X1940, label = "1940") %>% 
#'   define(X1945, label = "1945") %>% 
#'   define(X1950, label = "1950") %>% 
#'   define(X1955, label = "1955") %>% 
#'   define(X1960, label = "1960") %>% 
#'   footnotes("* In billions of dollars")
#' 
#' # Define report
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   add_content(tbl) 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Display in console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #     Table 1.0                                        Page 1 of 1
#' #     US Personal Expenditures from 1940 - 1960      World Almanac
#' # 
#' #     Category                1940    1945    1950    1955    1960
#' #     ------------------------------------------------------------
#' #     Food and Tobacco      $22.20  $44.50  $59.60  $73.20  $86.80
#' #     Household Operation   $10.50  $15.50  $29.00  $36.50  $46.20
#' #     Medical and Health     $3.53   $5.76   $9.71  $14.00  $21.10
#' #     Personal Care          $1.04   $1.98   $2.45   $3.40   $5.40
#' #     Private Education      $0.34   $0.97   $1.80   $2.60   $3.64
#' # 
#' #     * In billions of dollars
#' @export
title_header <- function(x, ..., right = "", 
                         blank_row = "below", borders = "none") {
  

  diff <- setdiff(class(x), c("list"))
  
  if (identical(diff, character(0)) | 
    all(!diff %in% c("report_spec", "table_spec", "plot_spec", "text_spec"))) {
    stop(paste("Title header can only be assigned ", 
                "to an objects of class 'report_spec', 'title_spec', 'plot_spec'",
                "or 'text_spec'."))
  }
  
  # Create title structure
  ttl_hdr <- structure(list(), class = c("title_hdr", "list"))
  
  if (length(c(...)) > 10)
    stop("Limit of 10 titles reached.") 
  
  if (!is.null(x$titles)){
    x$titles
    stop("Cannot add both titles and a title header.")
  }
  
  if (!is.null(x$page_header_left) | !is.null(x$page_header_right))
    stop("Cannot add both a page header and a title header.")
  
  if (!blank_row %in% c("above", "below", "both", "none"))
    stop(paste("Blank row parameter invalid.  Valid values are", 
               "'above', 'below', 'both', or 'none'."))
  
  if (!all(borders %in% c("top", "bottom", "all", "none")))
    stop(paste("Borders parameter invalid.  Valid values are", 
               "'top', 'bottom', 'all', or 'none'."))
  
  # Assign attributes
  ttl_hdr$titles <-  c(...)
  ttl_hdr$blank_row <- blank_row
  ttl_hdr$borders <- borders
  ttl_hdr$right <- right
  
  x$title_hdr[[length(x$title_hdr) + 1]] <- ttl_hdr
  
  return(x)
  
}

#' @title
#' Adds a title block 
#'
#' @description
#' This function adds one or more titles to an object as a title block.  
#' If added to a report, 
#' the titles will be added to
#' the page template, and thus appear on each page of the report. Titles may 
#' also be added to a table, text, or plot object.
#'
#' @details
#' The titles function accepts a set of strings of the desired title text. To
#' specify multiple titles for the block, pass them to the function 
#' as separate strings.
#' 
#' The titles may be aligned center, left or right using the align parameter.
#' The alignment will be applied to all titles contained in the 
#' block.  To control alignment of titles separately for each title, use 
#' multiple titles functions.
#' 
#' Titles may be assigned to a report, a table, a text specification, or a plot. 
#' If assigned to the report, the title will appear at the top of the page, and
#' be repeated for every page of the report.  If the titles are assigned to  
#' content, the titles will appear above the content, and be repeated if the 
#' content breaks to the next page.  
#' 
#' If titles are assigned to the report,
#' alignment will be oriented to the page body.  If titles are assigned to
#' content, alignment will be oriented to the edge of the content.
#' 
#' One title function accepts up to 10 titles.  However, multiple title 
#' blocks may be added to the same object if needed.  
#' 
#' Blank rows above or below the title block may be controlled using the 
#' \code{blank_row} parameter.
#' 
#' Each title string must fit within the available space.  The \strong{reporter}
#' package will not wrap titles.  If a title does not fit within the 
#' available space, a warning will be generated and the title will be 
#' truncated.  In these situations, either
#' shorten the title or split it into multiple titles that each fit within the
#' available space. 
#'
#' @param x The object to assign titles to.  Valid objects are a report, or
#' a table, text, or plot specification.
#' @param ... A set of title strings.
#' @param align The position to align the titles.  Valid values are 'left', 
#' 'right', 'center' or 'centre'.  For titles, the default is 'center'.
#' @param blank_row Where to place a blank row.  Valid values are 'above',
#' 'below', 'both', or 'none'.  Default is "below".
#' @param borders Whether and where to place a border. Valid values are 'top',
#' 'bottom', 'all', or 'none'.  Default is "none".
#' @return The modified report.
#' @family report
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create a temporary file
#' tmp <- file.path(tempdir(), "expenses.txt")
#' 
#' # Prepare data
#' dat <- data.frame(category = rownames(USPersonalExpenditure),
#'                   USPersonalExpenditure, stringsAsFactors = FALSE)
#' 
#' # Define table
#' tbl <- create_table(dat) %>% 
#'   titles("Table 1.0", "US Personal Expenditures from 1940 - 1960") %>% 
#'   column_defaults(from = X1940, to = X1960, width = .6, format = "$%.2f") %>%
#'   define(category, label = "Category") %>% 
#'   define(X1940, label = "1940") %>% 
#'   define(X1945, label = "1945") %>% 
#'   define(X1950, label = "1950") %>% 
#'   define(X1955, label = "1955") %>% 
#'   define(X1960, label = "1960") %>% 
#'   footnotes("* In billions of dollars")
#' 
#' # Define report
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   add_content(tbl) 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Display in console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' #                               Table 1.0
#' #               US Personal Expenditures from 1940 - 1960
#' # 
#' #     Category                1940    1945    1950    1955    1960
#' #     ------------------------------------------------------------
#' #     Food and Tobacco      $22.20  $44.50  $59.60  $73.20  $86.80
#' #     Household Operation   $10.50  $15.50  $29.00  $36.50  $46.20
#' #     Medical and Health     $3.53   $5.76   $9.71  $14.00  $21.10
#' #     Personal Care          $1.04   $1.98   $2.45   $3.40   $5.40
#' #     Private Education      $0.34   $0.97   $1.80   $2.60   $3.64
#' # 
#' #     * In billions of dollars
#' @export
titles <- function(x, ..., align = "center", blank_row = "below", 
                   borders = "none"){

  # Create title structure
  ttl <- structure(list(), class = c("title_spec", "list"))
  
  if (length(c(...)) > 10)
    stop("Limit of 10 titles reached.") 
  
  if (!is.null(x$title_hdr))
    stop("Cannot add both titles and a title header.")
  
  if (!blank_row %in% c("above", "below", "both", "none"))
    stop(paste("Blank row parameter invalid.  Valid values are", 
               "'above', 'below', 'both', or 'none'."))
  
  if (!all(borders %in% c("top", "bottom", "all", "none")))
    stop(paste("Borders parameter invalid.  Valid values are", 
               "'top', 'bottom', 'all', or 'none'."))

  # Assign attributes
  ttl$titles <-  c(...)
  ttl$blank_row <- blank_row
  ttl$borders <- borders
  ttl$align <- align
  

  x$titles[[length(x$titles) + 1]] <- ttl
  
  return(x)

}


#' @title
#' Adds a footnote block
#'
#' @description
#' The \code{footnotes} function adds one or more footnotes to the report.  
#' If added to the report specification, the footnotes will
#' be added to the page template, and thus appear on each page of the report.
#' Footnotes may also be added directly to table, text, or plot content.
#'
#' @details
#' The \code{footnotes} function accepts a set of strings of the desired 
#' footnote text. The footnotes may be aligned center, left or right using 
#' the align parameter. The user is responsible for adding desired footnote 
#' symbols. Footnote symbols will not be generated automatically.
#' 
#' If footnotes are assigned to the report,
#' alignment will be oriented to the page body.  If footnotes are assigned to
#' a table or text, alignment will be oriented to the edge of the content.
#' 
#' One footnote function accepts up to 25 footnotes.  However, multiple footnote 
#' blocks may be added to the same object.  
#' 
#' Blank rows above or below the footnote block may be controlled using the 
#' \code{blank_row} parameter.
#' 
#' Each footnote string must fit within the available space.  The \strong{reporter}
#' package will not wrap footnotes.  If a footnote does not fit within the 
#' available space, a warning will be generated and the footnote will be 
#' truncated.  In these situations, either
#' shorten the footnote or split it into multiple footnotes that each fit within 
#' the available space. 
#' 
#' @param x The object to assign footnotes to.
#' @param ... A set of footnote strings.
#' @param align The position to align the footnotes.  Valid values are: 'left',
#' 'right', 'center', or 'centre'.
#' @param blank_row Whether to print a blank row above or below the footnote.
#' Valid values are 'above', 'below', 'both', or 'none'.  Default is 'above'.
#' @param borders Whether to print a border above or below the footnote. Valid
#' values are 'top', 'bottom', 'all',  or 'none'.  Default is 'none'.  
#' For fixed width reports, the 
#' border character will be taken from the value of the \code{uchar} parameter
#' on the \code{\link{options_fixed}} function.
#' @param valign The vertical position to align the footnotes.  Valid
#' values are: 'top' and 'bottom'.  For footnotes attached to a report,
#' default is 'bottom'.  For footnotes attached to content, default is 'top'.
#' @return The modified report.
#' @family report
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create a temporary file
#' tmp <- file.path(tempdir(), "expenses.txt")
#' 
#' # Prepare data
#' dat <- data.frame(category = rownames(USPersonalExpenditure),
#'                   USPersonalExpenditure, stringsAsFactors = FALSE)
#' 
#' # Define table
#' tbl <- create_table(dat) %>% 
#'   titles("Table 1.0", "US Personal Expenditures from 1940 - 1960") %>% 
#'   column_defaults(from = X1940, to = X1960, width = .6, format = "$%.2f") %>%
#'   define(category, label = "Category") %>% 
#'   define(X1940, label = "1940") %>% 
#'   define(X1945, label = "1945") %>% 
#'   define(X1950, label = "1950") %>% 
#'   define(X1955, label = "1955") %>% 
#'   define(X1960, label = "1960") %>% 
#'   footnotes("* In billions of dollars")
#' 
#' # Define report
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   add_content(tbl) 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Display in console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #                               Table 1.0
#' #               US Personal Expenditures from 1940 - 1960
#' # 
#' #     Category                1940    1945    1950    1955    1960
#' #     ------------------------------------------------------------
#' #     Food and Tobacco      $22.20  $44.50  $59.60  $73.20  $86.80
#' #     Household Operation   $10.50  $15.50  $29.00  $36.50  $46.20
#' #     Medical and Health     $3.53   $5.76   $9.71  $14.00  $21.10
#' #     Personal Care          $1.04   $1.98   $2.45   $3.40   $5.40
#' #     Private Education      $0.34   $0.97   $1.80   $2.60   $3.64
#' # 
#' #     * In billions of dollars
#' @export
footnotes <- function(x, ..., align = "left", blank_row = "above", 
                      borders = "none"
                      , valign = NULL
                      ){

  # Create footnote structure
  ftn <- structure(list(), class = c("footnote_spec", "list"))
  
  ft <- c(...)
  
  if (length(ft) > 25){
    stop("footnotes function is limited to a maximum of 25 footnotes.")
  }
  
  if (!align %in% c("left", "right"))
    stop(paste("Align parameter invalid. Valid values are 'left' and 'right'"))
  
  if (!is.null(valign)) {
    if (!valign %in% c("top", "bottom"))
      stop(paste("Valign parameter invalid. Valid values are 'top' and 'bottom'"))
  }
  
  if (!blank_row %in% c("above", "below", "both", "none"))
    stop(paste("Blank row parameter invalid.  Valid values are", 
               "'above', 'below', 'both', or 'none'."))
  
  if (any(!borders %in% c("top", "bottom", "all", "none")))
    stop(paste("Borders parameter invalid.  Valid values are", 
               "'top', 'bottom', 'all', or 'none'."))

  ftn$footnotes <- ft
  ftn$blank_row <- blank_row
  ftn$align <- align
  ftn$borders <- borders
  
  if ("report_spec" %in% class(x)) {
    ftn$container <- "report" 
  } else {
    ftn$container <- "content" 
  }
  
  if (is.null(valign)) {
    if ("report_spec" %in% class(x))
      ftn$valign <- "bottom"
    else
      ftn$valign <- "top"
  } else {
    
   ftn$valign <- valign 
  }
  
  x$footnotes[[length(x$footnotes) + 1]] <- ftn

  return(x)

}

#' @title
#' Adds a page footer 
#'
#' @description
#' This function adds a page footer to the report.  The page footer will appear
#' on each page of the report, at the bottom of the page.  The page footer
#' contains three sections: left, center, and right.  Content for each section
#' may be specified with the appropriate parameter.
#'
#' @details
#' Only one page footer is allowed per report.  The page footer will appear 
#' on all pages of the report.  The page footer may contain text on the 
#' left, right, or center. Use the appropriate parameters to specify the 
#' desired text for each section. Multiple strings may be passed to each section 
#' as a vector of strings. 
#' 
#' If the width of the page header
#' string exceeds the available space, an error will be generated. In addition,
#' there is a limit of 5 strings for each page footer section.  
#' 
#' There are two special tokens to generate page numbers: [pg] and [tpg]. 
#' Use [pg] to indicate the current page number.  Use [tpg] to indicate the
#' total number of pages in the report.  These tokens may be placed anywhere 
#' in the page header or page footer. 
#' 
#' Use the \code{blank_row} parameter to control the blank space above the 
#' page footer.
#' 
#' Each footer string must fit within the available space.  The \strong{reporter}
#' package will not wrap footer.  If a footer string does not fit within the 
#' available space, an error will be generated.  In these situations, either
#' shorten the footer string or split it into multiple footers that each fit 
#' within the available space. 
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
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' dat <- data.frame(name = rownames(mtcars[1:10, ]), mtcars[1:10, 1:5], 
#'                   stringsAsFactors = FALSE)
#' 
#' # Create the report object
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   page_header("Client: Motor Trend", "Study: Cars") %>% 
#'   titles("MTCARS Sample Report") %>% 
#'   add_content(create_table(dat)) %>% 
#'   page_footer(Sys.time(), right = "Page [pg] of [tpg]")
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # Client: Motor Trend                                                Study: Cars
#' #                              MTCARS Sample Report
#' # 
#' #                name                 mpg cyl   disp   hp  drat
#' #                ----------------------------------------------
#' #                Mazda RX4             21   6    160  110   3.9
#' #                Mazda RX4 Wag         21   6    160  110   3.9
#' #                Datsun 710          22.8   4    108   93  3.85
#' #                Hornet 4 Drive      21.4   6    258  110  3.08
#' #                Hornet Sportabout   18.7   8    360  175  3.15
#' #                Valiant             18.1   6    225  105  2.76
#' #                Duster 360          14.3   8    360  245  3.21
#' #                Merc 240D           24.4   4  146.7   62  3.69
#' #                Merc 230            22.8   4  140.8   95  3.92
#' #                Merc 280            19.2   6  167.6  123  3.92
#' # 
#' # ...
#' # 
#' # 2020-10-17 11:53:51                                                Page 1 of 1
#' @export
page_footer <- function(x, left="",  center="", right="", blank_row = "above"){

  if (!"report_spec" %in% class(x))
    stop("Page header can only be assigned to an object of class 'report_spec'")
  
  if (length(left) > 5 | length(right) > 5 | length(center) > 5){
    stop("Footer string count exceeds limit of 5 strings per section.")
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


#' @title
#' Adds a page by variable  
#'
#' @description
#' The \code{page_by} function adds a page by variable to a report, table,
#' or plot. 
#' The page by will generate a page break for each value of the page by variable.
#' A label will appear above the content showing the page
#' by variable value. You must be sort the data by the
#' page by variable prior to reporting.
#'
#' @details
#' Only one page by is allowed per report, table, or plot.  The page by 
#' label will 
#' appear on all pages of the object.  The page by label may be aligned on the 
#' left, right, or center. Use the \code{align} parameter to specify the 
#' alignment. 
#' 
#' You must be sort the data by the page by variable prior to reporting.
#' The page by labels will appear in the sorted order.  Failure to sort 
#' the page by variable prior to reporting may produce unexpected results.
#' 
#' @param x The report specification to assign the page by to.
#' @param var The page by variable.  There can be only one page by per report, 
#' and one page by variable.  The page by can be passed either quoted or 
#' unquoted.
#' @param label A label to be used as a prefix to the page by variable value.
#' By default, the label will be assigned to the variable name.  Alternatively,
#' you may specify a string value to use for the label.
#' @param align How to align the page by.  Default value is 'left'. Valid
#' values are 'left', 'right', 'center', or 'centre'.
#' @param blank_row Indicates whether a blank row is desired above or below
#' the page by.  Default value is 'none'.  Valid values are 'above', 'below',
#' 'both', or 'none'.
#' @family report
#' @seealso \code{\link{create_table}} to create a table, and 
#' \code{\link{create_plot}} to create a plot.  
#' @examples 
#' library(reporter)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "iris.txt")
#' 
#' # Sample and sort iris data frame
#' dat <- iris[sample(1:150, 50), ]
#' dat <- dat[order(dat$Species), ]
#' 
#' # Create table
#' tbl <- create_table(dat) %>% 
#'   page_by(Species, "Species: ") %>%  
#'   define(Species, visible = FALSE)
#'   
#' # Create the report object
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   page_header("Sponsor: Iris Society", "Study: flowers") %>% 
#'   titles("Table 2.0", "IRIS Sample Report with Page By") %>% 
#'   add_content(tbl) %>% 
#'   page_footer(Sys.time(), right = "Page [pg] of [tpg]")
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # Sponsor: Iris Society                                           Study: flowers
#' #                                  Table 2.0
#' #                       IRIS Sample Report with Page By
#' # 
#' #              Species: setosa
#' # 
#' #              Sepal.Length Sepal.Width Petal.Length Petal.Width
#' #              -------------------------------------------------
#' #                       5.4         3.9          1.7         0.4
#' #                       4.9         3.1          1.5         0.1
#' #                       4.8         3.1          1.6         0.2
#' #                       5.1         3.5          1.4         0.3
#' #                         5         3.5          1.6         0.6
#' #                         5         3.3          1.4         0.2
#' #                       4.4           3          1.3         0.2
#' #                       5.1         3.5          1.4         0.2
#' #                       5.4         3.4          1.5         0.4
#' #                       4.9         3.6          1.4         0.1
#' #                       4.6         3.1          1.5         0.2
#' #                       4.6         3.2          1.4         0.2
#' #                       5.1         3.3          1.7         0.5
#' # ...
#' # 2020-10-25 19:33:35                                                Page 1 of 3
#' #
#' # Sponsor: Iris Society                                           Study: flowers
#' #                                  Table 2.0
#' #                       IRIS Sample Report with Page By
#' # 
#' #              Species: versicolor
#' # 
#' #              Sepal.Length Sepal.Width Petal.Length Petal.Width
#' #              -------------------------------------------------
#' #                       4.9         2.4          3.3           1
#' #                       6.3         3.3          4.7         1.6
#' #                       6.1         2.8          4.7         1.2
#' #                         6         2.9          4.5         1.5
#' #                       6.7           3            5         1.7
#' #                       5.6           3          4.5         1.5
#' #                       5.8         2.7          4.1           1
#' #                       6.7         3.1          4.7         1.5
#' #                       6.1         2.9          4.7         1.4
#' #                         5           2          3.5           1
#' #                       5.9         3.2          4.8         1.8
#' #                       5.5         2.5            4         1.3
#' #                         7         3.2          4.7         1.4
#' #                       6.3         2.3          4.4         1.3
#' #                       6.1         2.8            4         1.3
#' #                         6         2.2            4           1
#' #                       5.5         2.6          4.4         1.2
#' #                         6         3.4          4.5         1.6
#' #                         5         2.3          3.3           1
#' #                       5.5         2.4          3.7           1
#' # ...
#' # 2020-10-25 19:33:35                                                Page 2 of 3
#' #
#' # Sponsor: Iris Society                                           Study: flowers
#' #                                  Table 2.0
#' #                       IRIS Sample Report with Page By
#' # 
#' #              Species: versicolor
#' # 
#' #              Sepal.Length Sepal.Width Petal.Length Petal.Width
#' #              -------------------------------------------------
#' #                       6.3         3.4          5.6         2.4
#' #                       7.9         3.8          6.4           2
#' #                       6.7         3.1          5.6         2.4
#' #                       6.2         2.8          4.8         1.8
#' #                       6.7         3.3          5.7         2.5
#' #                       6.2         3.4          5.4         2.3
#' #                       5.6         2.8          4.9           2
#' #                       7.7         3.8          6.7         2.2
#' #                       7.7         2.6          6.9         2.3
#' #                       6.9         3.1          5.4         2.1
#' #                       6.5         3.2          5.1           2
#' #                       6.1         2.6          5.6         1.4
#' #                       5.7         2.5            5           2
#' #                       6.5           3          5.8         2.2
#' #                       6.3         2.8          5.1         1.5
#' #                       7.6           3          6.6         2.1
#' #                       6.3         2.5            5         1.9
#' # ...
#' # 2020-10-25 19:33:35                                                Page 3 of 3 
#' @export
page_by <- function(x, var, label = NULL, align = "left",
                    blank_row = "below") {
  
  
  # Create page by structure
  pb <- structure(list(), class = c("page_by", "list"))

  if (!align %in% c("left", "right", "center", "centre" )){
    stop(paste("align value is invalid. Valid values are 'left', 'right',",
               "'center', or 'centre'."))
  }
  
  if (!blank_row %in% c("above", "below", "both", "none" )){
    stop(paste("blank_row value is invalid. Valid values are 'above', 'below',",
               "'both', or 'none'."))
  }
  
  var_c <- as.character(substitute(var, env = environment()))
  
  pb$var <- var_c
  if (is.null(label))
    pb$label <- paste0(var_c, ": ")
  else
    pb$label <- label
  
  pb$align <- align
  pb$blank_row <- blank_row

  x$page_by <- pb
  
  return(x)
  
}

# Functions ---------------------------------------------------------------


#' @title Add content to a report
#' @description This function adds an object to the report content list. A 
#' report will accept multiple pieces of content.  The \code{add_content} 
#' function also controls overall alignment of the content on the page, and
#' whether there is a page break before or after.
#' @details 
#' The \code{add_content} function adds a piece of content to a report. For a 
#' text report, valid objects are a table or text object.  For an RTF or PDF
#' report, valid objects are a table, text, or plot object.  See 
#' \code{\link{create_table}}, \code{\link{create_text}}, or 
#' \code{\link{create_plot}} for further 
#' information on how to create content objects.  
#' 
#' Content will be
#' appended to the report in the order it is added.  By default, a page break
#' is added after the content.  You can stack two pieces of content together
#' closely by setting the \code{page_break} parameter to FALSE, and 
#' the \code{blank_row} parameter to "none".
#' 
#' @param x The report_spec to append content to.
#' @param object The object to append.
#' @param page_break Whether to add a page break after the object. 
#' Valid values are TRUE or FALSE.  You can manipulate the \code{page_break} 
#' parameter to add multiple objects to the same page.  
#' @param align How to align the content.  Valid values are 'left', 'right',
#' 'center', and 'centre'.
#' @param blank_row Whether to put a blank row above or below the content.
#' Valid values are 'above', 'below', 'both', or 'none'.
#' @return The modified report_spec.
#' @family report
#' @seealso \code{\link{create_table}}, \code{\link{create_text}}, and
#' \code{\link{create_plot}} to create content for a report. 
#' @examples
#' library(reporter)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Create first table 
#' tbl1 <- create_table(mtcars[1:5, 1:6]) %>% 
#'   column_defaults(width = .5) 
#' 
#' # Create second table
#' tbl2 <- create_table(mtcars[6:10, 1:6], headerless=TRUE) %>% 
#'   column_defaults(width = .5) 
#' 
#' # Create the report object
#' rpt <- create_report(tmp) %>%
#'   titles("MTCARS Sample Data", align = "left") %>%
#'   add_content(tbl1, page_break = FALSE, align = "left", blank_row = "none") %>%
#'   add_content(tbl2, page_break = FALSE, align = "left") %>%
#'   add_content(create_text("* NOTE: Above table is actually two tables stacked."))
#' 
#' # Write the report to the file system
#' res <- write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # MTCARS Sample Data
#' # 
#' #    mpg    cyl   disp     hp   drat     wt
#' # -----------------------------------------
#' #     21      6    160    110    3.9   2.62
#' #     21      6    160    110    3.9  2.875
#' #   22.8      4    108     93   3.85   2.32
#' #   21.4      6    258    110   3.08  3.215
#' #   18.7      8    360    175   3.15   3.44
#' #   18.1      6    225    105   2.76   3.46
#' #   14.3      8    360    245   3.21   3.57  
#' #   24.4      4  146.7     62   3.69   3.19
#' #   22.8      4  140.8     95   3.92   3.15
#' #   19.2      6  167.6    123   3.92   3.44
#' # 
#' # * NOTE: Above table is actually two tables stacked.
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
#' specifications provided in the object.
#' @details 
#' The function renders the report in the requested format, and writes it
#' to the location specified in the report \code{file_path} parameter.  Attempts
#' to write an object that is not of class "report_spec" will generate an error.
#' 
#' The \code{write_report} function is a driver for very complex set of 
#' rendering functions. The rendering functions
#' perform most of the advanced functionality of the \strong{reporter} package:
#' generating spanning headers, page wrapping and breaking, creating stub 
#' columns, etc.  When things go wrong, they will usually go wrong during this
#' function call.  For that reason, although this function can be part of 
#' the pipeline that creates the report object, it is best to call 
#' \code{write_report} independently, to help isolate any issues from the 
#' report definition procedure.
#' 
#' @param x The report object to write.
#' @param file_path The file name and path to write the report to.  If supplied,
#' this parameter overrides the \code{file_path} parameter on the 
#' \code{create_report} function. Default is NULL.
#' @param output_type The output file type.  This parameter will override
#' the \code{output_type} on the \code{create_report} function.  This 
#' parameter can be used to output the same report object to 
#' multiple output types. Default value is NULL, meaning it will not override
#' the \code{create_report} value.  Valid values are 'TXT', 'RTF', and 'PDF'.
#' @param preview Whether to write the entire report, or a report preview.
#' A report preview is a subset of pages of the report.  The default value is 
#' NULL, meaning the entire report will be written.  You may also pass 
#' a number of pages to write.  For example, passing the number 1 will print
#' the first page, while passing a 5 will print the first five pages.
#' @return The report spec, with settings modified during rendering.  These 
#' modified settings can sometimes be useful for documentation, and for 
#' debugging issues with the procedure.
#' @family report
#' @examples 
#' library(reporter)
#' library(fmtr)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "beaver2.txt")
#' 
#' # Take Sample of Data
#' dat <- beaver2[sample(1:100, 15), ]
#' 
#' # Create format for active variable
#' fmt <- value(condition(x == 0, "No"),
#'              condition(x == 1, "Yes"))
#' 
#' # Create the table
#' tbl <- create_table(dat) %>% 
#'   titles("Table 1.0", "BEAVERS Sample Report") %>% 
#'   column_defaults(width = .75) %>% 
#'   define(day, label = "Day", format = "Day %s") %>% 
#'   define(time, label = "Time") %>% 
#'   define(temp, label = "Temperature", width = 1, format = "%.1f") %>% 
#'   define(activ,label = "Active", format = fmt) %>% 
#'   footnotes("* NOTE: Data on beaver habits")
#' 
#' # Create the report object
#' rpt <- create_report(tmp) %>% 
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report to the file system
#' res <- write_report(rpt)
#' 
#' # Write the modified report object to the console
#' print(res)
#' 
#' # Write the report to console
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #                 Table 1.0
#' #           BEAVERS Sample Report
#' # 
#' #      Day      Time  Temperature    Active
#' # -----------------------------------------
#' #  Day 307      1020         37.2        No
#' #  Day 307      1030         37.2        No
#' #  Day 307       940         36.7        No
#' #  Day 307      1340         37.1        No
#' #  Day 307      1410         37.2        No
#' #  Day 307      1400         37.1        No
#' #  Day 307      1130         36.9        No
#' #  Day 307      1140         37.0        No
#' #  Day 307      1120         37.0        No
#' #  Day 307      1000         37.1        No
#' #  Day 307      1250         37.0        No
#' #  Day 307      2100         37.9       Yes
#' #  Day 307      1210         37.0        No
#' #  Day 307      1740         38.0       Yes
#' #  Day 308       130         37.8       Yes
#' # 
#' # * NOTE: Data on beaver habits
#' @export
write_report <- function(x, file_path = NULL, 
                         output_type = NULL, preview = NULL) {
  
  
  if (!"report_spec" %in% class(x)) {
    
    stop(paste0("report object missing or invalid."))
  }
  
  if (!is.null(file_path)) {
    
    x$file_path <- file_path

  }
  
  # Trap missing or invalid output_type parameter
  if (!is.null(output_type)) {
    if (!toupper(output_type) %in% c("TXT", "PDF", "RTF")) {
      
      stop(paste0("output_type parameter on create_report() ",
                  "function is invalid: '", output_type,
                  "'\n\tValid values are: 'TXT', 'PDF', 'RTF'."))
    }
    x$output_type <- toupper(output_type)

    x <- options_fixed(x, editor = x$editor, 
                       cpuom = x$cpuom, lpuom = x$lpuom,
                       min_margin = x$min_margin, blank_margins = x$blank_margins,
                       font_size = x$font_size, line_size = x$user_line_size, 
                       line_count = x$user_line_count,
                       uchar = x$uchar)
  }
  

  if (nchar(x$file_path) > 0 & length(getExtension(x$file_path)) == 0) {
      x$modified_path <- paste0(x$file_path, ".", tolower(x$output_type))
  } else 
    x$modified_path <- x$file_path
  
  #print(x$modified_path)
  
  if (!is.null(preview)) {
    if (is.numeric(preview)) {
      if (preview > 0)
        x$preview <- preview
      else
        stop("Preview value must be greater than zero.")
    } else
      stop("Preview value must be a number")
  }
  
  # Trap missing or invalid output_type parameter
  if (x$modified_path == "") {
    
    stop(paste0("report file_path missing or invalid."))
  } else {
    
    if (file.exists(x$modified_path))
      file.remove(x$modified_path)
    
    if (!dir.exists(dirname(x$modified_path)))
      dir.create(dirname(x$modified_path))
    
  }

  ret <- ""

  if (x$output_type == "TXT") {
    
    ret <- write_report_text(x)
    
  } else if (x$output_type == "RTF") {
    
    if (tolower(x$font) == "fixed")
      ret <- write_report_rtf(x)
    else 
      ret <- write_report_rtf2(x)
  
  } else if (x$output_type == "PDF") {
    

    ret <- write_report_pdf(x)


  } else {
   stop(paste("Output type currently not supported:", x$output_type))
  }
  # } else if (x$output_type == "docx") {
  #   ret <- write_report_docx(x, ...)
  # }
  
  log_logr(ret)
  
  return(ret)
}


#' @title Prints the report specification
#' @description A function to print the report specification.
#' The \strong{print} function will print the report spec in summary 
#' form by default.  To print in list form, set the \code{verbose} parameter
#' to TRUE.
#' @param x The report spec.
#' @param ... Additional parameters to pass to the underlying print function.
#' @param verbose Whether to print the report object in verbose (list) form
#' or summary form.  The default is FALSE.
#' @seealso 
#' \code{\link{create_report}} function to create a report specification.
#' @return The report spec, invisibly.
#' @family report
#' @examples 
#' library(reporter)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Create the table
#' tbl <- create_table(mtcars) %>% 
#'   titles("Table 1.0", "MTCARS Sample Report") %>% 
#'   footnotes("* NOTE: Data from 1974")
#' 
#' # Create the report object
#' rpt <- create_report(tmp) %>% 
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report to the file system
#' res <- write_report(rpt)
#' 
#' # Write the modified report object to the console
#' print(res)
#' 
#' # # A report specification: 1 pages
#' # - file_path: 'C:/Users/User/AppData/Local/Temp/RtmpWQybXs/mtcars.txt'
#' # - output_type: TXT
#' # - units: inches
#' # - orientation: landscape
#' # - line size/count: 108/45
#' # - content: 
#' # # A table specification:
#' # - data: data.frame 'mtcars' 32 rows 11 cols
#' # - show_cols: all
#' # - use_attributes: all
#' # - title 1: 'Table 1.0'
#' # - title 2: 'MTCARS Sample Report'
#' # - footnote 1: '* NOTE: Data from 1974'
#' @import crayon
#' @export
print.report_spec <- function(x, ..., verbose = FALSE){
  
  
  if (verbose == TRUE) {
    
    # If verbose mode is requested, print in list form
    for (nm in names(x)) {
      
      cat("$", nm, "\n", sep = "")
      if (nm == "content") {
        
        for (i in seq_along(x[[nm]]))
          print(x[[nm]][[i]], ..., verbose = TRUE)
      }
      else  {
        
        print(x[[nm]], ...)
      }
      cat("\n")
    }
    
    
  } else {
    
    grey60 <- make_style(grey60 = "#999999")

    # Print header
    cat(grey60("# A report specification: "))
    if (!is.null(x$pages)) {
      if (!is.null(x$preview)) {
        if (x$preview > 0)
          cat(grey60(paste0(as.character(x$pages), " pages"), red(" PREVIEW")))
        else
          cat(grey60(paste0(as.character(x$pages), " pages")))
      } else {
        
        cat(grey60(paste0(as.character(x$pages), " pages")))
      }
    }
    
    # Print key attributes
    cat("\n")
    cat(paste0("- file_path: '", x$modified_path, "'\n"))
    cat(paste0("- output_type: ", x$output_type, "\n"))
    cat(paste0("- units: ", x$units, "\n"))
    cat(paste0("- orientation: ", x$orientation, "\n"))
    cat(paste0("- margins: top ", x$margin_top, " bottom ", 
               x$margin_bottom, " left ", x$margin_left, 
               " right ", x$margin_right, "\n"))
    if (!is.null(x$line_size)) 
      cat(paste0("- line size/count: ", x$line_size, "/", x$line_count, "\n"))
    
    # if (!is.null(x$column_widths)) {
    #   cat("- column widths: \n")
    #   cat(x$column_widths)
    #   
    # }
    if (!is.null(x$page_by)) {
      cat(paste0("- page by: ", x$page_by$var, "\n"))
      
    }
    
    # Print page header
    if (!is.null(x$page_header_left) |
        !is.null(x$page_header_right)) {
      cat("- page_header:")
      if (!is.null(x$page_header_left)) {
        cat(" left=")
        cat(paste0(x$page_header_left, collapse = ", "))
      }
      if (!is.null(x$page_header_right)) {
        cat(" right=")
        cat(paste0(x$page_header_right, collapse = ", "))
      }
      cat("\n")
    }
    
    print_title_header(x$title_hdr)

    
    print_titles(x$titles)
    
    print_footnotes(x$footnotes)
    

    
    # Print page footer
    if (!is.null(x$page_footer_left) | 
        !is.null(x$page_footer_center) |
        !is.null(x$page_footer_right)) {
      cat("- page_footer:")
      if (!is.null(x$page_footer_left)) {
        cat(" left=")
        cat(paste0(x$page_footer_left, collapse = ", "))
      }
      if (!is.null(x$page_footer_center)) {
        cat(" center=")
        cat(paste0(x$page_footer_center, collapse = ", "))
      }
      if (!is.null(x$page_footer_right)) {
        cat(" right=")
        cat(paste0(x$page_footer_right, collapse = ", "))
      }
      cat("\n")
    }
    if (!is.null(x$content)) {
      cat("- content: \n")
      for (i in seq_along(x$content)) {
        
        print(x$content[[i]]$object)
        
      }
    }
    
  }
  
  invisible(x)
}


# Write Registration File -------------------------------------------------

#' @title 
#' Create a registration file 
#' @description 
#' This function will create a registration file to help determine
#' the correct \code{cpuom} and \code{lpuom} for your editor/printer.  
#' @details
#' The \code{cpi} and \code{lpi} are 
#' used in \code{output_type = "TXT"} to determine available space on
#' the page. The registration file can help determine the correct settings
#' for the target text editor and printer.  Failure to set the 
#' correct characters per 
#' unit of measure (cpuom) and lines per unit of measure (lpuom) may result
#' in misalignment of content on the page when printing text output.
#' 
#' @section How to Use the Registration File:
#' To use the registration file, first decide the units of measure you
#' wish to use, inches or centimeters.  Next, create the registration file
#' by calling the \code{write_registration_file} function.  Then print the 
#' registration file.
#' 
#' Once the registration file is printed, take a ruler and measure both the 
#' horizontal and vertical registration lines from zero to 60 in the desired
#' units of measure.  For example, if your units of measure is 'inches', measure
#' the registration lines in inches.    
#' 
#' Record the 
#' distance measured in each direction.  For each direction, divide 60 by the 
#' distance measured, and round to three decimal places.  The horizontal 
#' result is the characters per unit of measure (cpuom).  The vertical result
#' is the lines per unit of measure (lpuom). To get an accurate printing 
#' of text reports, 
#' assign these values to the \code{cpuom} and \code{lpuom} parameters
#' on the \code{\link{options_fixed}} function.
#' 
#' For best results, test the calculated values by printing some reports and
#' checking for undesired page breaks or wrapped lines.  If necessary, adjust
#' the calculated cpuom and lpuom values until all content stays within 
#' the available space without wrapping or breaking.
#' 
#' @param file_path The full or relative file name and path to create the 
#' registration file.
#' @examples 
#' library(reporter)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "reg.txt")
#' 
#' # Create the registration file 
#' write_registration_file(tmp)
#' 
#' # Write registration file to the console
#' writeLines(readLines(tmp))
#' 
#' # 0--------+---------+---------+---------+---------+---------+
#' # -       10        20        30        40        50        60
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # + 10
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # + 20
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # + 30
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # + 40
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # + 50
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # -
#' # + 60
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




