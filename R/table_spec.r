


# Table Spec Functions ---------------------------------------------------------
#' @title Create a table
#' @description 
#' The \code{create_table} function creates a table object to which 
#' further specifications can be added.  The object is implemented as an 
#' S3 object of class 'table_spec'. The object can be added to a report
#' using the \code{\link{add_content}} function.
#' @details 
#' A table specification is a container to hold information about a table.  The 
#' only required information for a table is the table data.  All other 
#' parameters and functions are optional.
#' 
#' By default, the table will display all columns in the data frame.  To change
#' this default, use the \code{show_cols} parameter.  Setting this parameter
#' to 'none' will display none of the columns in the data, unless they are
#' explicitly defined with a \code{\link{define}} function.  
#' 
#' The \code{show_cols} parameter also accepts a vector of quoted column names.
#' The column name vector performs two functions.  First, it will display only
#' those columns on the report.  Second, it will display them in the order
#' specified in the vector.  The \code{show_cols} parameter is the easiest way 
#' to change the column order of the table.
#' 
#' The \code{create_table} function also provides the capabilities to create
#' a "headerless" table.  A headerless table is useful when combining two tables 
#' into one report.
#' 
#' Since the purpose of the \strong{rptr} package is to create statistical 
#' reports, the \code{create_table} function makes it easy to add population
#' counts to the table header.  These population counts are added to column
#' labels and spanning header labels using the function indicated in the 
#' \code{n_format} function. The package provides four population count
#' formatting functions.  You may create your own formatting function if one of
#' these functions does not meet your needs.  See \code{\link{upcase_parens}}
#' for further details.
#' 
#' @param x The data frame or tibble from which to create the table object.
#' @param show_cols Whether to show all columns by default.  Valid values are
#' "all", "none", or a vector of column names.  "all" means show all columns, 
#' unless overridden by the column definitions.  
#' "none" means don't show any 
#' columns unless specified in the column definitions.  If a vector of column
#' names is supplied, those columns will be shown in the report in the order
#' specified, whether or not a definition is supplied.  
#' @param first_row_blank Whether to place a blank row under the table header.
#' Valid values are TRUE or FALSE.  Default is FALSE.
#' @param n_format The formatting function to apply to the header "N=" label. 
#' The default formatting function is \code{\link{upcase_parens}}. 
#' @param headerless Whether to create a headerless table.  Default is FALSE. 
#' @family table
#' @seealso \code{\link{create_report}} to create  report, 
#' \code{\link{create_text}} to create text content, and 
#' \code{\link{add_content}} to append content to a report.
#' @examples 
#' library(rptr)
#' library(magrittr)
#' 
#' # Create temp file path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' #Subset cars data
#' dat <- mtcars[1:10, 1:7]
#' 
#' # Calculate means for all columns
#' dat_sum <- data.frame(all_cars = "All cars average", as.list(sapply(dat, mean)))
#' 
#' # Get vehicle names into first column
#' dat_mod <- data.frame(vehicle = rownames(dat), dat)
#'                       
#' # Create table for averages
#' tbl1 <- create_table(dat_sum) %>% 
#'         titles("Table 1.0", "MTCARS Sample Data") %>% 
#'         define(all_cars, label = "", width = 2) %>% 
#'         define(mpg, format = "%.1f") %>% 
#'         define(disp, format = "%.1f") %>% 
#'         define(hp, format = "%.0f") %>% 
#'         define(qsec, format = "%.2f")
#' 
#' # Create table for modified data
#' tbl2 <- create_table(dat_mod, headerless = TRUE) %>% 
#'         define(vehicle, width = 2) 
#' 
#' # Create the report object
#' rpt <- create_report(tmp) %>% 
#'   add_content(tbl1, align = "left", page_break = FALSE) %>% 
#'   add_content(tbl2, align = "left") 
#' 
#' # Write the report to the file system
#' write_report(rpt)
#' 
#' # Write report to console
#' writeLines(readLines(tmp))
#' 
#' #                                 Table 1.0
#' #                             MTCARS Sample Data
#' # 
#' #                             mpg    cyl   disp     hp   drat     wt   qsec
#' # -------------------------------------------------------------------------
#' # All cars average           20.4    5.8  208.6    123  3.538  3.128  18.58
#' # 
#' # Mazda RX4                    21      6    160    110    3.9   2.62  16.46
#' # Mazda RX4 Wag                21      6    160    110    3.9  2.875  17.02
#' # Datsun 710                 22.8      4    108     93   3.85   2.32  18.61
#' # Hornet 4 Drive             21.4      6    258    110   3.08  3.215  19.44
#' # Hornet Sportabout          18.7      8    360    175   3.15   3.44  17.02
#' # Valiant                    18.1      6    225    105   2.76   3.46  20.22
#' # Duster 360                 14.3      8    360    245   3.21   3.57  15.84
#' # Merc 240D                  24.4      4  146.7     62   3.69   3.19     20
#' # Merc 230                   22.8      4  140.8     95   3.92   3.15   22.9
#' # Merc 280                   19.2      6  167.6    123   3.92   3.44   18.3
#' # 
#' @export
create_table <- function(x, show_cols = "all", first_row_blank=FALSE,
                         n_format = upcase_parens, headerless = FALSE) {
  if (is.null(x)) {
    stop("Data parameter 'x' missing or invalid.") 
    
  }
  
  if (!"data.frame" %in% class(x)) {
    stop(paste("ERROR: data parameter 'x' on",
               "page_template() function is invalid.",
               "\n\tValid values are a data.frame or tibble."))
  }
  
  ret <- structure(list(), class = c("table_spec", "list"))

  ret$data <- x
  ret$n_format <- n_format
  ret$col_defs <- list()
  ret$col_spans <- list()
  ret$show_cols <- show_cols
  ret$first_row_blank <- first_row_blank
  ret$headerless <- headerless
  ret$stub <- NULL

  return(ret)

}

#' @title Defines a column 
#' @description A function to define the specification for a table column.  The
#' \code{define} function contains a variety of a parameters to control the 
#' appearance of the report.  Using the \code{define} function, you can control
#' simple options like column alignment and format, but also control more 
#' sophisticated options like page wrapping and page breaking.
#' @details 
#' Column definitions are optional.  By default, all columns in the data
#' are displayed in the order and with the formatting attributes assigned to 
#' the data frame.  The report will use attributes assigned to the data frame 
#' such as 'width', 'justify', 'label', and 'format'.  In other words, 
#' some control over the column 
#' formatting is available by manipulating the data frame attributes prior
#' to assigning the data frame to \code{\link{create_table}}.
#' 
#' The \code{define} function is used to provide additional control over
#' column appearance.  For example, you may use the \code{define} function
#' to assign an "N=" population count, eliminate duplicates from the column,
#' or place a blank row after each unique value of the column. 
#' See the parameters below for additional options.
#' 
#' Some of the parameters on the \code{define} function are used in the 
#' creation of a table stub.  See the \code{\link{stub}} function for additional
#' details.
#' 
#' @param x The table containing a variable to define.
#' @param var The unquoted variable name to define a column for.  
#' @param label The label to use for the column header.  If a label is assigned
#' to the label column attribute, it will be used as a default.  Otherwise,
#' the column name will be used.
#' @param format The format to use for the column data.  The format can 
#' be a string format, a formatting function, a lookup list, or a format object. 
#' All formatting is performed by the \code{\link[fmtr]{fmtr}} package.  For 
#' additional information, see the help for that package.
#' @param align The column alignment.  Valid values are "left", "right", 
#' "center", and "centre".
#' @param label_align How to align the header labels for this column.
#' Valid values are "left", "right", "center", and "centre".
#' @param width The width of the column in the specified units of measure.
#' The units of measure are specified on the \code{uom} parameter of the
#' \code{\link{create_report}} function.  If no width is supplied, the
#' \code{\link{write_report}} function will assign a default width based on the 
#' width of the column data and the label.  \code{write_report} will not set a 
#' column width less than the width of the largest word in the data or label.
#' In other words, \code{write_report} will not break words. 
#' @param visible Whether or not the column should be visible on the report.
#' This parameter can be used as a simple way to drop columns from the report.
#' @param n The n value to place in the "N=" header label.  Formatting for
#' the n value will be performed by the formatting function assigned to the 
#' \code{n_format} parameter on \code{\link{create_table}}.
#' @param blank_after Whether to place a blank row after unique values of this
#' variable.  Valid values are TRUE or FALSE.  Default is FALSE.
#' @param dedupe Whether to dedupe the values for this variable.  Variables
#' that are deduped only show the value on the first row for each group.  This 
#' option is commonly used for grouping variables.
#' @param id_var Whether this variable should be considered an ID variable.
#' ID variables are retained on each page when the page is wrapped. ID variables
#' are also moved to the far left of the page.
#' @param page_wrap Force a page wrap on this variable.  A page wrap is a vertical
#' page break necessary when the table is too wide to fit on a single page.
#' The excess variables will be wrapped to the next page.  Page wraps will
#' continue until all columns are displayed.  Use the \code{id_vars}
#' parameter to identify rows across wrapped pages. 
#' @param indent How much to indent the column values.  Parameter takes a 
#' numeric value that will be interpreted according to the \code{uom} 
#' (Unit Of Measure) setting on the report.  This parameter can be used to 
#' help create a stub column.  The default value is NULL, meaning the column
#' should not be indented.
#' @param label_row Whether the values of the variable should be used to
#' create a label row.  Valid values are TRUE or FALSE.  Default is FALSE.
#' If \code{label_row} is set to TRUE, the dedupe parameter will also be 
#' set to TRUE.  This parameter is often used in conjunction with the 
#' \code{\link{stub}} function to create a stub column.
#' @return The modified table spec.
#' @family table
#' @examples
#' library(rptr)
#' library(magrittr)
#'  
#' # Create temp file name
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Prepare data
#' dat <- mtcars[1:10, ]
#' dat <- data.frame(vehicle = rownames(dat), dat)
#' 
#' # Define table
#' tbl <- create_table(dat, show_cols = 1:8) %>% 
#'   define(vehicle, label = "Vehicle", width = 3, id_var = TRUE, align = "left") %>% 
#'   define(mpg, label = "Miles per Gallon", width = 1) %>% 
#'   define(cyl, label = "Cylinders", format = "%.1f") %>% 
#'   define(disp, label = "Displacement") %>% 
#'   define(hp, label = "Horsepower", page_wrap = TRUE) %>% 
#'   define(drat, visible = FALSE) %>% 
#'   define(wt, label = "Weight") %>% 
#'   define(qsec, label = "Quarter Mile Time", width = 1.5) 
#' 
#' 
#' # Create the report
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   titles("Listing 2.0", "MTCARS Data Listing with Page Wrap") %>% 
#'   add_content(tbl, align = "left") %>% 
#'   page_footer(right = "Page [pg] of [tpg]")
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Send report to console for viewing
#' writeLines(readLines(tmp))
#' 
#' #                                  Listing 2.0
#' #                       MTCARS Data Listing with Page Wrap
#' # 
#' #                                         Miles per
#' # Vehicle                                    Gallon Cylinders Displacement
#' # ------------------------------------------------------------------------
#' # Mazda RX4                                      21       6.0          160
#' # Mazda RX4 Wag                                  21       6.0          160
#' # Datsun 710                                   22.8       4.0          108
#' # Hornet 4 Drive                               21.4       6.0          258
#' # Hornet Sportabout                            18.7       8.0          360
#' # Valiant                                      18.1       6.0          225
#' # Duster 360                                   14.3       8.0          360
#' # Merc 240D                                    24.4       4.0        146.7
#' # Merc 230                                     22.8       4.0        140.8
#' # Merc 280                                     19.2       6.0        167.6
#' # 
#' # ...
#' # 
#' #                                                                    Page 1 of 2
#' #                                  Listing 2.0
#' #                       MTCARS Data Listing with Page Wrap
#' # 
#' # Vehicle                              Horsepower Weight  Quarter Mile Time
#' # -------------------------------------------------------------------------
#' # Mazda RX4                                   110   2.62              16.46
#' # Mazda RX4 Wag                               110  2.875              17.02
#' # Datsun 710                                   93   2.32              18.61
#' # Hornet 4 Drive                              110  3.215              19.44
#' # Hornet Sportabout                           175   3.44              17.02
#' # Valiant                                     105   3.46              20.22
#' # Duster 360                                  245   3.57              15.84
#' # Merc 240D                                    62   3.19                 20
#' # Merc 230                                     95   3.15               22.9
#' # Merc 280                                    123   3.44               18.3
#' # 
#' # ...
#' # 
#' #                                                                    Page 2 of 2
#' @export
define <- function(x, var, label = NULL, format = NULL, 
                   align=NULL, label_align=NULL, width=NULL,
                   visible=TRUE, n = NULL, blank_after=FALSE,
                   dedupe=FALSE, id_var = FALSE, page_wrap = FALSE,
                   indent = NULL, label_row = FALSE) {
  
  # Check that variable exists
  var_c <- as.character(substitute(var, env = environment()))
  if (!is.null(x$data)) {
    if (!var_c %in% names(x$data)) {
      stop(paste0("Variable '", var_c, "' does not exist in data."))
      
    }
  }

  def <- structure(list(), class = c("col_def", "list"))
  
  def$var = deparse(substitute(var, env = environment()))
  def$var_c = var_c
  def$label = label
  def$format = format
  #def$col_type = col_type  # Not fully defined yet, but not ready to kill
  def$align = align
  def$label_align = if (is.null(label_align) & !is.null(align))
                                align else label_align
  def$width = width
  def$visible = visible
  def$n = n
  def$blank_after = blank_after
  def$dedupe = dedupe
  def$id_var = id_var
  def$page_wrap = page_wrap
  def$indent = indent
  def$label_row = label_row
  if (label_row == TRUE)
    def$dedupe <- TRUE

  x$col_defs[[length(x$col_defs) + 1]] <- def

  return(x)
}

#' @title Defines a spanning header
#' @description Create a header that spans multiple columns.  Spanning headers
#' are used to group related columns.  Such groupings are a common 
#' feature of statistical reports.
#' @details 
#' A spanning header is a label and underline that spans one or more 
#' columns.  A spanning header is defined minimally by identifying 
#' the columns to be spanned, and a label.  A label alignment and "N="
#' value may also be specified.
#' 
#' There are three ways to identify the columns to span: by a sequence of 
#' column positions, by a vector of column names, or by a named vector 
#' indicating "from" and "to" column names.  When identifying the spanning
#' column names, all names should be quoted.
#' @param x The table object to add spanning headers to.
#' @param span_cols The columns to span.  The spanning columns may be defined as
#' a vector of column positions or quoted names. You may also supply a named 
#' vector, with the names "from" and "to" equal to the starting and 
#' ending columns to span.
#' @param label The label to apply to the spanning header.
#' @param label_align The alignment to use for the label. Valid values are 
#' "left", "right", "center", and "centre".  The default for spanning columns
#' is "center".
#' @param level The level to use for the spanning header.  The lowest
#' spanning level is level 1, the next level above is level 2, and so on.  
#' By default, the level is set to 1.
#' @param n The population count to use for the "N=" label on the spanning 
#' header. The "N=" label will be formatted according to the \code{n_format}
#' parameter on the \code{\link{create_table}} function.
#' @return The modified table spec.
#' @family table
#' @examples 
#' library(rptr)
#' library(magrittr)
#' 
#' # Create temporary path
#' tmp <- file.path(tempdir(), "mtcars.txt")
#' 
#' # Prepare Data
#' dat <- mtcars[1:10, ]
#' df <- data.frame(vehicle = rownames(dat), dat)
#' 
#' # Define Table with spanning headers
#' tbl <- create_table(df) %>% 
#'   titles("Table 1.0", "MTCARS Spanning Headers") %>% 
#'   spanning_header(span_cols = c("mpg", "cyl", "hp"),
#'                   label = "Span 1", n = 10) %>%
#'   spanning_header(span_cols = c("drat", "wt", "qsec"),
#'                   label = "Span 2", n = 10) %>%
#'   spanning_header(span_cols = c("vs", "gear", "carb"),
#'                   label = "Span 3", n = 10) %>%
#'   spanning_header(span_cols = c(from = "drat", to = "carb"), 
#'                   label = "Super Span", level = 2) %>%
#'   define(vehicle, label = "Vehicle") %>% 
#'   define(mpg, format = "%.1f") %>% 
#'   define(disp, visible = FALSE) %>% 
#'   define(am, visible = FALSE) 
#' 
#' # Create Report and add table 
#' rpt <- create_report(tmp) %>%
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report
#' res <- write_report(rpt)
#' 
#' # View in console
#' writeLines(readLines(tmp))
#' 
#' #                                    Table 1.0
#' #                              MTCARS Spanning Headers
#' # 
#' #                                                         Super Span
#' #                                         -----------------------------------------
#' #                           Span 1               Span 2               Span 3
#' #                           (N=10)               (N=10)               (N=10)
#' #                    -------------------- -------------------- --------------------
#' # Vehicle               mpg    cyl     hp   drat     wt   qsec     vs   gear   carb
#' # ---------------------------------------------------------------------------------
#' # Mazda RX4            21.0      6    110    3.9   2.62  16.46      0      4      4
#' # Mazda RX4 Wag        21.0      6    110    3.9  2.875  17.02      0      4      4
#' # Datsun 710           22.8      4     93   3.85   2.32  18.61      1      4      1
#' # Hornet 4 Drive       21.4      6    110   3.08  3.215  19.44      1      3      1
#' # Hornet Sportabout    18.7      8    175   3.15   3.44  17.02      0      3      2
#' # Valiant              18.1      6    105   2.76   3.46  20.22      1      3      1
#' # Duster 360           14.3      8    245   3.21   3.57  15.84      0      3      4
#' # Merc 240D            24.4      4     62   3.69   3.19     20      1      4      2
#' # Merc 230             22.8      4     95   3.92   3.15   22.9      1      4      2
#' # Merc 280             19.2      6    123   3.92   3.44   18.3      1      4      4
#' #
#' @export
spanning_header <- function(x, span_cols, label = "",
                            label_align = "center", level = 1, n = NULL) {
  
  nms <- names(x$data)
  if (is.character(span_cols)) {
    for (nm in span_cols) {
      if (!nm %in% nms) {
        stop(paste0("Variable '", nm, "' does not exist in data."))
        
      }
    }
  } else if (is.numeric(span_cols)) {
    s <- seq(from = 1, to = length(nms))
    for (elem in span_cols) {
      if (!elem %in% s) {
        stop(paste0("Variable position '", elem, "' does not exist in data."))
        
      }
    }
    
  } else
    stop("span_cols parameter value is invalid.")
  
  if (!label_align %in% c("left", "right", "center", "centre")) {
   stop(paste0("label_align '", label_align, "' is invalid. ",
               "Valid values are 'left', 'right', 'center', or 'centre'."))
  }
  
  if (!is.numeric(level) | is.na(level) | is.null(level)) {
   stop(paste0("level parameter value '", level, "' is invalid.")) 
  }
  
  sh <- structure(list(), class = c("span_def", "list"))
  
  sh$span_cols = span_cols
  sh$label = label
  sh$label_align = label_align
  sh$level = level
  sh$n = n

  x$col_spans[[length(x$col_spans) + 1]] <- sh

  return(x)
}

#' @title Defines a report stub
#' @description Combine columns into a nested report stub.  The report stub
#' is a common feature of statistical reports.  The stub is created with
#' the \code{stub} function in combination with some parameters from the 
#' \code{\link{define}} function.
#' @details 
#' The table stub is a nested set of labels that identify rows 
#' on the table. The stub is created by combining two or more columns into 
#' a single stub column.  The relationship between the columns is typically 
#' visualized as a hierarchy, with lower level concepts indented under 
#' higher level concepts.  
#' 
#' A typical stub is created with the following steps:
#' \itemize{
#'   \item Prepare the data with multiple, hierarchical columns.
#'   \item Create the table object.
#'   \item Define the stub on the table using the stub function, and identify
#'   the columns to be combined.
#'   \item Identify higher level concepts with the \code{label_row} parameter
#'   on the \code{\link{define}} function. 
#'   \item Identify lower level concepts using the \code{indent} parameter 
#'   on the \code{\link{define}} function.
#' }
#' 
#' The stub will be automatically added as an identity variable on the report, 
#' and will always appear as the leftmost column.  There can only be one stub 
#' defined on a report.
#' @param x The table spec.
#' @param vars A vector of quoted variable names from which to create the stub.
#' @param label The label for the report stub.  The default label is an empty
#' string ("").
#' @param label_align The alignment for the stub column label.  
#' Valid values are 'left', 'right', 'center', and 'centre'.  Default follows
#' the \code{align} parameter.
#' @param width The width of the stub, in report units of measure.
#' @param align How to align the stub column.  Valid values are 'left', 
#' 'right', 'center', and 'centre'.  Default is 'left'.
#' @param format A format to apply to the stub column.
#' @return The modified table spec.
#' @family table
#' @examples 
#' library(rptr)
#' library(magrittr)
#' 
#' # Create temporary path
#' tmp <- file.path(tempdir(), "example2.txt")
#' 
#' # Read in prepared data
#' df <- read.table(header = TRUE, text = '
#'       var      label        A             B          
#'       "ampg"   "N"          "19"          "13"         
#'       "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
#'       "ampg"   "Median"     "16.4"        "21.4"       
#'       "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
#'       "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
#'       "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
#'       "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
#'       "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
#' 
#' # Create table
#' tbl <- create_table(df, first_row_blank = TRUE) %>% 
#'   stub(c("var", "label")) %>% 
#'   define(var, blank_after = TRUE, label_row = TRUE, 
#'          format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
#'   define(label, indent = .25) %>% 
#'   define(A, label = "Group A", align = "center", n = 19) %>% 
#'   define(B, label = "Group B", align = "center", n = 13)
#' 
#' 
#' # Create report and add content
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
#'   titles("Table 1.0", "MTCARS Summary Table") %>% 
#'   add_content(tbl) %>% 
#'   footnotes("* Motor Trend, 1974") %>%
#'   page_footer(left = Sys.time(), 
#'               center = "Confidential", 
#'               right = "Page [pg] of [tpg]")
#' 
#' # Write out report
#' write_report(rpt)
#' 
#' # View report in console
#' writeLines(readLines(tmp))
#' 
#' # Client: Motor Trend                                                Study: Cars
#' #                                   Table 1.0
#' #                              MTCARS Summary Table
#' # 
#' #                                     Group A      Group B
#' #                                      (N=19)       (N=13)
#' #                  -------------------------------------------
#' # 
#' #                 Miles Per Gallon
#' #                    N                   19           13
#' #                    Mean            19.3 (6.7)   21.3 (4.8)
#' #                    Median             17.3         21.0
#' #                    Q1 - Q3        15.2 - 22.1  19.2 - 22.8
#' #                    Range          10.4 - 33.9  14.3 - 32.4
#' #
#' #                 Cylinders
#' #                    8 Cylinder     10 ( 52.6%)   4 ( 30.8%)
#' #                    6 Cylinder      3 ( 15.8%)   4 ( 30.8%)
#' #                    4 Cylinder      6 ( 31.6%)   5 ( 38.5%)
#' # 
#' # ...
#' # 
#' # 
#' # * Motor Trend, 1974
#' # 
#' # 2020-08-30 03:50:02              Confidential                      Page 1 of 1
#' #
#' @export
stub <- function(x, vars, label = "", label_align = NULL, 
                 align = "left", width = NULL, format = NULL) {
  
  def <- structure(list(), class = c("stub_def", "list"))
  
  def$label <- label
  def$label_align <- label_align
  def$align <- align
  def$vars <- vars
  def$width <- width
  def$format <- format
  
  x$stub <- def
  
  return(x)
}

# *Comment out for now.  Basically useless.  Everything is on the create_table()
# Defines options for the table
# @param x The table spec.
# @param first_row_blank Whether to create a blank on the first row after the
# table header.
# @export
# table_options <- function(x, first_row_blank=FALSE){
# 
# 
#   x$first_row_blank = first_row_blank
# 
# 
# }

#' @title Prints the table spec
#' @description A function to print the table spec.
#' The \strong{print} function will print the table spec primarily in list 
#' format.  The exception to list format is the data parameter, which will
#' be printed in data frame/tibble format.
#' @param x The table spec.
#' @param ... Additional parameters to pass to the underlying print function.
#' @seealso 
#' \code{\link{create_table}} function to create a table specification.
#' @return The table spec, invisibly.
#' @family table
#' @examples 
#' tbl <- create_table(mtcars)
#' print(tbl)
#' 
#' # $data
#' # mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#' # Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#' # Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#' # Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#' # Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#' # Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#' # Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#' # Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#' # Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#' # Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#' # Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#' # [ reached 'max' / getOption("max.print") -- omitted 22 rows ]
#' # 
#' # $n_format
#' # function(x) {
#' #   
#' #   ret <- paste0("\n(N=", x, ")")
#' #   
#' #   return(ret)
#' #   
#' # }
#' # <environment: namespace:rptr>
#' #   
#' #   $col_defs
#' # list()
#' # 
#' # $col_spans
#' # list()
#' # 
#' # $show_cols
#' # [1] "all"
#' # 
#' # $first_row_blank
#' # [1] FALSE
#' # 
#' # $headerless
#' # [1] FALSE
#' @export
print.table_spec <- function(x, ...){
  

  for (nm in names(x)) {
    
    cat("$", nm, "\n", sep = "")
    if (nm == "data") {

      m <- ncol(x[[nm]]) * 10
      print(x[[nm]], ..., max = m)
    }
    else  {
      
      print(x[[nm]], ...)
    }
    cat("\n")
  }
  
  invisible(x)
}


# Formats ----------------------------------------------------------------------


#' @title Functions to format the population label
#' @description These functions are used to format the "N=" population label
#' on column headers.  
#' @details Which function to use to format the population label is specified
#' on the \code{n_format} parameter on the \code{\link{create_table}} function.
#' These formatting functions provide several options for formatting the "N=", 
#' including whether the "N" should be upper case or lower case, and whether
#' or not to put the value in parentheses.  If one of these options does not 
#' meet the specifications for your report, you may also write your own 
#' formatting function and pass it to the \code{n_format} function.  
#' @usage lowcase_parens(x)
#' @usage upcase_parens(x)
#' @usage lowcase_n(x)
#' @usage upcase_n(x)
#' @aliases lowcase_parens upcase_parens lowcase_n upcase_n
#' @seealso 
#' \code{\link{create_table}} function to create a table.
#' @param x Population count
#' @examples 
#' # Create test data
#' l <- "Label"
#' n <- 47
#' 
#' cat(paste0(l, lowcase_parens(n)))
#' # Label
#' # (n=47)
#' 
#' cat(paste0(l, upcase_parens(n)))
#' # Label
#' # (N=47)
#' 
#' cat(paste0(l, lowcase_n(n)))
#' # Label
#' # n=47
#' 
#' cat(paste0(l, upcase_n(n)))
#' # Label
#' # N=47
#' @export
lowcase_parens <- function(x) {
  
  ret <- paste0("\n(n=", x, ")")
  
  return(ret)
}

#' @aliases lowcase_parens
#' @export
upcase_parens <- function(x) {
  
  ret <- paste0("\n(N=", x, ")")
  
  return(ret)
  
}

#' @aliases lowcase_parens
#' @export
lowcase_n <- function(x) {
  
  ret <- paste0("\nn=", x)
  
  return(ret)
}

#' @aliases lowcase_parens
#' @export
upcase_n <- function(x) {
  
  ret <- paste0("\nN=", x)
  
  return(ret)
  
}
