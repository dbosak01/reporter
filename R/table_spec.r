


# Table Spec Functions ---------------------------------------------------------
#' @title Create a table
#' @description 
#' The \code{create_table} function creates a table object to which 
#' further specifications can be added.  The object can be added to a report
#' using the \code{\link{add_content}} function. The object is implemented as an 
#' S3 object of class 'table_spec'.
#' @details 
#' A table object is a container to hold information about a table.  The 
#' only required information for a table is the table data.  All other 
#' parameters and functions are optional.
#' 
#' By default, the table will display all columns in the data frame.  To change
#' this default, use the \code{show_cols} parameter.  Setting this parameter
#' to 'none' will display none of the columns in the data, unless they are
#' explicitly defined with a \code{\link{define}} function.  
#' 
#' The \code{show_cols} parameter also accepts a vector of column positions
#' or column names. When a vector is supplied, \code{create_table} will 
#' display only those columns on the report, in the order encountered in the 
#' vector.  The \code{show_cols} parameter is the only mechanism in 
#' \code{create_table} to modify the column order. Otherwise, modify the 
#' order prior to sending the data to \code{create_table} using the many options
#' available in Base R or supplemental packages.
#' 
#' @section Setting Formatting Attributes:
#' Formatting attributes can be controlled in three ways.  By default, formatting
#' attributes assigned to the data frame will be passed through to the 
#' reporting functions.  The reporting functions will recognize the 'label',
#' 'format', 'width', and 'justify' attributes. In other words, you can control 
#' the column label, width, format, and alignment of your report columns simply by 
#' assigning those attributes to your data frame. The advantage of using
#' attributes assigned to data frame columns is that you can store those 
#' attributes permanently with the data frame, and those attributes will
#' not have to be re-specified for each report.  To ignore attributes assigned
#' to the data frame, set the \code{use_attributes} parameter to 'none'.
#' 
#' Secondly, attributes can be specified using the \code{\link{column_defaults}}
#' function.  This function allows the user to apply a default set of parameters
#' to one or more columns.  If no columns are specified in the \code{var} 
#' or \code{from} and \code{to} parameter of this function, the defaults 
#' will apply to all columns.  Any default parameter value can be overridden 
#' by the \code{\link{define}} function.
#' 
#' Lastly, the \code{\link{define}} function provides the most control over 
#' column parameters.  This function provides a significant amount of 
#' functionality that cannot be specified elsewhere.  See the 
#' \code{\link{define}} function for additional information.  The \code{define}
#' function will also override any formatting attributes assigned to the 
#' data frame, or anything set by the \code{column_defaults} function.
#' 
#' @section Additional Functionality:
#' The \code{create_table} function also provides the capabilities to create
#' a "headerless" table.  A headerless table is useful when combining two tables 
#' into one report.  The example below illustrates use of a headerless table.
#' 
#' Since the purpose of the \strong{rptr} package is to create statistical 
#' reports, the \code{create_table} function makes it easy to add population
#' counts to the table header.  These population counts are added to column
#' labels and spanning header labels using the \code{n} parameter on the 
#' \code{\link{define}} or \code{\link{spanning_header}} functions.  The 
#' population count is formatted according to the  
#' \code{n_format} parameter on \code{create_table}. The \strong{rptr} 
#' package provides four population count formatting functions.  
#' You may create your own formatting function 
#' if one of these functions does not meet your needs.  See 
#' \code{\link{upcase_parens}} for further details.
#' 
#' @param x The data frame or tibble from which to create the table object.
#' @param show_cols This parameter gives control over which columns in the 
#' input data to display on the report by default.  Valid values are
#' 'all', 'none', a vector of quoted column names, or a vector of 
#' column positions.  'all' means show all columns, 
#' unless overridden by the column definitions.  
#' 'none' means don't show any 
#' columns unless specified in the column definitions.  If a vector of column
#' names or positions is supplied, those columns will be shown in the report 
#' in the order specified, whether or not a definition is supplied. See the 
#' \code{\link{define}} function for additional information on how to
#' show/hide report columns.
#' @param use_attributes Whether or not to use any formatting attributes assigned
#' to the columns on the input data frame.  Valid values are 'all', 'none', or
#' a vector of attribute names to use.  Possible attributes that may be used
#' are 'label', 'format', 'width', and 'justify'.  By default, any of these
#' attribute values will be applied to the table.  For example, if you assign
#' a label to the 'label' attribute of a data frame column, pass that data 
#' frame into \code{create_table}, and don't override the label value on a 
#' \code{define} function, the label will appear as a column header on the
#' table.  The \code{use_attributes} parameter allows you to control this default
#' behavior, and use or ignore data frame attributes as desired.  
#' @param width The expected width of the table in the report units of 
#' measure.  By default, the width setting is NULL, and columns will be sized
#' according to the width of the data and labels.  If the width parameter is 
#' set, the function will attempt to size the table to the specified width.
#' If the sum of the column widths is less than the specified width, the 
#' function will adjust the columns widths proportionally to fit the specified
#' width.  If the sum of the column widths is wider than the table width 
#' parameter value, the table width parameter will be ignored. 
#' @param first_row_blank Whether to place a blank row under the table header.
#' Valid values are TRUE or FALSE.  Default is FALSE.
#' @param n_format The formatting function to apply to the header "N=" label. 
#' The default formatting function is \code{\link{upcase_parens}}. 
#' @param headerless Whether to create a headerless table. A headerless
#' table displays the table data only. Default is FALSE, meaning the table
#' will have a header. 
#' @family table
#' @seealso \code{\link{create_report}} to create a report, 
#' \code{\link{create_plot}} to create a plot,
#' \code{\link{create_text}} to create text content, and 
#' \code{\link{add_content}} to append content to a report.  Also see
#' the \code{\link{titles}}, \code{\link{footnotes}}, and \code{\link{page_by}}
#' functions to add those items to the table if desired.
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
#'         column_defaults(width = .5) %>% 
#'         define(all_cars, label = "", width = 2) %>% 
#'         define(mpg, format = "%.1f") %>% 
#'         define(disp, format = "%.1f") %>% 
#'         define(hp, format = "%.0f") %>% 
#'         define(qsec, format = "%.2f")
#' 
#' # Create table for modified data
#' tbl2 <- create_table(dat_mod, headerless = TRUE) %>% 
#'         column_defaults(width = .5) %>% 
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
#' writeLines(readLines(tmp, encoding = "UTF-8"))
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
create_table <- function(x, show_cols = "all", use_attributes = "all",
                         width = NULL, 
                         first_row_blank=FALSE,
                         n_format = upcase_parens, headerless = FALSE) {
  if (is.null(x)) {
    stop("Data parameter 'x' missing or invalid.") 
    
  }
  
  if (!"data.frame" %in% class(x)) {
    stop(paste("ERROR: data parameter 'x' on",
               "page_template() function is invalid.",
               "\n\tValid values are a data.frame or tibble."))
  }
  
  if (is.null(use_attributes))
    stop("use_attributes parameter cannot be null.")
  else if (any(use_attributes %in%  
               c("all", "none", "label", "width", "justify", "format") == FALSE)){
    
    stop(paste("Invalid use_attributes value.  Valid values are 'all', 'none'", 
               "or a vector of any of the following attributes names: 'label',",
               "'format', 'width', or 'justify'"))
  }
    
  
  ret <- structure(list(), class = c("table_spec", "list"))

  ret$data <- x
  ret$dataname <- deparse1(substitute(x, env = environment()))
  ret$n_format <- n_format
  ret$col_defs <- list()
  ret$col_spans <- list()
  if (is.integer(show_cols)) {
    ret$show_cols <- names(x)[show_cols]
  } else if (is.null(show_cols)) {
    ret$show_cols <- "all"
  } else {
    ret$show_cols <- show_cols
  }
  ret$first_row_blank <- first_row_blank
  ret$headerless <- headerless
  ret$stub <- NULL
  ret$width <- width
  ret$page_var <- NULL
  if (any(use_attributes == "all"))
    ret$use_attributes <- c("label", "width", "justify", "format")
  else if (all(use_attributes == "none"))
    ret$use_attributes <- c("")
  else  
    ret$use_attributes <- use_attributes

  return(ret)

}

#' @title Defines a column 
#' @description A function to define a table column.  The
#' \code{define} function contains a variety of a parameters to control the 
#' appearance of the report.  Using the \code{define} function, you can control
#' simple options like column alignment and width, but also control more 
#' sophisticated options like page wrapping and page breaking.
#' @details 
#' Column definitions are optional.  By default, all columns in the data
#' are displayed in the order assigned to the data frame. 
#' 
#' The report will use attributes assigned to the data frame 
#' such as 'width', 'justify', 'label', and 'format'.  In other words, 
#' some control over the column 
#' formatting is available by manipulating the data frame attributes prior
#' to assigning the data frame to \code{create_table}.  See 
#' \code{\link{create_table}} for more details.
#' 
#' The \code{define} function is used to provide additional control over
#' column appearance.  For example, you may use the \code{define} function
#' to assign an "N=" population count, eliminate duplicates from the column,
#' or place a blank row after each unique value of the variable. 
#' See the parameter documentation for additional options.
#' 
#' Some of the parameters on the \code{define} function are used in the 
#' creation of a table stub.  Specifically, the \code{label_row} and 
#' \code{indent} parameters participate in the creation of the stub column.
#' See the \code{\link{stub}} function for further
#' information.
#' 
#' A single column definition may be defined for multiple variables.  
#' To create a definition for multiple variables, pass the variables as
#' a quoted or unquoted vector.  When creating a single definition for 
#' multiple variables, the parameters will be unified across those variables.
#' Note that some parameters (such as \code{page_break}) may only be set
#' once per report, and cannot be shared across multiple variables.  
#' 
#' @param x The table spec.
#' @param vars The variable name or names to define a column for.  Names may
#' be quoted or unquoted.  If defining for multiple variables, 
#' pass them as a vector of names.
#' @param label The label to use for the column header.  If a label is assigned
#' to the label column attribute, it will be used as a default.  Otherwise,
#' the column name will be used.
#' @param format The format to use for the column data.  The format can 
#' be a string format, a formatting function, a lookup list, a user-defined
#' format, or a formatting list. 
#' All formatting is performed by the \code{\link[fmtr]{fapply}} function from
#' the \code{\link[fmtr]{fmtr}} package.  For 
#' a list of common formatting codes, see \link[fmtr]{FormattingStrings}.
#' @param align The column alignment.  Valid values are "left", "right", 
#' "center", and "centre".  By default, text columns will be left aligned
#' and numeric columns will be right aligned.
#' @param label_align How to align the header labels for this column.
#' Valid values are "left", "right", "center", and "centre".  By default, 
#' the label alignment will follow any alignment set on the column \code{align}
#' parameter.
#' @param width The width of the column in the specified units of measure.
#' The units of measure are specified on the \code{units} parameter of the
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
#' that are deduped only show the value on the first row of each group.  This 
#' option is commonly used for grouping variables.
#' @param id_var Whether this variable should be considered an ID variable.
#' ID variables are retained on each page when the page is wrapped. ID variables
#' are also moved to the far left of the page.
#' @param page_wrap Force a page wrap on this variable.  A page wrap is a vertical
#' page break necessary when the table is too wide to fit on a single page.
#' The excess variables will be wrapped to the next page.  Page wraps will
#' continue until all columns are displayed.  Use the \code{id_var}
#' parameter to identify rows across wrapped pages. 
#' @param page_break You may control when page breaks occur by defining
#' a page break variable yourself, and setting this parameter to TRUE for
#' that variable.  Only one page break variable can be defined per table.
#' If two or more variables are defined as a page break, an error will be 
#' generated.
#' @param indent How much to indent the column values.  The parameter takes a 
#' numeric value that will be interpreted according to the \code{units} 
#' (Unit Of Measure) setting on the report.  This parameter can be used to 
#' help create a stub column.  The default value is NULL, meaning the column
#' should not be indented.  See the \code{\link{stub}} function for additional
#' information on creating a stub column.
#' @param label_row Whether the values of the variable should be used to
#' create a label row.  Valid values are TRUE or FALSE.  Default is FALSE.
#' If \code{label_row} is set to TRUE, the dedupe parameter will also be 
#' set to TRUE.  This parameter is often used in conjunction with the 
#' \code{\link{stub}} function and \code{indent} parameter to create a 
#' stub column.
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
define <- function(x, vars, label = NULL, format = NULL, 
                   align=NULL, label_align=NULL, width=NULL,
                   visible=TRUE, n = NULL, blank_after=FALSE,
                   dedupe=FALSE, id_var = FALSE, page_wrap = FALSE,
                   page_break = FALSE, indent = NULL, label_row = FALSE) {
  
  
  # Determine if it is a vector or not.  "language" is a vector.
  if (typeof(substitute(vars, env = environment())) == "language") 
    v <- substitute(vars, env = environment())
  else 
    v <- substitute(list(vars), env = environment())
  
  # Turn each item into a character
  vars_c <- c()
  if (length(v) > 1) {
    for (i in 2:length(v)) {
      vars_c[[length(vars_c) + 1]] <- as.character(v[[i]]) 
    }
    
  }
  
  # Convert list to vector
  vars_c <- unlist(vars_c)
  
  # Check that variable exists in data frame
  if (!is.null(x$data) & !is.null(vars_c)) {
    if (!any(vars_c %in% names(x$data))) {
      for (nm in vars_c) {
        if (!nm %in% names(x$data)) 
          stop(paste0("Variable does not exist in data: ", nm))
      }
    }
  }
  
  # For each passed variable, create an individual definition
  # This make subsequent processing much easier
  for (nm in vars_c) {
    
    
   def <- define_c(nm, label = label, format = format, 
                   align=align, label_align=label_align, width=width,
                   visible=visible, n = n, blank_after=blank_after,
                   dedupe=dedupe, id_var = id_var, page_wrap = page_wrap,
                   page_break = page_break, indent = indent, 
                   label_row = label_row)
   
   x$col_defs[[nm]] <- def
   
   if (page_break == TRUE) {
     if (is.null(x$page_var)) {
       x$page_var <- nm
     } else
       stop("Cannot define more than one page break variable")
   }
    
  }

  return(x)
}

#' @description Define a variable with a quoted name. Used internally.
#' @noRd
define_c <- function(var, label = NULL, format = NULL, 
                   align=NULL, label_align=NULL, width=NULL,
                   visible=TRUE, n = NULL, blank_after=FALSE,
                   dedupe=FALSE, id_var = FALSE, page_wrap = FALSE,
                   page_break = FALSE, indent = NULL, label_row = FALSE) {
  
  
  if (class(var) != "character")
    stop("class of var must be character.")
 
  def <- structure(list(), class = c("col_def", "list"))
  
  def$var = var
  def$var_c = var
  def$label = label
  def$format = format
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
  def$page_break = page_break
  if (label_row == TRUE)
    def$dedupe <- TRUE
  
  
  return(def)
}


#' @title Set default attributes for one or more columns
#' @description A function to set default attributes for columns on a table.  
#' The \code{column_defaults} function contains a subset of the parameters
#' on the \code{\link{define}} function that can be shared across variables.
#' Any attributes set by \code{column_defaults} can be overridden by 
#' the \code{define} function.  The overall purpose of the 
#' function is to minimize redundancy in column definitions. 
#' @details 
#' Column defaults can be specified for multiple variables.  By default,
#' the function will apply to all variables.  Alternately, you can 
#' specify a vector of columns on the \code{vars} parameter, or a range of
#' columns using the \code{from} and \code{to} parameters.  Both the 
#' \code{vars} parameters and the \code{from} and \code{to} parameters
#' will accept column positions, quoted variable names, or unquoted variable 
#' names.
#' 
#' The parameters that can be set with the \code{column_defaults} 
#' include the formatting attributes 'width', 'justify', 'label', and 
#' 'format'.  Any parameters set with \code{column_defaults} will override
#' any attributes set on the data frame.
#' 
#' Note that you may call the \code{column_defaults} function multiple times
#' on the same table specification.  Typically, multiple \code{column_defaults}
#' calls would be made with a different set or range of variables.
#' 
#' @param x A table spec.
#' @param vars The variable name or names to define defaults for.  Variable
#' names may be quoted or unquoted.  The parameter will also accept 
#' integer column positions instead of names.  For multiple variables, 
#' pass the names or positions as a vector. 
#' @param from The variable name or position that starts a column range.  
#' If passed as a variable name, it may be quoted or unquoted.
#' @param to The variable name or position that ends a column range. 
#' If passed as a variable name, it 
#' may be quoted or unquoted.
#' @param label The label to use for a column header.  This label will be 
#' applied to all variables assigned to the \code{column_defaults} function.
#' @param format The format to use for the column data.  The format can 
#' be a string format, a formatting function, a lookup list, a user-defined
#' format, or a formatting list. 
#' All formatting is performed by the \code{\link[fmtr]{fmtr}} package.  For 
#' additional information, see the help for that package.
#' @param align The column alignment.  Valid values are "left", "right", 
#' "center", and "centre".
#' @param label_align How to align the header labels for this column.
#' Valid values are "left", "right", "center", and "centre".
#' @param width The width of the column in the specified units of measure.
#' The units of measure are specified on the \code{units} parameter of the
#' \code{\link{create_report}} function.  If no width is supplied, the
#' \code{\link{write_report}} function will assign a default width based on the 
#' width of the column data and the label.  \code{write_report} will not set a 
#' column width less than the width of the largest word in the data or label.
#' In other words, \code{write_report} will not break words. 
#' @param n The n value to place in the "N=" header label.  Formatting for
#' the n value will be performed by the formatting function assigned to the 
#' \code{n_format} parameter on \code{\link{create_table}}.
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
#'   column_defaults(from = mpg, to = qsec, width = .5, format = "%.1f") %>% 
#'   define(vehicle, label = "Vehicle", width = 1.5, align = "left") %>% 
#'   define(c(cyl, hp), format = "%.0f") 
#' 
#' # Create the report
#' rpt <- create_report(tmp, orientation = "portrait") %>% 
#'   titles("Table 2.5", "MTCARS Sample Report") %>% 
#'   add_content(tbl) 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' # Send report to console for viewing
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #                                 Table 2.5
#' #                            MTCARS Sample Report
#' # 
#' #    Vehicle               mpg    cyl   disp     hp   drat     wt   qsec
#' #    -------------------------------------------------------------------
#' #    Mazda RX4            21.0      6  160.0    110    3.9    2.6   16.5
#' #    Mazda RX4 Wag        21.0      6  160.0    110    3.9    2.9   17.0
#' #    Datsun 710           22.8      4  108.0     93    3.8    2.3   18.6
#' #    Hornet 4 Drive       21.4      6  258.0    110    3.1    3.2   19.4
#' #    Hornet Sportabout    18.7      8  360.0    175    3.1    3.4   17.0
#' #    Valiant              18.1      6  225.0    105    2.8    3.5   20.2
#' #    Duster 360           14.3      8  360.0    245    3.2    3.6   15.8
#' #    Merc 240D            24.4      4  146.7     62    3.7    3.2   20.0
#' #    Merc 230             22.8      4  140.8     95    3.9    3.1   22.9
#' #    Merc 280             19.2      6  167.6    123    3.9    3.4   18.3
#' #
#' @export
column_defaults <- function(x, vars = NULL, from = NULL, to = NULL, label = NULL, 
                  format = NULL, align=NULL, label_align=NULL, width=NULL,
                   n = NULL) {
  
  if (!"table_spec" %in% class(x))
    stop("Input object must be of class 'table_spec'.")

  
  # Determine if it is a vector or not.  "language" is a vector.
  
  if (typeof(substitute(vars, env = environment())) == "language") {
    ret <- tryCatch({
      if (is.numeric(vars)) {
        names(x$data)[vars]
        
      }

    }, error = function(e) {
      FALSE
    })
    
    if (all(ret == FALSE))
      v <- substitute(vars, env = environment())
    else 
      v <- ret
    
  } else 
    v <- substitute(list(vars), env = environment())
  
  
  # Turn each item into a character
  if (!is.character(v)) {
    vars_c <- c()
    if (length(v) > 1) {
      for (i in 2:length(v)) {
        vars_c[[length(vars_c) + 1]] <- as.character(v[[i]]) 
      }
      
    }
  } else 
    vars_c <- v
  
  # Convert list to vector
  vars_c <- unlist(vars_c)
  
  # Check that variable exists in data frame
  if (!is.null(x$data) & !is.null(vars_c)) {
    if (!any(vars_c %in% names(x$data))) {
      for (nm in vars_c) {
        if (!nm %in% names(x$data)) 
        stop(paste0("Variable does not exist in data: ", nm))
      }
    }
  }
  
  # Create column default object
  dflt <- structure(list(), class = c("col_dflt", "list"))
  
  if (!identical(vars_c, character(0)))
    dflt$vars = vars_c
  
  # Assign from value
  f <- as.character(substitute(from, env = environment()))
  if (!identical(f, character(0))) {
    
    if (suppressWarnings(!is.na(as.integer(f))))
        f <- names(x$data)[as.integer(f)]
    
    if (!f %in% names(x$data))
      stop(paste("Variable does not exist in input data frame:", f))
    
    dflt$from =  f
  }
  
  # Assign to value
  t <- as.character(substitute(to, env = environment()))
  if (!identical(t, character(0))) {
    
    if (suppressWarnings(!is.na(as.integer(t))))
      t <- names(x$data)[as.integer(t)]
  
    if (!t %in% names(x$data))
      stop(paste("Variable does not exist in input data frame:", t))
    
    dflt$to =  t
  }
  
  # Catch range mismatches
  if (!is.null(dflt$from) & is.null(dflt$to)) {
    stop("'to' parameter cannot be null if 'from' is populated.") 
  }
  
  if (is.null(dflt$from) & !is.null(dflt$to)) {
    stop("'from' parameter cannot be null if 'to' is populated.") 
  }
  
  dflt$label = label
  dflt$format = format
  dflt$align = align
  dflt$label_align = if (is.null(label_align) & !is.null(align))
    align else label_align
  dflt$width = width
  dflt$n = n

  x$col_dflts[[length(x$col_dflts) + 1]] <- dflt
  
  
  return(x)
}


#' @title Defines a spanning header
#' @description Create a header that spans multiple columns.  Spanning headers
#' are used to group related columns.  Such groupings are a common 
#' feature of statistical reports.
#' @details 
#' A spanning header is a label and underline that spans one or more 
#' columns.  A spanning header is defined minimally by identifying 
#' column range to be spanned, and a label.  A label alignment and "N="
#' value may also be supplied.
#' 
#' The spanning column range is defined by the \code{from} and \code{to} 
#' parameters.  The range identifies a continuous set of variables on the data.
#' Variables can be identified by position, a quoted variable name, or an 
#' unquoted variable name.
#' @param x The table object to add spanning headers to.
#' @param from The starting column to span.  Spanning columns are defined as
#' range of columns 'from' and 'to'. The columns may be identified by position, 
#' or by quoted or unquoted variable names.
#' The \code{from} parameter is required.  
#' @param to The ending column to span.  Spanning columns are defined as
#' range of columns 'from' and 'to'. The columns may be identified by position,
#' or by quoted or unquoted variable names.
#' The \code{to} parameter is required. 
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
#' # Create a temporary file
#' tmp <- file.path(tempdir(), "iris.txt")
#' 
#' # Prepare data
#' dat <- iris[sample(1:150, 15), c(5, 1, 2, 3, 4)]
#' dat <- dat[order(dat$Species), ]
#'
#' # Define table
#' tbl <- create_table(dat) %>% 
#'   titles("Table 3.2", "IRIS Sample Report") %>% 
#'   spanning_header(2, 3, label = "Sepal") %>% 
#'   spanning_header(4, 5, label = "Petal") %>% 
#'   column_defaults(2:5, format = "%.1f") %>% 
#'   define(Species, align = "left", dedupe = TRUE, blank_after = TRUE) %>% 
#'   define(Sepal.Length, label = "Length") %>% 
#'   define(Sepal.Width, label = "Width") %>% 
#'   define(Petal.Length, label = "Length") %>% 
#'   define(Petal.Width, label = "Width") %>% 
#'   footnotes("* From Fisher's Iris Dataset")
#'        
#' # Define report
#' rpt <- create_report(tmp, orientation="portrait") %>%
#'   options_fixed(blank_margins = TRUE) %>% 
#'   set_margins(top = 1, bottom =1) %>% 
#'   add_content(tbl, align = "left") 
#' 
#' # Write the report
#' write_report(rpt)
#' 
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' #
#' #
#' #
#' #
#' #                      Table 3.2
#' #                  IRIS Sample Report
#' #
#' #                       Sepal        Petal
#' #                   ------------ ------------
#' #       Species     Length Width Length Width
#' #       -------------------------------------
#' #       setosa         5.0   3.0    1.6   0.2
#' #                      4.6   3.4    1.4   0.3
#' #                      5.0   3.4    1.6   0.4
#' #                      5.7   3.8    1.7   0.3
#' #
#' #       versicolor     5.7   2.8    4.1   1.3
#' #                      6.2   2.9    4.3   1.3
#' #                      7.0   3.2    4.7   1.4
#' #                      6.6   2.9    4.6   1.3
#' #
#' #       virginica      6.2   3.4    5.4   2.3
#' #                      7.2   3.0    5.8   1.6
#' #                      6.9   3.1    5.1   2.3
#' #                      5.6   2.8    4.9   2.0
#' #                      7.7   2.6    6.9   2.3
#' #                      6.3   2.8    5.1   1.5
#' #                      7.7   2.8    6.7   2.0
#' #
#' #
#' #       * From Fisher's Iris Dataset
#' @export
spanning_header <- function(x, from, to, label = "",
                            label_align = "center", level = 1, n = NULL) {
  
  
  f <- as.character(substitute(from, env = environment()))
  t <- as.character(substitute(to, env = environment()))
  
  if (!is.na(suppressWarnings(as.numeric(f))))
    f <- as.numeric(f)
  
  if (!is.na(suppressWarnings(as.numeric(t))))
    t <- as.numeric(t)
  
  nms <- names(x$data)
  
  if (is.character(f)) {
    if (!f %in% nms) {
      stop(paste0("From variable '", f, "' does not exist in data."))

    }
  }
  
  if (is.character(t)) {
    if (!t %in% nms) {
      stop(paste0("To variable '", t, "' does not exist in data."))
      
    }
  }
  
  if (is.numeric(f)) {

      if (!(f > 0 & f <= ncol(x$data))) {
        stop(paste0("From variable position '", f, "' is invalid."))

      } else
        f <- nms[[f]]
  } 
  
  if (is.numeric(t)) {

    if (!(t > 0 & t <= ncol(x$data))) {
      stop(paste0("To variable position '", t, "' is invalid."))
      
    } else 
      t <- nms[[t]]
  } 
  
  if (!label_align %in% c("left", "right", "center", "centre")) {
   stop(paste0("label_align '", label_align, "' is invalid. ",
               "Valid values are 'left', 'right', 'center', or 'centre'."))
  }
  
  if (!is.numeric(level) | is.na(level) | is.null(level)) {
   stop(paste0("level parameter value '", level, "' is invalid.")) 
  }
  
  sh <- structure(list(), class = c("span_def", "list"))
  
  # Get spanning range
  nms <- names(x$data)
  startpos <- match(f, nms)
  endpos <- match(t, nms)
  spn <- nms[startpos:endpos]
  
  sh$span_cols <- spn
  sh$from = f
  sh$to = t
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
#' the \code{stub} function, and frequently appears in combination with the 
#' \code{label_row} and \code{indent} parameters from the 
#' \code{\link{define}} function.  These elements work together to define
#' the appearance of the stub.
#' @details 
#' The table stub is a nested set of labels that identify rows 
#' on the table. The stub is created by combining two or more columns into 
#' a single stub column.  The relationship between the columns is typically 
#' visualized as a hierarchy, with lower level concepts indented under 
#' higher level concepts.  
#' 
#' A typical stub is created with the following steps:
#' \itemize{
#'   \item Prepare the data. 
#'   \item Create the table object.
#'   \item Define the stub on the table using the \code{stub} function, 
#'   and identify the variables to be combined.
#'   \item Identify higher level concepts with the \code{label_row} parameter
#'   on the \code{\link{define}} function. 
#'   \item Identify lower level concepts using the \code{indent} parameter 
#'   on the \code{\link{define}} function.
#' }
#' 
#' The stub will be automatically added as an identity variable on the report, 
#' and will always appear as the leftmost column.  There can only be one stub 
#' defined on a report.
#' 
#' If you wish to create multiple levels of nested labels, use
#' an NA value to prevent lower level labels from overwriting
#' higher level labels. 
#' 
#' For example, the following data:
#' \preformatted{
#' continent          country   state_province    
#' "North America"    NA        NA   
#' "North America"    "Canada"  NA   
#' "North America"    "Canada"  "Ontario"   
#' "North America"    "USA"     NA   
#' "North America"    "USA"     "New York"   
#' "South America"    NA        NA   
#' "South America"    "Brazil"  NA   
#' "South America"    "Brazil"  "Amazonas"   
#' "South America"    "Brazil"  "Bahia"   
#' }
#' Will produce the following stub:   
#' \preformatted{
#' North America   
#'   Canada   
#'     Ontario   
#'   USA   
#'     New York   
#' South America  
#'   Brazil 
#'     Amazonas   
#'     Bahia   
#' }
#' With the following code:
#' \preformatted{
#' tbl <- create_table(dat) %>% 
#'   stub(c(continent, country, state_province)) %>% 
#'   define(country, indent = .25) %>% 
#'   define(state_province, indent = .5)
#' }
#' @param x The table spec.
#' @param vars A vector of quoted or unquoted variable names from 
#' which to create the stub.
#' @param label The label for the report stub.  The default label is an empty
#' string.
#' @param label_align The alignment for the stub column label.  
#' Valid values are 'left', 'right', 'center', and 'centre'.  Default follows
#' the \code{align} parameter.
#' @param width The width of the stub, in report units of measure.
#' @param align How to align the stub column.  Valid values are 'left', 
#' 'right', 'center', and 'centre'.  Default is 'left'.
#' @return The modified table spec.
#' @family table
#' @examples 
#' library(rptr)
#' library(magrittr)
#' 
#' # Create temporary path
#' tmp <- file.path(tempdir(), "stub.txt")
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
#'   stub(c(var, label)) %>% 
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
#' writeLines(readLines(tmp, encoding = "UTF-8"))
#' 
#' # Client: Motor Trend                                                Study: Cars
#' #                                   Table 1.0
#' #                              MTCARS Summary Table
#' # 
#' #                                     Group A      Group B
#' #                                      (N=19)       (N=13)
#' #                 -------------------------------------------
#' # 
#' #                 Miles Per Gallon
#' #                    N                   19           13
#' #                    Mean            18.8 (6.5)   22.0 (4.9)
#' #                    Median             16.4         21.4
#' #                    Q1 - Q3        15.1 - 21.2  19.2 - 22.8
#' #                    Range          10.4 - 33.9  14.7 - 32.4
#' #
#' #                 Cylinders
#' #                    8 Cylinder     10 ( 52.6%)   4 ( 30.8%)
#' #                    6 Cylinder      4 ( 21.1%)   3 ( 23.1%)
#' #                    4 Cylinder      5 ( 26.3%)   6 ( 46.2%)
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
                 align = "left", width = NULL) {
  
  def <- structure(list(), class = c("stub_def", "list"))
  
  
  # Determine if it is a vector or not.  "language" is a vector.
  if (typeof(substitute(vars, env = environment())) == "language") 
    v <- substitute(vars, env = environment())
  else 
    v <- substitute(list(vars), env = environment())
  
  # Turn each item into a character
  vars_c <- c()
  if (length(v) > 1) {
    for (i in 2:length(v)) {
      vars_c[[length(vars_c) + 1]] <- as.character(v[[i]]) 
    }
    
  }
  
  # Convert list to vector
  vars_c <- unlist(vars_c)
  
  # Check that variable exists in data frame
  if (!is.null(x$data) & !is.null(vars_c)) {
    if (!any(vars_c %in% names(x$data))) {
      for (nm in vars_c) {
        if (!nm %in% names(x$data)) 
          stop(paste0("Variable does not exist in data: ", nm))
      }
    }
  }
  
  
  def$label <- label
  def$label_align <- label_align
  def$align <- align
  def$vars <- vars_c
  def$width <- width
  
  x$stub <- def
  
  return(x)
}


#' @title Prints the table spec
#' @description A function to print the table spec.
#' The \strong{print} function will print the table spec in summary 
#' form.  To view all parameters, set the \code{verbose} parameter to TRUE.
#' @param x The table spec.
#' @param ... Additional parameters to pass to the underlying print function.
#' @param verbose Whether to print in verbose form, which is similar to 
#' a list.  Default is FALSE, which prints in summary form.
#' @seealso 
#' \code{\link{create_table}} function to create a table specification.
#' @return The table spec, invisibly.
#' @family table
#' @examples 
#' library(magrittr)
#' 
#' # Create Table
#' tbl <- create_table(mtcars) %>% 
#'   define(mpg, label = "Miles Per Gallon", width = .5) %>% 
#'   define(cyl, label = "Cylinders") %>% 
#'   titles("Table 6.4", "MTCARS Sample Table") %>% 
#'   footnotes("* Motor Trend, 1974")
#'   
#' tbl
#'
#' # A table specification:
#' # - data: data.frame 'mtcars' 32 rows 11 cols
#' # - show_cols: all
#' # - use_attributes: all
#' # - title 1: 'Table 6.4'
#' # - title 2: 'MTCARS Sample Table'
#' # - footnote 1: '* Motor Trend, 1974'
#' # - define: mpg 'Miles Per Gallon' width=0.5 
#' # - define: cyl 'Cylinders' 
#' @import crayon
#' @export
print.table_spec <- function(x, ..., verbose = FALSE){
  

  if (verbose == TRUE) {
    
    # If verbose mode is indicated, print values as a list
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
    
  } else {
    
    if ("tbl_df" %in% class(x$data))
      dtyp = "tibble "
    else 
      dtyp = "data.frame "
   
    grey60 <- make_style(grey60 = "#999999")
    #pcolor <- make_style("snow")
    
    # Print header
    cat(grey60("# A table specification:\n"))
    cat(paste0("- data: ", dtyp, "'", x$dataname,
                 "' ", nrow(x$data), " rows ", ncol(x$data),
                 " cols\n"))
    
    if (!is.null(x$show_cols)) {
      cat(paste0("- show_cols: ",
            paste(as.character(x$show_cols), collapse = " "), "\n"))
      
    }
    
    if (all(!is.null(x$use_attributes))) {
      
      if (all(x$use_attributes == ""))
        ua <- "none"
      else if (all(x$use_attributes %in% c("label", "width", "justify", "format")))
        ua <- "all"
      else 
        ua <- paste(x$use_attributes, collapse = " ")
      
      cat(paste0("- use_attributes: ",  ua, "\n"))
      
  
    }
    
    if (!is.null(x$width))
      cat(paste0("- width: ", as.character(x$width), "\n"))
    
    if (!is.null(x$headerless)) {
      if (x$headerless != FALSE)
        cat(paste0("- headerless: ", as.character(x$headerless), "\n"))
    }
    
    if (!is.null(x$page_by)) {
      cat(paste0("- page by: ", x$page_by$var, "\n"))
      
    }
    
    print_title_header(x$title_hdr)
    
    print_titles(x$titles)
    
    print_footnotes(x$footnotes)
    
    
    # Print spanning headers
    if (!is.null(x$col_spans)) {
      
      for (def in x$col_spans) {
        
        cat(paste0("- spanning_header: from='", def$from,
            "' to='", def$to, "' "))
        
        if (!is.null(def[["label"]]))
          cat(paste0("'", def[["label"]], "' "))
        
        if (!is.null(def$level)) 
          cat(paste0("level=", def$level, " "))

        
        cat("\n")
        
      }
    }
    # dedupe=FALSE, id_var = FALSE, page_wrap = FALSE,
    
    # Print column definitions
    if (!is.null(x$col_defs)) {
      
      for (def in x$col_defs) {

        cat(paste0("- define: ", def$var_c, " "))
        if (!is.null(def[["label"]]))
          cat(paste0("'", def[["label"]], "' "))

        if (!is.null(def$width)) 
          cat(paste0("width=", def$width, " "))
        
        if (!is.null(def$align)) 
          cat(paste0("align='", def$align, "' "))
        
        if (def$visible == FALSE) 
          cat(paste0("visible='", def$visible, "' "))
        
        if (def$id_var == TRUE) 
          cat(paste0("id_var='", def$id_var, "' "))
        
        if (def$dedupe == TRUE) 
          cat(paste0("dedupe='", def$dedupe, "' "))

        if (def$page_wrap == TRUE) 
          cat(paste0("page_wrap='", def$page_wrap, "' "))
        
        if (def$page_break == TRUE) 
          cat(paste0("page_break='", def$page_break, "' "))
        
        cat("\n")
        
      }
    }
    
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
#' formatting function and pass it to the \code{n_format} function.  When an 
#' N value is supplied, the output of this function will be concatenated 
#' to the header label.
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
#' 
#' customN <- function(n) {
#'   return(paste0(": N=", n))
#' }
#' cat(paste0(l, customN(n)))
#' # Label: N=47
#' 
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
