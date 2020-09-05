


# Table Spec Functions ---------------------------------------------------------
#' A function to create a table_spec object
#' @param x The data frame to create a table spec for.
#' @param show_cols Whether to show all column by default.  Valid values are
#' "all", "none", or a vector of column names.  "all" means show all columns 
#' by default, unless overridden by the column definitions.  
#' "none" means don't show any 
#' columns unless specified in the column definitions.  If a vector of column
#' names is supplied, those columns will be shown in the report in the order
#' specified, whether or not a definition is supplied.  
#' @param first_row_blank Whether to place a blank row under the table header.
#' @param n_format The format function to apply to the header n label.
#' @param headerless Whether to create a headerless table.  Default is FALSE. 
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

#' @title Defines a column specification for a table
#' @description A function to define the specification for a table column.
#' @details 
#' Column definitions are optional.  By default, all columns in the data
#' are displayed in the order and with the formatting attributes assigned.
#' The \strong{define} function is used to provide additional control over
#' the columns.  For example, you may use the \strong{define} function
#' to assign formatting properties, a label, and an "N=" population count
#' for the column header. See the parameters below for additional options.
#' 
#' @param x The table spec.
#' @param var The variable to define a column for.
#' @param label The label to use for the column header.
#' @param format The format to use for the column data.  The format can 
#' be a string format, a formatting function, or a format object from the 
#' \strong{fmtr} package.
# @param col_type The column type.
#' @param align The column alignment.  Value values are "left", "right", and
#' "center".
#' @param label_align How to align the header labels for this column.
#' Value values are "left", "right", and "center".
#' @param width The width of the column in inches.
#' @param visible Whether or not the column should be visible on the report.
#' @param n The n value to place in the n header label.
#' @param blank_after Whether to place a blank row after unique values of this
#' variable.
#' @param dedupe Whether to dedupe the values for this variable.  Variables
#' that are deduped only show the value on the first row in a group.
#' @param id_var Whether this variable should be considered an ID variable.
#' ID variables are retained on each page when the page is wrapped.  
#' @param page_wrap Force a page wrap on this variable.  A page wrap is a vertical
#' page break necessary when the table is too wide to fit on a single page.
#' The excess variables will be wrapped to the next page.
#' @param indent How much to indent the column values.  Parameter takes a 
#' numeric value that will be interpreted according to the 'uom' 
#' (Unit Of Measure) setting on the report.  
#' @param label_row Whether the values of the variable should be used to
#' create a label row.  Valid values are TRUE or FALSE.  Default is FALSE.
#' If label_row is set to TRUE, the dedupe parameter will also be set to TRUE.
#' @seealso \code{\link{create_table}} to create a table, and 
#' \link{table_options} to see define options illustrated.
#' @export
define <- function(x, var, label = NULL, format = NULL, #col_type = NULL,
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
#' @description Create a header that spans multiple columns.
#' @details 
#' A spanning header is a label and underline that spans one or more 
#' column headers.  A spanning header is defined minimally by identifying 
#' the columns to be spanned, and spanning header label.  A label alignment 
#' value may also be specified.
#' 
#' There are three ways to identify the columns to span: by a sequence of 
#' column positions, by a vector of column names, or by a named vector 
#' indicating "from" and "to" column names.  When identifying the spanning
#' column names, all names should be quoted.
#' @param x The table spec.
#' @param span_cols The columns to span.  The spanning columns may be defined as
#' a vector of column positions or names.  If defined by names, the names
#' should be quoted.  You may also supply a named vector, with the names
#' "from" and "to" equal to the starting and ending columns to span.
#' @param label The label to apply to the spanning header.
#' @param label_align The alignment to use for the label.Valid values are 
#' "left", "right", "center", and "centre".  The default for text columns is 
#' "left", and the default for numeric columns is "right".
#' @param level The level to use for the spanning header.  The lowest
#' spanning level is level 1, the next level above is level 2, and so on.  
#' By default, the level is set to 1.
#' @param n The n value to use for the n label on the spanning header.
#' @return The modified table spec.
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
#' @description Combine one or more columns into a nested report stub.  
#' @details 
#' Here are some details.
#' @param x The table spec.
#' @param vars A vector of quoted variable names from which to create the stub.
#' @param label The label for the report stub.
#' @param label_align The alignment for the label.  Valid values are 'left', 
#' 'right', 'center', and 'centre'.  Default is 'left'.
#' @param width The width of the stub, in report units of measure.
#' @param align How to align the stub column.  Valid values are 'left', 
#' 'right', 'center', and 'centre'.  Default is 'left'.
#' @param format A format to apply to the stub column.
#' @return The modified table spec.
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
#' on the \strong{n_format} parameter on the \code{\link{create_table}} function.
#' These formatting functions provide several options for formatting the "N=", 
#' including whether the "N" should be upper case or lower case, and whether
#' or not to put the value in parenthesis.  If one of these options does not 
#' meet the specifications for your report, you may also write your own 
#' formatting function and pass it to the \strong{n_format} function.  
#' @usage lowcase_parens(x)
#' @usage upcase_parens(x)
#' @usage lowcase_n(x)
#' @usage upcase_n(x)
#' @aliases lowcase_parens upcase_parens lowcase_n upcase_n
#' @seealso 
#' \code{\link{create_table}} function to create a table.
#' @param x Population count
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
