


# Table Spec Functions ---------------------------------------------------------
#' A function to create a table_spec object
#' @param x The data frame to create a table spec for.
#' @param n_format The format function to apply to the header n label.
#' @param page_var A variable in the data frame to use for a page variable.
#' @param show_cols Whether to show all column by default.  Valid values are
#' "all", "none", or a vector of column names.  "all" means show all columns 
#' by default, unless overridden by the column definitions.  
#' "none" means don't show any 
#' columns unless specified in the column definitions.  If a vector of column
#' names is supplied, those columns will be shown in the report, whether
#' or not a definition is supplied.
#' @param first_row_blank Whether to place a blank row under the table header.
#' @param align Aligns the table on the page.  Valid values are "left", 
#' "right", and "center".  Default value is "center".
#' @param headerless Whether to create a headerless table.  Default is FALSE. 
#' @export
create_table <- function(x, n_format = upcase_parens, page_var = NULL,
                         show_cols = "all", first_row_blank=FALSE, 
                         headerless = FALSE) {
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
  ret$page_var <- page_var
  ret$col_defs <- list()
  ret$col_spans <- list()
  ret$show_cols <- show_cols
  ret$first_row_blank <- first_row_blank
  ret$headerless <- headerless

  return(ret)

}

#' Defines a column specification
#' @param x The table spec.
#' @param var The variable to define a column for.
#' @param label The label to use for the column header.
#' @param format The format to use for the column data.  The format can 
#' be a string format, a formatting function, or a format object from the 
#' \strong{fmtr} package.
#' @param col_type The column type.
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
#' @param wrap Force a page wrap on this variable.  A page wrap is a vertical
#' page break necessary when the table is too wide to fit on a single page.
#' The excess variables will be wrapped to the next page.
#' @export
define <- function(x, var, label = NULL, format = NULL, col_type = NULL,
                   align=NULL, label_align=NULL, width=NULL,
                   visible=TRUE, n = NULL, blank_after=FALSE,
                   dedupe=FALSE, id_var = FALSE, wrap = FALSE) {
  
  # Check that variable exists
  var_c <- as.character(substitute(var))
  if (!is.null(x$data)) {
    if (!var_c %in% names(x$data)) {
      stop(paste0("Variable '", var_c, "' does not exist in data."))
      
    }
  }

  def <- structure(list(), class = c("col_def", "list"))
  
  def$var = deparse(substitute(var))
  def$var_c = var_c
  def$label = label
  def$format = format
  def$col_type = col_type
  def$align = align
  def$label_align = if (is.null(label_align) & !is.null(align))
                                align else label_align
  def$width = width
  def$visible = visible
  def$n = n
  def$blank_after = blank_after
  def$dedupe = dedupe
  def$id_var = id_var
  def$wrap = wrap

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

#' Defines options for the table
#' @param x The table spec.
#' @param first_row_blank Whether to create a blank on the first row after the
#' table header.
#' @export
table_options <- function(x, first_row_blank=FALSE){


  x$first_row_blank = first_row_blank


}

#' Prints the table spec
#' @param x The table spec.
#' @param ... Additional parameters.
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

# ts <- create_table(mtcars[1:10, ])
# ts
# rpt <- create_report("fork.out") %>% add_content(ts)
# rpt
