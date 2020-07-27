

#' Creates a flextable
#' @param fi The flextable.
#' @noRd
create_flextable <- function(fi) {
  
  
  ret <- flextable(fi$data, theme_fun = NULL)
  ret <- theme_normal(ret, fontname = fi$font_name)
  ret <- set_header_labels(ret, values = fi$label)
  
  for(i in seq_along(fi$keys))
  {
    ret <- width(ret, j = i, width = fi$col_width[i])
    ret <- align(ret, j = i, align = fi$col_align[i], part = "body")
    ret <- align(ret, j = i, align = fi$label_align[i], part = "header")
    
  }
  
  
  
  return(ret)
}



# Internal functions ------------------------------------------------------


# Basic strategy of this function is to determine info for entire data table,
# then split it up according to pages.  For each page, create a flextable
# for that page.  Collect flextables in a list and return.
#' Create flextable objects
#' @noRd
create_flextables <- function(ts, body_size, font_name = "Courier New") {
  
  if (ts$show_cols == "only" & length(ts$col_defs) == 0) {
    
    stop("ERROR: At least one column must be defined if show_cols = \"only\".")
  }
  
  family <- get_font_family(font_name)
  
  # Get vector of all included column names
  # Not all columns in dataset are necessarily included
  # depends on show_all parameter on create_table and
  # visible parameter on column definitions
  keys <- get_table_cols(ts)
  
  # Filter dataset by included columns
  dat <- ts$data[ , keys]
  
  # Get labels
  labels <- get_labels(dat, ts$col_defs, ts$n_format)
  
  # Get column alignments
  aligns <- get_aligns(dat, ts$col_defs)
  
  # Get alignment for labels
  # Follows column alignment by default
  label_aligns <- get_label_aligns(ts$col_defs, aligns)
  
  
  # Get column widths
  widths <- get_col_widths(dat, ts$col_defs, labels, font_family = family)
  
  # Get available space for table data
  data_size <- get_data_size(body_size, widths, labels, font_family = family)
  
  
  # Break columns into pages
  wraps <- get_page_wraps(data_size, ts$col_defs, widths)
  
  
  # Add blank lines as specified
  dat <- prep_data(dat, ts$col_defs, first_blank = ts$first_row_blank)
  
  
  # split rows
  splits <- get_splits(dat, widths, data_size, font_family = family)
  
  
  flx_lst <- list()
  for(s in splits) {
    for(pg in wraps) {
      f <- flex_info(data= s[, pg], keys = pg, label=labels[pg],
                     col_width = widths[pg], col_align = aligns[pg],
                     font_name = font_name, label_align = label_aligns[pg])
      flx_lst[[length(flx_lst) + 1]] <- create_flextable(f)
    }
  }
  
  
  return(flx_lst)
  
}


# bs <- get_body_size(rpt)
# fts <- create_flextables(tb, bs)

# labels <- get_labels(tb$data, tb$col_defs, tb$n_format)


# Flex Info --------------------------------------------------------------------

#' An object to hold flextable information
#' @noRd
flex_info <- function(data, keys, font_name, col_width, col_align,
                      label, label_align) {
  
  ret <- structure(list(), class = c("flex_info", "list"))
  
  ret$data <- data
  ret$keys <- keys
  ret$font_name <- font_name
  ret$col_width <- col_width
  ret$col_align <- col_align
  ret$label <- label
  ret$label_align <- label_align
  
  
  return(ret)
  
}





