

#' @import fmtr
#' @noRd
create_tables_text <- function(rs, ts) {

  
  if (ts$show_cols == "only" & length(ts$col_defs) == 0) {
    
    stop("ERROR: At least one column must be defined if show_cols = \"only\".")
  }

  family <- "mono"
  font_name <- "Courier New"
  
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

  justification(dat) <- aligns
    
  # Get alignment for labels
  # Follows column alignment by default
  label_aligns <- get_label_aligns(ts$col_defs, aligns)

  # Get column formats
  formats(dat) <- get_col_formats(ts$col_defs)

  #print(formats(dat))
  
  # Get column widths
  cwidths <- get_col_widths(dat, ts$col_defs, labels, font_family = family)

  # Convert to text measurements
  widths(dat) <- round(cwidths / rs$char_width)
  
  fdat <- fdata(dat)
  #print(fdat)

  #ret <- get_table_body(rs, ts, fdat)
  
  # Get available space for table data
  data_size <- get_data_size(rs$body_size, cwidths, labels, font_family = family)
  #print(data_size)

  # Break columns into pages
  wraps <- get_page_wraps(data_size, ts$col_defs, cwidths)
  #print(wraps)


  # Add blank lines as specified
  fdat <- prep_data(fdat, ts$col_defs)
  print(fdat)

  # split rows
  #splits <- get_splits(fdat, widths, data_size, font_family = family)
  #print(splits)
  splits <- list(fdat)
  

  pg_lst <- list()
  for(s in splits) {
    for(pg in wraps) {
      pi <- page_info(data= s[, pg], keys = pg, label=labels[pg],
                     col_width = cwidths[pg], col_align = aligns[pg],
                     font_name = font_name, label_align = label_aligns[pg])
      pg_lst[[length(pg_lst) + 1]] <- create_table_text(rs, ts, pi)
    }
  }
  
  
  return(pg_lst)
  
}

#' @noRd
create_table_text <- function(rs, ts, pi) {
  
 hdrs <- get_table_header(rs, ts, pi)  
  
 rws <- get_table_body(rs, ts, pi)
 
 blnks <- rep("", rs$body_line_count - length(hdrs) - length(rws) - 2)
 
 ret <- c("", hdrs, rws, blnks, "")
 
 return(ret) 
}

#' @noRd
get_table_header <- function(rs, ts, pi) {
  
  lbls <- pi$label
  lbla <- pi$label_align
  w <- round(pi$col_width / rs$char_width)
  
  ret <- c()
  ln <- c()
  
  #for (i in seq_along(pi$label) {
    
    r <- ""
    for (nm in names(lbls)) {
      
      r <- paste0(r, format(lbls[[nm]], width = w[[nm]], 
                            justify = get_justify(lbla[[nm]])), " ")
    }
    

    ln[[length(ln) + 1]] <- r 
    
    sep <- paste0(rep("-", nchar(r)), collapse = "")
    ln[[length(ln) + 1]] <- sep
    
  #}
    
  # Justify entire header
  for (k in seq_along(ln)) {
    
   ret[[k]] <- format(ln[[k]], width = rs$line_size, 
                    justify = get_justify(ts$align))
  }
  
  if (ts$first_row_blank)
    ret[[length(ret) + 1]] <- ""
  
  ret <- unlist(ret)
  
  return(ret)
}

#' @noRd
get_table_body <- function(rs, ts, pi) {
  
  df <- pi$data
  ret <- c()
  
  for (i in seq_len(nrow(df))) {
    
    r <- ""
    for (j in seq_len(ncol(df))) {
      v <- ""
      p <-df[i, j]
      if (!is.null(p) & !is.na(p))
        v <- p
      r <- paste0(r, v, " ")
    }
    
    
    ret[length(ret) + 1] <- format(r, width = rs$line_size, 
                                   justify = get_justify(ts$align))
  
  }
  
  return(ret)
  
}

#' @noRd
get_justify <- function(x) {
 
  ret <- "left"
  if (is.null(x))
    ret <- "left"
  else if (x == "center")
    ret <- "centre"
  else if (!is.na(x))
    ret <- x
  
  return(ret)
}

