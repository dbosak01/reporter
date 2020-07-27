

#' @import fmtr
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

  
  # Get column widths
  cwidths <- get_col_widths(dat, ts$col_defs, labels, font_family = family)

  # Convert to text measurements
  widths(dat) <- round(cwidths / rs$char_width)
  
  fdat <- fdata(dat)
  print(fdat)

  #ret <- get_table_body(rs, ts, fdat)
  
  # Get available space for table data
  data_size <- get_data_size(rs$body_size, cwidths, labels, font_family = family)
  print(data_size)

  # Break columns into pages
  wraps <- get_page_wraps(data_size, ts$col_defs, cwidths)
  print(wraps)


  # Add blank lines as specified
  #fdat <- prep_data(fdat, ts$col_defs, first_blank = ts$first_row_blank)
  #print(fdat)

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


create_table_text <- function(rs, ts, pi) {
  
 hdrs <- get_table_header(rs, ts, pi)  
  
 rws <- get_table_body(rs, ts, pi)
 
 blnks <- rep("", rs$body_line_count - length(hdrs) - length(rws) - 2)
 
 ret <- c("", hdrs, rws, blnks, "")
 
 return(ret) 
}

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


get_table_body <- function(rs, ts, pi) {
  
  df <- pi$data
  ret <- c()
  
  for (i in seq_len(nrow(df))) {
    
    r <- ""
    for (j in seq_len(ncol(df))) {
      
      r <- paste0(r, df[i, j], " ")
    }
    
    
    ret[length(ret) + 1] <- format(r, width = rs$line_size, 
                                   justify = get_justify(ts$align))
  
  }
  
  return(ret)
  
}

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



create_flextables2 <- function(ts, body_size, font_name = "Courier New") {
  
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

get_table_body2 <- function(rs, ts) {
  
  df <- ts$data
  pad <- 1
  
  ret <- c()
  
  ## error checking
  stopifnot(is.data.frame(df))
  ## internal functions
  SepLine <- function(n, pad = 1) {
    tmp <- lapply(n, function(x, pad) paste(rep("-", x + (2* pad)),
                                            collapse = ""),
                  pad = pad)
    paste0("+", paste(tmp, collapse = "+"), "+")
  }
  Row <- function(x, n, pad = 1) {
    foo <- function(i, x, n) {
      fmt <- paste0("%", n[i], "s")
      sprintf(fmt, as.character(x[i]))
    }
    rowc <- sapply(seq_along(x), foo, x = x, n = n)
    paste0("|", paste(paste0(rep(" ", pad), rowc, rep(" ", pad)),
                      collapse = "|"),
           "|")
  }
  ## convert everything to characters
  df <- as.matrix(df)
  ## nchar in data
  mdf <- apply(df, 2, function(x) max(nchar(x)))
  ## nchar in names
  cnames <- nchar(colnames(df))
  ## max nchar of name+data per elements
  M <- pmax(mdf, cnames)
  ## write the header
  sep <- SepLine(M, pad = pad)
  ret[1] <- sep
  ret[2] <- Row(colnames(df), M, pad = pad)
  ret[3] <- sep
  ## write the rows
  for(i in seq_len(nrow(df))) {
    ## write a row
      ret[length(ret) + 1] <- Row(df[i,], M, pad = pad)
      ## write separator
      ret[length(ret) + 1] <- sep
  }
  
  
  # Set alignment and body height
  for (i in 1:rs$body_line_count) {
    if (!is.na(ret[i])) {
      if (ts$align == "center") {
        ret[i] <- stri_pad_both(ret[i], width = rs$line_size) 
      } else if (ts$align == "right") {
        ret[i] <- stri_pad_left(ret[i], width = rs$line_size)
      }
    } else {
      ret[i] <- ""
    }
  }
  
  return(ret)
}
