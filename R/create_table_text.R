

# Globals -----------------------------------------------------------------


control_cols <- c("..blank", "..page", "..row")



# Create Tables -----------------------------------------------------------


#' @import fmtr
#' @noRd
create_tables_text <- function(rs, ts) {

  
  if (ts$show_cols == "only" & length(ts$col_defs) == 0) {
    
    stop("ERROR: At least one column must be defined if show_cols = \"only\".")
  }

  family <- "mono"
  font_name <- "Courier New"
  
  # Set up control columns
  dat <- as.data.frame(ts$data)  #as.data.frame(ts$data)
  dat$..blank <- ""
  dat$..page <- NA
  dat$..row <- NA
  
  # Get vector of all included column names
  # Not all columns in dataset are necessarily included
  # depends on show_all parameter on create_table and
  # visible parameter on column definitions
  keys <- get_table_cols(ts)
  #print("keys")
  #print(keys)

  # Filter dataset by included columns
  dat <- dat[ , keys]
  
  # Get labels
  labels <- get_labels(dat, ts$col_defs, ts$n_format)

  # Get column alignments
  aligns <- get_aligns(dat, ts$col_defs)
    
  # Get alignment for labels
  # Follows column alignment by default
  label_aligns <- get_label_aligns(ts$col_defs, aligns)

  # Get column formats
  formats(dat) <- get_col_formats(ts$col_defs)

  #print(formats(dat))
  
  # Get column widths
  widths_uom <- get_col_widths(dat, ts$col_defs, labels, font_family = family)

  # Convert to text measurements
  widths_char <- round(widths_uom / rs$char_width)

  # Apply formatting
  fdat <- fdata(dat)
  #print(fdat)

  # Add blank lines as specified
  fdat <- prep_data(fdat, ts$col_defs)
  #print(fdat)

  # Split long text strings onto multiple rows
  fdat <- split_cells(fdat, widths_char)

  # Apply widths and justification
  widths(fdat) <- widths_char
  justification(fdat) <- aligns
  fdat <- fdata(fdat)
  
  # Get available space for table data
  data_size <- get_data_size_text(rs, widths_uom, labels)
  #print(data_size)

  # Break columns into pages
  wraps <- get_page_wraps(data_size, ts$col_defs, widths_char)
  #wraps <- list(keys)
  #print("wraps")
  #print(wraps)


  # split rows
  #splits <- get_splits(fdat, widths, data_size, font_family = family)
  splits <- get_splits_text(fdat, widths_uom, data_size[["height"]])
  #print(splits)
  
  pg_lst <- list()
  for(s in splits) {
    for(pg in wraps) {
      pi <- page_info(data= s[, pg], keys = pg, label=labels[pg],
                     col_width = widths_uom[pg], col_align = aligns[pg],
                     font_name = font_name, label_align = label_aligns[pg])
      pg_lst[[length(pg_lst) + 1]] <- create_table_text(rs, ts, pi)
    }
  }
  
  return(pg_lst)
  
}

#' @noRd
create_table_text <- function(rs, ts, pi) {
  
 shdrs <- get_spanning_header(rs, ts, pi)   
 #print(shdrs)
 #print("Here1")
 hdrs <- get_table_header(rs, ts, pi)  
 #print("Here2")
 rws <- get_table_body(rs, ts, pi)
 #print("Here3")
 
 #print(length(hdrs))
 #print(length(rws))
 #print(rs$body_line_count - length(hdrs) - length(rws) - 2)
 blnks <- rep("", rs$body_line_count - length(shdrs) - length(hdrs) - length(rws))
 #print("Here4")
 ret <- c("", shdrs, hdrs, rws, blnks, "")
 
 return(ret) 
}

#' @description Return a vector of strings for the table header
#' @details Basic idea of this function is to create a list
#' of string vectors of label words sized according to the column 
#' widths, then combine by line/row, and concatenate everything and justify.
#' @noRd
get_table_header <- function(rs, ts, pi) {
  
  lbls <- pi$label
  lbla <- pi$label_align
  w <- round(pi$col_width / rs$char_width)
  
  ret <- c()
  ln <- c()

  # Wrap header labels if needed
  d <- data.frame(as.list(lbls))
  names(d) <- names(lbls)
  d <- split_cells(d, w)
  d <- push_down(d)

  # Label justification, width, and row concatenation
  for (i in seq_len(nrow(d))) {
    
    r <- ""

    for (nm in names(d)) {
      if (!is.control(nm))
        r <- paste0(r, format(d[i, nm], width = w[[nm]],
                            justify = get_justify(lbla[[nm]])), " ")
    }
    
    ln[[length(ln) + 1]] <- r 
  }
  
  
  # Underline
  sep <- paste0(paste0(rep("-", nchar(r) - 1), collapse = ""), " ")
  ln[[length(ln) + 1]] <- sep
    
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


#' @description Return a vector of strings for the table spanning headers
#' @details Basic idea of this function is to figure out which columns 
#' the header spans, add widths, then call get_table_header.  Everything
#' from there is the same.  
#' @import stats
#' @noRd
get_spanning_header <- function(rs, ts, pi) {
  
  spns <- ts$col_spans
  cols <- pi$keys
  cols <- cols[!is.controlv(cols)]
  w <- round(pi$col_width / rs$char_width)
  w <- w[cols]
  
  #print(cols)
  #print(w)

  # Figure out how many levels there are, 
  # and organize spans by level
  lvls <- c()     # Unique levels
  slvl <- list()  # Will be a list of lists of spans
  for (spn in spns) {

    if (!spn$level %in% lvls) {
      lvls[length(lvls) + 1] <- spn$level
      slvl[[spn$level]] <- list()
    }
    slvl[[spn$level]][[length(slvl[[spn$level]]) + 1]] <- spn
  }
  
  # Get unique levels and sort in decreasing order so we go from top down
  lvls <- sort(unique(lvls), decreasing = TRUE)
  #print(lvls)
  #print(slvl)
  
  # Create data structure to map spans to columns and columns widths by level
  # - Seed span_num with negative index numbers to identify unspanned columns
  # - Also add one to each column width for the blank space between columns 
  d <- data.frame(colname = cols, colwidth = w + 1, 
                  span_num = seq(from = -1, to = -length(cols), by = -1))
  
  wlvl <- list()  # Create one data structure for each level
  for (l in lvls) {

    t <- d  # Copy to temporary variable
    
    # if column is in spanning column list, populate structure with index.
    # Otherwise, leave as negative value.
    for (i in 1:length(slvl[[l]])) {
      cl <- slvl[[l]][[i]]$span_cols
      
      # Deal with from and to specified spans
      if ("from" %in% names(cl) & "to" %in% names(cl)) {
        sq <- seq(from = match(cl["from"], cols), to = match(cl["to"], cols))
        cl <- cols[sq]
      }
      
      # Span specifications can be a vector of column names or numbers
      if (typeof(cl) == "character")
        t$span_num <- ifelse(t$colname %in% cl, i, t$span_num)
      else 
        t$span_num <- ifelse(t$colname %in% cols[cl], i, t$span_num)
    }
    
    # Aggregate data structures to get span widths for each span
    s <- aggregate(x = list(width = t$colwidth), by = list(span = t$span_num), FUN = sum)

    # Then put back in original column order
    s$span <- factor(s$span, levels = unique(t$span_num))
    s <- s[order(s$span), ]
    rownames(s) <- NULL

    # Prep data structure
    s$span <- unique(t$span_num)
    s$label <- ""
    s$align <- ""
    s$n <- NA
    s$name <- ""
    
    # Populate data structure with labels, alignments, and n values from 
    # spanning column objects
    counter <- 1
    for (index in s$span) {
      if (index > 0) {
        s$label[counter] <- slvl[[l]][[index]]$label
        s$align[counter] <- slvl[[l]][[index]]$label_align
        if (!is.null(slvl[[l]][[index]]$n))
          s$n[counter] <- slvl[[l]][[index]]$n

      }
      s$name[counter] <- paste0("Span", counter)
      counter <- counter + 1
    }
    
    # Apply n counts to labels
    if (!is.null(ts$n_format)) {
      s$label <- ifelse(is.na(s$n), s$label, paste0(s$label, ts$n_format(s$n))) 
    }

    wlvl[[l]] <- s

  }
  
  # At this point we have a data frame with labels, etc. and spanning
  # column widths, and are ready to create spanning header rows
  #print(wlvl)
  
  # Format labels for each level
  ln <- c()
  for (l in lvls) {
    
    s <- wlvl[[l]]
    
    # Wrap header labels if needed
    d <- data.frame(as.list(s$label))
    w <- s$width
    j <- s$align
    names(d) <- s$name
    names(w) <- s$name
    names(j) <- s$name
    d <- split_cells(d, w)
    d <- push_down(d)
    
    #print(d)
    
    # Label justification, width, and row concatenation
    for (i in seq_len(nrow(d))) {

      r <- ""
      


      for (nm in names(d)) {
        if (!is.control(nm)) {

          if (j[[nm]] == "right") # Adjust 1 space for right alignment
            r <- paste0(r, format(d[i, nm], width = w[[nm]] -1,
                                  justify = get_justify(j[[nm]])), " ")
          else
            r <- paste0(r, format(d[i, nm], width = w[[nm]],
                                  justify = get_justify(j[[nm]])))
          

        }
      }

      ln[[length(ln) + 1]] <- r
    }
    
    r <- ""
    #print(s)
    
    for (i in seq_len(nrow(s))) {

      if (s$span[i] > 0) {
        r <- paste0(r, paste0(rep("-", s$width[i] - 1), collapse = ""), " ")
      } else {
        r <- paste0(r, paste0(rep(" ", s$width[i]), collapse = ""))
      }
    }
    
    # r <- ifelse(s$span > 0, paste0(r, paste0(rep("-", 4), collapse = ""), " "),
    #                         paste0(r, rep(" ", 4), collapse = ""))
    ln[[length(ln) + 1]] <- r
    
    #print(ln)
  }
  
  
  ret <- c()
  #print(rs$line_size)
  #print(ts$align)
  #print(ln)
  # Justify entire header
  for (k in seq_along(ln)) {

    ret[[k]] <- format(ln[[k]], width = rs$line_size,
                       justify = get_justify(ts$align))
  }

  ret <- unlist(ret)
  
  return(ret)
}


#' @noRd
get_table_body <- function(rs, ts, pi) {
  
  df <- pi$data
  ret <- c()
  nm <- names(df)
  
  for (i in seq_len(nrow(df))) {
    
    r <- ""
    for (j in seq_len(ncol(df))) {
      v <- ""
      p <-df[i, j]
      if (!is.null(p) & !is.na(p))
        v <- p

      if (!is.control(nm[j]) & trimws(df[i, "..blank"]) == "")
        r <- paste0(r, v, " ")
    }
    
    
    ret[length(ret) + 1] <- format(r, width = rs$line_size, 
                                   justify = get_justify(ts$align))
  
  }
  
  return(ret)
  
}




# Utilities ---------------------------------------------------------------

#' @noRd
is.control <- function(x) {
  
 ret <- FALSE
 if (!is.na(x)) {
   if (substr(x, 1, 2) == "..")
     ret <- TRUE
 }
 
 return(ret)
  
}

is.controlv <- Vectorize(function(x) {
  
  return(is.control(x))
})


#' @noRd
get_justify <- function(x) {
  
  ret <- "left"
  if (is.null(x) | is.na(x))
    ret <- "left"
  else if (x == "center")
    ret <- "centre"
  else if (x %in% c("left", "right", "centre"))
    ret <- x
  
  return(ret)
}


