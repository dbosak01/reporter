

# Globals -----------------------------------------------------------------


control_cols <- c("..blank", "..page", "..row", "..page_by")



# Create Tables -----------------------------------------------------------

#' @details 
#' Order of these operations is very important.  Any change in the order 
#' will require considerable thought, and understanding of any potential
#' consequences.
#' @import fmtr
#' @noRd
create_table_pages_text <- function(rs, cntnt, lpg_rows) {

  #print("Create Table")
  
  ts <- cntnt$object
  content_blank_row <- cntnt$blank_row
  
  pgby_var <- NA
  if (!is.null(rs$page_by))
    pgby_var <- rs$page_by$var
  else if (!is.null(ts$page_by))
    pgby_var <- ts$page_by$var
  
  
  if (all(ts$show_cols == "none") & length(ts$col_defs) == 0) {
    
    stop("ERROR: At least one column must be defined if show_cols = \"none\".")
  }

  font_name <- "Courier New"
  
  
  # Set up control columns
  dat <- as.data.frame(ts$data, stringsAsFactors = FALSE)  
  dat$..blank <- ""
  dat$..row <- NA
  dat$..page_by <- NA
  
  # If page_break variable has been defined, use it
  if (is.null(ts$page_var)) {
    if (is.na(pgby_var))
      dat$..page <- NA
    else 
      dat$..page <-  dat[[pgby_var]]
    
  } else 
    dat$..page <- dat[[ts$page_var]]
  
  # If page by is defined, use it
  if (!is.na(pgby_var)) {
    dat$..page_by <-  dat[[pgby_var]]
    if (is.unsorted(dat[[pgby_var]], strictly = FALSE))
      message("Page by variable not sorted.")
  }

  # Get vector of all included column names
  # Not all columns in dataset are necessarily included
  # depends on show_cols parameter on create_table and
  # visible parameter on column definitions
  keys <- get_table_cols(ts)
  # print("keys")
  # print(keys)

  # Filter dataset by included columns
  dat <- get_data_subset(dat, keys, rs$preview)
   # print("Key columns:")
   # print(dat)
  
  # Update column definitions with column defaults
  ts$col_defs <- set_column_defaults(ts, keys)
  # print("col_defs:")
  # print(ts$col_defs)
  
  # Get labels
  labels <- get_labels(dat, ts)
  # print("Labels:")
  # print(labels)
  
  # Get column alignments
  aligns <- get_aligns(dat, ts)
    
  # Get alignment for labels
  # Follows column alignment by default
  label_aligns <- get_label_aligns(ts, aligns)
  # print("Label Aligns:")
  # print(label_aligns)

  # Clear out existing formats
  cdat <- clear_formats(dat)
  
  # Get column formats
  formats(cdat) <- get_col_formats(dat, ts)
  # print("formats:")
  # print(formats(cdat))

  # Apply formatting
  fdat <- fdata(cdat)
  # print("fdata:")
  # print(fdat)
  
  # Prep data for blank lines, indents, and stub columns
  fdat <- prep_data(fdat, ts, rs$char_width, rs$missing)
  # print("prep_data")
  # print(fdat)
  # str(fdat)
  
  # Reset keys, since prep_data can add/remove columns for stub
  keys <- names(fdat)
  # print("Keys")
  # print(keys)
  # print("Aligns")
  # print(aligns)
  
  # Copy any width attributes to formatted data frame
  if ("width" %in% ts$use_attributes)
    widths(fdat) <- widths(dat)
  # print("Original Widths")
  # print(widths(dat))
  
  # Get column widths
  widths_uom <- get_col_widths(fdat, ts, labels, rs$char_width, rs$units)
  # print("Widths UOM")
  # print(widths_uom)
  
  # Try to figure out a way to put these widths in report spec
  #rs$column_widths[length(rs$column_widths) + 1] <- widths_uom

  # Convert to text measurements
  # Adjust by -1 to account for space between columns
  widths_char <- round(widths_uom / rs$char_width) - 1
  # print("Widths Char")
  # print(widths_char)

  # Split long text strings into multiple rows
  fdat <- split_cells(fdat, widths_char)
  # print("split_cells")
  # print(fdat)

  # Apply widths and justification
  widths(fdat) <- widths_char
  # print(widths_char)
  justification(fdat) <- aligns
  # print(aligns)
  fdat <- fdata(fdat)
  # print("fdata2")
  # print(fdat)
  
  # Break columns into pages
  wraps <- get_page_wraps(rs$line_size, ts, widths_char)
  # print("wraps")
  # print(wraps)

  # Create a temporary page info to pass into get_content_offsets
  tmp_pi <- list(keys = keys, col_width = widths_uom, label = labels,
                 label_align = label_aligns)
  # print("Temp PI")
  # print(tmp_pi)
  
  # Offsets are needed to calculate splits and page breaks
  content_offset <- get_content_offsets(rs, ts, tmp_pi, content_blank_row)
  
  # split rows
  splits <- get_splits_text(fdat, widths_uom, rs$body_line_count, 
                            lpg_rows, content_offset, ts)
  # print("splits")
  # print(splits)
  
  # Subset splits by preview, if requested
  if (!is.null(rs$preview)) {
    if (rs$preview < length(splits))
      splits <- splits[seq(1, rs$preview)] 
  }
  
  tot_count <- length(splits) * length(wraps)
  counter <- 0
  wrap_flag <- FALSE
  blnk_ind <- "none"
  
  pg_lst <- list()
  for(s in splits) {
    for(pg in wraps) {
      counter <- counter + 1

      if (counter < tot_count)
        wrap_flag <- TRUE
      else 
        wrap_flag <- FALSE
      
      #print(s)
      # Ensure content blank rows are added only to the first and last pages
      blnk_ind <- get_blank_indicator(counter, tot_count, content_blank_row,
                                      rs$body_line_count, content_offset, nrow(s))
      #print(blnk_ind)
      
      if (!is.na(pgby_var))
        pgby <- trimws(s[1, "..page_by"])
      else 
        pgby <- NULL
      
      
      pi <- page_info(data= s[, pg], keys = pg, label=labels[pg],
                     col_width = widths_uom[pg], col_align = aligns[pg],
                     font_name = font_name, label_align = label_aligns[pg],
                     pgby)
      pg_lst[[length(pg_lst) + 1]] <- create_table_text(rs, ts, pi, 
                                                        blnk_ind, wrap_flag,
                                                        lpg_rows)
    }
  }
  
  ret <- list(widths = widths_uom, page_list = pg_lst)
  
  return(ret)
  
}

#' @noRd
create_table_text <- function(rs, ts, pi, content_blank_row, wrap_flag, 
                              lpg_rows) {
  
  shdrs <- c()
  hdrs <- c()
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header(rs, ts, pi)   
    hdrs <- get_table_header(rs, ts, pi)  
  }
  
  rws <- get_table_body(pi$data)
  
  
  ls <- rs$line_size
  if (length(rws) > 0)
    ls <- max(nchar(rws))
  
  if (!is.null(ts$title_hdr))
    ttls <- get_title_header(ts$title_hdr, ls - 1, rs$uchar)
  else
    ttls <- get_titles(ts$titles, ls, rs$uchar) 
  
  ftnts <- c()
  vflag <- FALSE
  
  # Deal with valign paramter
  if (!is.null(ts$footnotes)) {
    if (!is.null(ts$footnotes[[length(ts$footnotes)]])) {
      if (ts$footnotes[[length(ts$footnotes)]]$valign == "bottom") {

        vflag <- TRUE
        ftnts <- get_footnotes(ts$footnotes, rs$line_size, rs$uchar) 
      } else {
        
        ftnts <- get_footnotes(ts$footnotes, ls, rs$uchar) 
      }
    
    }
  } else {

    if (!is.null(rs$footnotes[[1]])) {
      if (!is.null(rs$footnotes[[1]]$valign)) {
        if (rs$footnotes[[1]]$valign == "top") {

          ftnts <- get_footnotes(rs$footnotes, rs$line_size, rs$uchar) 
        } 
      }
    }
  }
  #print("Titles")
  #print(ttls)
  
  if (!is.null(rs$page_by))
    pgby <- get_page_by(rs$page_by, rs$line_size - 1, pi$page_by)
  else if(!is.null(ts$page_by))
    pgby <- get_page_by(ts$page_by, ls, pi$page_by)
  else 
    pgby <- c()
  
  # print("Create table text:")
  # print(length(pgby))
  
  a <- NULL
  if (content_blank_row %in% c("above", "both"))
    a <- ""
  
  b <- NULL
  if (content_blank_row %in% c("below", "both"))
    b <- ""
  
  blnks <- c()
  if (vflag) {
    ret <- c(a, ttls, pgby, shdrs, hdrs, rws, b)

    len_diff <- rs$body_line_count - lpg_rows - length(ret) - length(ftnts)
    
    # At some point will have to deal with wrap flag
    # Right now cannot put anything in between end of content
    # and the start of the footnote.  Edge case but should still
    # fix at some point.
    if (len_diff > 0) {
      blnks <- rep("", len_diff)
      ret <- c(ret, blnks, ftnts) 
    }
      
  } else { 
    
    ret <- c(a, ttls, pgby, shdrs, hdrs, rws, ftnts, b)
    
    len_diff <- rs$body_line_count - lpg_rows - length(ret)
    if (wrap_flag & len_diff > 0) {
      blnks <- rep("", len_diff)
      ret <- c(ret, blnks) 
    }
    
  }
  

  
  return(ret) 
}

# Sub-Functions -----------------------------------------------------------


#' Get content offsets for table header, titles, footnotes, and content blanks.
#' Needed to calculate page breaks accurately.
#' @return A vector of upper and lower offsets
#' @noRd
get_content_offsets <- function(rs, ts, pi, content_blank_row) {
  
  ret <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  
  shdrs <- c()
  hdrs <- c()
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header(rs, ts, pi)   
    hdrs <- get_table_header(rs, ts, pi)  
  }

  if (is.null(ts$title_hdr))
    ttls <- get_titles(ts$titles, rs$line_size, rs$uchar) 
  else 
    ttls <- get_title_header(ts$title_hdr, rs$line_size, rs$uchar)
  
  pgb <- c()
  if (!is.null(ts$page_by))
    pgb <- get_page_by(ts$page_by, rs$line_size, NULL)
  else if (!is.null(rs$page_by))
    pgb <- get_page_by(rs$page_by, rs$line_size, NULL)
  
  #print(length(pgb))
  
  # print(paste("Table titles:", ttls))
  
  ret["upper"] <- length(shdrs) + length(hdrs) + length(ttls) + length(pgb)
  
  if (content_blank_row %in% c("above", "both"))
      ret["blank_upper"] <- 1
  
  ftnts <- get_footnotes(ts$footnotes, rs$line_size) 
  
  ret["lower"] <- length(ftnts) 

  if (content_blank_row %in% c("both", "below"))
    ret["blank_lower"] <- 1

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
  w <- round(pi$col_width / rs$char_width) - 1 # Adjust for gutter
  # print("Label A")
  # print(lbla)
  ret <- c()
  ln <- c()

  # Wrap header labels if needed
  d <- data.frame(as.list(lbls), stringsAsFactors = FALSE)
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
  if (rs$output_type == "PDF")
    sep <- paste0(paste0(rep("-", nchar(r) - 1), collapse = ""), " ")
  else
    sep <- paste0(paste0(rep(rs$uchar, nchar(r) - 1), collapse = ""), " ")
  
  ln[[length(ln) + 1]] <- sep

  
  ret <- ln
  
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
  w <- round(pi$col_width / rs$char_width) - 1 # Adjust for gutter
  w <- w[cols]
  
  # print("Cols:")
  # print(cols)
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
  # print("Levels:")
  # print(lvls)
  # 
  # print("Spanning levels:")
  # print(slvl)
  
  # Create data structure to map spans to columns and columns widths by level
  # - Seed span_num with negative index numbers to identify unspanned columns
  # - Also add one to each column width for the blank space between columns 
  d <- data.frame(colname = cols, colwidth = w + 1, 
                  span_num = seq(from = -1, to = -length(cols), by = -1), 
                  stringsAsFactors = FALSE)
  
  wlvl <- list()  # Create one data structure for each level
  for (l in lvls) {

    t <- d  # Copy to temporary variable
    
    # if column is in spanning column list, populate structure with index.
    # Otherwise, leave as negative value.
    for (i in 1:length(slvl[[l]])) {
      cl <- slvl[[l]][[i]]$span_cols
      
      
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
    s$underline <- TRUE
    
    # Populate data structure with labels, alignments, and n values from 
    # spanning column objects
    counter <- 1
    for (index in s$span) {
      if (index > 0) {
        s$label[counter] <- slvl[[l]][[index]]["label"]
        s$align[counter] <- slvl[[l]][[index]]$label_align
        s$underline[counter] <- slvl[[l]][[index]]$underline
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
    d <- data.frame(as.list(s$label), stringsAsFactors = FALSE)
    w <- s$width
    j <- s$align
    names(d) <- s$name
    names(w) <- s$name
    names(j) <- s$name
    
    # print("Ready for split cells")
    # print(paste("d:", d))
    # print(paste("w:", w))
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
        if (rs$output_type == "PDF")
          r <- paste0(r, paste0(rep("-", s$width[i] - 1), collapse = ""), " ")
        else {
          if (s$underline[i])
            r <- paste0(r, paste0(rep(rs$uchar, s$width[i] - 1), collapse = ""), " ")
          else 
            r <- paste0(r, paste0(rep(" ", s$width[i] - 1), collapse = ""), " ")
        }
          
      } else {
        r <- paste0(r, paste0(rep(" ", s$width[i]), collapse = ""))
      }
    }
    
    # r <- ifelse(s$span > 0, paste0(r, paste0(rep("-", 4), collapse = ""), " "),
    #                         paste0(r, rep(" ", 4), collapse = ""))
    ln[[length(ln) + 1]] <- r
    
    #print(ln)
  }

  ret <- unlist(ln)
  
  return(ret)
}


#' @noRd
get_table_body <- function(dat) {
  
  df <- dat
  ret <- c()
  nm <- names(df)
  
  for (i in seq_len(nrow(df))) {
    
    r <- ""
    for (j in seq_len(ncol(df))) {
      v <- ""
      p <-df[i, j]
      if (!is.null(p) & !is.na(p))
        v <- p

      if (!is.control(nm[j]) & 
          (trimws(df[i, "..blank"]) == "" | trimws(df[i, "..blank"]) == "L")) {

        r <- paste0(r, v, " ")

      }
    }
    
    ret[length(ret) + 1] <- r
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

#' @noRd
get_blank_indicator <- function(pg_num, tot_pg, content_blanks,
                                page_size, content_offset, num_rows) {
  
  if (pg_num == 1 & pg_num == tot_pg & content_blanks == "both")
    blnk_ind <- "both"
  else if (pg_num == 1 & content_blanks %in% c("both", "above"))
    blnk_ind <- "above"
  else if ((pg_num == tot_pg & content_blanks %in% c("both", "below")) &
           (num_rows < page_size - content_offset["upper"] - content_offset["lower"])) {
    # Exception when number of rows exactly equals available space.
    # Then don't put a blank row below.
    # print(num_rows)
    # print( page_size )
    # print(content_offset)
    blnk_ind <- "below"
  } else 
    blnk_ind <- "none"
  
  return(blnk_ind)
}

