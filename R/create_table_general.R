

# Create Tables -----------------------------------------------------------

#' @details 
#' Order of these operations is very important.  Any change in the order 
#' will require considerable thought, and understanding of any potential
#' consequences.
#' @import fmtr
#' @noRd
create_table_pages_general <- function(rs, cntnt, lpg_rows) {
  
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
create_table_general <- function(rs, ts, pi, content_blank_row, wrap_flag, 
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
