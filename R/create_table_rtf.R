

# Create Tables ----------------------------------------------------------


#' @noRd
create_table_pages_rtf <- function(rs, cntnt, lpg_rows) {
  
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
  
  font_name <- rs$font
  
  # Set up control columns
  dat <- as.data.frame(ts$data, stringsAsFactors = FALSE)  
  dat$..blank <- ""
  dat$..row <- 1
  dat$..page_by <- NA
  
  # If page_break variable has been defined, use it
  if (is.null(ts$page_var)) {
    if (is.na(pgby_var))
      dat$..page <- NA
    else 
      dat$..page <-  dat[[pgby_var]]
    
  } else {
    if (any(class(dat[[ts$page_var]]) == "factor"))
      dat$..page <- as.character(dat[[ts$page_var]])
    else 
      dat$..page <- dat[[ts$page_var]]
  }
  
  # If page by is defined, use it
  if (!is.na(pgby_var)) {

    # Clear out factors on page by if they exist
    # Needed because split cells used to convert everything to characters
    if (any(class(dat[[pgby_var]]) == "factor")) {
      dat[[pgby_var]] <- as.character(dat[[pgby_var]] )
      dat$..page_by <- dat[[pgby_var]] 
      
      if (any(class(dat$..page) == "factor")) 
        dat$..page <- as.character(dat[[pgby_var]])
      
    } else {
      
      dat$..page_by <-  dat[[pgby_var]] 
    }
    
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
  fdat <- prep_data(fdat, ts, rs$char_width, rs$missing) # OK
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
  widths_uom <- get_col_widths_variable(fdat, ts, labels, 
                                        rs$font, rs$font_size, rs$units, 
                                        rs$gutter_width) 
  # print("Widths UOM")
  # print(widths_uom)
  
  # Split long text strings into multiple rows. Number of rows are stored in
  # ..row variable. If too slow, may need to be rewritten in C
  fdat <- split_cells_variable(fdat, widths_uom, rs$font, 
                                rs$font_size, rs$units, rs$output_type)$data 
  # print("split_cells")
  # print(fdat)
  
  
  # Break columns into pages
  wraps <- get_page_wraps(rs$line_size, ts, 
                          widths_uom, 0)  # No gutter width for RTF
  # print("wraps")
  # print(wraps)
  
  
  # Create a temporary page info to pass into get_content_offsets
  tmp_pi <- list(keys = keys, col_width = widths_uom, label = labels,
                 label_align = label_aligns, table_align = cntnt$align)
  # print("Temp PI")
  # print(tmp_pi)
  

  # Offsets are needed to calculate splits and page breaks
  content_offset <- get_content_offsets_rtf(rs, ts, tmp_pi, content_blank_row)
  

  # split rows
  splits <- get_splits_text(fdat, widths_uom, rs$body_line_count, 
                            lpg_rows, content_offset$lines, ts, TRUE)  
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
                                      rs$body_line_count, content_offset$lines, 
                                      nrow(s))
      #print(blnk_ind)
      
      if (!is.na(pgby_var))
        pgby <- trimws(s[1, "..page_by"])
      else 
        pgby <- NULL
      
      
      pi <- page_info(data= s[, pg], keys = pg, label=labels[pg],
                      col_width = widths_uom[pg], col_align = aligns[pg],
                      font_name = font_name, label_align = label_aligns[pg],
                      pgby, cntnt$align)
      pg_lst[[length(pg_lst) + 1]] <- create_table_rtf(rs, ts, pi, 
                                                        blnk_ind, wrap_flag,
                                                        lpg_rows)
    }
  }
  
  ret <- list(widths = widths_uom, page_list = pg_lst)
  
  return(ret)
  
  
}



#' @noRd
create_table_rtf <- function(rs, ts, pi, content_blank_row, wrap_flag, 
                              lpg_rows) {
  rh <- rs$row_height
  shdrs <- list(lines = 0, twips = 0)
  hdrs <- list(lines = 0, twips = 0)
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header_rtf(rs, ts, pi)   

    hdrs <- get_table_header_rtf(rs, ts, pi$col_width, 
                                 pi$label, pi$label_align, pi$table_align)  
  }
  
  # rs, ts, widths,  algns, halgns, talgn
  rws <- get_table_body_rtf(rs, pi$data, pi$col_width, 
                            pi$col_align, pi$table_align, ts$borders, 
                            ts$first_row_blank)
  
  # Default to content width
  ls <- rs$content_size[["width"]]
  
  # Get table width
  if (!is.null(pi$col_width))
     ls <- sum(pi$col_width, na.rm = TRUE)
  
  if (!is.null(ts$title_hdr))
    ttls <- get_title_header_rtf(ts$title_hdr, ls, rs, pi$table_align)
  else
    ttls <- get_titles_rtf(ts$titles, ls, rs, pi$table_align) 
  
  
  if (!is.null(rs$page_by)) {
    pgby <- get_page_by_rtf(rs$page_by, rs$content_size[["width"]], 
                            pi$page_by, rs, pi$table_align)
  } else if(!is.null(ts$page_by))
    pgby <- get_page_by_rtf(ts$page_by, ls, pi$page_by, rs, pi$table_align)
  else 
    pgby <- c()
  
  
  a <- NULL
  if (content_blank_row %in% c("above", "both"))
    a <- paste0("\\par", a)
  
  
  blnks <- c()
  
  # Get row count by summing ..row variable
  # if ("..row" %in% names(pi$data)) {
  #   rcnt <- sum(pi$data$..row)
  # } else {
  #   rcnt <- nrow(pi$data) 
  # }
    
  # Determine sum of all lines
  rc <- sum(ttls$lines, pgby$lines, shdrs$lines, 
           hdrs$lines, rws$lines,
           length(a))
  
  # Get footnotes, passing in sum of all current lines
  ftnts <- get_page_footnotes_rtf(rs, ts, ls, lpg_rows, rc,
                                  wrap_flag, content_blank_row,  pi$table_align)

  # Deal with cell padding.  Don't count this in line count.
  cp <- paste0("\\li", rs$cell_padding, "\\ri", rs$cell_padding, rs$spacing_multiplier)
  
  # On LibreOffice, have to protect the table from the title width or
  # the table row will inherit the title row width. Terrible problem.
  tpt <- "{\\pard\\fs1\\sl0\\par}"
  if (any(ts$borders %in% c("all", "top", "outside"))) {
    if (ttls$border_flag | rs$page_template$titles$border_flag |  
        rs$page_template$title_hdr$border_flag)
      tpt <- ""
    
    if (length(pgby) > 0) {
      if (pgby$border_flag)
        tpt <- ""
    }
  }
  
  # Same thing as above with titles.  If footnote block is contiguous
  # with table and footnotes are wider than table, row width of footnotes
  # will infect row width of table.  On libre only.  So this is to protect
  # the table width.
  bpt <- "{\\pard\\fs1\\sl0\\par}"
  if (any(ts$borders %in% c("all", "top", "outside", "body"))) {
    if (!is.null(ftnts)) {
      if (ftnts$border_flag)
        bpt <- ""
    }
    
    if (!is.null(rs$page_template$footnotes)) {
      if (rs$page_template$footnotes$border_flag)
        bpt <- ""
    }
  }
  
  ret <- list(rtf = c(a, cp, ttls$rtf, cp, pgby$rtf, tpt, cp, shdrs$rtf, 
                      hdrs$rtf, rws$rtf, bpt, cp, ftnts$rtf),
              lines = rc  + ftnts$lines)
    
  return(ret) 
}

get_page_footnotes_rtf <- function(rs, spec, spec_width, lpg_rows, row_count,
                                   wrap_flag, content_blank_row, talgn) {
  
  ftnts <- list(lines = 0, twips = 0, border_flag = FALSE)
  vflag <- "none"
  
  # Deal with valign parameter
  if (!is.null(spec$footnotes)) {
    if (!is.null(spec$footnotes[[length(spec$footnotes)]])) {
      if (spec$footnotes[[length(spec$footnotes)]]$valign == "bottom") {
        
        vflag <- "bottom"
        ftnts <- get_footnotes_rtf(spec$footnotes, 
                                   spec_width, rs, 
                                   talgn) 
      } else {
        vflag <- "top"
        ftnts <- get_footnotes_rtf(spec$footnotes, spec_width, rs, talgn) 
      }
      
    }
  } else {
    
    if (!is.null(rs$footnotes[[1]])) {
      if (!is.null(rs$footnotes[[1]]$valign)) {
        if (rs$footnotes[[1]]$valign == "top") {
          vflag <- "top"
          ftnts <- get_footnotes_rtf(rs$footnotes, 
                                     spec_width, rs, 
                                     talgn) 
        } else {
          
          if (wrap_flag)
            vflag <- "bottom" 
        }
      }
    }
  }
  
  b <- NULL
  blen <- 0
  if (content_blank_row %in% c("below", "both")) {
    b <- "\\par"
    blen <- 1
  }
  # } else {
  #   b <- paste0("\\fs1\\sl0\\par\\pard", rs$font_rtf, rs$spacing_multiplier)
  # }
  
  # Add extra offsets if table has a lot of borders turned on
  # to avoid undesired page wraps
  boff <- 0
  if (any(class(spec) == "table_spec") &
      any(spec$borders %in% c("all", "inside"))) {

    boff <- round(row_count * rs$border_height / rs$row_height)
  }
  
  ublnks <- c()
  lblnks <- c()
  
  # Determine number of filler lines needed
  len_diff <- rs$body_line_count - row_count - ftnts$lines - lpg_rows - blen - boff


  if (vflag == "bottom" & len_diff > 0) {
  
      ublnks <- c(b, rep("\\par", len_diff))
  
  } else {
    
    if ((wrap_flag & len_diff > 0)) {
      if (vflag == "bottom" | has_bottom_footnotes(rs))
        lblnks <- c(rep("\\par", len_diff), b)
    } else {
      lblnks <- b
    }
      
    
  }
  
  tlns <- sum(ftnts$lines, length(ublnks), length(lblnks))
  ret <- list(rtf = c(ublnks, ftnts$rtf, lblnks),
              lines = tlns,
              twips = tlns * rs$twip_conversion,
              border_flag = ftnts$border_flag)
  
  return(ret)
}

# Sub-Functions ---------------------------------------------------------


#' Get content offsets for table header, titles, footnotes, and content blanks.
#' Needed to calculate page breaks accurately.
#' @return A vector of upper and lower offsets
#' @noRd
get_content_offsets_rtf <- function(rs, ts, pi, content_blank_row) {
  
  ret <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  cnt <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  
  # Width is normally the width of the table, not the page
  wdth <- rs$content_size[["width"]]
  if (!is.null(pi$col_width))
    wdth <- sum(pi$col_width)
  
  # Default to zero
  shdrs <- list(lines = 0, twips = 0)
  hdrs <- list(lines = 0, twips = 0)
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header_rtf(rs, ts, pi)   

    hdrs <- get_table_header_rtf(rs, ts, pi$col_width, 
                                 pi$label, pi$label_align, 
                                 pi$table_align)  
  }
  
  # Get title headers or titles
  if (is.null(ts$title_hdr))
    ttls <- get_titles_rtf(ts$titles, wdth, rs) 
  else 
    ttls <- get_title_header_rtf(ts$title_hdr, wdth, rs)
  
  # Get page by if it exists
  pgb <- list(lines = 0, twips = 0)
  if (!is.null(ts$page_by))
    pgb <- get_page_by_rtf(ts$page_by, wdth, NULL, rs, pi$table_align)
  else if (!is.null(rs$page_by))
    pgb <- get_page_by_rtf(rs$page_by, wdth, NULL, rs, pi$table_align)

  
  #print(length(pgb))
  
  # print(paste("Table titles:", ttls))
  
  # Add everything up
  ret[["upper"]] <- shdrs$twips + hdrs$twips + ttls$twips + pgb$twips
  cnt[["upper"]] <- shdrs$lines + hdrs$lines + ttls$lines + pgb$lines
  
  if (content_blank_row %in% c("above", "both")) {
    ret[["blank_upper"]] <- rs$line_height
    cnt[["blank_upper"]] <- 1 
  }
  
  ftnts <- get_footnotes_rtf(ts$footnotes, wdth, rs) 
  rftnts <- get_footnotes_rtf(rs$footnotes, wdth, rs)
  
  if (has_top_footnotes(rs)) {
    ret[["lower"]] <- ftnts$twips + rftnts$twips
    cnt[["lower"]] <- ftnts$lines + rftnts$lines
  } else {
    ret[["lower"]] <- ftnts$twips
    cnt[["lower"]] <- ftnts$lines
  }
  
  # Add extra offsets if table has a lot of borders turned on
  # to avoid undesired page wraps
  if (any(ts$borders %in% c("all", "inside"))) {
    ret[["lower"]] <- ret[["lower"]] + (rs$row_height * 2)
    cnt[["lower"]] <- cnt[["lower"]] + 2
  }
    
  if (content_blank_row %in% c("both", "below")) {
    ret[["blank_lower"]] <- rs$line_height
    cnt[["blank_lower"]] <- 1 
  }
  
  res <- list(lines = cnt,
              twips = ret)
  
  return(res)
  
}


#' @description Return a vector of strings for the table header
#' @details Basic idea of this function is to create a list
#' of string vectors of label words sized according to the column 
#' widths, then combine by line/row, and concatenate everything and justify.
#' @noRd
get_table_header_rtf <- function(rs, ts, widths, lbls, halgns, talgn) {
  
  ret <- c()
  cnt <- 0
  rh <- rs$row_height
  tbl <- ts$data
  conv <- rs$twip_conversion
  nms <- names(lbls)
  nms <- nms[!is.controlv(nms)]
  
  # Get cell widths
  sz <- c()
  for (k in seq_along(widths)) {
    if (!is.control(nms[k])) {
      if (k == 1)
        sz[k] <- round(widths[k] * conv)
      else 
        sz[k] <- round(widths[k] * conv + sz[k - 1])
    }
  }
  
  # Table alignment
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"

  
  brdrs <- ts$borders
  if (length(ts$col_spans) > 0) {
    
    if (any(ts$borders %in% c("outside")))
      brdrs <- c("left", "right")
    
    if (any(ts$borders %in% "top"))
      brdrs <- brdrs[!brdrs %in% "top"]
    
  }
  
  # Header Cell alignment
  ha <- c()
  for (k in seq_along(halgns)) {
    if (!is.control(nms[k])) {
      if (halgns[k] == "left")
        ha[k] <- "\\ql"
      else if (halgns[k] == "right")
        ha[k] <- "\\qr"
      else if (halgns[k] %in% c("center", "centre"))
        ha[k] <- "\\qc"
    }
  }
  
  # Table Header
  ret[1] <-  paste0("\\trowd\\trgaph0", ta, "\\trrh", rh)
  
  # Loop for cell definitions
  for(j in seq_along(nms)) {
    if (!is.control(nms[j])) {
      b <- get_cell_borders(1, j, 2, length(nms), brdrs)
      ret[1] <- paste0(ret[1], "\\clvertalb", b, "\\clbrdrb\\brdrs\\cellx", sz[j])
    }
  }
  
  cnt <-  1 
  
  # Loop for column names
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = rs$font_size)
  
  for(k in seq_along(nms)) {
    if (!is.control(nms[k])) {
      
      # Split label strings if they exceed column width
      tmp <- split_string_rtf(lbls[k], widths[k], rs$units, rs$font)
      
      tb <- tmp$rtf
      if (ts$header_bold)
        tb <- paste0("\\b ", tmp$rtf, "\\b0")
      
      ret[1] <- paste0(ret[1], ha[k], " ", tb, "\\cell")

      # Add in extra lines for labels that wrap
      xtr <- tmp$lines
      if (xtr > cnt)
        cnt <- xtr
    }
  }
  dev.off()
  
  ret[1] <- paste(ret[1], "\\row")
  
  if (ts$first_row_blank == TRUE) {
    
    if (any(brdrs == "body"))
      b <- get_cell_borders(1, 1, 3, 1, c("left", "right"))
    else
      b <- get_cell_borders(1, 1, 3, 1, brdrs)
    
    w <- round(sum(widths, na.rm = TRUE) * conv)
    
    ret[1] <- paste0(ret[1], "\n\\trowd\\trgaph0", ta, 
                     "\\trrh", rh, b, "\\cellx", w,
                     "\\ql\\cell\\row")
   cnt <- cnt + 1
  }
  
  res <- list(rtf = ret,
              lines = cnt,
              twips = cnt * rh)
  
  return(res)

}

# Need to fix this
#' @description Return a vector of rtf strings for the table spanning headers
#' @details Basic idea of this function is to figure out which columns 
#' the header spans, add widths, then call get_table_header.  Everything
#' from there is the same.  
#' @import stats
#' @noRd
get_spanning_header_rtf <- function(rs, ts, pi) {
  
  spns <- ts$col_spans
  cols <- pi$keys
  cols <- cols[!is.controlv(cols)]
  w <- pi$col_width 
  w <- w[cols]
  gutter <- 0
  cnt <- c()
  
  # print("Cols:")
  # print(cols)
  #print(w)
  wlvl <- get_spanning_info(rs, ts, pi, w, gutter)
  lvls <- sort(seq_along(wlvl), decreasing = TRUE)
  
  # At this point we have a data frame with labels, etc. and spanning
  # column widths, and are ready to create spanning header rows
  #print(wlvl)
  
  conv <- rs$twip_conversion
  talgn <- pi$table_align

  # Table alignment
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"

  # Get borders
  brdrs <- ts$borders
  if (any(ts$borders %in% "bottom"))
    brdrs <- brdrs[!brdrs %in% "bottom"]
  
  rh <- rs$row_height
  
  # Format labels for each level
  ln <- c()
  for (l in lvls) {
    
    s <- wlvl[[l]]
    
    widths <- s$width
    names(widths) <- s$name
    algns <- s$align
    names(algns) <- s$name
    lbls <- s$label
    names(lbls) <- s$name
    cs <- s$col_span
    
    # Get cell widths
    sz <- c()
    for (k in seq_along(widths)) {

      if (k == 1)
        sz[k] <- round(widths[k] * conv)
      else
        sz[k] <- round(widths[k] * conv + sz[k - 1])
      
    }
    
    # Header Cell alignment
    ha <- c()
    for (k in seq_along(algns)) {

      if (algns[k] == "right")
        ha[k] <- "\\qr"
      else if (algns[k] %in% c("center", "centre"))
        ha[k] <- "\\qc"
      else
        ha[k] <- "\\ql"
      
    }

    r <- ""
    cnt[length(cnt) + 1] <- 1 
    
    # Table Header
    r <-  paste0("\\trowd\\trgaph0", ta, "\\trrh", rh)
    
    # Label justification, width, and row concatenation

    # Loop for cell definitions
    for(j in seq_along(sz)) {
      
      # Get cell borders
      b <- get_cell_borders(length(wlvl) - l + 1, j, 10, nrow(s), brdrs)
      
      # Add colspans
      vl <- lbls[j]
      
      # Special handling of borders for different situations.  
      # I hate this, but can't see a way around it.
      if (any(brdrs %in% c("all", "inside"))) {
        sflg <- "B"
        if (j > 1) {
          if (cs[j] > 1)
            sflg <- ""
        }
        
        if (vl == "") {
          bb <- get_cell_borders(length(lvls) - l + 1, j, length(lvls), 
                                      length(sz), brdrs, 
                                      flag = sflg)
        } else 
          bb <- b
      } else if (all(brdrs == "outside")) {
        
        if (vl == "")
          bb <- b
        else {
          
          bb <- b #paste0(b, "\\clbrdrb\\brdrs")
          
          
        }
        
      } else {
        
        if (vl == "") {
          bb <- get_cell_borders(length(lvls) - l + 1, j, length(lvls), 
                                      length(sz), brdrs, 
                                      flag = "")
        } else 
          bb <- b  #paste0(b, "\\clbrdrb\\brdrs")
        
      }
      
      
      if (s$span[j] > 0 & s$underline[j])
        r <- paste0(r, "\\clvertalb", bb, "\\clbrdrb\\brdrs\\cellx", sz[j])
      else 
        r <- paste0(r, "\\clvertalb", bb, "\\cellx", sz[j])
    }
    
    # Open device context
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    # Loop for labels
    for(k in seq_along(lbls)) {

      # Split label strings if they exceed column width
      tmp <- split_string_rtf(lbls[k], widths[k], rs$units, rs$font)
      
      # Concat label
      r <- paste0(r, ha[k], " ", tmp$rtf, "\\cell")
      # print(lbls[k])
      # print(widths[k])
      # Add in extra lines for labels that wrap
      xtr <- tmp$lines
      if (xtr > cnt[length(cnt)])
        cnt[length(cnt)] <- xtr
      
    }
    dev.off()
    
    
    r <- paste0(r, "\\row")
    
    ln[[length(ln) + 1]] <- r
    
  }
  
  
  ret <- unlist(ln)
  
  res <- list(rtf = ret, 
              lines = sum(cnt), 
              twips = sum(cnt) * rh)
  
  return(res)
}

#' @description This function counts lines per row independantly because
#' the ..row field does not account for page wrapping.  Need number
#' of lines on this particular page.
#' @noRd
get_table_body_rtf <- function(rs, tbl, widths, algns, talgn, tbrdrs, frb) {
  
  if ("..blank" %in% names(tbl))
    flgs <- tbl$..blank
  else 
    flgs <- NA
  
  # Count lines per row
  rws <- c()
  
  nms <- names(widths)
  nms <- nms[!is.na(nms)]
  nms <- nms[!is.controlv(nms)]
  wdths <- widths[nms]
  if (length(nms) == 1) {
    t <- as.data.frame(tbl[[nms]])
    names(t) <- nms
  } else 
    t <- tbl[ , nms]

  brdrs <- tbrdrs
  if (all(tbrdrs == "body"))
    brdrs <- c("top", "bottom", "left", "right")
  
  # Get line height.  Don't want to leave editor default.
  rh <- rs$row_height
  conv <- rs$twip_conversion
  
  
  # Get cell widths
  sz <- c()
  for (k in seq_along(wdths)) {
    if (!is.control(nms[k])) {
      if (k == 1)
        sz[k] <- round(wdths[k] * conv)
      else 
        sz[k] <- round(wdths[k] * conv + sz[k - 1])
    }
  }
  
  # Table alignment
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  # Cell alignment
  ca <- c()
  for (k in seq_along(algns)) {
    if (!is.control(nms[k])) {
      if (algns[k] == "left")
        ca[k] <- "\\ql"
      else if (algns[k] == "right")
        ca[k] <- "\\qr"
      else if (algns[k] %in% c("center", "centre"))
        ca[k] <- "\\qc"
    }
  }
  
  ret <- c()
  
  # Table Body
  for(i in seq_len(nrow(t))) {
    
    
    if (i ==  1)
      ret[i] <- paste0("{\\trowd\\trgaph0\\trrh", rh, ta)
    else 
      ret[i] <- paste0("\\trowd\\trgaph0\\trrh", rh, ta)
    
    
    # Loop for cell definitions
    for(j in seq_len(ncol(t))) {
      if (!is.control(nms[j])) {
        radj <- 0
        if (frb == TRUE) 
          radj <- 1
        
        b <- get_cell_borders(i + radj, j, nrow(t) + radj, ncol(t), brdrs, flgs[i])
        ret[i] <- paste0(ret[i], b, "\\cellx", sz[j])
      }
    }
    
    mxrw <- 1
    
    # Loop for cell values
    for(j in seq_len(ncol(t))) {
      

      if (!is.control(nms[j])) {
        
        # Construct rtf
        ret[i] <- paste0(ret[i], ca[j], " ", t[i, j], "\\cell")
        
        vl <- t[i, j]
        if (all(class(vl) != "character"))
          vl <- as.character(vl)
        
        # Count lines in cell 
        cl <- strsplit(vl, "\\line", fixed = TRUE)[[1]]
        if (length(cl) > mxrw)
            mxrw <- length(cl)
      }
      
    }
    
    rws[i] <- mxrw
    
    ret[i] <- paste0(ret[i], "\\row")
    
    
  }
  

  ret[length(ret)] <- paste0(ret[length(ret)], "}\\pard",
                             rs$font_rtf, rs$spacing_multiplier)
  
  
  res <- list(rtf = ret,
              lines = sum(rws))
  
  return(res)
  
  
}

