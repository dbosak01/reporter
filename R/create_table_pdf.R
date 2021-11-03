

# Create Tables ----------------------------------------------------------


#' @noRd
create_table_pages_pdf <- function(rs, cntnt, lpg_rows) {
  
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
  sp <- split_cells_variable(fdat, widths_uom, rs$font, 
                               rs$font_size, rs$units, rs$output_type) 
  fdat <- sp$data
  wdat <- sp$widths
  
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
  content_offset <- get_content_offsets_pdf(rs, ts, tmp_pi, content_blank_row)
  
  # Need to do something with widths
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
  spstart <- 1
  spend <- 1
  
  pg_lst <- list()
  for(s in splits) {
    
    # Subset text widths by current split rows
    spend <- spstart + nrow(s) - 1
    spwidths <- wdat[seq(spstart, spend)]
    
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
      pg_lst[[length(pg_lst) + 1]] <- create_table_pdf(rs, ts, pi, 
                                                       blnk_ind, wrap_flag,
                                                       lpg_rows, spwidths)
    }
    
    spstart <- spend + 1
  }
  
  ret <- list(widths = widths_uom, page_list = pg_lst)
  
  return(ret)
  
  
}



#' @noRd
create_table_pdf <- function(rs, ts, pi, content_blank_row, wrap_flag, 
                             lpg_rows, spwidths) {
  rh <- rs$row_height
  ys <- lpg_rows * rh
  conv <- rs$point_conversion
  
  # Default to content width
  ls <- rs$content_size[["width"]]
  
  ys <- sum(ys, rs$page_template$page_header$points, 
            rs$page_template$titles$points, rs$page_template$title_hdr$points)
  
  # Get table width
  if (!is.null(pi$col_width))
    ls <- sum(pi$col_width, na.rm = TRUE)
  
  if (content_blank_row %in% c("above", "both"))
    ys <- ys + rh
  
  if (!is.null(ts$title_hdr))
    ttls <- get_title_header_pdf(ts$title_hdr, ls, rs, pi$table_align, 
                                 ystart = ys)
  else
    ttls <- get_titles_pdf(ts$titles, ls, rs, pi$table_align, ystart = ys) 
  
  ys <-  ys + ttls$points 
  
  if (!is.null(rs$page_by)) {
    pgby <- get_page_by_pdf(rs$page_by, rs$content_size[["width"]], 
                            pi$page_by, rs, pi$table_align, ystart = ys)
  } else if(!is.null(ts$page_by))
    pgby <- get_page_by_pdf(ts$page_by, ls, pi$page_by, rs, pi$table_align, 
                            ystart = ys)
  else 
    pgby <- c()
  
  if (length(pgby) > 0)
    ys <- ys + pgby$points
  
  shdrs <- list(lines = 0, points = 0)
  hdrs <- list(lines = 0, points = 0)
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header_pdf(rs, ts, pi, ystart = ys)
    
    ys <- ys + shdrs$points
    
    hdrs <- get_table_header_pdf(rs, ts, pi$col_width, 
                                 pi$label, pi$label_align, 
                                 pi$table_align, ystart = ys)
    
    ys <- ys + hdrs$points
  }
  
  
  # rs, ts, widths,  algns, halgns, talgn
  bdy <- get_table_body_pdf(rs, pi$data, pi$col_width, 
                            pi$col_align, pi$table_align, ts$borders,
                            ystart = ys,
                            spwidths)
  ys <- ys + bdy$points


  
  
  # blnks <- c()
  
  # Get row count by summing ..row variable
  # if ("..row" %in% names(pi$data)) {
  #   rcnt <- sum(pi$data$..row)
  # } else {
  #   rcnt <- nrow(pi$data) 
  # }
  
  
  # Get footnotes, passing in sum of all current lines
  ftnts <- get_page_footnotes_pdf(rs, ts, ls, lpg_rows, ys,
                                  wrap_flag, content_blank_row,  pi$table_align)
  
  # Deal with cell padding.  Don't count this in line count.
  # cp <- paste0("\\li", rs$cell_padding, "\\ri", rs$cell_padding, rs$spacing_multiplier)
  
  # # On LibreOffice, have to protect the table from the title width or
  # # the table row will inherit the title row width. Terrible problem.
  # tpt <- "{\\pard\\fs1\\sl0\\par}"
  # if (any(ts$borders %in% c("all", "top", "outside"))) {
  #   if (ttls$border_flag | rs$page_template$titles$border_flag |  
  #       rs$page_template$title_hdr$border_flag)
  #     tpt <- ""
  #   
  #   if (length(pgby) > 0) {
  #     if (pgby$border_flag)
  #       tpt <- ""
  #   }
  # }
  # 
  # # Same thing as above with titles.  If footnote block is contiguous
  # # with table and footnotes are wider than table, row width of footnotes
  # # will infect row width of table.  On libre only.  So this is to protect
  # # the table width.
  # bpt <- "{\\pard\\fs1\\sl0\\par}"
  # if (any(ts$borders %in% c("all", "top", "outside", "body"))) {
  #   if (!is.null(ftnts)) {
  #     if (ftnts$border_flag)
  #       bpt <- ""
  #   }
  #   
  #   if (!is.null(rs$page_template$footnotes)) {
  #     if (rs$page_template$footnotes$border_flag)
  #       bpt <- ""
  #   }
  # }
  
  # Determine sum of all lines
  rc <- sum(ttls$lines, pgby$lines, shdrs$lines, 
            hdrs$lines, bdy$lines, ftnts$lines)
  
  ret <- list(pdf = c(ttls$pdf, pgby$pdf, shdrs$pdf, 
                      hdrs$pdf, bdy$pdf, ftnts$pdf),
              lines = rc,
              points = rc * rh)
  
  return(ret) 
}

get_page_footnotes_pdf <- function(rs, spec, spec_width, lpg_rows, ystart,
                                   wrap_flag, content_blank_row, talgn) {
  
  ftnts <- list(lines = 0, twips = 0, border_flag = FALSE)
  vflag <- "none"
  
  fl <- rs$page_template$page_footer$lines
  
  # Deal with valign parameter
  if (!is.null(spec$footnotes)) {
    if (!is.null(spec$footnotes[[length(spec$footnotes)]])) {
      if (spec$footnotes[[length(spec$footnotes)]]$valign == "bottom") {
        
        vflag <- "bottom"
        ftnts <- get_footnotes_pdf(spec$footnotes, 
                                   spec_width, rs, 
                                   talgn, footer_lines = fl) 
      } else {
        vflag <- "top"
        ftnts <- get_footnotes_pdf(spec$footnotes, spec_width, rs, talgn,
                                   ystart = ystart) 
      }
      
    }
  } else {
    
    if (!is.null(rs$footnotes[[1]])) {
      if (!is.null(rs$footnotes[[1]]$valign)) {
        if (rs$footnotes[[1]]$valign == "top") {
          vflag <- "top"
          ftnts <- get_footnotes_pdf(rs$footnotes, 
                                     spec_width, rs, 
                                     talgn, ystart = ystart) 
        } else {
          
          if (wrap_flag)
            vflag <- "bottom" 
        }
      }
    }
  }
  

  blen <- 0
  if (content_blank_row %in% c("below", "both")) {
    blen <- 1
  }
  # } else {
  #   b <- paste0("\\fs1\\sl0\\par\\pard", rs$font_rtf, rs$spacing_multiplier)
  # }
  
  # Add extra offsets if table has a lot of borders turned on
  # to avoid undesired page wraps
  # boff <- 0
  # if (any(class(spec) == "table_spec") &
  #     any(spec$borders %in% c("all", "inside"))) {
  #   
  #   boff <- round(row_count * rs$border_height / rs$row_height)
  # }
  
  tlns <- ftnts$lines + blen
  ret <- list(pdf = ftnts$pdf,
              lines = tlns,
              points = tlns * rs$line_height,
              border_flag = ftnts$border_flag)
  
  return(ret)
}

# Sub-Functions ---------------------------------------------------------


#' Get content offsets for table header, titles, footnotes, and content blanks.
#' Needed to calculate page breaks accurately.
#' @return A vector of upper and lower offsets
#' @noRd
get_content_offsets_pdf <- function(rs, ts, pi, content_blank_row) {
  
  ret <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  cnt <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  
  # Width is normally the width of the table, not the page
  wdth <- rs$content_size[["width"]]
  if (!is.null(pi$col_width))
    wdth <- sum(pi$col_width)
  
  # Default to zero
  shdrs <- list(lines = 0, points = 0)
  hdrs <- list(lines = 0, points = 0)
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header_pdf(rs, ts, pi)   
    
    hdrs <- get_table_header_pdf(rs, ts, pi$col_width, 
                                 pi$label, pi$label_align, 
                                 pi$table_align)  
  }
  
  # Get title headers or titles
  if (is.null(ts$title_hdr))
    ttls <- get_titles_pdf(ts$titles, wdth, rs) 
  else 
    ttls <- get_title_header_pdf(ts$title_hdr, wdth, rs)
  
  # Get page by if it exists
  pgb <- list(lines = 0, points = 0)
  if (!is.null(ts$page_by))
    pgb <- get_page_by_pdf(ts$page_by, wdth, NULL, rs, pi$table_align)
  else if (!is.null(rs$page_by))
    pgb <- get_page_by_pdf(rs$page_by, wdth, NULL, rs, pi$table_align)
  
  
  #print(length(pgb))
  
  # print(paste("Table titles:", ttls))
  
  # Add everything up
  ret[["upper"]] <- shdrs$points + hdrs$points + ttls$points + pgb$points
  cnt[["upper"]] <- shdrs$lines + hdrs$lines + ttls$lines + pgb$lines
  
  if (content_blank_row %in% c("above", "both")) {
    ret[["blank_upper"]] <- rs$line_height
    cnt[["blank_upper"]] <- 1 
  }
  
  ftnts <- get_footnotes_pdf(ts$footnotes, wdth, rs) 
  rftnts <- get_footnotes_pdf(rs$footnotes, wdth, rs)
  
  if (has_top_footnotes(rs)) {
    ret[["lower"]] <- ftnts$points + rftnts$points
    cnt[["lower"]] <- ftnts$lines + rftnts$lines
  } else {
    ret[["lower"]] <- ftnts$points
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
              points = ret)
  
  return(res)
  
}


#' @description Return a vector of strings for the table header
#' @details Basic idea of this function is to create a list
#' of string vectors of label words sized according to the column 
#' widths, then combine by line/row, and concatenate everything and justify.
#' @noRd
get_table_header_pdf <- function(rs, ts, widths, lbls, halgns, talgn, 
                                 ystart = 0) {
  
  ret <- c()
  cnt <- 0
  rh <- rs$row_height
  tbl <- ts$data
  conv <- rs$point_conversion
  nms <- names(lbls)[is.controlv(names(lbls)) == FALSE]
  unts <- rs$units
  wdths <- widths[nms]

  # Get cell widths
  # sz <- c()
  # for (k in seq_along(widths)) {
  #   if (!is.control(nms[k])) {
  #     if (k == 1)
  #       sz[k] <- widths[k] 
  #     else 
  #       sz[k] <- round(widths[k] + sz[k - 1])
  #   }
  # }
  
  # Table alignment
  # ta <- "\\trql"
  # if (talgn == "right")
  #   ta <- "\\trqr"
  # else if (talgn %in% c("center", "centre"))
  #   ta <- "\\trqc"
  
  # if (length(ts$col_spans) == 0)
  #   brdrs <- ts$borders
  # else
  #   brdrs <- "none"
  
  # brdrs <- ts$borders
  # if (length(ts$col_spans) > 0) {
  #   
  #   if (any(ts$borders %in% c("outside")))
  #     brdrs <- c("left", "right")
  #   
  #   if (any(ts$borders %in% "top"))
  #     brdrs <- brdrs[!brdrs %in% "top"]
  #   
  # }
  
  # Header Cell alignment
  # ha <- c()
  # for (k in seq_along(halgns)) {
  #   if (!is.control(nms[k])) {
  #     if (halgns[k] == "left")
  #       ha[k] <- "\\ql"
  #     else if (halgns[k] == "right")
  #       ha[k] <- "\\qr"
  #     else if (halgns[k] %in% c("center", "centre"))
  #       ha[k] <- "\\qc"
  #   }
  # }
  
  # Table Header
  # ret[1] <-  paste0("\\trowd\\trgaph0", ta, "\\trrh", rh)
  
  # Loop for cell definitions
  # for(j in seq_along(tbl)) {
  #   if (!is.control(nms[j])) {
  #     b <- get_cell_borders(1, j, 2, ncol(tbl), brdrs)
  #     ret[1] <- paste0(ret[1], "\\clvertalb", b, "\\clbrdrb\\brdrs\\cellx", sz[j])
  #   }
  # }
  
  # Sum up widths 
  width <- sum(wdths, na.rm = TRUE)
  
  # Get content alignment codes
  if (talgn == "right") {
    tlb <- rs$content_size[["width"]] - width
    trb <- rs$content_size[["width"]]
  } else if (talgn %in% c("center", "centre")) {
    tlb <- (rs$content_size[["width"]] - width) / 2
    trb <- width + tlb
  } else {
    tlb <- 0
    trb <- width
  }
  
  cnt <-  1 

  
  # Loop for column names
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = rs$font_size)
  
  
  tmplst <- list()
  mxlns <- 0
  for(k in seq_along(nms)) {
    
    tmplst[[k]] <- split_string_text(lbls[k], wdths[k], rs$units)
    
    if (tmplst[[k]]$lines > mxlns)
      mxlns <- tmplst[[k]]$lines
  }
  

  for(k in seq_along(nms)) {
    
    # Split label strings if they exceed column width
    tmp <- tmplst[[k]]
    
    yline <- ystart + (rh * (mxlns - tmp$lines))
  
    #ret[1] <- paste0(ret[1], ha[k], " ", tmp$rtf, "\\cell")
    
    if (k == 1) {
      lb <- tlb
      rb <- lb + wdths[k]
    } else {
      lb <- rb
      rb <- lb + wdths[k]
    }
      
    for (ln in seq_len(tmp$lines)) {
      
      ret[[length(ret) + 1]] <- page_text(tmp$text[ln], rs$font_size, 
                                          bold = FALSE,
                                          xpos = get_points(lb, 
                                                            rb,
                                                            tmp$widths[ln],
                                                            units = unts,
                                                            align = halgns[k]),
                                          ypos = yline)
      yline <- yline + rh
    }
    
    # Add in extra lines for labels that wrap
    xtr <- tmp$lines
    if (xtr > cnt)
      cnt <- xtr
    
  }
  
  dev.off()
  
  yline <- ystart + (cnt * rh) - (rh * .75) + 1
  
  ret[[length(ret) + 1]] <- page_hline(tlb * conv, 
                                       yline, 
                                       (trb - tlb) * conv)
  cnt <- cnt + .5
  
  #ret[1] <- paste(ret[1], "\\row")
  
  res <- list(pdf = ret,
              lines = cnt,
              points = cnt * rh)
  
  return(res)
  
}

#' @description Return a vector of pdf codes for the table spanning headers
#' @details Basic idea of this function is to figure out which columns 
#' the header spans, add widths, then call get_table_header.  Everything
#' from there is the same.  
#' @import stats
#' @noRd
get_spanning_header_pdf <- function(rs, ts, pi, ystart = 0) {
  
  spns <- ts$col_spans
  cols <- pi$keys
  cols <- cols[!is.controlv(cols)]
  w <- pi$col_width 
  w <- w[cols]
  gutter <- 0
  cnt <- c()
  rh <- rs$row_height
  
  # print("Cols:")
  # print(cols)
  #print(w)
  wlvl <- get_spanning_info(rs, ts, pi, w, gutter)
  lvls <- sort(seq_along(wlvl), decreasing = TRUE)
  
  # At this point we have a data frame with labels, etc. and spanning
  # column widths, and are ready to create spanning header rows
  #print(wlvl)
  
  conv <- rs$point_conversion
  talgn <- pi$table_align
  
  # Table alignment
  # ta <- "\\trql"
  # if (talgn == "right")
  #   ta <- "\\trqr"
  # else if (talgn %in% c("center", "centre"))
  #   ta <- "\\trqc"
  
  # Get borders
  brdrs <- ts$borders
  
  rh <- rs$row_height
  
  width <- sum(w)
  
  if (talgn == "right") {
    tlb <- rs$content_size[["width"]] - width
    trb <- rs$content_size[["width"]]
  } else if (talgn %in% c("center", "centre")) {
    tlb <- (rs$content_size[["width"]] - width) / 2
    trb <- width + tlb
  } else {
    tlb <- 0
    trb <- width
  }
  
  ret <- list()
  lline <- ystart
  
  # Open device context
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = rs$font_size)
  
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
    # sz <- c()
    # for (k in seq_along(widths)) {
    #   
    #   if (k == 1)
    #     sz[k] <- round(widths[k] * conv)
    #   else
    #     sz[k] <- round(widths[k] * conv + sz[k - 1])
    #   
    # }
    
    # Header Cell alignment
    # ha <- c()
    # for (k in seq_along(algns)) {
    #   
    #   if (algns[k] == "right")
    #     ha[k] <- "\\qr"
    #   else if (algns[k] %in% c("center", "centre"))
    #     ha[k] <- "\\qc"
    #   else
    #     ha[k] <- "\\ql"
    #   
    # }
    
    r <- ""
    cnt[length(cnt) + 1] <- 1 
    
    # Table Header
    # r <-  paste0("\\trowd\\trgaph0", ta, "\\trrh", rh)
    
    # Label justification, width, and row concatenation
    
    # Loop for cell definitions
 #   for(j in seq_along(widths)) {
      
      # Get cell borders
      # b <- get_cell_borders(length(wlvl) - l + 1, j, 10, nrow(s), brdrs)
      
      # Add colspans
  #    vl <- lbls[j]
      
      # Special handling of borders for different situations.  
      # I hate this, but can't see a way around it.
      # if (any(brdrs %in% c("all", "inside"))) {
      #   sflg <- "B"
      #   if (j > 1) {
      #     if (cs[j] > 1)
      #       sflg <- ""
      #   }
      #   
      #   if (vl == "") {
      #     bb <- get_cell_borders(length(lvls) - l + 1, j, length(lvls), 
      #                            length(sz), brdrs, 
      #                            flag = sflg)
      #   } else 
      #     bb <- b
      # } else if (all(brdrs == "outside")) {
      #   
      #   if (vl == "")
      #     bb <- b
      #   else 
      #     bb <- paste0(b, "\\clbrdrb\\brdrs")
      #   
      # } else {
      #   
      #   if (vl == "") {
      #     bb <- get_cell_borders(length(lvls) - l + 1, j, length(lvls), 
      #                            length(sz), brdrs, 
      #                            flag = "")
      #   } else 
      #     bb <- paste0(b, "\\clbrdrb\\brdrs")
      #   
      # }
      
      # Don't forget about underline parameter
      # if (s$span[j] > 0 & s$underline[j])
      #   r <- paste0(r, "\\clvertalb", bb, "\\clbrdrb\\brdrs\\cellx", sz[j])
      # else 
      #   r <- paste0(r, "\\clvertalb", bb, "\\cellx", sz[j])
  #  }
    
   mxyl <- 0
    
    # Loop for labels
   for(k in seq_along(lbls)) {
     
     yline <- lline
     
     # Do something with this
     if (k == 1) {
       lb <- tlb
       rb <- lb + widths[k]
     } else {
       lb <- rb
       rb <- lb + widths[k]
     }
     
     if (lbls[k] != "") {
      
        # Split label strings if they exceed column width
        tmp <- split_string_text(lbls[k], widths[k], rs$units)
        
        
        for (ln in seq_len(tmp$lines)) {
          
          ret[[length(ret) + 1]] <- page_text(tmp$text[ln], rs$font_size, 
                                              bold = FALSE,
                                              xpos = get_points(lb, 
                                                                rb,
                                                                tmp$widths[ln],
                                                                units = rs$units,
                                                                align = algns[k]),
                                              ypos = yline)
          yline <- yline + rh
          if (yline > mxyl)
            mxyl <- yline
        }
        
        # yline <- mxyl + (rh * .75) + 1
        # 
        # ret[[length(ret) + 1]] <- page_hline((lb * conv) + gutter, 
        #                                      yline, 
        #                                      ((rb - lb) * conv) - (gutter * 2))
        #cnt <- cnt + .5
        
        
        # Concat label
        #r <- paste0(r, ha[k], " ", tmp$rtf, "\\cell")
        # print(lbls[k])
        # print(widths[k])
        # Add in extra lines for labels that wrap
        xtr <- tmp$lines #+ .5
        if (xtr > cnt[length(cnt)])
          cnt[length(cnt)] <- xtr
      
     }
      
   }

    lline <- mxyl #+ (rh * .5)
    
    # r <- paste0(r, "\\row")
    # 
    # ln[[length(ln) + 1]] <- r
    
  }
  dev.off()
  
  #ret <- unlist(ln)
  
  res <- list(pdf = ret, 
              lines = sum(cnt), 
              points = sum(cnt) * rh)
  
  return(res)
}

#' @description This function counts lines per row independently because
#' the ..row field does not account for page wrapping.  Need number
#' of lines on this particular page.
#' @noRd
get_table_body_pdf <- function(rs, tbl, widths, algns, talgn, tbrdrs, 
                               ystart = 0, spwidths = list()) {
  
  if ("..blank" %in% names(tbl))
    flgs <- tbl$..blank
  else 
    flgs <- NA
  
  # Count lines per row
  rws <- c()
  cnt <- 0
  
  nms <- names(widths)
  nms <- nms[!is.na(nms)]
  nms <- nms[!is.controlv(nms)]
  wdths <- widths[nms]
  
  # Deal with one column situation
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
  conv <- rs$point_conversion
  unts <- rs$units
  
  # Get cell widths
  # sz <- c()
  # for (k in seq_along(wdths)) {
  #   if (!is.control(nms[k])) {
  #     if (k == 1)
  #       sz[k] <- round(wdths[k] * conv)
  #     else 
  #       sz[k] <- round(wdths[k] * conv + sz[k - 1])
  #   }
  # }
  
  # Table alignment
  # ta <- "\\trql"
  # if (talgn == "right")
  #   ta <- "\\trqr"
  # else if (talgn %in% c("center", "centre"))
  #   ta <- "\\trqc"
  
  # Cell alignment
  # ca <- c()
  # for (k in seq_along(algns)) {
  #   if (!is.control(nms[k])) {
  #     if (algns[k] == "left")
  #       ca[k] <- "\\ql"
  #     else if (algns[k] == "right")
  #       ca[k] <- "\\qr"
  #     else if (algns[k] %in% c("center", "centre"))
  #       ca[k] <- "\\qc"
  #   }
  # }
  
  # Sum up widths 
  width <- sum(wdths, na.rm = TRUE)
  
  # Get content alignment codes
  if (talgn == "right") {
    tlb <- rs$content_size[["width"]] - width
    trb <- rs$content_size[["width"]]
  } else if (talgn %in% c("center", "centre")) {
    tlb <- (rs$content_size[["width"]] - width) / 2
    trb <- width + tlb
  } else {
    tlb <- 0
    trb <- width
  }
  
  rline <- ystart
  
  ret <- c()
  
  fs <- rs$font_size
  
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = fs)
  
  # Loop for rows
  for(i in seq_len(nrow(t))) {
    
    yline <- rline
    mxrw <- yline
    cnt <- cnt + 1
    
    # if (i ==  1)
    #   ret[i] <- paste0("{\\trowd\\trgaph0\\trrh", rh, ta)
    # else 
    #   ret[i] <- paste0("\\trowd\\trgaph0\\trrh", rh, ta)
    
    
    # # Loop for cell definitions
    # for(j in seq_len(ncol(t))) {
    #   if (!is.control(nms[j])) {
    #     b <- get_cell_borders(i, j, nrow(t), ncol(t), brdrs, flgs[i])
    #     ret[i] <- paste0(ret[i], b, "\\cellx", sz[j])
    #   }
    # }
  
    
    # Loop for columns 
    for(j in nms) {
      
      # Seems like this should be done already
      # Need to get widths from split_cells
      #tmp <- split_string_text(ttls$titles[[i]], width, rs$units)
      if (class(tbl[i, j]) != "character")
        vl <- as.character(tbl[i, j])
      else 
        vl <- tbl[i, j]
      
      tmp <- strsplit(vl, "\n", fixed = TRUE)[[1]]
        
      # Construct rtf
      # ret[i] <- paste0(ret[i], ca[j], " ", t[i, j], "\\cell")
      
      # Count lines in cell 
      #cl <- grep("\\line", t[i, j], fixed = TRUE)
      
      if (j == nms[1]) {
        lb <- tlb
        rb <- lb + wdths[j]
      } else {
        lb <- rb
        rb <- lb + wdths[j]
      }
      
      # Loop for cell wraps
      for (ln in seq_len(length(tmp))) {
        
        ret[[length(ret) + 1]] <- page_text(tmp[ln], fs, 
                                            bold = FALSE,
                                            xpos = get_points(lb, 
                                                              rb,
                                                              spwidths[[i]][[j]][ln],
                                                              units = unts,
                                                              align = algns[j]),
                                            ypos = yline)
        yline <- yline + rh
      }
      
      if (yline > mxrw)
        mxrw <- yline
      
      yline <- rline
    
      
    }
    
    rline <- mxrw
  
  
  }
  
  dev.off()
  
  rws <- rline
  
  res <- list(pdf = ret,
              lines = cnt ,
              points = cnt * rh)
  
  return(res)
  
  
}

