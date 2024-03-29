

# Create Tables ----------------------------------------------------------


#' @noRd
create_table_pages_pdf <- function(rs, cntnt, lpg_rows) {
  
  ts <- cntnt$object
  content_blank_row <- cntnt$blank_row
  
  pgby_var <- NA
  pgby_cnt <- 0
  pgby_fmt <- NULL
  if (!is.null(rs$page_by)) {
    pgby_var <- rs$page_by$var
    pgby_fmt <- rs$page_by$format
  } else if (!is.null(ts$page_by)) {
    pgby_var <- ts$page_by$var
    pgby_fmt <- ts$page_by$format
  }
  
  
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
      
      if (!is.null(pgby_fmt))
        dat$..page_by <-  fapply(dat[[pgby_var]], pgby_fmt)
      else 
        dat$..page_by <- dat[[pgby_var]]
    }
    
    pgby_cnt <- get_pgby_cnt(dat$..page_by)
    
    # Commenting out for now
    # if (is.unsorted(dat[[pgby_var]], strictly = FALSE))
    #   message("Page by variable not sorted.")
  }
  
  # Deal with invisible columns
  if (!is.null(ts$col_defs)) {
    for (def in ts$col_defs) {
      if (def$visible == FALSE) {
        nnm <- paste0("..x.", def$var_c)
        dat[[nnm]] <- dat[[def$var_c]]
      }
    }
  }
  
  # Get control column names 
  control_cols <- names(dat)[is.controlv(names(dat))]
  
  # Get vector of all included column names
  # Not all columns in dataset are necessarily included
  # depends on show_cols parameter on create_table and
  # visible parameter on column definitions
  keys <- get_table_cols(ts, control_cols)
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
  
  # Deal with styles
  styles <- get_styles(ts)
  
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
                               rs$font_size, rs$units, rs$output_type, rs$char_width) 
  fdat <- sp$data
  wdat <- sp$widths
  
  # print("split_cells")
  # print(fdat)
  
  
  # Break columns into pages
  wraps <- get_page_wraps(rs$line_size, ts, 
                          widths_uom, 0, control_cols)  # No gutter width for RTF
  # print("wraps")
  # print(wraps)
  
  
  # Create a temporary page info to pass into get_content_offsets
  tmp_pi <- list(keys = keys, col_width = widths_uom, label = labels,
                 label_align = label_aligns, table_align = cntnt$align)
  # print("Temp PI")
  # print(tmp_pi)
  
  
  # Offsets are needed to calculate splits and page breaks
  content_offset <- get_content_offsets_pdf(rs, ts, tmp_pi, 
                                            content_blank_row, pgby_cnt)
  
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
  fp_offset <- lpg_rows
  
  pg_lst <- list()
  for(s in splits) {
    
    # Subset text widths by current split rows
    #spend <- spstart + nrow(s) - 1
    rnms <- rownames(s)
    spstart <- rnms[1]
    spend <- rnms[length(rnms)]
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
                                                       fp_offset, spwidths, styles)
      
      fp_offset <- 0
    }
    
    #spstart <- spend + 1
  }
  
  ret <- list(widths = widths_uom, page_list = pg_lst)
  
  return(ret)
  
  
}



#' @noRd
create_table_pdf <- function(rs, ts, pi, content_blank_row, wrap_flag, 
                             lpg_rows, spwidths, styles) {
  rh <- rs$row_height
  ys <- lpg_rows * rh
  conv <- rs$point_conversion
  bf <- FALSE
  
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
  if (ttls$lines > 0)
    bf <- ttls$border_flag
  
  if (!is.null(rs$page_by)) {
    pgby <- get_page_by_pdf(rs$page_by, rs$content_size[["width"]], 
                            pi$page_by, rs, pi$table_align, ystart = ys,
                            brdr_flag = bf)
  } else if(!is.null(ts$page_by))
    pgby <- get_page_by_pdf(ts$page_by, ls, pi$page_by, rs, pi$table_align, 
                            ystart = ys, brdr_flag = bf)
  else 
    pgby <- c()
  
  if (length(pgby) > 0) {
    ys <- ys + pgby$points
    bf <- pgby$border_flag 
  }
  
  shdrs <- list(lines = 0, points = 0)
  hdrs <- list(lines = 0, points = 0)
  
  if (ts$headerless == FALSE) {
    shdrs <- get_spanning_header_pdf(rs, ts, pi, ystart = ys,
                                     brdr_flag = bf)
    
    ys <- ys + shdrs$points
    
    hs <- FALSE
    if (shdrs$lines > 0) {
      bf <- shdrs$border_flag
      hs <- TRUE
    }
    
    hdrs <- get_table_header_pdf(rs, ts, pi$col_width, 
                                 pi$label, pi$label_align, 
                                 pi$table_align, ystart = ys,
                                 brdr_flag = bf, has_spans = hs)
    
    ys <- ys + hdrs$points
  }
  
  
  # rs, ts, widths,  algns, halgns, talgn
  bdy <- get_table_body_pdf(rs, pi$data, pi$col_width, 
                            pi$col_align, pi$table_align, ts$borders,
                            ystart = ys,
                            spwidths, frb = ts$first_row_blank, styles = styles)
  ys <- ys + bdy$points


  if (bdy$lines > 0)
    bf <- bdy$border_flag
  
  # blnks <- c()
  
  # Get row count by summing ..row variable
  # if ("..row" %in% names(pi$data)) {
  #   rcnt <- sum(pi$data$..row)
  # } else {
  #   rcnt <- nrow(pi$data) 
  # }
  
  
  # Get footnotes, passing in sum of all current lines
  ftnts <- get_page_footnotes_pdf(rs, ts, ls, lpg_rows, ys,
                                  wrap_flag, content_blank_row,  pi$table_align,
                                  brdr_flag = bf)
  
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
                                   wrap_flag, content_blank_row, talgn,
                                   brdr_flag = FALSE) {
  
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
                                   ystart = ystart,
                                   brdr_flag = brdr_flag) 
      }
      
    }
  } else {
    
    if (!is.null(rs$footnotes[[1]])) {
      if (!is.null(rs$footnotes[[1]]$valign)) {
        if (rs$footnotes[[1]]$valign == "top") {
          vflag <- "top"
          ftnts <- get_footnotes_pdf(rs$footnotes, 
                                     spec_width, rs, 
                                     talgn, ystart = ystart,
                                     brdr_flag = brdr_flag) 
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
get_content_offsets_pdf <- function(rs, ts, pi, content_blank_row, pgby_cnt = NULL) {
  
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
    pgb <- get_page_by_pdf(ts$page_by, wdth, NULL, rs, pi$table_align, pgby_cnt = pgby_cnt)
  else if (!is.null(rs$page_by))
    pgb <- get_page_by_pdf(rs$page_by, wdth, NULL, rs, pi$table_align, pgby_cnt = pgby_cnt)
  
  
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
  # to avoid undesired overlay
  # Not perfect but good enough
  brdrs <- strip_borders(ts$borders )
  if (any(brdrs %in% c("all", "inside"))) {
    epnts <- floor(rs$body_line_count - cnt[["lower"]] - cnt[["upper"]]) * rs$border_height 
    ret[["lower"]] <- ret[["lower"]] + epnts - rs$line_height
    cnt[["lower"]] <- cnt[["lower"]] + floor(epnts / rs$row_height) - 1
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
                                 ystart = 0, brdr_flag = FALSE, 
                                 has_spans = FALSE) {
  
  ret <- c()
  cnt <- 0
  rh <- rs$row_height 
  bs <- rs$border_spacing
  bh <- rs$border_height
  tbl <- ts$data
  conv <- rs$point_conversion
  nms <- names(lbls)[is.controlv(names(lbls)) == FALSE]
  unts <- rs$units
  wdths <- widths[nms]
  brdrs <- strip_borders(ts$borders)
  pnts <- 0
  # print("ystart header")
  # print(ystart)
  
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
    
    pnts <- mxlns * rh
  }
  
  if (any(brdrs %in% c("all", "outside", "top"))) {
    if (!brdr_flag & !has_spans) {
      tbs <- ystart + bs - rh 
      pnts <- pnts + bs + 1
    } else {
      tbs <- ystart - rh + 1
      pnts <- pnts + 3
    }
  } else {
    tbs <- ystart - rh + 1
    pnts <- pnts + bh
  }

  for(k in seq_along(nms)) {
    
    # Split label strings if they exceed column width
    tmp <- tmplst[[k]]
    

    yline <- ystart + (rh * (mxlns - tmp$lines)) 
    if (any(brdrs %in% c("all", "outside", "top")) & !brdr_flag & !has_spans) {
      yline <- yline + bh

    }
    
    #print(yline)
    
    if (k == 1) {
      lb <- tlb
      rb <- lb + wdths[k]
    } else {
      lb <- rb
      rb <- lb + wdths[k]
    }
      
    for (ln in seq_len(tmp$lines)) {
      
      ret[[length(ret) + 1]] <- page_text(tmp$text[ln], rs$font_size, 
                                          bold = ts$header_bold,
                                          xpos = get_points(lb, 
                                                            rb,
                                                            tmp$widths[ln],
                                                            units = unts,
                                                            align = halgns[k]),
                                          ypos = yline)
      yline <- yline + rh
    }
    
    if (any(brdrs %in% c("all", "inside"))) {
      
      
      ret[[length(ret) + 1]] <- page_vline(rb * conv, 
                                           tbs, 
                                           (rh * mxlns) + bs) 
      
    }
    
    # Add in extra lines for labels that wrap
    xtr <- tmp$lines
    if (xtr > cnt)
      cnt <- xtr
    
  }
  
  dev.off()

  if (ts$first_row_blank == TRUE) {
    cnt <- cnt + 1
    pnts <- pnts + rh
  }
  
  # Top border
  if ((any(brdrs == "all")) | (any(brdrs %in% c("outside", "top")) & !has_spans)) {
    
    ret[[length(ret) + 1]] <- page_hline(tlb * conv, 
                                         tbs, 
                                         (trb - tlb) * conv) 
    
  }
  
  # Left border
  if (any(brdrs %in% c("all", "outside", "left"))) {
    
    
    ret[[length(ret) + 1]] <- page_vline(tlb * conv, 
                                         tbs, 
                                         (rh * cnt) + bs) 
    
  }
  
  # Right border
  if (any(brdrs %in% c("all", "outside", "right"))) {
    
    
    ret[[length(ret) + 1]] <- page_vline(trb * conv, 
                                         tbs, 
                                         (rh * cnt) + bs) 
    
  }
  

  yline <- tbs + (rh * mxlns) + bs 
  if (!any(brdrs %in% c("all", "outside", "top")) & brdr_flag) {
    # yline <- yline + bs
    # pnts <- pnts + bs
  }
  
  
  # First row blank
  if (ts$first_row_blank == TRUE) {
    if (any(brdrs %in% c("all", "inside")) ) {
      
      ret[[length(ret) + 1]] <- page_hline(tlb * conv, 
                                           yline + rh, 
                                           (trb - tlb) * conv)
      
    }
  }
  
  # print("yline")
  # print(yline)
  
  # Bottom header border always present
  ret[[length(ret) + 1]] <- page_hline(tlb * conv, 
                                       yline, 
                                       (trb - tlb) * conv)
  
  
  
  res <- list(pdf = ret,
              lines = pnts / rh,
              points = pnts )
  
  return(res)
  
}

#' @description Return a vector of pdf codes for the table spanning headers
#' @details Basic idea of this function is to figure out which columns 
#' the header spans, add widths, then call get_table_header.  Everything
#' from there is the same.  Border adjustments are the closest thing to spaghetti
#' in this package. But it works.
#' @import stats
#' @noRd
get_spanning_header_pdf <- function(rs, ts, pi, ystart = 0, brdr_flag = FALSE) {
  
  spns <- ts$col_spans
  cols <- pi$keys
  cols <- cols[!is.controlv(cols)]
  w <- pi$col_width 
  w <- w[cols]
  gutter <- 0
  gap <- 3
  cnt <- c()
  rh <- rs$row_height
  bs <- rs$border_spacing
  bh <- rs$border_height
  border_flag <-  FALSE
  
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
  brdrs <- strip_borders(ts$borders)
  
  rh <- rs$row_height
  if (any(brdrs %in% c("all", "inside")))
    rh <-  rh + bh
  
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
  
  tbrdrs <- any(brdrs %in% c("top", "outside", "all"))
  
  # Nightmare
  lline <- ystart
  if (brdr_flag & !tbrdrs) {
    lline <- ystart + bs
  } else if (brdr_flag & tbrdrs) {
    if (all(brdrs %in% "outside"))
      lline <- ystart + 1
    else 
      lline <- ystart + bs 
  } else if (!brdr_flag & tbrdrs) {
    lline <- ystart + bs + bs
  }
  
  ybegin <- lline
  
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
    
    r <- ""
    cnt[length(cnt) + 1] <- 1 
  
    

    
   mxyl <- 0
   mxlns <- 0
   lnadj <- 0
   tmps <- list()
   
   # Get max lines for this spanning level
   for (k in seq_along(lbls)) {
     
     # Split label strings if they exceed column width
     tmps[[k]] <- split_string_text(lbls[k], widths[k], rs$units) 
     
     if (tmps[[k]]$lines > mxlns)
       mxlns <- tmps[[k]]$lines
   }
    
    # Loop for labels
   for(k in seq_along(lbls)) {

     yline <- lline 
     
     # Get left and right bounds for this span
     # Will accumulate as it goes across 
     if (k == 1) {
       lb <- tlb
       rb <- lb + widths[k]
     } else {
       lb <- rb
       rb <- lb + widths[k]
     }
     
     if (lbls[k] != "") {
      
        tmp <- tmps[[k]]
        
        for (ln in seq_len(tmp$lines)) {
          
          # Adjust yline to push down labels
          if (ln == 1)
            yline <- yline + ((mxlns - tmp$lines) * rh)
          
          adj <- 0
          if (any(brdrs %in% c("all", "inside")))
              adj <- 4
          
          # Get pdf text for label
          ret[[length(ret) + 1]] <- page_text(tmp$text[ln], rs$font_size, 
                                              bold = s$bold[k],
                                              xpos = get_points(lb, 
                                                                rb,
                                                                tmp$widths[ln],
                                                                units = rs$units,
                                                                align = algns[k]),
                                              ypos = yline - adj)
          yline <- yline + rh
          if (yline > mxyl)
            mxyl <- yline
        }
        
        if (any(brdrs %in% c("all", "inside"))) { 
          

          ret[[length(ret) + 1]] <- page_vline(lb * conv,
                                               lline - rh,
                                               (mxlns * rh) +  .5)
          
          ret[[length(ret) + 1]] <- page_vline(rb * conv,
                                               lline - rh,
                                               (mxlns * rh) +  .5)
          
          
        } else {
          # Get underline for label if requested
          if (s$span[k] > 0 & s$underline[k]) {
            
            lnadj <- .5
            
            yline <- mxyl + lnadj - (rh * .75) + 1 
    
            ret[[length(ret) + 1]] <- page_hline((lb * conv) + gap,
                                                 yline,
                                                 ((rb - lb) * conv) - (gap * 2))
  
          }
        }
        

        # Add in extra lines for labels that wrap
        xtr <- tmp$lines + lnadj
        if (xtr > cnt[length(cnt)])
          cnt[length(cnt)] <- xtr
      
     }
      
   }
   
   if (any(brdrs %in% c("all", "inside"))) {
     
     ret[[length(ret) + 1]] <- page_hline(tlb * conv, 
                                          lline - rh, 
                                          (trb - tlb) * conv) 
     
   }

    lline <- mxyl + (rh * lnadj)
    
    
  }
  dev.off()
  
  cnts <- sum(cnt)
  
  if (length(lvls) > 0) {

    
    ypos <- ybegin - rh #+ bs 
    
    if (any(brdrs %in% c("top", "outside"))) {
      
      ret[[length(ret) + 1]] <- page_hline(tlb * conv,
                                           ypos,
                                           (trb - tlb) * conv)
      
    }
    
    # if (any(brdrs %in% c("all"))) {
    #   
    #   ret[[length(ret) + 1]] <- page_hline(tlb * conv, 
    #                                        ystart - rh  + (cnts * rh) + (1.5 * bs), 
    #                                        (trb - tlb) * conv) 
    #   
    # }
    
    if (any(brdrs %in% c("all", "outside", "left"))) {
      
      
      ret[[length(ret) + 1]] <- page_vline(tlb * conv, 
                                           ypos, 
                                           (cnts * rh)) 
      
    }
    
    if (any(brdrs %in% c("all", "outside", "right"))) {
      
      
      ret[[length(ret) + 1]] <- page_vline(trb * conv, 
                                           ypos, 
                                           (cnts * rh)) 
      
    }
  }
  
  pnts <- cnts * rh
  if (!brdr_flag & tbrdrs)
    pnts <- pnts + bs
  
  res <- list(pdf = ret, 
              lines = cnts, 
              points = pnts,
              border_flag = border_flag)
  
  return(res)
}

#' @description This function counts lines per row independently because
#' the ..row field does not account for page wrapping.  Need number
#' of lines on this particular page.
#' @noRd
get_table_body_pdf <- function(rs, tbl, widths, algns, talgn, tbrdrs, 
                               ystart = 0, spwidths = list(), 
                               brdr_flag = FALSE, frb = FALSE, styles) {
  
  border_flag <- FALSE
  
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
  
  #saveRDS(spwidths, "spwidths0a.rds")
  
  if (!"..blank" %in% names(tbl)) 
    blnks <- rep("", nrow(tbl))
  else 
    blnks <- tbl$..blank
  
  # Deal with one column situation
  if (length(nms) == 1) {
    t <- as.data.frame(tbl[[nms]])
    names(t) <- nms
  } else 
    t <- tbl[ , nms]
  
  conv <- rs$point_conversion
  unts <- rs$units
  bs <- rs$border_spacing
  bh <- rs$border_height
  cp <- rs$cell_padding
  
  brdrs <- strip_borders(tbrdrs)
  if (all(tbrdrs == "body"))
    brdrs <- c("top", "bottom", "left", "right")

  
  # Get line height.  Don't want to leave editor default.
  if (any(brdrs %in% c("all", "inside")))
    rh <- rs$row_height + bh
  else 
    rh <- rs$row_height
  
  
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
  
  rline <- ystart + 1
  
  ret <- c()
  
  fs <- rs$font_size
  
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = fs)
  
  # Loop for rows
  for(i in seq_len(nrow(t))) {
    
    yline <- rline
    mxrw <- yline
    cnt <- cnt + 1
  
    # Loop for columns 
    for(j in nms) {
      
      # Seems like this should be done already
      # Need to get widths from split_cells
      if (all(class(tbl[i, j]) != "character"))
        vl <- as.character(tbl[i, j])
      else 
        vl <- tbl[i, j]
      
      if (flgs[i] %in% c("B", "L")) {
        
        # Strip out line feeds for label rows
        #vl <- gsub("\n", " ", vl, fixed = TRUE)
        
        #browser()
        # Recalculate based on total width of table
        #stmp <- split_string_text(vl, sum(wdths), rs$units)
        
        #tmp <- stmp$text
        
        tmp <- strsplit(vl, "\n", fixed = TRUE)[[1]]
        
      } else {
      
        tmp <- strsplit(vl, "\n", fixed = TRUE)[[1]]
      }
        
      
      if (j == nms[1]) {
        lb <- tlb
        rb <- lb + wdths[j]
      } else {
        lb <- rb
        rb <- lb + wdths[j]
      }
      
      # If value is empty or blank
      if (length(tmp) == 0) {
        yline <- yline + rh
      } else if (length(trimws(tmp)) == 0) {
        yline <- yline + rh
      } else {
        
        stl <- get_cell_styles(j, styles, flgs, i, tbl)
        
        bflg <- FALSE
        if ("bold" %in% stl) {
          bflg <- TRUE 
        }
      
        # Loop for cell wraps
        for (ln in seq_len(length(tmp))) {
          
          ret[[length(ret) + 1]] <- page_text(tmp[ln], fs, 
                                              bold = bflg,
                                              xpos = get_points(lb, 
                                                                rb,
                                                                spwidths[[i]][[j]][ln],
                                                                units = unts,
                                                                align = algns[j]),
                                              ypos = yline)
          yline <- yline + rh
        }
      }
      
      if (yline > mxrw)
        mxrw <- yline
      
      yline <- rline
      
    }
    

    # Loop for columns 
    for(j in nms) {
      
      # Applies inside vertical borders
      if (any(brdrs %in% c("all", "inside")) & j != nms[length(nms)] 
          & !blnks[i] %in% c("B", "L")) {
        
        if (j == nms[1]) {
          lb <- tlb
          rb <- lb + wdths[j]
        } else {
          lb <- rb
          rb <- lb + wdths[j]
        }
        
        if (i == 1) { 
          
          if (is.null(frb)) {
            ret[[length(ret) + 1]] <- page_vline(rb * conv, (rline + bs) - rh - 1, 
                                               mxrw - rline + 1)
           } else if (frb == FALSE) {
            ret[[length(ret) + 1]] <- page_vline(rb * conv, (rline + bs) - rh - 1, 
                                               mxrw - rline + 1)
           } else {
             ret[[length(ret) + 1]] <- page_vline(rb * conv, (rline + bs) - rh, 
                                                  mxrw - rline + 1)
           }
          
        } else if (i == nrow(t)) {
          
          ret[[length(ret) + 1]] <- page_vline(rb * conv, (rline + bs) - rh, 
                                               mxrw - rline + 1)
        
        } else {
          
          ret[[length(ret) + 1]] <- page_vline(rb * conv, (rline + bs) - rh, 
                                               mxrw - rline )
          
        }
        
      }
      
    }

    
    
    # Applies inside horizontal borders
    if (any(brdrs %in% c("all", "inside")) & i < nrow(t)) {
    
      ret[[length(ret) + 1]] <- page_hline(tlb * conv, mxrw -rh + bs, 
                                           (trb - tlb) * conv)
    
    }
    
    rline <- mxrw
  
  }
  
  dev.off()
  
  if (frb & "body" %in% tbrdrs) {
    
    
    ypos <- ystart - rs$row_height - rs$row_height + bh 
    
    #ylen <- cnt * rh
    ylen <- rline - rh + rs$row_height + bs + 1
    
  } else {

    ypos <- ystart - rs$row_height + bh - 1
    
    #ylen <- cnt * rh
    ylen <- rline - rh + bs + 1
  }
  
  if (any(brdrs %in% c("all", "left", "outside"))) {
    
    ret[[length(ret) + 1]] <- page_vline(tlb * conv, ypos, ylen - ypos)
  }
  if (any(brdrs %in% c("all", "right", "outside"))) {
    
    ret[[length(ret) + 1]] <- page_vline(trb * conv, ypos, ylen - ypos)
  }
  
  # pnts <- cnt * rh
  pnts <- ylen - ystart + rh 
  
  if (any(brdrs %in% c("all", "bottom", "outside"))) {
    
    
    # ret[[length(ret) + 1]] <- page_hline(tlb * conv, ypos + ylen, 
    #                                      (trb - tlb) * conv)
    
    ret[[length(ret) + 1]] <- page_hline(tlb * conv, ylen, 
                                         (trb - tlb) * conv)
    
    border_flag <- TRUE
    if (any(brdrs %in% c("all"))) {
      
      pnts <- pnts - bh 
    }
   # pnts <- pnts + bh
    
  }

  # fcntr <- fcntr + 1
  # saveRDS(ret, paste0("ret", fcntr, ".rds"))

  
  rws <- rline
  
  res <- list(pdf = ret,
              lines = pnts / rs$row_height,
              points = pnts ,
              border_flag = border_flag)
  
  return(res)
  
  
}

fcntr <- 0
