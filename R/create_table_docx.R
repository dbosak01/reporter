

# Create Tables ----------------------------------------------------------


#' @noRd
create_table_pages_docx <- function(rs, cntnt, lpg_rows) {
  
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
  # fdat <- split_cells_variable(fdat, widths_uom, rs$font,
  #                              rs$font_size, rs$units, rs$output_type)$data
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
  content_offset <- get_content_offsets_docx(rs, ts, tmp_pi, content_blank_row)
  
  
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
      pg_lst[[length(pg_lst) + 1]] <- create_table_docx(rs, ts, pi, 
                                                       blnk_ind, wrap_flag,
                                                       lpg_rows)
    }
  }
  
  ret <- list(widths = widths_uom, page_list = pg_lst)
  
  return(ret)
  
  
}



#' @noRd
create_table_docx <- function(rs, ts, pi, content_blank_row, wrap_flag, 
                             lpg_rows) {
  rh <- rs$row_height
  shdrs <- list(lines = 0, twips = 0)
  hdrs <- list(lines = 0, twips = 0)
  conv <- rs$twip_conversion
  
  # Default to content width
  ls <- rs$content_size[["width"]]
  
  # Get table width
  if (!is.null(pi$col_width))
    ls <- sum(pi$col_width, na.rm = TRUE)
  
  if (!is.null(ts$title_hdr))
    ttls <- get_title_header_docx(ts$title_hdr, ls, rs, pi$table_align)
  else
    ttls <- get_titles_docx(ts$titles, ls, rs, pi$table_align) 
  
  
  if (!is.null(rs$page_by)) {
    pgby <- get_page_by_docx(rs$page_by, rs$content_size[["width"]], 
                            pi$page_by, rs, pi$table_align, ttls$border_flag)
  } else if(!is.null(ts$page_by)) {
    pgby <- get_page_by_docx(ts$page_by, ls, pi$page_by, rs, 
                             pi$table_align, ttls$border_flag)
  } else 
    pgby <- c()
  
  exbrdr <- ttls$border_flag
  if (length(pgby) > 0)
    exbrdr <- pgby$border_flag
  
  if (ts$headerless == FALSE) {
    
    # Get table header also includes spanning header
    hdrs <- get_table_header_docx(rs, ts, pi, exbrdr)  
  }
  
  # rs, ts, widths,  algns, halgns, talgn
  rws <- get_table_body_docx(rs, pi$data, pi$col_width, 
                             pi$col_align, pi$table_align, ts$borders, exbrdr)
  
  a <- NULL
  if (content_blank_row %in% c("above", "both"))
    a <- "<w:p/>"
  
  
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
  ftnts <- get_page_footnotes_docx(rs, ts, ls, lpg_rows, rc,
                                  wrap_flag, content_blank_row,  pi$table_align, 
                                  rws$border_flag)
  
  # Deal with cell padding.  Don't count this in line count.
  # cp <- paste0("\\li", rs$cell_padding, "\\ri", rs$cell_padding, rs$spacing_multiplier)
  
  # On LibreOffice, have to protect the table from the title width or
  # the table row will inherit the title row width. Terrible problem.
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
  
  # Same thing as above with titles.  If footnote block is contiguous
  # with table and footnotes are wider than table, row width of footnotes
  # will infect row width of table.  On libre only.  So this is to protect
  # the table width.
  # bpt <- "{\\pard\\fs1\\sl0\\par}"
  # if (any(ts$borders %in% c("all", "top", "outside"))) {
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
  # 
  # ta <- "align=\"left\" "
  # if (pi$table_align == "right")
  #   ta <- "align=\"right\" "
  # else if (pi$table_align %in% c("center", "centre"))
  #   ta <- "align=\"center\" "
  
  u <- rs$units
  if (u == "inches")
    u <- "in"
  
  # ds <- paste0("<div ", ta, ">")
  # ts <- paste0("<table style=\"width:", 
  #              round(sum(pi$col_width, 
  #                        na.rm = TRUE), 3), u,";\">")
  
  
  tb <- get_table_borders_docx(ts$borders)
  tw <- sum(round(sum(pi$col_width, na.rm = TRUE) * conv),
            round(length(pi$col_width[!is.na(pi$col_width)]) * .08 * conv))
  
 
  ta <- ""
  
  if (any(ts$borders %in% c("outside", "all", "left"))) {  
    if (pi$table_align == "right")
      aw <- round(rs$line_size * conv) - tw + rs$base_indent
    else if (pi$table_align %in% c("center", "centre"))
      aw <- round(((rs$line_size * conv) - tw) / 2) + rs$base_indent
    else 
      aw <- round(rs$base_indent)
    
    ta <- paste0('<w:tblInd w:w="', aw, '" w:type="dxa"/>')
  }
  
  
  ts <- paste0("<w:tbl>", "<w:tblPr>",
               ta,
               '<w:tblStyle w:val="TableGrid"/>',
               '<w:tblW w:w="', tw,'"',
               ' w:type="dxa"/>', tb,
               '<w:tblCellMar>
        					<w:left w:w="72" w:type="dxa"/>
        					<w:right w:w="72" w:type="dxa"/>
        				</w:tblCellMar>',
               "</w:tblPr>")
  
  ret <- list(docx = c(a, ttls$docx, pgby$docx, ts, shdrs$docx, 
                      hdrs$docx, rws$docx, "</w:tbl>", rs$table_break, ftnts$docx),
              lines = rc  + ftnts$lines)
  
  return(ret) 
}

#' Footnotes takes special handling because of the valign option.
#' @noRd
get_page_footnotes_docx <- function(rs, spec, spec_width, lpg_rows, row_count,
                                   wrap_flag, content_blank_row, talgn, 
                                   ex_brdr = FALSE) {
  
  ftnts <- list(lines = 0, twips = 0, border_flag = FALSE)
  vflag <- "none"

  
  # Deal with valign parameter
  if (!is.null(spec$footnotes)) {
    if (!is.null(spec$footnotes[[length(spec$footnotes)]])) {
      if (spec$footnotes[[length(spec$footnotes)]]$valign == "bottom") {
        
        vflag <- "bottom"
        ftnts <- get_footnotes_docx(spec$footnotes, 
                                   spec_width, rs, 
                                   talgn, FALSE) 
      } else {
        vflag <- "top"
        ftnts <- get_footnotes_docx(spec$footnotes, spec_width, rs, talgn, ex_brdr) 
      }
      
    }
  } else {
    
    if (!is.null(rs$footnotes[[1]])) {
      if (!is.null(rs$footnotes[[1]]$valign)) {
        if (rs$footnotes[[1]]$valign == "top") {
          vflag <- "top"
          ftnts <- get_footnotes_docx(rs$footnotes, 
                                     spec_width, rs, 
                                     talgn, ex_brdr) 
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
    #b <- "<br>"
    blen <- 1
  }
  
  
  # Add extra offsets if table has a lot of borders turned on
  # to avoid undesired page wraps
  boff <- 0
  if (any(class(spec) == "table_spec") &
      any(spec$borders %in% c("all", "inside"))) {

    #boff <- round(row_count * rs$border_height / rs$row_height)
    boff <- 1
  }

  ublnks <- c()
  lblnks <- c()

  if (rs$paper_size != "none") {
    
    # Determine number of filler lines needed
    len_diff <- rs$body_line_count - row_count - ftnts$lines - lpg_rows - blen - boff
    
    if (vflag == "bottom" & len_diff > 0) {
  
      ublnks <- c(b, rep("<w:p/>", len_diff))
  
    } else {
  
      if ((wrap_flag & len_diff > 0)) {
        if (vflag == "bottom" | has_page_footer(rs))
          lblnks <- c(rep("<w:p/>", len_diff), b)
      } else {
        lblnks <- b
      }
    }
  }
  
  tlns <- sum(ftnts$lines, length(ublnks), length(lblnks))
  ret <- list(docx = c(ublnks, ftnts$docx, lblnks),
              lines = tlns)
  
  
  return(ret)
}

# Sub-Functions ---------------------------------------------------------


#' Get content offsets for table header, titles, footnotes, and content blanks.
#' Needed to calculate page breaks accurately.
#' @return A vector of upper and lower offsets
#' @noRd
get_content_offsets_docx <- function(rs, ts, pi, content_blank_row) {
  
  cnt <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  
  # Width is normally the width of the table, not the page
  wdth <- rs$content_size[["width"]]
  if (!is.null(pi$col_width))
    wdth <- sum(pi$col_width)
  
  # Default to zero
  shdrs <- list(lines = 0, twips = 0)
  hdrs <- list(lines = 0, twips = 0)
  
  
  if (ts$headerless == FALSE) {
    
    # Spanning headers now inside get_table_header_html
    hdrs <- get_table_header_docx(rs, ts, pi)  
  }
  
  # Get title headers or titles
  if (is.null(ts$title_hdr))
    ttls <- get_titles_docx(ts$titles, wdth, rs) 
  else 
    ttls <- get_title_header_docx(ts$title_hdr, wdth, rs)
  
  # Get page by if it exists
  pgb <- list(lines = 0, twips = 0)
  if (!is.null(ts$page_by))
    pgb <- get_page_by_docx(ts$page_by, wdth, NULL, rs, pi$table_align)
  else if (!is.null(rs$page_by))
    pgb <- get_page_by_docx(rs$page_by, wdth, NULL, rs, pi$table_align)
  
  # Add everything up
  cnt[["upper"]] <- shdrs$lines + hdrs$lines + ttls$lines + pgb$lines
  
  if (content_blank_row %in% c("above", "both")) {
    #ret[["blank_upper"]] <- rs$line_height
    cnt[["blank_upper"]] <- 1 
  }
  
  ftnts <- get_footnotes_docx(ts$footnotes, wdth, rs) 
  rftnts <- get_footnotes_docx(rs$footnotes, wdth, rs)
  
  if (has_top_footnotes(rs)) {
    cnt[["lower"]] <- ftnts$lines + rftnts$lines
  } else {
    cnt[["lower"]] <- ftnts$lines
  }
  
  # Add extra offsets if table has a lot of borders turned on
  # to avoid undesired page wraps
  if (any(ts$borders %in% c("all", "inside"))) {
    #ret[["lower"]] <- ret[["lower"]] + (rs$row_height * 2)
    cnt[["lower"]] <- cnt[["lower"]] + 1
  }
  
  if (content_blank_row %in% c("both", "below")) {
    cnt[["blank_lower"]] <- 1 
  }
  
  res <- list(lines = cnt)
  
  return(res)
  
}


#' @description Return a vector of strings for the table header
#' @details Basic idea of this function is to create a list
#' of string vectors of label words sized according to the column 
#' widths, then combine by line/row, and concatenate everything and justify.
#' @noRd
get_table_header_docx <- function(rs, ts, pi, ex_brdr = FALSE) {
  
  ret <- c()
  cols <- c()
  cnt <- 0
  rh <- rs$row_height
  tbl <- ts$data
  conv <- rs$twip_conversion


  
  widths <- pi$col_width[!is.na(pi$col_width)]
  lbls <- pi$label
  halgns <- pi$label_align
  talgn <- pi$table_align
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  
  conv <- rs$twip_conversion
  nms <- names(lbls)
  
  # u <- rs$units
  # if (rs$units == "inches")
  #   u <- "in"
  
  # Get cell widths
  sz <- c()
  for (k in seq_along(widths)) {
    if (!is.control(nms[k])) {
        sz[k] <- round(widths[k] * conv)
    }
  }
  

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
        ha[k] <- "left"
      else if (halgns[k] == "right")
        ha[k] <- "right"
      else if (halgns[k] %in% c("center", "centre"))
        ha[k] <- "center"
    }
  }
  
  # Table Header
  ret[1] <- ""
  cols[1] <- "<w:tblGrid>\n"
  

  
  cnt <-  1 
  bd <- "border-bottom: thin solid;"
  
  # Loop for column names
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = rs$font_size)
  
  for(k in seq_along(widths)) {
    if (!is.control(nms[k])) {
      
      cols[1] <- paste0(cols[1], "<w:gridCol w:w=\"", sz[k], "\"/>\n")
      
      # b <- get_cell_borders_html(1, k, 2, length(widths), brdrs, 
      #                            exclude = exclude_top)
      
      # Split label strings if they exceed column width
      tmp <- split_string_html(lbls[k], widths[k], rs$units)

      #if (b == "") {
        ret[1] <- paste0(ret[1], cell_abs(tmp$html, ha[k], sz[k], bborder = TRUE))
                         # "<w:tc>", 
                         # para(tmp$html, ha[k]), "</w:tc>\n")
      # } else {
      #   ret[1] <- paste0(ret[1], "<w:tc class=\"thdr ", ha[k], "\" ", 
      #                            "style=\"", b, "\">", 
      #                    encodeHTML(tmp$html), "</td>\n")
        
      # }
      
      # Add in extra lines for labels that wrap
      xtr <- tmp$lines
      if (xtr > cnt)
         cnt <- xtr
    }
  }
  
  
  dev.off()
  
  rht <- get_row_height(round(rs$row_height * cnt * conv))
  
  cols[1] <- paste0(cols[1], "</w:tblGrid>\n")
  ret[1] <- paste0("<w:tr>", rht, "\n", ret[1], "</w:tr>\n")
  
  # Get spanning headers
  # sphdrs <- get_spanning_header_html(rs, ts, pi,
  #                                    ifelse(is.null(exclude_top), FALSE, TRUE))
  
  # res <- list(docx = paste0(cols, "<thead>\n", paste0(sphdrs$docx, collapse=""), 
  #                           ret, "</thead>\n"),
  #             lines = cnt + sphdrs$lines)
  
  res <- list(docx = paste0(cols,  
                            ret),
              lines = cnt ) #+ sphdrs$lines)
  
  return(res)
  
}


#' @description Return a vector of html strings for the table spanning headers
#' @details Basic idea of this function is to figure out which columns 
#' the header spans, add widths, then call get_table_header.  Everything
#' from there is the same.  
#' @import stats
#' @noRd
get_spanning_header_docx <- function(rs, ts, pi, ex_brdr = FALSE) {
  
  spns <- ts$col_spans
  cols <- pi$keys
  cols <- cols[!is.controlv(cols)]
  w <- pi$col_width 
  w <- w[cols]
  gutter <- 0
  cnt <- c()
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  # print("Cols:")
  # print(cols)
  #print(w)
  wlvl <- get_spanning_info(rs, ts, pi, w, gutter)
  lvls <- sort(seq_along(wlvl), decreasing = TRUE)
  
  # At this point we have a data frame with labels, etc. and spanning
  # column widths, and are ready to create spanning header rows
  #print(wlvl)
  
  
  # Get borders
  brdrs <- ts$borders
  
  # Format labels for each level
  ln <- c()
  for (l in lvls) {
    
    s <- wlvl[[l]]
   # print(s)
    widths <- s$width
    names(widths) <- s$name
    algns <- s$align
    names(algns) <- s$name
    lbls <- s$label
    names(lbls) <- s$name
    cs <- s$col_span
    
    # Header Cell alignment
    ha <- c()
    for (k in seq_along(algns)) {
      
      if (algns[k] == "right")
        ha[k] <- "text-align:right;"
      else if (algns[k] %in% c("center", "centre"))
        ha[k] <- "text-align:center;"
      else
        ha[k] <- "text-align:left;"
      
    }
    
    r <- ""
    cnt[length(cnt) + 1] <- 1 
    
    # Start row
    r <-  "<tr>\n"
    
    # Open device context
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    # Loop for labels
    for(k in seq_along(lbls)) {
      
      # Split label strings if they exceed column width
      tmp <- split_string_html(lbls[k], widths[k], rs$units)
      
      
      b <- get_cell_borders_html(length(lvls) - l + 1, k, length(lvls) + 1, 
                                 length(widths), brdrs, 
                                 exclude = exclude_top)
      
      # Add colspans
      vl <- tmp$html
      
      # Special handling of borders for different situations.  
      # I hate this, but can't see a way around it.
      if (any(brdrs %in% c("all", "inside"))) {
        sflg <- "B"
        if (k > 1) {
          if (cs[k - 1] > 1)
            sflg <- ""
        }
        
        if (vl == "") {
          bb <- get_cell_borders_html(length(lvls) - l + 1, k, length(lvls), 
                                                 length(widths), brdrs, 
                                                 flag = sflg,
                                                 exclude = exclude_top)
        } else 
          bb <- b
      } else if (all(brdrs == "outside")) {
        
        if (vl == "")
          bb <- b
        else 
          bb <- paste0(b, "border-bottom:thin solid")
        
      } else {
        
        if (vl == "") {
          bb <- get_cell_borders_html(length(lvls) - l + 1, k, length(lvls), 
                                                      length(widths), brdrs, 
                                                      flag = "",
                                                      exclude = c("bottom", exclude_top))
        } else 
          bb <- paste0(b, "border-bottom:thin solid")
        
      }
      
      
      
      r <- paste0(r, "<td colspan=\"", cs[k], 
                  "\" style=\"vertical-align:bottom;", ha[k], bb, "\">", 
                  encodeHTML(vl), "</td>\n")
      # print(lbls[k])
      # print(widths[k])
      # Add in extra lines for labels that wrap
      xtr <- tmp$lines
      
      if (xtr > cnt[length(cnt)])
        cnt[length(cnt)] <- xtr
      
    }
    dev.off()
    
    
    r <- paste0(r, "</tr>\n")
    
    ln[[length(ln) + 1]] <- r
    
  }
  
  
  ret <- unlist(ln)
  
  res <- list(docx = ret, 
              lines = sum(cnt))
  
  return(res)
}

#' @description This function counts lines per row independently because
#' the ..row field does not account for page wrapping.  Need number
#' of lines on this particular page.
#' @noRd
get_table_body_docx <- function(rs, tbl, widths, algns, talgn, tbrdrs, ex_brdr = FALSE) {
  
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  
  if ("..blank" %in% names(tbl))
    flgs <- tbl$..blank
  else 
    flgs <- NA
  
  # Count lines per row
  rws <- c()
  border_flag <- FALSE
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  
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

  # Cell alignment
  ca <- c()
  for (k in seq_along(algns)) {
    if (!is.control(nms[k])) {
      if (algns[k] == "left")
        ca[k] <- "left"
      else if (algns[k] == "right")
        ca[k] <- "right"
      else if (algns[k] %in% c("center", "centre"))
        ca[k] <- "center"
    }
  }
  
  ret <- c()
  
  
  # Table Body
  for(i in seq_len(nrow(t))) {
    
    
    # if (i == 1)
    #   ret[i] <- "<tbody>\n<tr>"
    # else
      ret[i] <- paste0("<w:tr>", rht)
    
    mxrw <- 1
    
    # Loop for cell values
    for(j in seq_len(ncol(t))) {
      
      
      if (!is.control(nms[j])) {
        
        # b <- get_cell_borders_html(i, j, nrow(t), ncol(t), brdrs, flgs[i], 
        #                            exclude = exclude_top)
        
        vl <- t[i, j]
        if (!is.character(vl))
          vl <- as.character(vl)
        
        # Construct html
        # if (b == "")
          ret[i] <- paste0(ret[i], "<w:tc>", 
                           para(vl, ca[j]), "</w:tc>")
        # else 
        #   ret[i] <- paste0(ret[i], "<td ", ca[j], " style=\"", b, "\">", 
        #                    encodeHTML(t[i, j]), "</td>")
        
        # Count lines in cell 
        cl <- grep("\n", vl, fixed = TRUE)
        if (length(cl) >= mxrw)
          mxrw <- length(cl) + 1
      }
      
    }
    
    rws[i] <- mxrw
    
    # if (i == nrow(t))
    #   ret[i] <- paste0(ret[i], "</w:tr>\n</tbody>")
    # else 
      ret[i] <- paste0(ret[i], "</w:tr>")
    
    
  }
  
  if ("bottom" %in% get_outer_borders(brdrs))
    border_flag <- TRUE
  
  res <- list(docx = ret,
              lines = sum(rws), 
              border_flag = border_flag)
  
  return(res)
  
  
}

#' @description Have to check wrapping on a lot of files.  May have unintended 
#' results.
#' @noRd
encodeDOCX <- function(strng) {
  
  ret <- strng
  
  ret <- gsub("&", "&amp;", ret , fixed = TRUE)
  ret <- gsub(">", "&gt;", ret , fixed = TRUE)
  ret <- gsub("<", "&lt;", ret , fixed = TRUE)
  ret <- gsub("\n", "<br>", ret , fixed = TRUE)
  ret <- gsub(" ", "&nbsp;", ret, fixed = TRUE)
  if (ret == "")
    ret <- "&nbsp;"
  
  return(ret)
}

