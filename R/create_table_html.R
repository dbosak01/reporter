

# Create Tables ----------------------------------------------------------


#' @noRd
create_table_pages_html <- function(rs, cntnt, lpg_rows) {
  
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
        dat$..page_by <-  dat[[pgby_var]]
    
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
  
  if ("..stub_var" %in% names(fdat)){
    control_cols <- c(control_cols, "..stub_var")
  }
  
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
  
  # Don't merge if there is a stub with background color
  merge_label_row <- TRUE
  if (has_style(rs, "table_stub_background") & "stub" %in% keys)
    merge_label_row <- FALSE
  
  # Get column widths
  widths_uom <- get_col_widths_variable(fdat, ts, labels, 
                                        rs$font, rs$font_size, rs$units, 
                                        rs$gutter_width, merge_label_row) 
  # print("Widths UOM")
  # print(widths_uom)
  
  # Split long text strings into multiple rows. Number of rows are stored in
  # ..row variable. If too slow, may need to be rewritten in C
  fdat <- split_cells_variable(fdat, widths_uom, rs$font,
                               rs$font_size, rs$units, rs$output_type, 
                               rs$char_width, ts)$data
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
  content_offset <- get_content_offsets_html(rs, ts, tmp_pi, 
                                             content_blank_row, pgby_cnt)
  
  
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
      pg_lst[[length(pg_lst) + 1]] <- create_table_html(rs, ts, pi, 
                                                       blnk_ind, wrap_flag,
                                                       lpg_rows, styles)
    }
  }
  
  ret <- list(widths = widths_uom, page_list = pg_lst)
  
  return(ret)
  
  
}



#' @noRd
create_table_html <- function(rs, ts, pi, content_blank_row, wrap_flag, 
                             lpg_rows, styles) {
  rh <- rs$row_height
  shdrs <- list(lines = 0, twips = 0)
  hdrs <- list(lines = 0, twips = 0)
  
  # Default to content width
  ls <- rs$content_size[["width"]]
  
  # Get table width
  if (!is.null(pi$col_width))
    ls <- sum(pi$col_width, na.rm = TRUE)
  
  if (!is.null(ts$title_hdr))
    ttls <- get_title_header_html(ts$title_hdr, ls, rs, pi$table_align)
  else
    ttls <- get_titles_html(ts$titles, ls, rs, pi$table_align) 
  
  
  if (!is.null(rs$page_by)) {
    pgby <- get_page_by_html(rs$page_by, rs$content_size[["width"]], 
                            pi$page_by, rs, pi$table_align, ttls$border_flag)
  } else if(!is.null(ts$page_by)) {
    pgby <- get_page_by_html(ts$page_by, ls, pi$page_by, rs, 
                             pi$table_align, ttls$border_flag)
  } else 
    pgby <- c()
  
  exbrdr <- ttls$border_flag
  if (length(pgby) > 0)
    exbrdr <- pgby$border_flag
  
  if (ts$headerless == FALSE) {
    
    # Get table header also includes spanning header
    hdrs <- get_table_header_html(rs, ts, pi, exbrdr)  
  }
  
  # rs, ts, widths,  algns, halgns, talgn
  rws <- get_table_body_html(rs, pi$data, pi$col_width, 
                             pi$col_align, pi$table_align, ts$borders, 
                             !ts$headerless,
                             ts$first_row_blank, styles, ts)
  
  a <- NULL
  if (content_blank_row %in% c("above", "both"))
    a <- "<br>"
  
  
  blnks <- c()

  
  # Determine sum of all lines
  rc <- sum(ttls$lines, pgby$lines, shdrs$lines, 
            hdrs$lines, rws$lines,
            length(a))
  
  # Get footnotes, passing in sum of all current lines
  ftnts <- get_page_footnotes_html(rs, ts, ls, lpg_rows, rc,
                                  wrap_flag, content_blank_row,  pi$table_align, 
                                  rws$border_flag)
  

  
  u <- rs$units
  if (u == "inches")
    u <- "in"
  
  # ds <- paste0("<div ", ta, ">")
  ts <- paste0("<table class=\"tbs\" style=\"width:", 
               round(sum(pi$col_width, 
                         na.rm = TRUE), 3), u,";\">")
  
  ret <- list(html = c(a, ttls$html, pgby$html, ts, shdrs$html, 
                      hdrs$html, rws$html, "</table>", ftnts$html),
              lines = rc  + ftnts$lines)
  
  return(ret) 
}

# Haven't quite decided what to do with this function in HTML
# For now, just return the footnote and any blank lines above or below
get_page_footnotes_html <- function(rs, spec, spec_width, lpg_rows, row_count,
                                   wrap_flag, content_blank_row, talgn, 
                                   ex_brdr = FALSE) {
  
  ftnts <- list(lines = 0, twips = 0, border_flag = FALSE)
  vflag <- "none"

  
  # Deal with valign parameter
  if (!is.null(spec$footnotes)) {
    if (!is.null(spec$footnotes[[length(spec$footnotes)]])) {
      if (spec$footnotes[[length(spec$footnotes)]]$valign == "bottom") {
        
        vflag <- "bottom"
        ftnts <- get_footnotes_html(spec$footnotes, 
                                   spec_width, rs, 
                                   talgn, FALSE) 
      } else {
        vflag <- "top"
        ftnts <- get_footnotes_html(spec$footnotes, spec_width, rs, talgn, ex_brdr) 
      }
      
    }
  } else {
    
    if (!is.null(rs$footnotes[[1]])) {
      if (!is.null(rs$footnotes[[1]]$valign)) {
        if (rs$footnotes[[1]]$valign == "top") {
          vflag <- "top"
          ftnts <- get_footnotes_html(rs$footnotes, 
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
    b <- "<br>"
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
  
      ublnks <- c(b, rep("<br>", len_diff))
  
    } else {
  
      if ((wrap_flag & len_diff > 0)) {
        if (vflag == "bottom" | has_page_footer(rs))
          lblnks <- c(rep("<br>", len_diff), b)
      } else {
        lblnks <- b
      }
    }
  }
  
  tlns <- sum(ftnts$lines, length(ublnks), length(lblnks))
  ret <- list(html = c(ublnks, ftnts$html, lblnks),
              lines = tlns)
  
  # ret <- list(html =  ftnts$html,
  #             lines = ftnts$lines)
  
  return(ret)
}

# Sub-Functions ---------------------------------------------------------


#' Get content offsets for table header, titles, footnotes, and content blanks.
#' Needed to calculate page breaks accurately.
#' @return A vector of upper and lower offsets
#' @noRd
get_content_offsets_html <- function(rs, ts, pi, content_blank_row, pgby_cnt = NULL) {
  
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
    hdrs <- get_table_header_html(rs, ts, pi)  
  }
  
  # Get title headers or titles
  if (is.null(ts$title_hdr))
    ttls <- get_titles_html(ts$titles, wdth, rs) 
  else 
    ttls <- get_title_header_html(ts$title_hdr, wdth, rs)
  
  # Get page by if it exists
  pgb <- list(lines = 0, twips = 0)
  if (!is.null(ts$page_by))
    pgb <- get_page_by_html(ts$page_by, wdth, NULL, rs, pi$table_align, pgby_cnt = pgby_cnt)
  else if (!is.null(rs$page_by))
    pgb <- get_page_by_html(rs$page_by, wdth, NULL, rs, pi$table_align, pgby_cnt = pgby_cnt)
  
  # Add everything up
  cnt[["upper"]] <- shdrs$lines + hdrs$lines + ttls$lines + pgb$lines
  
  if (content_blank_row %in% c("above", "both")) {
    #ret[["blank_upper"]] <- rs$line_height
    cnt[["blank_upper"]] <- 1 
  }
  
  ftnts <- get_footnotes_html(ts$footnotes, wdth, rs) 
  rftnts <- get_footnotes_html(rs$footnotes, wdth, rs)
  
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
get_table_header_html <- function(rs, ts, pi, ex_brdr = FALSE) {
  
  ret <- c()
  cols <- c()
  cnt <- 0
  rh <- rs$row_height
  tbl <- ts$data

  
  widths <- pi$col_width[!is.na(pi$col_width)]
  lbls <- pi$label
  halgns <- pi$label_align
  talgn <- pi$table_align
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  
  #conv <- rs$twip_conversion
  nms <- names(lbls)
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"
  
  # Get cell widths
  sz <- c()
  for (k in seq_along(widths)) {
    if (!is.control(nms[k])) {
        sz[k] <- paste0(widths[k], u)
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
        ha[k] <- "tdl"
      else if (halgns[k] == "right")
        ha[k] <- "tdr"
      else if (halgns[k] %in% c("center", "centre"))
        ha[k] <- "tdc"
    }
  }
  
  # Table Header
  ret[1] <- "<tr>\n"
  cols[1] <- "<colgroup>\n"
  

  
  cnt <-  1 
  if (has_style(rs, "border_color")) {
    bd <- paste0("border-bottom: thin solid ", get_style(rs, "border_color"), ";")
  } else {
    bd <- "border-bottom: thin solid black;"
  }
  
  # Loop for column names
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = rs$font_size)
  
  for(k in seq_along(widths)) {
    if (!is.control(nms[k])) {
      
      cols[1] <- paste0(cols[1], "<col style=\"width:", sz[k], ";\">\n")
      
      
      b <- get_cell_borders_html(1, k, 2, length(widths), brdrs, 
                                 exclude = exclude_top,
                                 border_color = get_style(rs, "border_color"))
      
      # Split label strings if they exceed column width
      tmp <- split_string_html(lbls[k], widths[k], rs$units)
      

      if (ts$header_bold)
        tstr <- paste0("<b>", encodeHTML(tmp$html), "</b>")
      else 
        tstr <- encodeHTML(tmp$html)

      if (b == "") {
        ret[1] <- paste0(ret[1], "<td class=\"thdr ", ha[k], "\">", 
                         tstr, "</td>\n")
      } else {
        ret[1] <- paste0(ret[1], "<td class=\"thdr ", ha[k], "\" ", 
                                 "style=\"", b, "\">", 
                         tstr, "</td>\n")
        
      }
      
      # Add in extra lines for labels that wrap
      xtr <- tmp$lines
      if (xtr > cnt)
         cnt <- xtr
    }
  }
  dev.off()
  
  cols[1] <- paste0(cols[1], "</colgroup>\n")
  ret[1] <- paste0(ret[1], "</tr>\n")
  
  if (ts$first_row_blank == TRUE) {
    
    sflg <- nms[1] == "stub" &  has_style(rs, "table_stub_background")

    tstub <- ""
    if (sflg)
      tstub <- " ts"

    if (sflg) {
      
      if (any(brdrs == "body")) {
        
        b1 <- get_cell_borders_html(2, 1, 3, 1, "left",
                                    border_color = get_style(rs, "border_color"), 
                                    stub_flag = TRUE)
        
        b2 <- get_cell_borders_html(2, 1, 3, 1, "right",
                                    border_color = get_style(rs, "border_color"), 
                                    stub_flag = FALSE)
        
      } else {
        
        b1 <- get_cell_borders_html(2, 1, 3, 1, brdrs,
                                   border_color = get_style(rs, "border_color"), 
                                   stub_flag = TRUE)
        
        b2 <- get_cell_borders_html(2, 1, 3, 1, brdrs,
                                    border_color = get_style(rs, "border_color"), 
                                    stub_flag = FALSE)
      
      }

      ret[1] <- paste0(ret[1], "<tr><td class=\"tc ts\"", 
                       "\" style=\"", b1, "\">&nbsp;</td>",
                       "<td class=\"tc\" colspan=\"", length(sz) - 1, 
                       "\" style=\"", b2, "\">&nbsp;</td></tr>")

    } else {
    
      if (any(brdrs == "body"))
        b <- get_cell_borders_html(2, 1, 3, 1, c("left", "right"),
                                   border_color = get_style(rs, "border_color"))
      else {
        b <- get_cell_borders_html(2, 1, 3, 1, brdrs,
                                   border_color = get_style(rs, "border_color"))
      }
  
      if (b == "") {
        ret[1] <- paste0(ret[1], "<tr><td class=\"tc\" colspan=\"", length(sz), 
                       "\">&nbsp;</td></tr>")
      } else {
        ret[1] <- paste0(ret[1], "<tr><td class=\"tc\" colspan=\"", length(sz), 
                         "\" style=\"", b, "\">&nbsp;</td></tr>")
      }
    }
    cnt <- cnt + 1
  }
  
  # Get spanning headers
  sphdrs <- get_spanning_header_html(rs, ts, pi,
                                     ifelse(is.null(exclude_top), FALSE, TRUE))
  
  res <- list(html = paste0(cols, "<thead>\n", paste0(sphdrs$html, collapse=""), 
                            ret, "</thead>\n"),
              lines = cnt + sphdrs$lines)
  
  return(res)
  
}


#' @description Return a vector of html strings for the table spanning headers
#' @details Basic idea of this function is to figure out which columns 
#' the header spans, add widths, then call get_table_header.  Everything
#' from there is the same.  
#' @import stats
#' @noRd
get_spanning_header_html <- function(rs, ts, pi, ex_brdr = FALSE) {
  
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
  
  # Add information for gap
  if (length(wlvl) > 0) {
    wlvl <- get_spanning_gap_html(wlvl) 
  }
  
  # Get borders
  brdrs <- ts$borders
  
  brdrcolor <- get_style(rs, "border_color")
  if (brdrcolor == "")
    brdrcolor <- "black"
  
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
                                 exclude = exclude_top,
                                 border_color = get_style(rs, "border_color"))
      
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
                                                 exclude = exclude_top,
                                      border_color = get_style(rs, "border_color"))
        } else 
          bb <- b
      } else if (all(brdrs == "outside")) {
        
        if (vl == "") {
          bb <- b
        } else if (s$underline[k] == TRUE) {
          bb <- paste0(b, "border-bottom:thin solid ", brdrcolor)
        } else {
          bb <- b
        }
      } else {
        
        if (vl == "") {
          bb <- get_cell_borders_html(length(lvls) - l + 1, k, length(lvls), 
                                                      length(widths), brdrs, 
                                                      flag = "",
                                                      exclude = c("bottom", exclude_top),
                                      border_color = get_style(rs, "border_color"))
        } else if (s$underline[k] == TRUE) {
          bb <- paste0(b, "border-bottom:thin solid ", brdrcolor)
        } else {
          bb <- b
        }
      }
      
      if (s$bold[k])
        tstr <- paste0("<b>", encodeHTML(vl), "</b>")
      else 
        tstr <- encodeHTML(vl)
      
      # Check gap information
      gap <- ""
      if (!is.na(s$gap_width[k])){
        # This div appears to create a small white gap in the black bottom border of the cell
        gap <- sprintf(
          "<div style=\"position:absolute;left:-3px;width:%sin;height:1px;bottom:-1px;background-color:white;\"></div>",
          s$gap_width[k]
        )
        
        #  Sets the positioning context for absolute positioning within the cell
        bb <- paste0(bb, ";position:relative;")
      }
      
      r <- paste0(r, "<td class=\"shdr\" colspan=\"", cs[k], 
                  "\" style=\"vertical-align:bottom;", ha[k], bb, "\">",
                  gap, tstr, "</td>\n")
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
  
  res <- list(html = ret, 
              lines = sum(cnt))
  
  return(res)
}

#' @description Check gap and insert gap information for HTML
#' @details Check gap and insert gap information for HTML
#' @noRd
get_spanning_gap_html <- function(wlvl, gap_twips = 100){
  
  ret <- list()
  
  for (k in 1:length(wlvl)) {
    
    df <- wlvl[[k]]
    
    # Prepare gap variables
    df$gap_width <- rep(NA, nrow(df))
    
    # Extract spanning and underline only for detection
    df_span <- df[df$span > 0 & df$underline == TRUE,] 
    
    if (nrow(df_span) < 2) {
      ret[[k]] <- df
    } else {
      # Convert twips to inch (Default 100 twips for consistency)
      gap_width <- gap_twips/1440
      
      df_span$order <- as.numeric(stri_extract_first_regex(df_span$name, "\\d+"))
      
      for (i in 2:nrow(df_span)) {
        
        cur_order <- df_span$order[i]
        
        # Gap is processed only for next cell
        pre_order <- df_span$order[i - 1]
        
        # Continuous underline detection
        if (cur_order - pre_order == 1) {
          
          # Add indenting information for gap
          df_span$gap_width[i] <- gap_width
        }
        
      } # End of data row loop
      
      # Drop order
      df_span <- df_span[, setdiff(names(df_span), "order")]
      
      # Stack together and sort
      df_ret <- rbind(df_span, df[!(df$span > 0 & df$underline == TRUE),])
      df_ret <- df_ret[order(df_ret$name),]
      
      ret[[k]] <- df_ret
      
    } # End of gap process
  }
  
  return(ret)
}

#' @description This function counts lines per row independently because
#' the ..row field does not account for page wrapping.  Need number
#' of lines on this particular page.
#' @noRd
get_table_body_html <- function(rs, tbl, widths, algns, talgn, tbrdrs, 
                                ex_brdr = FALSE, frb = FALSE, styles,
                                ts) {
  
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
  castr <- c()
  for (k in seq_along(algns)) {
    if (!is.control(nms[k])) {
      if (is.na(nms[k]) | is.null(nms[k])) {
        tcls <- ""
      } else {
        if (nms[k] == "stub")
          tcls <- "ts"
        else 
          tcls <- ""
      }
      
      if (algns[k] == "left")
        ca[k] <- paste0("tc tdl ", tcls)
      else if (algns[k] == "right")
        ca[k] <- paste0("tc tdr ", tcls)
      else if (algns[k] %in% c("center", "centre"))
        ca[k] <- paste0("tc tdc ", tcls)
      
      
      if (algns[k] == "left")
        castr[k] <- paste0("tc tdl tbstr ", tcls)
      else if (algns[k] == "right")
        castr[k] <- paste0("tc tdr tbstr ", tcls)
      else if (algns[k] %in% c("center", "centre"))
        castr[k] <- paste0("tc tdc tbstr ", tcls)
    }
  }
  
  ret <- c()
  
  strpmod <- 1
  if (frb)
    strpmod <- 0
  
  merge_label_row <- TRUE
  if (has_style(rs, "table_stub_background") & "stub" %in% nms)
    merge_label_row <- FALSE
  
  pdf(NULL)
  par(family = get_font_family(rs$font), ps = rs$font_size)
  
  
  # Table Body
  for(i in seq_len(nrow(t))) {
    
    
    if (i == 1)
      ret[i] <- "<tbody>\n<tr>"
    else
      ret[i] <- "<tr>"
    
    mxrw <- 1
    
    # Loop for cell values
    for(j in seq_len(ncol(t))) {
      
      
      if (!is.control(nms[j])) {
        
        sflg <- nms[j] == "stub" &  has_style(rs, "table_stub_background")
        
        b <- get_cell_borders_html(i, j, nrow(t), ncol(t), brdrs, flgs[i], 
                                   exclude = exclude_top, 
                                   border_color = get_style(rs, "border_color"),
                                    stub_flag = sflg)
        
        # Put indent information into style
        if (rs$units == "inches") {
          ind_unit <- "in"
        } else if (rs$units == "cm") {
          ind_unit <- "cm"
        }
        pad <- ""
        
        defs <- ts$col_defs
        if (!is.null(defs[[nms[j]]]$indent)) {
          
          pad <- sprintf("padding-left: %s%s;",
                          defs[[nms[j]]]$indent,
                          ind_unit)
          
        } else if (nms[j] == "stub" & !is.null(ts$stub)) {
          
          stub_var <- tbl$..stub_var[i]
          if (!is.null(defs[[stub_var]]$indent)) {
            pad <- sprintf("padding-left: %s%s;",
                           defs[[stub_var]]$indent,
                           ind_unit)
          }
        }
        
        b <- paste0(b, pad)
        
        
        lrflg <- ""
        if ( nms[j] == "stub" & flgs[i] == "L")
          lrflg <- " tlr"
        
        # Stripe class
        if (i %% 2 == strpmod) {
          scls <- ca[j]
        } else {
          scls <- castr[j]
        }
        
        vl <- t[i, j]
        if (all(class(vl) != "character"))
          vl <- as.character(vl)
        else 
          vl <- encodeHTML(vl)
        
        if (merge_label_row  & flgs[i] %in% c("B", "A", "L")) {
          if (j == 1) {
            
            # # Strip out line feeds for label rows
            # vl <- gsub("\n", " ", vl, fixed = TRUE)
            # 
            # # Recalculate based on total width of table
            # tmp <- split_string_html(vl, sum(wdths), rs$units)
            # 
            # vl <- tmp$html
            
            stl <- get_cell_styles(nms[j], styles, flgs, i, tbl)
            
            if ("bold" %in% stl) {
              vl <- paste0("<b>", vl, "</b>") 
            }
            
          
            if (b == "") {
              
              ret[i] <- paste0(ret[i], "<td class=\"", scls, lrflg, "\"",
                               " colspan = \"", ncol(t), "\">", 
                               vl, "</td>")
            } else { 
              
              ret[i] <- paste0(ret[i], "<td class=\"", scls, lrflg, "\"",
                               " colspan = \"", ncol(t), "\"",
                               " style=\"", b, "\">", 
                               vl, "</td>")
            }
          
          
          }
        } else {
          
          stl <- get_cell_styles(nms[j], styles, flgs, i, tbl)
          
          if ("bold" %in% stl) {
            vl <- paste0("<b>", vl, "</b>") 
          } 
          
          # Construct html
          if (b == "") {
  
            ret[i] <- paste0(ret[i], "<td class=\"", scls, lrflg, "\">", 
                             vl, "</td>")
          } else { 
            
            ret[i] <- paste0(ret[i], "<td class=\"", scls, lrflg, 
                             "\" style=\"", b, "\">", 
                             vl, "</td>")
          }
        }
        
        # Count lines in cell
        cl <- strsplit(vl, "\n", fixed = TRUE)[[1]]
        if (length(cl) > mxrw)
          mxrw <- length(cl) 
      }
      
    }
    
    rws[i] <- mxrw
    
    if (i == nrow(t))
      ret[i] <- paste0(ret[i], "</tr>\n</tbody>")
    else 
      ret[i] <- paste0(ret[i], "</tr>")
    
    
  }
  
  dev.off()
  
  if ("bottom" %in% get_outer_borders(brdrs))
    border_flag <- TRUE
  
  res <- list(html = ret,
              lines = sum(rws), 
              border_flag = border_flag)
  
  return(res)
  
  
}

#' @description Have to check wrapping on a lot of files.  May have unintended 
#' results.
#' @noRd
encodeHTML <- function(strng) {
  
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

