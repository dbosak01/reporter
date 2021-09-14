

# Write Report RTF2 -------------------------------------------------------



#' @title
#' Write a RTF2 report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_rtf2 <- function(rs) {
  
  orig_path <- rs$modified_path
  
  if (file.exists(orig_path))
    file.remove(orig_path)
  
  # Establish content and body sizes
  rs <- page_setup_rtf(rs)
  
  # Document header is mostly independent of content
  hdr <- get_rtf_document(rs) 
  
  # Get content and break it into pages
  # Needs to return a list of pages so preview can work
  # Page numbers need to be included
  bdy <- paginate_content_rtf(rs)
  
  # Get column widths? Seems not necessary?
  
  # Deal with preview
  
  # Write content to file system
  # Later we can just return the stream
  write_content_rtf(orig_path, hdr, bdy)
  
  return(rs)
}


#' @description Returns header for RTF document.  This is independent of content,
#' except for the page header and footer.
#' @noRd
get_rtf_document <- function(rs) {
  
  # Set up vectors
  ret <- c()

  conv <- rs$twip_conversion
  
  fnt <- rs$font
  if (tolower(rs$font) == "times")
    fnt <- "Times New Roman"
  
  # Prepare header
  ret[length(ret) + 1] <- paste0("{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 ", fnt , ";}}")
  if (rs$orientation == "landscape") {
    ret[length(ret) + 1] <- "\\landscape\\horzdoc"
    ret[length(ret) + 1] <- paste0("\\paperw", round(rs$page_size[2] * conv),
                                   "\\paperh", round(rs$page_size[1] * conv))
  } else {
    ret[length(ret) + 1] <- "\\vertdoc"
    ret[length(ret) + 1] <- paste0("\\paperw", round(rs$page_size[1] * conv),
                                   "\\paperh", round(rs$page_size[2] * conv))
  }
  
  ret[length(ret) + 1] <- paste0("\\margl", round(rs$margin_left * conv),
                                 "\\margr", round(rs$margin_right * conv),
                                 "\\margt", round(rs$margin_top * conv),
                                 "\\margb", round(rs$margin_bottom  * conv),
                                 "\\headery", round(rs$margin_top  * conv),
                                 "\\footery", round(rs$margin_bottom  * conv))
  
  ph <- get_page_header_rtf(rs)
  if (ph$rtf != "")
    ret[length(ret) + 1] <- ph$rtf
  
  pf <- get_page_footer_rtf(rs)
  if (pf$rtf != "")
    ret[length(ret) + 1] <- pf$rtf
  
  # Line spacing values determined be trial and error.
  # Needed for LibreOffice.  Appear to be ignored in Word.
  if (rs$font_size == 10) {
    ret[length(ret) + 1] <- "\\sl-225\\slmult0\\fs20"
  } else if (rs$font_size == 12) {
    ret[length(ret) + 1] <- "\\sl-275\\slmult0\\fs24"
  } else if (rs$font_size == 8) {
    ret[length(ret) + 1] <- "\\sl-180\\slmult0\\fs16"
  }

  
  return(ret)
  
}

#' @noRd
write_content_rtf <- function(path, hdr, body) {
  
  
  # Write to file  
  f <- file(path, open="a")
  
  writeLines(hdr, con = f)
  
  for (pg in body){
  
    writeLines(pg, con = f)
  }
  
  writeLines("\\fs0\\par}", con = f)
  
  close(f)
  
}

# Don't forget to deal with encoding issues 
#body <- encodeRTF(ls)



# Write RTF Driver Functions ----------------------------------------------

# should be renamed to paginate_content_rtf
#' @noRd
paginate_content_rtf <- function(rs) {

  
  ret <- c()
  last_object <- FALSE
  lpg_twips <- 0
  pgs <- list()  # list of vectors with page rtf lines
  
  # Loop through content objects
  for (i in seq_along(rs$content)) {
    
    # Set last object flag
    if (i == length(rs$content))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    # Put content and object in variables for convenience
    cntnt <- rs$content[[i]] 
    obj <- cntnt$object
    
    # Remove blank row if last content object
    cbr <- obj$blank_row
    if (last_object) {
      if (all(cbr == "below")) 
        cbr <- "none"
      else if (all(cbr == "all"))
        cbr <- "above"
      
    }
    
    # Break each content type into a list of pages
    if (any(class(obj) == "table_spec")) {
      
      res <- create_table_pages_rtf(rs, cntnt, lpg_twips)
      
      
    } else if (any(class(obj) == "text_spec")) {
      
      res <- create_text_pages_rtf(rs, cntnt, lpg_twips, cbr)
      
      
    } else if (any(class(obj) == "plot_spec")) {
      
      # Needs fixing
      tmp_dir <- file.path(tempdir(), "temp.jpg")
      res <- create_plot_pages_rtf(rs, cntnt, lpg_twips, tmp_dir)
      
    }
    
    pgs <- append(pgs, res)
    
  }
  
  # Need to append content here
  
  
  
  # Can return something else if needed here
  ret <- pgs
  
  return(ret)
  
}

# Spec Functions ----------------------------------------------------------


#' @noRd
create_table_pages_rtf <- function(rs, cntnt, lpg_twips) {
  
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
  widths_uom <- get_col_widths(fdat, ts, labels, rs$char_width, rs$units) # ? fix this
  print("Widths UOM")
  print(widths_uom)
  
  # Split long text strings into multiple rows
  #fdat <- split_cells(fdat, widths_twips)  # ?  Work on this
  print("split_cells")
  print(fdat)
  
  
  # Break columns into pages
  gtr <- .2
  wraps <- get_page_wraps(rs$content_size[["width"]], ts, widths_uom, gtr)
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
                            lpg_twips, content_offset, ts)
  # print("splits")
  # print(splits)
  
  
  
  ### Old
  
  
  ttls <- get_titles_rtf(rs$titles, rs$content_size[["width"]], rs)
  
  ts <- cntnt$object
  
  dt <- ts$data
  conv <- rs$twip_conversion
  
  wdths <- rep(.75, ncol(dt)) #get_rtf_widths
  
  # rs, tbl, conv, widths, 
  # algns, halgns, talgn, brdrs
  
  algns <- rep("left", ncol(dt))
  halgns <- algns
  talgn <- cntnt$align 
  brdrs <- "outside"
  
  
  tbl <- get_table_lines_rtf(rs, dt, conv, wdths, algns, 
                             halgns, talgn, brdrs)
  
  
  lns <- append(ttls$rtf, tbl)
  
  return(lns)
}



#' @noRd
create_table_pages_rtf_back <- function(rs, cntnt, lpg_twips) {
  
  ttls <- get_titles_rtf(rs$titles, rs$content_size[["width"]], rs)
  
  ts <- cntnt$object
  
  dt <- ts$data
  conv <- rs$twip_conversion
  
  wdths <- rep(.75, ncol(dt)) #get_rtf_widths
  
  # rs, tbl, conv, widths, 
  # algns, halgns, talgn, brdrs
  
  algns <- rep("left", ncol(dt))
  halgns <- algns
  talgn <- cntnt$align 
  brdrs <- "outside"
  
  
  tbl <- get_table_lines_rtf(rs, dt, conv, wdths, algns, 
                             halgns, talgn, brdrs)
  
  
  lns <- append(ttls$rtf, tbl)
  
  return(lns)
}






# Table Functions ---------------------------------------------------------


#' @noRd
get_table_lines_rtf <- function(rs, tbl, conv, widths, 
                                algns, halgns, talgn, brdrs) {
 
  # Get line height.  Don't want to leave editor default.
  rh <- rs$row_height
  
  # Get cell widths
  sz <- c()
  for (k in seq_along(widths)) {
    if (k == 1)
      sz[k] <- widths[k] * conv
    else 
      sz[k] <- widths[k] * conv + sz[k - 1]
      
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
    if (algns[k] == "left")
      ca[k] <- "\\ql"
    else if (algns[k] == "right")
      ca[k] <- "\\qr"
    else if (algns[k] %in% c("center", "centre"))
      ca[k] <- "\\qc"
  }
  
  # Header Cell alignment
  ha <- c()
  for (k in seq_along(halgns)) {
    if (halgns[k] == "left")
      ha[k] <- "\\ql"
    else if (halgns[k] == "right")
      ha[k] <- "\\qr"
    else if (halgns[k] %in% c("center", "centre"))
      ha[k] <- "\\qc"
  }
    
  hdr <- c()
  
  # Table Header
  hdr[1] <-  paste0("\\trowd\\trgaph0", ta)
  
  # Loop for cell definitions
  for(j in seq_along(tbl)) {
    
    b <- get_cell_borders(1, j, 2, ncol(tbl), brdrs)
    hdr[1] <- paste0(hdr[1], b, "\\clbrdrb\\brdrs\\cellx", sz[j])
    
  }
  
  # Loop for column names
  nms <- names(tbl)
  for(k in seq_along(nms)) {
    hdr[1] <- paste0(hdr[1], ha[k], " ", nms[k], "\\cell")
  }
  
  hdr[1] <- paste(hdr[1], "\\row")
  
  
  bdy <- c()
  
  # Table Body
  for(i in seq_len(nrow(tbl))) {
    
    bdy[i] <- paste0("\\trowd\\trgaph0\\trrh", rh, ta)
    
    # Loop for cell definitions
    for(j in seq_len(ncol(tbl))) {
      b <- get_cell_borders(i, j, nrow(tbl), ncol(tbl), brdrs)
      bdy[i] <- paste0(bdy[i], b, "\\cellx", sz[j])
    }
    
    # Loop for cell values
    for(j in seq_len(ncol(tbl))) {
      
      bdy[i] <- paste0(bdy[i], ca[j], " ", tbl[i, j], "\\cell")
      
    }
    
    bdy[i] <- paste0(bdy[i], "\\row")
    
    
  }
  
  bdy[length(bdy)] <- paste0(bdy[length(bdy)], "\\pard")
  
  # Add header and body
  ret <- append(hdr, bdy)
  
  return(ret)
}



# Setup Functions ---------------------------------------------------------


#' @description Setup page for content
#' @details  Calculates available space for content and prepares text lines
#' for page template.
#' @noRd
page_setup_rtf <- function(rs) {
  
  debug <- TRUE
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  
  
  if (rs$font_size == 8) {
    rh <- 185 #round(.11 * 1440)
    lh <- 185 #round(.1 * 1440) 
    pb <- "\\page\\fs0\\par\\fs16\\pard"
  } else if (rs$font_size == 10) {
    rh <- 225 #round(.165 * 1440)
    lh <- 225 #round(.165 * 1440)  # 270
    pb <- "\\page\\fs0\\par\\fs20\\pard"
  } else if (rs$font_size == 12) {
    rh <- 270 #round(.2 * 1440)
    lh <- 270 #round(.1875 * 1440) #270
    pb <- "\\page\\fs0\\par\\fs24\\pard"
  }
  
  
  # Get conversion factor to twips
  if (rs$units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  rs$twip_conversion <- conv
  rs$row_height <- rh
  rs$line_height <- lh
  rs$page_break_rtf <- pb
  
  if (is.null(rs$user_line_count))
    rs$line_count <- floor(rs$content_size[[1]] * conv / rh)
  else 
    rs$line_count <- rs$user_line_count
  
  if (debug) {
    print(paste("Content Height:", rs$content_size[[1]]))
    print(paste("Content Width:", rs$content_size[[2]]))
    print(paste("Line Count:", rs$line_count))
    print(paste("Line Height:", rs$line_height))
  }
  
  hdr <- get_page_header_rtf(rs)
  ftr <- get_page_footer_rtf(rs)
  
  # Small adjustment by one line height
  rs$body_size <- 
    c(height = (rs$content_size[[1]] * conv) - hdr$twips - ftr$twips - lh, 
      width = rs$content_size[[2]] * conv)
  
  if (debug) {
    print(paste("Body Height:", rs$body_size[[1]]))
    print(paste("Body Width:", rs$body_size[[2]]))
  }
  
  
  return(rs)
}
