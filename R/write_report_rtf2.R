

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
  
  rs <- page_setup_rtf(rs)

  ls <- list("Hello") #create_report_general(rs)
  
  # Revise text and write to rtf
  fls <- write_rtf_output2(rs, ls, orig_path)
  
  return(rs)
}


# May need some adjustments/sophistication/options to this function
#' @return Vector of graphic file paths
#' @noRd
write_rtf_output2 <- function(rs, ls, orig_path) {
  
  # Set up vectors
  hdr <- c() 
  body <- c() 
  
  ret <- c()

  
  conv <- rs$twip_conversion
  
  fnt <- rs$font
  if (tolower(rs$font) == "times")
    fnt <- "Times New Roman"
  
  # Prepare header
  hdr[length(hdr) + 1] <- paste0("{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 ", fnt , ";}}")
  if (rs$orientation == "landscape") {
    hdr[length(hdr) + 1] <- "\\landscape\\horzdoc"
    hdr[length(hdr) + 1] <- paste0("\\paperw", round(rs$page_size[2] * conv),
                                   "\\paperh", round(rs$page_size[1] * conv))
  } else {
    hdr[length(hdr) + 1] <- "\\vertdoc"
    hdr[length(hdr) + 1] <- paste0("\\paperw", round(rs$page_size[1] * conv),
                                   "\\paperh", round(rs$page_size[2] * conv))
  }
  
  hdr[length(hdr) + 1] <- paste0("\\margl", round(rs$margin_left * conv),
                                 "\\margr", round(rs$margin_right * conv),
                                 "\\margt", round(rs$margin_top * conv),
                                 "\\margb", round(rs$margin_bottom  * conv),
                                 "\\headery", round(rs$margin_top  * conv),
                                 "\\footery", round(rs$margin_bottom  * conv))
  
  ph <- get_page_header_rtf(rs)
  if (ph != "")
    hdr[length(hdr) + 1] <- ph
  
  pf <- get_page_footer_rtf(rs)
  if (pf != "")
    hdr[length(hdr) + 1] <- pf
  
  # Line spacing values determined be trial and error.
  # Needed for LibreOffice.  Appear to be ignored in Word.
  if (rs$font_size == 10) {
    hdr[length(hdr) + 1] <- "\\sl-225\\slmult0\\fs20"
  } else if (rs$font_size == 12) {
    hdr[length(hdr) + 1] <- "\\sl-275\\slmult0\\fs24"
  } else if (rs$font_size == 8) {
    hdr[length(hdr) + 1] <- "\\sl-180\\slmult0\\fs16"
  }
  
  body <- get_rtf_body(rs, conv)
  
  # Start with all lines
  #body <- encodeRTF(ls)
  
  # if (rs$has_graphics) {

  #     
  #     img <- get_image_rtf(spec[[1]], as.numeric(spec[[3]]), 
  #                          as.numeric(spec[[2]]), rs$units, rs$font_size)
  #     ret[length(ret) + 1] <- spec[[1]]
  #     
  #     # Create rtf codes
  #     if (spec[[4]] == "left") {
  #       ltx <- paste0("\\par\\sl0\\ql\n"  )
  #       
  #     } else if (spec[[4]] == "right") {
  #       ltx <- paste0("\\par\\sl0\\qr\n"  )
  #     } else  {
  #       ltx <- paste0("\\par\\sl0\\qc\n"  )
  #     }

  
  
  # body <- gsub("\f", "\\page ", body, fixed = TRUE)
  # body <- paste0(body, ifelse(pgs, "", "\\line"))
  
  # Write to file  
  f <- file(orig_path, open="a")
  
  writeLines(hdr, con = f)
  
  writeLines(body, con = f)
  
  writeLines("}", con = f)
  
  close(f)
  
  
  return(ret)
  
}


# Write RTF Driver Functions ----------------------------------------------

# should be renamed to paginate_content_rtf
get_rtf_body <- function(rs, conv) {
  
  hdr <- c()
  # if (rs$page_header_blank_row == "below")
  #   hdr[length(hdr) + 1] <- "\\line"
  
  bdy <- c()
  
  lpg_rows <- 0
  
  for (cntnt in rs$content) {
    
    
    if (any(class(cntnt$object) == "table_spec")) {
      
      res <- create_table_pages_rtf(rs, cntnt, conv)
      
      
    } else if (any(class(cntnt$object) == "text_spec")) {
      
      res <- create_text_pages_rtf(rs, cntnt, lpg_rows, conv)
      
      
    } else if (any(class(cntnt$object) == "plot_spec")) {
      
      res <- create_plot_pages_rtf(rs, cntnt, conv)
      
    }
    
    bdy <- append(bdy, res)
    
  }
  
  
  ret <- c(hdr, bdy)
  
  return(ret)
  
}

# Spec Functions ----------------------------------------------------------

create_table_pages_rtf <- function(rs, cntnt, conv) {
  

  ttls <- get_titles_rtf(rs$titles, rs$content_size[["width"]], conv)
  
  ts <- cntnt$object
  
  dt <- ts$data
  
  wdths <- rep(.75, ncol(dt)) #get_rtf_widths
  
  # rs, tbl, conv, widths, 
  # algns, halgns, talgn, brdrs
  
  algns <- rep("left", ncol(dt))
  halgns <- algns
  talgn <- cntnt$align 
  brdrs <- "outside"
  
  
  tbl <- get_table_lines_rtf(rs, dt, conv, wdths, algns, 
                             halgns, talgn, brdrs)
  
  
  lns <- append(ttls, tbl)
  
  return(lns)
}






# Table Functions ---------------------------------------------------------



get_table_lines_rtf <- function(rs, tbl, conv, widths, 
                                algns, halgns, talgn, brdrs) {
 
  # Get line height.  Don't want to leave editor default.
  rh <- rs$line_height
  
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
  
  debug <- FALSE

  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  if (debug)
    print(paste0("Content Size: ", rs$content_size))
  
  
  if (rs$font_size == 8)
    lh <- round(.11 * 1440)
  else if (rs$font_size == 10)
    lh <- round(.16 * 1440 )
  else if (rs$font_size == 12)
    lh <- round(.2 * 1440)
  
  
  # Get conversion factor to twips
  if (rs$units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  rs$twip_conversion <- conv
  rs$line_height <- lh
  
  if (is.null(rs$user_line_count))
    rs$line_count <- floor(rs$content_size[[1]] * conv / lh)
  else 
    rs$line_count <- rs$user_line_count
  
  print(rs$content_size[[1]])
  print(rs$line_count)
  
  # Line size is the number of characters that will fit in the content size width
  # if (is.null(rs$user_line_size)) {
  #   if (rs$output_type == "RTF") {
  #     # 1 char adjustment to avoid occasional wrapping 
  #     rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width) - 1 
  #   } else {
  #     rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width) 
  #   }
  # } else 
  #   rs$line_size <- rs$user_line_size
  # if (debug)
  #   print(paste0("Line Size: ", rs$line_size))
  # 
  # # Line count is the number of lines that will fit in the content size height
  # if (is.null(rs$user_line_count))
  #   rs$line_count <- floor(rs$content_size[["height"]] / rs$line_height)
  # else
  #   rs$line_count <- rs$user_line_count
  # if (debug)
  #   print(paste0("Line Count: ", rs$line_count))
  

  
  return(rs)
}
