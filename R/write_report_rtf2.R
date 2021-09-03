

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
  
  rs <- page_setup_font(rs)

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
  
  
  # Get conversion factor to twips
  if (rs$units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
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
                                 "\\margb", round(rs$margin_bottom  * conv))
  
  ph <- get_rtf_page_header(rs, conv)
  if (ph != "")
    hdr[length(hdr) + 1] <- ph
  
  pf <- get_rtf_page_footer(rs, conv)
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
  
  if (rs$page_header_blank_row == "below")
    hdr[length(hdr) + 1] <- "\\line"
  
  rt <- get_rtf_report_titles(rs, conv)
  if (rt != "")
    hdr[length(hdr) + 1] <- rt
  
  # rs, tbl, conv, widths
  # w <- rep(.75, ncol(mtcars))
  # body <- get_rtf_table_lines(rs, mtcars[1:10, ], conv, w) 
  
  # Start with all lines
  #body <- encodeRTF(ls)
  
  # if (rs$has_graphics) {
  #   # Remove fill lines
  #   fill_tags <- grep("```fill```", body, fixed = TRUE)
  #   body <- body[-fill_tags]
  #   
  #   # Replace any plot tags with latex codes
  #   plt_tags <- grep("```([^}]*)```", body)
  #   
  #   for (i in plt_tags) {
  #     
  #     # Remove braces
  #     rw <- trimws(gsub("`", "", body[i], fixed = TRUE))
  #     
  #     # Split on pipe
  #     spec <- strsplit(rw, "|", fixed = TRUE)[[1]]
  #     
  #     # 1 = path
  #     # 2 = height
  #     # 3 = width
  #     # 4 = align
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
  #     
  #     # Replace original line with RTF codes
  #     body[[i]] <- paste0(ltx, img)
  #   }
  # }
  
  # Get page breaks
  # pgs <- grepl("\f", body, fixed = TRUE)
  
  # Adjust by one to get previous line
  # pgs <- pgs[2:length(pgs)]
  
  # Add one for end of document
  # pgs[length(pgs) + 1] <- TRUE 
  # print(length(body))
  # print(length(pgs))
  # print(pgs)
  
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


# Template Functions ------------------------------------------------------

get_rtf_page_header <- function(rs, conv) {
  
  ret <- ""
  
  hl <- rs$page_header_left
  hr <- rs$page_header_right

  maxh <- max(length(hl), length(hr))
  
  if (maxh > 0) {
    
    fs <- rs$font_size * 2
    
    c2 <- rs$content_size[["width"]] * conv
    c1 <- c2 / 2
  
    ret <- paste0("{\\header \\fs", fs)
    
    for (i in seq(1, maxh)) {
      ret <- paste0(ret, "\\trowd\\trgaph0\\cellx", c1, "\\cellx", c2 , " ")
      
      if (length(hl) >= i)
        ret <- paste0(ret, hl[[i]], "\\cell")
      else 
        ret <- paste0(ret, "\\cell")
      
      if (length(hr) >= i)
        ret <- paste0(ret, "\\qr ", hr[[i]], "\\cell\\row\n")
      else 
        ret <- paste0(ret, "\\qr \\cell\\row\n")
      
    }
    
    ret <- paste0(ret, "}")
  }
  
  return(ret)
}



get_rtf_page_footer <- function(rs, conv) {
  
  ret <- ""
  
  fl <- rs$page_footer_left
  fc <- rs$page_footer_center
  fr <- rs$page_footer_right
  
  maxf <- max(length(fl), length(fc), length(fr))
  
  if (maxf > 0) {
    
    fs <- rs$font_size * 2
    
    c3 <- rs$content_size[["width"]] * conv
    c1 <- c3 / 3
    c2 <- c1 * 2
    
    ret <- paste0("{\\footer \\fs", fs)
    
    for (i in seq(1, maxf)) {
      
      ret <- paste0(ret, "\\trowd\\trgaph0\\cellx", c1, 
                    "\\cellx", c2 , "\\cellx", c3, " ")
      
      if (length(fl) >= i)
        ret <- paste0(ret, fl[[i]], "\\cell")
      else 
        ret <- paste0(ret, "\\cell")
      
      if (length(fc) >= i)
        ret <- paste0(ret, "\\qc ", fc[[i]], "\\cell")
      else 
        ret <- paste0(ret, "\\qc \\cell")
      
      if (length(fr) >= i)
        ret <- paste0(ret, "\\qr ", fr[[i]], "\\cell\\row\n")
      else 
        ret <- paste0(ret, "\\qr \\cell\\row\n")
      
    }
    
    ret <- paste0(ret, "}")
  }
  
  return(ret)
}

get_rtf_report_titles <- function(rs, conv) {
  
  ret <- ""

  c1 <- rs$content_size[["width"]] * conv
  
  if (length(rs$titles) > 0) {
  
    for (ttls in rs$titles) {
      
      if (ttls$align == "center")
        algn <- "\\qc"
      else if (ttls$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      for (ttl in ttls$titles) {
        
        ret <- paste0(ret, "\\trowd\\trgaph0\\cellx", c1, 
                      algn, " ", ttl, "\\cell\\row\n")
      }
    }
    ret <- paste0(ret, "\\pard")
  }
  
  return(ret)
}


# Table Functions ---------------------------------------------------------



get_rtf_table_lines <- function(rs, tbl, conv, widths) {
 
  ret <- c()
  
  sz <- c()
  for (k in seq_along(widths)) {
    if (k == 1)
      sz[k] <- widths[k] * conv
    else 
      sz[k] <- widths[k] * conv + sz[k - 1]
      
  }
  
  for(i in seq_len(nrow(tbl))) {
    
    ret[i] <- "\\trowd\\trgaph0"
    
    # Loop for cell definitions
    for(j in seq_len(ncol(tbl))) {
      
      ret[i] <- paste0(ret[i], "\\cellx", sz[j])
      
    }
    
    # Loop for cell values
    for(j in seq_len(ncol(tbl))) {
      
      ret[i] <- paste0(ret[i], " ", tbl[i, j], "\\cell")
      
    }
    
    ret[i] <- paste0(ret[i], "\\row")
    
    
  }
  
  ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  
  return(ret)
}

# Setup Functions ---------------------------------------------------------


#' @description Setup page for content
#' @details  Calculates available space for content and prepares text lines
#' for page template.
#' @noRd
page_setup_font <- function(rs) {
  
  debug <- FALSE

  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  if (debug)
    print(paste0("Content Size: ", rs$content_size))
  
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
