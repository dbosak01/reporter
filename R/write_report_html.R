

# Write Report HTML -------------------------------------------------------



#' @title
#' Write an HTML report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_html <- function(rs) {
  
  orig_path <- rs$modified_path
  
  if (file.exists(orig_path))
    file.remove(orig_path)
  
  # Establish content and body sizes
  rs <- page_setup_html(rs)
  
  # Document header is mostly independent of content
  hdr <- get_html_document(rs) 
  
  # Put content in a new variable
  ls <- rs$content
  
  # Get content and break it into pages
  # Needs to return a list of pages so preview can work
  # Page numbers need to be included
  bdy <- paginate_content_html(rs, ls)
  
  # Get column widths
  rs$column_widths <- bdy[["widths"]]
  
  # Deal with preview
  if (!is.null(rs$preview)) {
    if (rs$preview < length(bdy[[1]]$pages))
      bdy[[1]]$pages <- bdy[[1]]$pages[seq(1, rs$preview)]
  }
  
  # Write content to file system
  # Later we can just return the stream
  rs <- write_content_html(rs, hdr, bdy, rs$page_template)
  
  # Update page numbers for title headers
  update_page_numbers_html(orig_path, rs$pages)
  
  return(rs)
}


#' @description Returns header for HTML document.  This is independent of content,
#' except for the page header and footer.
#' @noRd
get_html_document <- function(rs) {
  
  # Set up vectors
  ret <- c()
  
  conv <- rs$twip_conversion
  
  fnt <- rs$font
  if (tolower(rs$font) == "times")
    fnt <- "Times New Roman"
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"
  
  # Prepare header
  ret[length(ret) + 1] <- paste0("<!DOCTYPE html>\n",
                                 "<html>\n", "<head>")
  
  ret[length(ret) + 1] <- "<style>"
  ret[length(ret) + 1] <- paste0("body {\nfont-family: ", fnt,
                                 ";\nfont-size: ", rs$font_size, "pt;\n", 
                                 "margin-top: ", rs$margin_top, u, ";\n",
                                 "margin-bottom: ", rs$margin_bottom, u, ";\n",
                                 "margin-left: ", rs$margin_left, u, ";\n",
                                 "margin-right: ", rs$margin_right, u, ";\n",
                                 "}")
  ret[length(ret) + 1] <- paste0(".thdr {\nfont-weight: normal;\n", 
                                  "border-bottom: thin solid;\n", 
                                  "}\n")
  ret[length(ret) + 1] <- paste0(".tdc {\ntext-align:center;\n", 
                                 "}\n")
  ret[length(ret) + 1] <- paste0(".tdl {\ntext-align:left;\n", 
                                 "}\n")
  ret[length(ret) + 1] <- paste0(".tdr {\ntext-align:right;\n", 
                                 "}\n")
  ret[length(ret) + 1] <- "</style>"
  

  
  # Will need stuff in the header: style sheet, etc.
  
  # {\\fonttbl {\\f0 ", fnt , ";}}")
  # if (rs$orientation == "landscape") {
  #   ret[length(ret) + 1] <- "\\landscape\\horzdoc"
  #   ret[length(ret) + 1] <- paste0("\\paperw", round(rs$page_size[2] * conv),
  #                                  "\\paperh", round(rs$page_size[1] * conv))
  # } else {
  #   ret[length(ret) + 1] <- "\\vertdoc"
  #   ret[length(ret) + 1] <- paste0("\\paperw", round(rs$page_size[1] * conv),
  #                                  "\\paperh", round(rs$page_size[2] * conv))
  # }
  # 
  # ret[length(ret) + 1] <- paste0("\\margl", round(rs$margin_left * conv),
  #                                "\\margr", round(rs$margin_right * conv),
  #                                "\\margt", round(rs$margin_top * conv),
  #                                "\\margb", round(rs$margin_bottom  * conv),
  #                                "\\headery", round(rs$margin_top  * conv),
  #                                "\\footery", round(rs$margin_bottom  * conv))
  
  
  ret[length(ret) + 1] <- "</head>\n<body>"
  
  # Will need this ultimately
  # ph <- get_page_header_html(rs)
  # if (ph$rtf != "")
  #   ret[length(ret) + 1] <- ph$rtf
  # 
  # pf <- get_page_footer_html(rs)
  # if (pf$rtf != "")
  #   ret[length(ret) + 1] <- pf$rtf
  

  
  return(ret)
  
}



#' @noRd
paginate_content_html <- function(rs, ls) {
  
  ret <- c()
  last_object <- FALSE
  last_page_lines <- 0
  table_widths <- list()
  
  hrf <- has_bottom_footnotes(rs)
  
  
  # Loop through content objects
  for (i in seq_along(ls)) {
    
    pgs <- list()  # list of vectors with page rtf lines
    lns <- c()
    
    # Set last object flag
    if (i == length(ls))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    # Put content and object in variables for convenience
    cntnt <- ls[[i]] 
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
      
      res <- create_table_pages_html(rs, cntnt, last_page_lines)
      
      # Collect multiple pages and line counts
      for (j in seq_len(length(res$page_list))) {
        pgs[[length(pgs) + 1]] <- res$page_list[[j]]$html
        lns[[length(lns) + 1]] <- res$page_list[[j]]$lines
      }
      
      # Retrieve table widths.  These are useful for debugging.
      # Assigned to returning report object.
      table_widths[[length(table_widths) + 1]] <- res$widths
      
    } else if (any(class(obj) == "text_spec")) {
      
      res <- create_text_pages_html(rs, cntnt, last_page_lines, cbr)
      for (j in seq_len(length(res$html))) {
        pgs[[length(pgs) + 1]] <- res$html[[j]]
        lns[[length(lns) + 1]] <- res$lines[[j]]
        
      }
      
    } else if (any(class(obj) == "plot_spec")) {
      
      res <- create_plot_pages_html(rs, cntnt, last_page_lines, tempdir())
      for (j in seq_len(length(res$html))) {
        pgs[[length(pgs) + 1]] <- res$html[[j]]
        lns[[length(lns) + 1]] <- res$lines[[j]]
        
      }
    }   
    
    # Store pages and lines with content objects
    # The content settings will be used when writing content
    ls[[i]]$pages <- pgs
    ls[[i]]$lines <- lns
    
    # This section of code is appending blank lines to get
    # footnotes at the bottom of the page.  The complication
    # is when there are multiple pieces of content, and user-defined
    # page breaks.  So these can't be added earlier in
    # the process.  In short, these blanks are for in between
    # pieces of content.  Blanks within a piece of content are 
    # handled in the create_table_*, create_text_*, and create_plot_* 
    # functions.
    
    # Capture last page
    last_page <- pgs[[length(pgs)]]
    
    # Capture number of lines on the last page
    last_page_lines <- lns[[length(lns)]] + last_page_lines
    
    # print(last_page_lines)
    
    if (length(pgs) > 1)
      last_page_lines <- lns[[length(lns)]]
    
    # If there is a page break or it's the last object in the
    # content list, add the blank lines if needed.
    # if ((ls[[i]]$page_break | last_object) & hrf) {
    #   
    #   
    #   # Add extra offsets if table has a lot of borders turned on
    #   # to avoid undesired page wraps
    #   boff <- 0
    #   if (any(class(obj) == "table_spec") & 
    #       any(obj$borders %in% c("all", "inside"))) {
    #     
    #     boff <- round(last_page_lines * rs$border_height / rs$row_height)
    #   }
    #   
    #   blnks <- c()
    #   bl <- rs$body_line_count - last_page_lines - boff
    #   if (bl > 0)
    #     blnks <- rep("<br>", bl)
    #   
    #   last_page <- append(last_page, blnks)
    #   last_page_lines <- 0 
    #   
    # }
    # 
    # ls[[i]]$pages[[length(pgs)]] <- last_page
    
  }
  
  
  # Can return something else if needed here
  ret <- list(widths = table_widths, pages = ls)
  
  return(ret)
  
}


# Could be consolidated with RTF.
#' @title Write out content
#' @description This loop writes out pages created in paginate_content
#' Page template items added to the report (titles/footnotes/title_header)
#' are added in this step.  That means these items need to have been accounted
#' for in the previous steps.
#' @noRd
write_content_html <- function(rs, hdr, body, pt) {
  
  # Kill existing file
  if (file.exists(rs$modified_path))
    file.remove(rs$modified_path)
  
  counter <- 0
  page <- 0
  last_object <- FALSE
  last_page <- FALSE
  page_open <- FALSE
  
  
  f <- file(rs$modified_path, open="a", encoding = "native.enc")
  
  writeLines(hdr, con = f, useBytes = TRUE)
  
  
  for (cont in body$pages) {
    
    
    # Increment counter
    counter <- counter + 1
    page <- 0
    
    # Set last_object flag
    if (counter == length(body$pages))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    
    for (pg in cont$pages) {
      
      page <- page + 1
      
      if (page == length(cont$pages))
        last_page <- TRUE
      else
        last_page <- FALSE
      
      
      #print(page_open)
      if (page_open == FALSE) {
        
        if (!is.null(rs$page_template$page_header) & 
            !is.null(rs$page_template$page_header$html))
          writeLines(rs$page_template$page_header$html, con = f, useBytes = TRUE)
        
        if (!is.null(rs$title_hdr) & !is.null(pt$title_hdr$html))
          writeLines(pt$title_hdr$html, con = f, useBytes = TRUE)
        
        if (!is.null(rs$titles) & !is.null(pt$titles$html))
          writeLines(pt$titles$html, con = f, useBytes = TRUE)
        
      }
      
      if (!is.null(pg)) {
        
        writeLines(pg, con = f, useBytes = TRUE)
        
      }
      
      # Set page_open flag based on status of page_break and current objects
      if (last_object == FALSE & last_page == TRUE & cont$page_break == FALSE)
        page_open <- TRUE
      else 
        page_open <- FALSE
      
      if (page_open == FALSE) {
        
        if (!is.null(rs$footnotes) & !is.null(pt$footnotes$html))
          writeLines(pt$footnotes$html, con = f, useBytes = TRUE)
        
        
        if (!is.null(rs$page_template$page_footer) & 
            !is.null(rs$page_template$page_footer$html))
          writeLines(rs$page_template$page_footer$html, con = f, useBytes = TRUE)
        
        
        # Add form feed character for text page break
        if (last_object == FALSE | last_page == FALSE) {
          
          if (is.null(rs$pages))
            rs$pages <- 1
          else 
            rs$pages <- rs$pages + 1 
          
          writeLines(rs$page_break_html, con = f, useBytes = TRUE) 
          
        }
      }
      
      if (last_object == TRUE & last_page == TRUE) {
        
        rs$pages <- rs$pages + 1 
        
      }
    }
    
  }
  
  writeLines("</body></html>", con = f, useBytes = TRUE)
  
  close(f)
  
  return(rs)
  
}

#' @description Could be consolidated with RTF
#' @noRd
update_page_numbers_html <- function(path, tpg) {
  
  
  lns <- readLines(path, encoding = "UTF-8")
  
  lns <- gsub("[tpg]", tpg, lns, fixed = TRUE)
  
  f <- file(path, open = "w+", encoding = "native.enc")
  
  writeLines(lns, con = f, useBytes = TRUE)
  
  close(f)
  
}




# Setup Functions ---------------------------------------------------------


#' @description Setup page for content
#' @details  Calculates available space for content and prepares text lines
#' for page template.
#' @noRd
page_setup_html <- function(rs) {
  
  debug <- FALSE
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  
  rs$gutter_width <- 0.1
  rs$line_size <- 10000
  rs$body_line_count <- 10000
  
  rs$page_break_html <- "<div style=\"page-break-after: always;\"></div>"
  
  # # A lot of these values are guesses.  Need to test.
  # # Row height and line height were defined independently in case
  # # they are different.  Right now, appear to be the same.
  # if (rs$font_size == 8) {
  #   rh <- 185 #round(.11 * 1440)
  #   lh <- 185 #round(.1 * 1440) 
  #   #pb <- "\\fs1\\sl0\\par\\pard\\fs16\\page\\fs1\\sl0\\par\\pard\\fs16"
  #   pb <- "{\\pard\\pagebb\\fs1\\sl0\\par}\\fs16"
  #   gtr <- .1 
  #   cw <- .1
  #   cp <- 40
  #   sm <- "\\sl-180\\slmult0"
  # } else if (rs$font_size == 10) {
  #   rh <- 228 #round(.165 * 1440) # 225
  #   lh <- 228 #round(.165 * 1440)  
  #   #pb <- "\\page\\line" #fs1\\sl0\\par\\pard\\fs20"
  #   pb <-  "{\\pard\\pagebb\\fs1\\sl0\\par}\\fs20"
  #   gtr <- .1
  #   cw <- .11
  #   cp <- 40
  #   sm <- "\\sl-225\\slmult0"
  # } else if (rs$font_size == 12) {
  #   rh <- 275 #round(.2 * 1440)
  #   lh <- 275 #round(.1875 * 1440) #270
  #   pb <- "{\\pard\\pagebb\\fs1\\sl0\\par}\\fs24"
  #   gtr <- .11
  #   cw <- .12
  #   cp <- 40
  #   sm <- "\\sl-275\\slmult0"
  # }
  # 
  # 
  # # Get conversion factor to twips
  # if (rs$units == "inches") {
  #   conv <- 1440
  # } else {
  #   conv <- 566.9291
  # }
  # 
  # rs$twip_conversion <- conv
  # rs$row_height <- rh
  # rs$line_height <- lh
  # rs$page_break_rtf <- paste0(pb, sm)
  # rs$char_width <- cw
  # rs$line_size <- rs$content_size[["width"]]
  # rs$cell_padding <- cp
  # rs$spacing_multiplier <- sm
  # rs$border_height <- 15
  # 
  # # Line spacing values determined by trial and error.
  # # Needed for LibreOffice.  Appear to be ignored in Word.
  # if (rs$font_size == 10) {
  #   rs$font_rtf <-  "\\f0\\fs20"
  # } else if (rs$font_size == 12) {
  #   rs$font_rtf <-  "\\f0\\fs24"
  # } else if (rs$font_size == 8) {
  #   rs$font_rtf  <- "\\f0\\fs16"
  # }
  # 
  # 
  # if (rs$units == "cm")
  #   rs$gutter_width <- ccm(gtr)
  # else 
  #   rs$gutter_width <- gtr
  # 
  # if (is.null(rs$user_line_count)) {
  #   # There is one row above the page footer that is not printable.
  #   # Therefore adjust by 1.
  #   rs$line_count <- floor(rs$content_size[[1]] * conv / rh) - 1
  # } else 
  #   rs$line_count <- rs$user_line_count
  # 
  # if (debug) {
  #   print(paste("Content Height:", rs$content_size[[1]]))
  #   print(paste("Content Width:", rs$content_size[[2]]))
  #   print(paste("Line Count:", rs$line_count))
  #   print(paste("Line Height:", rs$line_height))
  #   print(paste("Gutter Width:", rs$gutter_width))
  #   print(paste("Char Width:", rs$char_width))
  # }
  # 
  # Get page template
  pt <- page_template_html(rs)
  rs$page_template <- pt
  # 
  # # Body size in twips
  # # Small adjustment by one line height
  # # This gets used to determine lines on a page.
  # rs$body_size <- 
  #   c(height = floor((rs$content_size[[1]] * conv) - pt$page_header$twips - pt$page_footer$twips - lh), 
  #     width = floor(rs$content_size[[2]] * conv))
  # 
  # if (debug) {
  #   print(paste("Body Height:", rs$body_size[[1]]))
  #   print(paste("Body Width:", rs$body_size[[2]]))
  # }
  # 
  # 
  # # Get the page template row count
  # # Include all the rows associated with the page template
  # rs$page_template_header_count <- sum(pt$page_header$lines, pt$titles$lines, 
  #                                      pt$title_hdr$lines, pt$page_by$lines)
  # if (debug)
  #   print(paste("Page Template Header Count:", rs$page_template_header_count))
  # 
  # rs$page_template_footer_count <- sum(pt$footnotes$lines, pt$page_footer$lines)
  # if (debug)
  #   print(paste("Page Template Footer Count:", rs$page_template_footer_count))
  # 
  # rs$page_template_row_count <- rs$page_template_header_count + 
  #   rs$page_template_footer_count
  # if (debug)
  #   print(paste("Page Template Row Count:", rs$page_template_row_count))
  # 
  # # Body line count is the number of rows available for content on each page
  # rs$body_line_count <- rs$line_count - rs$page_template_row_count
  # if (debug)
  #   print(paste0("Body Line Count: ", rs$body_line_count))
  
  return(rs)
}
