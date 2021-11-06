

# Write Report PDF2 -------------------------------------------------------



#' @title
#' Write a PDF2 report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_pdf2 <- function(rs) {
  

  if (file.exists(rs$modified_path))
    file.remove(rs$modified_path)
  
  # Establish content and body sizes
  rs <- page_setup_pdf(rs)
  
  # Document header is mostly independent of content
  doc <- create_pdf(filename = rs$modified_path,
                  margin_top = rs$margin_top,
                  margin_left = rs$margin_left,
                  fontname = rs$font,
                  fontsize = rs$font_size,
                  page_height = rs$page_size[2],
                  page_width = rs$page_size[1],
                  orientation = rs$orientation,
                  units = rs$units,
                  conversion = rs$point_conversion,
                  info = TRUE)


  
  # Get content and break it into pages
  # Needs to return a list of pages so preview can work
  # Page numbers need to be included
  doc <- paginate_content_pdf(rs, doc)
  
  # Get column widths
  rs$column_widths <- doc[["widths"]]
  
  # Deal with preview
  if (!is.null(rs$preview)) {
    if (rs$preview < length(doc$pages))
      doc$pages <- doc$pages[seq(1, rs$preview)]
  }
  
  # Write content to file system
  # Later we can just return the stream
  #rs <- write_content_pdf(rs, hdr, bdy, rs$page_template)
  doc <- write_pdf(doc)
  rs$pages <- doc$pages
  
  # Update page numbers for title headers
  #update_page_numbers_rtf(orig_path, rs$pages)
  
  return(rs)
}


#' @description Returns header for RTF document.  This is independent of content,
#' except for the page header and footer.
#' @noRd
get_pdf_document <- function(rs) {
  
  # Set up vectors
  ret <- c()
  
  conv <- rs$twip_conversion
  
  fnt <- rs$font
  if (tolower(rs$font) == "times")
    fnt <- "Times New Roman"
  
  # Prepare header
  r <- create_pdf(filename = rs$modified_path,
                  margin_top = rs$margin_top,
                  margin_left = rs$margin_left,
                  fontsize = rs$font_size,
                  page_height = rs$page_size[2],
                  page_width = rs$page_size[1],
                  orientation = rs$orientation,
                  units = rs$units,
                  info = TRUE)
  
  # ph <- get_page_header_pdf(rs)
  # if (ph$pdf != "")
  #   ret[length(ret) + 1] <- ph$pdf
  # 
  # pf <- get_page_footer_pdf(rs)
  # if (pf$pdf != "")
  #   ret[length(ret) + 1] <- pf$pdf
  

  
  
  
  return(ret)
  
}



#' @noRd
paginate_content_pdf <- function(rs, doc) {
  
  # Put content in a new variable
  ls <- rs$content
  ret <- c()
  last_object <- FALSE
  last_page_lines <- 0
  table_widths <- list()
  
  # Get report titles and footnotes
  rttls <- rs$page_template$titles
  rttl_hdr <- rs$page_template$title_hdr
  rftnts <- rs$page_template$footnotes
  rheader <- rs$page_template$page_header
  rfooter <- rs$page_template$page_footer
  
  hrf <- has_bottom_footnotes(rs)
  
  
  # Loop through content objects
  for (i in seq_along(ls)) {
    
    pgs <- list()  # list of vectors with page pdf lines
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
      
      res <- create_table_pages_pdf(rs, cntnt, last_page_lines)
      
      for (j in seq_len(length(res$page_list))) {
        
        if (j == 1 & last_page_lines > 0 & 
            (ceiling(last_page_lines + res$page_list[[j]]$lines) <= rs$body_line_count)) {
          
          doc$pages[[length(doc$pages)]] <- append(doc$pages[[length(doc$pages)]],
                                                   res$page_list[[j]]$pdf)
          
          
        } else {
        
          doc <- add_page(doc, res$page_list[[j]]$pdf, 
                          rttls$pdf, rttl_hdr$pdf, 
                          rftnts$pdf, rheader$pdf, rfooter$pdf)
        

        }
        
        if (j == length(res$page_list)) {
          
          last_page_lines <- last_page_lines + res$page_list[[j]]$lines
        } else {
          last_page_lines <- 0 
        }
      }
      # 
      # # Collect multiple pages and line counts
      # for (j in seq_len(length(res$page_list))) {
      #   pgs[[length(pgs) + 1]] <- res$page_list[[j]]$pdf
      #   lns[[length(lns) + 1]] <- res$page_list[[j]]$lines
      # }
      # 
      # Retrieve table widths.  These are useful for debugging.
      # Assigned to returning report object.
      table_widths[[length(table_widths) + 1]] <- res$widths
      
    } else if (any(class(obj) == "text_spec")) {
      
      res <- create_text_pages_pdf(rs, cntnt, last_page_lines, cbr)
      for (j in seq_len(length(res$pdf))) {
        
        if (j == 1 & last_page_lines > 0 & 
            (ceiling(last_page_lines + res$lines[[j]]) <= rs$body_line_count)) {
          
          doc$pages[[length(doc$pages)]] <- append(doc$pages[[length(doc$pages)]],
                                                   res$pdf[[j]])
        
          
        } else {
          
          doc <- add_page(doc, res$pdf[[j]], rttls$pdf, rttl_hdr$pdf, 
                          rftnts$pdf, rheader$pdf, rfooter$pdf) 
        
        }
        
        if (j == length(res$pdf)) {
          
          last_page_lines <- last_page_lines + res$lines[[j]]
        } else {
          last_page_lines <- 0 
        }
      }
      
      # for (j in seq_len(length(res$pdf))) {
      #   pgs[[length(pgs) + 1]] <- res$pdf[[j]]
      #   lns[[length(lns) + 1]] <- res$lines[[j]]
      #   
      # }
      
    } else if (any(class(obj) == "plot_spec")) {
      
      # Plots not started rtf2 conversion
      # res <- create_plot_pages_pdf(rs, cntnt, last_page_lines, tempdir())
      # for (j in seq_len(length(res$pdf))) {
      #   pgs[[length(pgs) + 1]] <- res$pdf[[j]]
      #   lns[[length(lns) + 1]] <- res$lines[[j]]
      #   
      # }
      
      res <- create_plot_pages_pdf(rs, cntnt, last_page_lines, tempdir())
      for (j in seq_len(length(res$pdf))) {
        
        if (j == 1 & last_page_lines > 0 & 
            (ceiling(last_page_lines + res$lines[[j]]) <= rs$body_line_count)) {
          
          doc$pages[[length(doc$pages)]] <- append(doc$pages[[length(doc$pages)]],
                                                   res$pdf[[j]])
          
          
          
        } else {
        
          doc <- add_page(doc, res$pdf[[j]], rttls$pdf, rttl_hdr$pdf, 
                          rftnts$pdf, rheader$pdf, rfooter$pdf) 
        
        }
        
        if (j == length(res$pdf)) {
          
          last_page_lines <- last_page_lines + res$lines[[j]]
        } else {
          last_page_lines <- 0 
        }
      }
    }
    
    if (cntnt$page_break == TRUE | last_page_lines >= rs$body_line_count)
      last_page_lines <- 0
    
    # Store pages and lines with content objects
    # The content settings will be used when writing content
    # ls[[i]]$pages <- pgs
    # ls[[i]]$lines <- lns
    
    # This section of code is appending blank lines to get
    # footnotes at the bottom of the page.  The complication
    # is when there are multiple pieces of content, and user-defined
    # page breaks.  So these can't be added earlier in
    # the process.  In short, these blanks are for in between
    # pieces of content.  Blanks within a piece of content are 
    # handled in the create_table_*, create_text_*, and create_plot_* 
    # functions.
    
    # Capture last page
    # last_page <- pgs[[length(pgs)]]
    # 
    # # Capture number of lines on the last page
    # last_page_lines <- lns[[length(lns)]] + last_page_lines
    
    # print(last_page_lines)
    # 
    # if (length(pgs) > 1)
    #   last_page_lines <- lns[[length(lns)]]
    # 
    # # If there is a page break or it's the last object in the
    # # content list, add the blank lines if needed.
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
    #     blnks <- rep("\\par", bl)
    #   
    #   last_page <- append(last_page, blnks)
    #   last_page_lines <- 0 
    #   
    # }
    
    # ls[[i]]$pages[[length(pgs)]] <- last_page
    
  }
  
  doc$widths <- table_widths
  
  # Can return something else if needed here
  # ret <- list(widths = table_widths, pages = ls)
  
  return(doc)
  
}



#' @title Write out content
#' @description This loop writes out pages created in paginate_content
#' Page template items added to the report (titles/footnotes/title_header)
#' are added in this step.  That means these items need to have been accounted
#' for in the previous steps.
#' @noRd
write_content_pdf <- function(rs, hdr, body, pt) {
  
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
        
        
        if (!is.null(rs$title_hdr) & !is.null(pt$title_hdr$rtf))
          writeLines(pt$title_hdr$rtf, con = f, useBytes = TRUE)
        
        if (!is.null(rs$titles) & !is.null(pt$titles$rtf))
          writeLines(pt$titles$rtf, con = f, useBytes = TRUE)
        
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
        
        if (!is.null(rs$footnotes) & !is.null(pt$footnotes$rtf))
          writeLines(pt$footnotes$rtf, con = f, useBytes = TRUE)
        
        
        # Add form feed character for text page break
        if (last_object == FALSE | last_page == FALSE) {
          
          if (is.null(rs$pages))
            rs$pages <- 1
          else 
            rs$pages <- rs$pages + 1 
          
          writeLines(rs$page_break_rtf, con = f, useBytes = TRUE) 
          
        }
      }
      
      if (last_object == TRUE & last_page == TRUE) {
        
        rs$pages <- rs$pages + 1 
        
      }
    }
    
  }
  
  writeLines("{\\pard\\fs1\\sl0  \\par}}", con = f, useBytes = TRUE)
  
  close(f)
  
  return(rs)
  
}


update_page_numbers_pdf <- function(path, tpg) {
  
  
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
page_setup_pdf <- function(rs) {
  
  debug <- FALSE
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  

  if (rs$font_size == 8) {

    gtr <- .1 
    cw <- .1

  } else if (rs$font_size == 9) {

    gtr <- .11
    cw <- .11

  } else if (rs$font_size == 10) {
   
    gtr <- .11
    cw <- .11

  } else if (rs$font_size == 11) {
  
    gtr <- .11
    cw <- .12

  } else if (rs$font_size == 12) {

    gtr <- .11
    cw <- .12

  }
  
  # Line height is for text
  lh <- get_line_height_pdf(rs$font_size)
  
  # Row height is for tables.  Right now both the same.
  rh <- lh
  
  
  # Get conversion factor to points
  if (rs$units == "inches") {
    conv <- 72
  } else {
    conv <- 1/2.54 * 72 
  }
  
  rs$point_conversion <- conv
  rs$row_height <- rh
  rs$line_height <- lh
  rs$char_width <- cw
  rs$line_size <- rs$content_size[["width"]]
  rs$cell_padding <- 1
  rs$border_height <- .5
  
  
  # Assume landscape
  pg_h <- rs$page_size[1]
  pg_w <- rs$page_size[2]
  
  # Change to portrait
  if(rs$orientation == "portrait") {
    pg_w <- rs$page_size[1]
    pg_h <- rs$page_size[2]
  }
  
  if (rs$units == "cm")
    rs$gutter_width <- ccm(gtr)
  else 
    rs$gutter_width <- gtr
  
  if (is.null(rs$user_line_count)) {
    rs$line_count <- floor(rs$content_size[[1]] * conv / rh) 
  } else 
    rs$line_count <- rs$user_line_count
  
  if (debug) {
    print(paste("Content Height:", rs$content_size[[1]]))
    print(paste("Content Width:", rs$content_size[[2]]))
    print(paste("Line Count:", rs$line_count))
    print(paste("Line Height:", rs$line_height))
    print(paste("Gutter Width:", rs$gutter_width))
    print(paste("Char Width:", rs$char_width))
  }
  
  # Get page template
  pt <- page_template_pdf(rs)
  rs$page_template <- pt
  
  # Body size in twips
  # Small adjustment by one line height
  # This gets used to determine lines on a page.
  rs$body_size <- 
    c(height = floor((rs$content_size[[1]] * conv) - pt$page_header$points - pt$page_footer$points), 
      width = floor(rs$content_size[[2]] * conv))
  
  if (debug) {
    print(paste("Body Height:", rs$body_size[[1]]))
    print(paste("Body Width:", rs$body_size[[2]]))
  }
  
  
  # Get the page template row count
  # Include all the rows associated with the page template
  rs$page_template_header_count <- sum(pt$page_header$lines, pt$titles$lines, 
                                       pt$title_hdr$lines, pt$page_by$lines)
  if (debug)
    print(paste("Page Template Header Count:", rs$page_template_header_count))
  
  rs$page_template_footer_count <- sum(pt$footnotes$lines, pt$page_footer$lines)
  if (debug)
    print(paste("Page Template Footer Count:", rs$page_template_footer_count))
  
  rs$page_template_row_count <- rs$page_template_header_count + 
    rs$page_template_footer_count
  if (debug)
    print(paste("Page Template Row Count:", rs$page_template_row_count))
  
  # Body line count is the number of rows available for content on each page
  rs$body_line_count <- rs$line_count - rs$page_template_row_count
  if (debug)
    print(paste0("Body Line Count: ", rs$body_line_count))
  
  return(rs)
}

# Row heights determined by trial and error on RTF and converted to points
# for PDF (divide by 20).
#' @noRd
get_line_height_pdf <- function(font_size) {

  if (font_size == 8) {
    sm <- 185 / 20
  } else if (font_size == 9) {
    sm <- 218 / 20
  } else if (font_size == 10) {
    sm <- 228 / 20
  } else if (font_size == 11) {
    sm <- 250 / 20
  } else if (font_size == 12) {
    sm <- 275 / 20
  } else if (font_size == 13) {
    sm <- 300 / 20
  } else if (font_size == 14) {
    sm <- 325 /20
  } else {
    sm <- 250 / 20
  }

  return(sm)
}
