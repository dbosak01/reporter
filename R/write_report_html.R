

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
  
  if (file.exists(orig_path)) {
    file.remove(orig_path)
  }
  remove_image_files(orig_path)
  
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

#' @noRd
remove_image_files <- function(htmlpath) {
  
  f <- paste0(gsub(".html", "", basename(htmlpath)), "-")
  d <- dirname(htmlpath)
  im <- paste0(d, "/images")
  ret <- FALSE
  
  if (dir.exists(im)) {
    fls <- list.files(path = im, paste0("^", f), full.names = TRUE,
                      include.dirs = FALSE, no.. = TRUE)
    
    ret <- file.remove(fls)
  }

  return(ret)
}

#' @description Returns header for HTML document.  This is independent of content,
#' except for the page header and footer.
#' @noRd
get_html_document <- function(rs) {
  
  # Set up vectors
  ret <- c()
  
  sty <- rs$style
  
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
  ret[length(ret) + 1] <- paste0("@media print{\n",
                                 "@page {size:", rs$orientation, ";",
                                  "margin:0;}\n",
                                  ".noprint{display:none;}}")
  ret[length(ret) + 1] <- paste0("body {\nfont-family: ", fnt,
                                 ";\nfont-size: ", rs$font_size, "pt;\n", 
                                 "margin-top: ", rs$margin_top, u, ";\n",
                                 "margin-bottom: ", 
                                 round(rs$margin_bottom/2, 3), u, ";\n",
                                 "margin-left: ", rs$margin_left, u, ";\n",
                                 "margin-right: ", rs$margin_right, u, ";\n",
                                 get_style_html(rs, "background_color"),
                                 get_style_html(rs, "text_color"),
                                 "}")
  
  brdrcolor <- get_style(rs, "border_color")
  if (brdrcolor == "")
    brdrcolor <- "black"
  
  ret[length(ret) + 1] <- paste0(".thdr {", 
                                  "border-bottom: thin solid ", brdrcolor, ";", 
                                  get_style_html(rs, "table_header_background"),
                                  get_style_html(rs, "table_header_font_color"),
                                  get_style_html(rs, "table_header_font_bold", FALSE),
                                  "}")
  ret[length(ret) + 1] <- paste0(".shdr {", 
                                 get_style_html(rs, "table_header_background"),
                                 get_style_html(rs, "table_header_font_color"),
                                 get_style_html(rs, "table_header_font_bold", FALSE),
                                 "}")
  ret[length(ret) + 1] <- paste0(".tdc {text-align:center;}")
  ret[length(ret) + 1] <- paste0(".tdl {text-align:left;}")
  ret[length(ret) + 1] <- paste0(".tdr {text-align:right;}")
  ret[length(ret) + 1] <- paste0(".tc {", 
                                 get_style_html(rs, "table_body_background"), 
                                 "}")

  ret[length(ret) + 1] <- paste0(".tbs {", 
                                 get_style_html(rs, "table_body_font_color"),
                                 "}")
  ret[length(ret) + 1] <- paste0(".tbstr {", 
                                 get_style_html(rs, "table_body_stripe"),
                                 "}")
  ret[length(ret) + 1] <- paste0(".ts {", 
                                 get_style_html(rs, "table_stub_background"), 
                                 get_style_html(rs, "table_stub_font_bold"),
                                 get_style_html(rs, "table_stub_font_color"),
                                 "}")
  ret[length(ret) + 1] <- paste0(".tlr {", 
                                 get_style_html(rs, "table_label_row_bold"), 
                                 "}")
  ret[length(ret) + 1] <- paste0("table {",
                                 "border-spacing: 0;",
                                 "border-collapse: collapse;",
                                 "}")
  ret[length(ret) + 1] <- paste0("td {padding:0px 2px 0px 2px;}")
  ret[length(ret) + 1] <- "</style>"
  

  
  
  ret[length(ret) + 1] <- "</head>\n<body>"
  

  

  
  return(ret)
  
}



#' @noRd
paginate_content_html <- function(rs, ls) {
  
  ret <- c()
  last_object <- FALSE
  last_page_lines <- 0
  table_widths <- list()
  
  hrf <- has_bottom_footnotes(rs)
  if (hrf ==  FALSE)
    hrf <- has_page_footer(rs)
  
  
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
    cbr <- cntnt$blank_row
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
    if (rs$paper_size != "none") {
      if ((ls[[i]]$page_break | last_object) & hrf) {
  
  
        # Add extra offsets if table has a lot of borders turned on
        # to avoid undesired page wraps
        boff <- 0
        if (any(class(obj) == "table_spec") &
            any(obj$borders %in% c("all", "inside"))) {

          #boff <- round(last_page_lines * rs$border_height / rs$row_height)
          boff <- 1
        }
  
        blnks <- c()
        bl <- rs$body_line_count - last_page_lines - boff
        if (bl > 0)
          blnks <- rep("<br>", bl)
  
        last_page <- append(last_page, blnks)
        last_page_lines <- 0
  
      }
    }
    
    if (cntnt$page_break == TRUE | last_page_lines >= rs$body_line_count)
      last_page_lines <- 0

    ls[[i]]$pages[[length(pgs)]] <- last_page
    
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
    
    ta <- "align=\"left\" "
    if (cont$align == "right")
      ta <- "align=\"right\" "
    else if (cont$align %in% c("center", "centre"))
      ta <- "align=\"center\" "
    
    
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
          writeLines(update_page(rs$page_template$page_header$html,  rs$pages), 
                     con = f, useBytes = TRUE)
        
        # Write content div to keep page together
        writeLines(paste0("<div ", ta, ">"), con = f, useBytes = TRUE)
        
        
        if (!is.null(rs$title_hdr) & !is.null(pt$title_hdr$html))
          writeLines(update_page(pt$title_hdr$html,  rs$pages), con = f, 
                     useBytes = TRUE)
        
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
          writeLines(update_page(pt$footnotes$html,  rs$pages), 
                     con = f, useBytes = TRUE)
        
        # Content div
        writeLines("</div>", con = f, useBytes = TRUE)
        
        if (!is.null(rs$page_template$page_footer) & 
            !is.null(rs$page_template$page_footer$html))
          writeLines(update_page(rs$page_template$page_footer$html, rs$pages), 
                     con = f, useBytes = TRUE)
        
        
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

update_page <- function(lns, pg) {
  
 ret <- gsub("[pg]", pg + 1, lns, fixed = TRUE) 
 
 return(ret)
  
}


# Setup Functions ---------------------------------------------------------

# # A lot of these values are guesses.  Need to test.
# # Row height and line height were defined independently in case
# # they are different.  Right now, appear to be the same.

#' @description Setup page for content
#' @details  Calculates available space for content and prepares text lines
#' for page template.
#' @noRd
page_setup_html <- function(rs) {
  
  debug <- FALSE
  
  if (is.null(rs$font_size))
    rs$font_size <- 10
  
  if (is.null(rs$font))
    rs$font <- "Courier"
  
  if (rs$font == "fixed")
    rs$font <- "Courier"
  
  if (rs$font_size == 8) {
    
    if (tolower(rs$font) == "times")
      rh <- 0.1178  
    else
      rh <- 0.127451  
        
    gtr <- .1
    cw <- .1    # na
    
  } else if (rs$font_size == 9) {
    
    if (tolower(rs$font) == "times")
      rh <- 0.158
    else 
      rh <- 0.148 
    cw <- .11  # na
    gtr <- .1
    
  } else if (rs$font_size == 10) {
    
    if (tolower(rs$font) == "times")
      rh <- 0.17  
    else 
      rh <- 0.1585366
    
    gtr <- .11
    cw <- .11   # na

  } else if (rs$font_size == 11) {
    
    if (tolower(rs$font) == "times")
      rh <- 0.18
    else 
      rh <- 0.168 # na
    gtr <- .1
    cw <- .11  # na
    
  } else if (rs$font_size == 12) {
    
    # inches 
    rh <- 0.1911765  
    gtr <- 0.11
    cw <- .12  #na
  }
  
  rs$border_height <- 1/72
  
  if (rs$units == "cm") {
    rh <- ccm(rh)
    cw <- ccm(cw)
    rs$border_height <- ccm(rs$border_height)
    gtr <- ccm(gtr)
  }
  
  rs$row_height <- rh
  rs$line_height <- rh
  rs$char_width <- cw
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  rs$line_size <- rs$content_size[["width"]]
  

  rs$gutter_width <- gtr
  if (rs$units == "cm")
    rs$gutter_width <- ccm(rs$gutter_width)
  
  rs$page_break_html <- paste0("<hr class=\"noprint\"><div style=\"page-break-before: always;",
                               "height:", rs$margin_top, 
                               units_html(rs$units), ";", "\"></div>")

  if (is.null(rs$user_line_count)) {
    rs$line_count <- round(rs$content_size[[1]] / rh) 
  } else
    rs$line_count <- rs$user_line_count

  if (debug) {
    print(paste("Font Size:", rs$font_size))
    print(paste("Content Height:", rs$content_size[[1]]))
    print(paste("Content Width:", rs$content_size[[2]]))
    print(paste("Line Count:", rs$line_count))
    print(paste("Line Height:", rs$line_height))
    print(paste("Gutter Width:", rs$gutter_width))
    print(paste("Char Width:", rs$char_width))
  }
  # 
  # Get page template
  pt <- page_template_html(rs)
  rs$page_template <- pt


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

