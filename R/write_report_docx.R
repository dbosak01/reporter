

# Write Report DOCX -------------------------------------------------------



#' @title
#' Write an DOCX report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_docx <- function(rs) {
  
  orig_path <- rs$modified_path
  
  if (file.exists(orig_path)) {
    file.remove(orig_path)
  }

  
  # Establish content and body sizes
  rs <- page_setup_docx(rs)
  
  # Document header is mostly independent of content
  hdr <- get_docx_document(rs) 
  
  # Put content in a new variable
  #ls <- rs$content
  
  # Get content and break it into pages
  # Needs to return a list of pages so preview can work
  # Page numbers need to be included
  #bdy <- paginate_content_docx(rs, ls)
  
  # Get column widths
  #rs$column_widths <- bdy[["widths"]]
  
  # Deal with preview
  # if (!is.null(rs$preview)) {
  #   if (rs$preview < length(bdy[[1]]$pages))
  #     bdy[[1]]$pages <- bdy[[1]]$pages[seq(1, rs$preview)]
  # }
  
  
  bdy <- para("Forker")
  
  # Write content to file system
  # Later we can just return the stream
  rs <- write_content_docx(rs, hdr, bdy, rs$page_template)
  
  # Update page numbers for title headers
 # update_page_numbers_docx(orig_path, rs$pages)
  
  return(rs)
}



#' @description Returns header for DOCX document.  This is independent of content,
#' except for the page header and footer.
#' @noRd
get_docx_document <- function(rs) {
  
  # Set up vectors
  ret <- c()
  
  conv <- rs$twip_conversion
  
  fnt <- rs$font
  if (tolower(rs$font) == "times")
    fnt <- "Times New Roman"
  
  # Prepare header
  ret[length(ret) + 1] <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document
	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
	xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">'
	
	ret[length(ret) + 1] <- '<w:body>'
  

  # Assume landscape
  pg_h <- rs$page_size[1]
  pg_w <- rs$page_size[2]
  
  # Change to portrait
  if(rs$orientation == "portrait") {
    pg_w <- rs$page_size[1]
    pg_h <- rs$page_size[2]
  }
  
  ret[length(ret) + 1] <- paste0('<w:sectPr w:rsidR="00444C49" w:rsidRPr="00444C49">
    <w:headerReference w:type="default" r:id="rId6"/>
    <w:footerReference w:type="default" r:id="rId7"/>',
    '<w:pgSz w:w="', pg_w * conv, '" w:h="', pg_h * conv, '"/>',
    '<w:pgMar w:top="', rs$margin_top * conv, '" w:right="', rs$margin_right * conv, '" ', 
    'w:bottom="', rs$margin_bottom * conv, '" w:left="', rs$margin_left * conv, '" ',
    'w:header="720" w:footer="720" w:gutter="0"/>',
    '<w:cols w:space="720"/>
    <w:docGrid w:linePitch="360"/>
    </w:sectPr>')
  
  return(ret)
  
}



#' @noRd
paginate_content_docx <- function(rs, ls) {
  
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
write_content_docx <- function(rs, hdr, body, pt) {
  

  
  counter <- 0
  page <- 0
  last_object <- FALSE
  last_page <- FALSE
  page_open <- FALSE
  
  # Create new document in temp location
  tf <- create_new_docx(rs$font, rs$font_size)
  
  # Write out header
  create_header(tf, rs$page_template$page_header$docx)
  
  # Write out footer
  # create_footer(tf, rs$page_template$page_footer)
  
  fp <- file.path(tf, "word/document.xml")
  
  f <- file(fp, open="a", encoding = "native.enc")
  

  writeLines(hdr, con = f, useBytes = TRUE)
  
  
  writeLines(body, con = f, useBytes = TRUE)
  
  # for (cont in body$pages) {
  #   
  #   
  #   # Increment counter
  #   counter <- counter + 1
  #   page <- 0
  #   
  #   # Set last_object flag
  #   if (counter == length(body$pages))
  #     last_object <- TRUE
  #   else 
  #     last_object <- FALSE
  #   
  #   ta <- "align=\"left\" "
  #   if (cont$align == "right")
  #     ta <- "align=\"right\" "
  #   else if (cont$align %in% c("center", "centre"))
  #     ta <- "align=\"center\" "
  #   
  #   
  #   for (pg in cont$pages) {
  #     
  #     page <- page + 1
  #     
  #     if (page == length(cont$pages))
  #       last_page <- TRUE
  #     else
  #       last_page <- FALSE
  #     
  #     
  #     #print(page_open)
  #     if (page_open == FALSE) {
  #       
  #       if (!is.null(rs$page_template$page_header) & 
  #           !is.null(rs$page_template$page_header$html))
  #         writeLines(update_page(rs$page_template$page_header$html,  rs$pages), 
  #                    con = f, useBytes = TRUE)
  #       
  #       # Write content div to keep page together
  #       writeLines(paste0("<div ", ta, ">"), con = f, useBytes = TRUE)
  #       
  #       
  #       if (!is.null(rs$title_hdr) & !is.null(pt$title_hdr$html))
  #         writeLines(update_page(pt$title_hdr$html,  rs$pages), con = f, 
  #                    useBytes = TRUE)
  #       
  #       if (!is.null(rs$titles) & !is.null(pt$titles$html))
  #         writeLines(pt$titles$html, con = f, useBytes = TRUE)
  #       
  #     }
  #     
  #     if (!is.null(pg)) {
  #       
  #       writeLines(pg, con = f, useBytes = TRUE)
  #       
  #     }
  #     
  #     # Set page_open flag based on status of page_break and current objects
  #     if (last_object == FALSE & last_page == TRUE & cont$page_break == FALSE)
  #       page_open <- TRUE
  #     else 
  #       page_open <- FALSE
  #     
  #     if (page_open == FALSE) {
  #       
  #       if (!is.null(rs$footnotes) & !is.null(pt$footnotes$html))
  #         writeLines(update_page(pt$footnotes$html,  rs$pages), 
  #                    con = f, useBytes = TRUE)
  #       
  #       # Content div
  #       writeLines("</div>", con = f, useBytes = TRUE)
  #       
  #       if (!is.null(rs$page_template$page_footer) & 
  #           !is.null(rs$page_template$page_footer$html))
  #         writeLines(update_page(rs$page_template$page_footer$html, rs$pages), 
  #                    con = f, useBytes = TRUE)
  #       
  #       
  #       # Add form feed character for text page break
  #       if (last_object == FALSE | last_page == FALSE) {
  #         
  #         if (is.null(rs$pages))
  #           rs$pages <- 1
  #         else 
  #           rs$pages <- rs$pages + 1 
  #         
  #         writeLines(rs$page_break_html, con = f, useBytes = TRUE) 
  #         
  #       }
  #     }
  #     
  #     if (last_object == TRUE & last_page == TRUE) {
  #       
  #       rs$pages <- rs$pages + 1 
  #       
  #     }
  #   }
  #   
  # }
  
  writeLines("</w:body>\n</w:document>", con = f, useBytes = TRUE)
  
  close(f)
  
  
  # Kill existing file
  if (file.exists(rs$modified_path))
    file.remove(rs$modified_path)
  
  write_docx(tf, rs$modified_path)
  
  return(rs)
  
}

#' @description Could be consolidated with RTF
#' @noRd
update_page_numbers_docx <- function(path, tpg) {
  
  
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



#' @description Setup page for content
#' @details  Calculates available space for content and prepares text lines
#' for page template.
#' @noRd
page_setup_docx <- function(rs) {
  
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
  
  
  # Get conversion factor to twips
  if (rs$units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  rs$twip_conversion <- conv
  
  rs$row_height <- rh
  rs$line_height <- rh
  rs$char_width <- cw
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  rs$line_size <- rs$content_size[["width"]]
  

  rs$gutter_width <- gtr
  if (rs$units == "cm")
    rs$gutter_width <- ccm(rs$gutter_width)
  
  rs$page_break_docx <- paste0("<hr class=\"noprint\"><div style=\"page-break-before: always;",
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
  
  
  # Create temp directory structure for docx file
  #rs$temp_dir_docx <- create_new_docx()

  # Get page template
  pt <- page_template_docx(rs)
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
