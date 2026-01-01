

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
  ls <- rs$content
  
  
  # Get content and break it into pages
  # Needs to return a list of pages so preview can work
  # Page numbers need to be included
  bdy <- paginate_content_docx(rs, ls)
  
  # Get column widths
  rs$column_widths <- bdy[["widths"]]
  
  # Deal with preview
  # if (!is.null(rs$preview)) {
  #   if (rs$preview < length(bdy[[1]]$pages))
  #     bdy[[1]]$pages <- bdy[[1]]$pages[seq(1, rs$preview)]
  # }

  
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
    '<w:pgSz w:w="', pg_w * conv, '" w:h="', pg_h * conv, '" w:orient="', 
    rs$orientation, '"/>',
    '<w:pgMar w:top="', rs$margin_top * conv, '" w:right="', rs$margin_right * conv, '" ', 
    'w:bottom="', rs$margin_bottom * conv, '" w:left="', rs$margin_left * conv, '" ',
    'w:header="', rs$margin_top * conv, '" w:footer="', rs$margin_bottom * conv, 
    '" w:gutter="0"/>',
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
  imgCnt <- 0
  imgPaths <- c()
  
  hrf <- has_bottom_footnotes(rs)
  
  
  # Loop through content objects
  for (i in seq_along(ls)) {
    
    pgs <- list()  # list of vectors with page rtf lines
    lns <- c()
    imgs <- list()
    
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
      
      res <- create_table_pages_docx(rs, cntnt, last_page_lines)
      
      # Collect multiple pages and line counts
      for (j in seq_len(length(res$page_list))) {
        pgs[[length(pgs) + 1]] <- res$page_list[[j]]$docx
        lns[[length(lns) + 1]] <- res$page_list[[j]]$lines
      }
      
      # Retrieve table widths.  These are useful for debugging.
      # Assigned to returning report object.
      table_widths[[length(table_widths) + 1]] <- res$widths
      
    } else if (any(class(obj) == "text_spec")) {
      
      res <- create_text_pages_docx(rs, cntnt, last_page_lines, cbr)
      for (j in seq_len(length(res$docx))) {
        pgs[[length(pgs) + 1]] <- res$docx[[j]]
        lns[[length(lns) + 1]] <- res$lines[[j]]
        
      }
      
    } else if (any(class(obj) == "plot_spec")) {
      
      res <- create_plot_pages_docx(rs, cntnt, last_page_lines, tempdir(), imgCnt)
      for (j in seq_len(length(res$docx))) {
        pgs[[length(pgs) + 1]] <- res$docx[[j]]
        lns[[length(lns) + 1]] <- res$lines[[j]]
        imgs[[length(imgs) + 1]] <- res$images[[j]]
        imgCnt <- imgCnt + 1
        imgPaths[[length(imgPaths) + 1]] <- res$images[[j]]
      }
    } else {
      
      stop(paste("Invalid content: ", class(obj))) 
    }
    
    # Store pages and lines with content objects
    # The content settings will be used when writing content
    ls[[i]]$pages <- pgs
    ls[[i]]$lines <- lns
    ls[[i]]$images <- imgs
    
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

          boff <- round(last_page_lines * rs$border_height / rs$row_height)
          #boff <- 1
        }

        blnks <- c()
        bl <- rs$body_line_count - last_page_lines - boff
        if (bl > 0)
          blnks <- rep(rs$blank_row_below, bl)

        last_page <- append(last_page, blnks)
        last_page_lines <- 0

      } 
    }
    
    if (cntnt$page_break == TRUE | last_page_lines >= rs$body_line_count)
      last_page_lines <- 0

    ls[[i]]$pages[[length(pgs)]] <- last_page
    
  }
  
  
  # Can return something else if needed here
  ret <- list(widths = table_widths, pages = ls, imageCount = imgCnt, 
              imagePaths = imgPaths)
  
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
  imgCnt <- 0
  
  
  
  # Create new document in temp location
  tf <- create_new_docx(rs$font, rs$font_size, body$imageCount, body$imagePaths)
  
  # Write out header
  create_header(tf, rs$page_template$page_header$docx)
  
  # Write out footer
  create_footer(tf, rs$page_template$page_footer$docx)
  
  fp <- file.path(tf, "word/document.xml")
  
  f <- file(fp, open="a", encoding = "native.enc")
  

  writeLines(hdr, con = f, useBytes = TRUE)
  
  
 # writeLines(body, con = f, useBytes = TRUE)
  
  for (cont in body$pages) {
    
    # Copy images to document folder
    if (length(cont$images) > 0) {
      for (im in cont$images) { 
        imgCnt <- imgCnt + 1
        ext <- tools::file_ext(im)
        if (ext == "jpg") {
          ext <- "jpeg"
        }
        ifp <- file.path(tf, paste0("word/media/image", imgCnt, ".", ext))
        file.copy(im, ifp)
      
      }
    }


    # Increment counter
    counter <- counter + 1
    page <- 0

    # Set last_object flag
    if (counter == length(body$pages))
      last_object <- TRUE
    else
      last_object <- FALSE

    # ta <- "align=\"left\" "
    # if (cont$align == "right")
    #   ta <- "align=\"right\" "
    # else if (cont$align %in% c("center", "centre"))
    #   ta <- "align=\"center\" "


    for (pg in cont$pages) {

      page <- page + 1

      if (page == length(cont$pages))
        last_page <- TRUE
      else
        last_page <- FALSE


      #print(page_open)
      if (page_open == FALSE) {

        # if (!is.null(rs$page_template$page_header) &
        #     !is.null(rs$page_template$page_header$html))
        #   writeLines(update_page(rs$page_template$page_header$html,  rs$pages),
        #              con = f, useBytes = TRUE)

        # Write content div to keep page together
        # writeLines(paste0("<div ", ta, ">"), con = f, useBytes = TRUE)


        if (!is.null(rs$title_hdr) & !is.null(pt$title_hdr$docx))
          writeLines(update_page(pt$title_hdr$docx,  rs$pages), con = f,
                     useBytes = TRUE)

        if (!is.null(rs$titles) & !is.null(pt$titles$docx))
          writeLines(pt$titles$docx, con = f, useBytes = TRUE)

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

        if (!is.null(rs$footnotes) & !is.null(pt$footnotes$docx))
          writeLines(update_page(pt$footnotes$docx,  rs$pages),
                     con = f, useBytes = TRUE)

        # Content div
        # writeLines("</div>", con = f, useBytes = TRUE)

        # if (!is.null(rs$page_template$page_footer) &
        #     !is.null(rs$page_template$page_footer$html))
        #   writeLines(update_page(rs$page_template$page_footer$html, rs$pages),
        #              con = f, useBytes = TRUE)


        # Add form feed character for text page break
        if (last_object == FALSE | last_page == FALSE) {

          if (is.null(rs$pages))
            rs$pages <- 1
          else
            rs$pages <- rs$pages + 1

          writeLines(rs$page_break_docx, con = f, useBytes = TRUE)

        }
      }

      if (last_object == TRUE & last_page == TRUE) {

        rs$pages <- rs$pages + 1

      }
    }

  }
  
  # Close document body
  writeLines("</w:body>\n</w:document>", con = f, useBytes = TRUE)
  
  close(f)
  
  
  # Kill existing file
  if (file.exists(rs$modified_path))
    file.remove(rs$modified_path)
  
  
  # Copy from temp path to perm path and zip
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
    
    gtr <- .12
    cw <- .1    # na
    radj <- 56
    
  } else if (rs$font_size == 9) {
    
    gtr <- .13
    cw <- .1  # na
    radj <- 36

    
  } else if (rs$font_size == 10) {
    
    if (tolower(rs$font) == "courier")
      gtr <- .14
    else 
      gtr <- .15
    
    cw <- .11   # na
    radj <- 8

  } else if (rs$font_size == 11) {
    
    if (tolower(rs$font) == "courier")
      gtr <- .15
    else 
      gtr <- .16
    
    cw <- .11  # na
    radj <- -16
    
  } else if (rs$font_size == 12) {
    
    if (tolower(rs$font) == "courier")
      gtr <- 0.16
    else
      gtr <- 0.16
    
    cw <- .12  #na
    radj <- -42
  }
  
  rh <- get_rh(rs$font, rs$font_size)
  
  rs$border_height <- 1/72/2
  
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
  
  # A zero height paragraph to break between tables.
  # Otherwise, Word will treat as one table and the column
  # widths will be messed up.
  rs$table_break <- '<w:p><w:pPr>
              				<w:spacing w:after="0" w:line="0" w:lineRule="none"/>
              				<w:rPr>
              					<w:sz w:val="0"/>
              				</w:rPr>
              			</w:pPr></w:p>\n'
  
  rs$blank_row <- paste0('<w:p><w:pPr>
              				<w:spacing w:after="8" w:line="', round(rh * conv) + radj,
              				'" w:lineRule="auto"/>
              				<w:contextualSpacing/>
              				<w:rPr>
              					<w:sz w:val="', rs$font_size * 2, '"/>
              				</w:rPr>
              			</w:pPr></w:p>\n')
  
  # This is for adding extra blanks after the table, left more buffer to prevent from
  # unexpected page break
  rs$blank_row_below <- paste0('<w:p><w:pPr>
              				<w:spacing w:after="8" w:line="', round(rh * conv) + radj - 3,
                         '" w:lineRule="auto"/>
              				<w:contextualSpacing/>
              				<w:rPr>
              					<w:sz w:val="', rs$font_size * 2, '"/>
              				</w:rPr>
              			</w:pPr></w:p>\n')
  
  rs$cell_margin <- paste0('<w:tblCellMar>
                           <w:left w:w="32" w:type="dxa"/>
                           <w:right w:w="32" w:type="dxa"/>
                           </w:tblCellMar>')
  
  # The starting point for relationship IDs.
  # This is used when adding images to the document.
  # See plot_spec.
  rs$relIndex <- 9
  
  rs$twip_conversion <- conv
  
  rs$row_height <- rh
  rs$line_height <- rh
  rs$char_width <- cw
  rs$base_indent <- 130
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  rs$line_size <- rs$content_size[["width"]]
  

  rs$gutter_width <- gtr

  
  # rs$page_break_docx <- paste0('<w:p>
  #                     <w:pPr>
  #                     <w:pStyle w:val="Normal"/>
  #             				<w:spacing w:after="0" w:before="0" w:line="120" w:lineRule="auto"/>
  #             				<w:contextualSpacing/>
  #             				  <w:rPr>
  #             					<w:sz w:val="0"/>
  #             				  </w:rPr>
  #             				</w:pPr>
  #             				<w:r>
  #             				  <w:rPr>
  #             					<w:sz w:val="0"/>
  #             				  </w:rPr>
  #             				</w:r>
  #               			<w:r>
  #               				<w:br w:type="page"/>
  #               			</w:r>
  #               		</w:p>')
  
  
  # rs$page_break_docx <- paste0('<w:p><w:pPr><w:pStyle w:val="Normal"/>',
  #                              '<w:bidi w:val="0"/><w:jc w:val="left"/><w:rPr></w:rPr>',
  #                              '</w:pPr><w:r><w:rPr></w:rPr></w:r>', 
  #                              '<w:r><w:br w:type="page"/></w:r></w:p>')
  # 
  # 
  # rs$page_break_docx <- paste0('<w:p><w:pPr><w:pStyle w:val="Normal"/>',
  #    '<w:spacing w:after="0" w:before="0" w:line="120" w:lineRule="auto"/>',
  #    '<w:bidi w:val="0"/><w:jc w:val="left"/><w:rPr></w:rPr>',
  #    '</w:pPr><w:r><w:rPr><w:sz w:val="0"/></w:rPr></w:r>', 
  #    '<w:r><w:br w:type="page"/></w:r></w:p>')
  
  
  # rs$page_break_docx <- paste0('<w:p><w:pPr>',
  #    '<w:rPr><w:sz w:val="0"/></w:rPr>',
  #    '</w:pPr><w:r><w:rPr><w:sz w:val="0"/></w:rPr></w:r>', 
  #    '<w:r><w:br w:type="page"/></w:r></w:p>')
  
  rs$page_break_docx <- paste0('<w:p><w:pPr>',
     '<w:spacing w:after="0" w:before="0" w:line="0" w:lineRule="none"/>',
     '<w:pageBreakBefore/>',
     '<w:rPr><w:sz w:val="0"/></w:rPr>',
     '</w:pPr><w:r><w:rPr><w:sz w:val="0"/></w:rPr></w:r>', 
     '</w:p>')
  

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
  
  # DOCX would still have one blank row on top even no page headers
  rs$page_template_header_count <- max(c(1, rs$page_template_header_count))
  
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
  # - 1 adjustment needed for footer buffer
  
  rs$body_line_count <- rs$line_count - rs$page_template_row_count - 1
  if (debug)
    print(paste0("Body Line Count: ", rs$body_line_count))
  
  return(rs)
}

get_rh <- function(font, font_size) {
  
  rh <- 0
  
  if (font_size == 8) {
    
    if (tolower(font) == "times")
      rh <- 0.141  
    else if (tolower(font) == "arial")
      rh <- 0.141 
    else 
      rh <- 0.141
    
    
  } else if (font_size == 9) {
    
    if (tolower(font) == "times")
      rh <- 0.156
    else if (tolower(font) == "arial")
      rh <- 0.156 
    else 
      rh <- 0.155 
    
  } else if (font_size == 10) {
    
    if (tolower(font) == "times")
      rh <- 0.178  
    else if (tolower(font) == "arial")
      rh <- 0.178 #0.182  # 0.1585366
    else
      rh <- 0.175 #0.182  # 0.1585366
    
  } else if (font_size == 11) {
    
    if (tolower(font) == "times")
      rh <- 0.195
    else if (tolower(font) == "arial")
      rh <- 0.195 # 0.168
    else 
      rh <- 0.192 # 0.168
    
    
  } else if (font_size == 12) {
    
    # inches 
    if (tolower(font) == "times")
      rh <- 0.212  # 1911765 
    else if (tolower(font) == "arial")
      rh <- 0.212 #0.212  # 1911765 
    else 
      rh <- 0.212  # 1911765 
    
  } else if (font_size == 14) {
    
    # inches 
    if (tolower(font) == "times")
      rh <- 0.25 
    else if (tolower(font) == "arial")
      rh <- 0.25 
    else 
      rh <- 0.25 
    
  }
  
  return(rh)
}


get_rh2 <- function(font, font_size) {
  
  rh <- 0
  
  if (font_size == 8) {
    
    if (tolower(font) == "times")
      rh <- 0.144  
    else if (tolower(font) == "arial")
      rh <- 0.144 
    else 
      rh <- 0.144
    
    
  } else if (font_size == 9) {
    
    if (tolower(font) == "times")
      rh <- 0.166
    else if (tolower(font) == "arial")
      rh <- 0.166 
    else 
      rh <- 0.1625 
    
  } else if (font_size == 10) {
    
    if (tolower(font) == "times")
      rh <- 0.185  
    else if (tolower(font) == "arial")
      rh <- 0.185 #0.182  # 0.1585366
    else
      rh <- 0.185 #0.182  # 0.1585366
    
  } else if (font_size == 11) {
    
    if (tolower(font) == "times")
      rh <- 0.208
    else if (tolower(font) == "arial")
      rh <- 0.208 # 0.168
    else 
      rh <- 0.203 # 0.168

    
  } else if (font_size == 12) {
    
    # inches 
    if (tolower(font) == "times")
      rh <- 0.23  # 1911765 
    else if (tolower(font) == "arial")
      rh <- 0.23 #0.212  # 1911765 
    else 
      rh <- 0.23  # 1911765 

  } else if (font_size == 14) {
    
    # inches 
    if (tolower(font) == "times")
      rh <- 0.25 
    else if (tolower(font) == "arial")
      rh <- 0.25 
    else 
      rh <- 0.25 
    
  }
  
  return(rh)
}
