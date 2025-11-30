# The page template is everything except the content: page header/footer,
# titles, footnotes, etc.

# Page Template DOCX Functions ---------------------------------------------


#' Create a page template with header, titles, footnotes, and footer.
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_docx <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_html", "list"))
  
  pt$page_header <- get_page_header_docx(rs)
  pt$title_hdr <- get_title_header_docx(rs$title_hdr, rs$line_size, rs)
  pt$titles <- get_titles_docx(rs$titles, rs$line_size, rs)
  pt$footnotes <- c()
  if (!is.null(rs$footnotes)) {
    if (!is.null(rs$footnotes[[1]])) {
      if (rs$footnotes[[1]]$valign == "bottom")
        pt$footnotes <- get_footnotes_docx(rs$footnotes, rs$line_size, rs)
    }
    
  } 
  pt$page_footer <- get_page_footer_docx(rs)
  
  pt$lines <- sum(pt$page_header$lines, pt$page_footer$lines,
                  pt$title_hdr$lines, pt$titles$lines, pt$footnotes$lines)
  
  
  # Page by not here.  Messes up line counts.
  
  return(pt)
}

#' @import grDevices
#' @noRd
get_page_header_docx <- function(rs) {
  
  ret <- ""
  cnt <- 0
  
  hl <- rs$page_header_left
  hr <- rs$page_header_right
  maxh <- max(length(hl), length(hr))
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  
  # User controlled width of left column
  lwdth <- rs$page_header_width
  if (is.null(lwdth))
    lwdth <- rs$content_size[["width"]]/2
  
  # Calculate right column width
  rwdth <- rs$content_size[["width"]] - lwdth
  lpct <- round(5000 * lwdth / rs$content_size[["width"]])
  rpct <- round(5000 * rwdth / rs$content_size[["width"]])
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"

  if (maxh > 0) {
    
    ret <- paste0("<w:tbl> ",
                  "<w:tblPr>",
                  '<w:tblStyle w:val="TableGrid"/>',
                  '<w:tblW w:w="5000"',
                  ' w:type="pct"/>',
                  "</w:tblPr>",
                  "<w:tblGrid>",
                  '<w:gridCol w:w="', lpct, '" w:type="pct"/>',
                  '<w:gridCol w:w="', rpct, '" w:type="pct"/>',
                  "</w:tblGrid>", collapse = "")

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)


    for (i in seq(1, maxh)) {

      cret <- ""

      if (length(hl) >= i) {

        # Split strings if they exceed width
        tmp <- split_string_html(hl[[i]], lwdth, rs$units)
        
        cret <- paste0(cret, 
                      '<w:tc><w:tcPr><w:tcW w:w="', lpct, '" w:type="pct"/></w:tcPr>', 
                      get_page_numbers_docx(para(tmp$html)),
                           "</w:tc>\n")
        
        lcnt <- tmp$lines  

      } else {
        cret <- paste0(cret, 
                      '<w:tc><w:tcPr><w:tcW w:w="', lpct, '" w:type="pct"/></w:tcPr>', 
                      para(" "), "</w:tc>\n")
        lcnt <- 1
      }

      if (length(hr) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_html(hr[[i]], rwdth, rs$units)

        
        cret <- paste0(cret, 
                      '<w:tc><w:tcPr><w:tcW w:w="', rpct, '" w:type="pct"/></w:tcPr>', 
                      get_page_numbers_docx(para(tmp2$html, "right")), 
                      "</w:tc></w:tr>\n")
        
        rcnt <- tmp2$lines 

      } else {
        cret <- paste0(cret, 
                      '<w:tc><w:tcPr><w:tcW w:w="', rpct, '" w:type="pct"/></w:tcPr>', 
                      para(" ", "right"), "</w:tc></w:tr>\n")
        rcnt <- 1
      }

      if (lcnt > rcnt) {
        trht <- get_row_height(round(rs$row_height * lcnt * conv))
        cnt <- cnt + lcnt

      } else {
        trht <- get_row_height(round(rs$row_height * rcnt * conv))
        cnt <- cnt + rcnt
      }
      
      
      ret <- paste0(ret, '<w:tr>', trht, cret)
      
    }
    
    ret <- paste0(ret, "</w:tbl>")

    dev.off()
    
    if (rs$page_header_blank_row == "below") {
      ret <- paste0(ret, rs$blank_row)
      cnt <- cnt + 1
    }
    
  }
    
  if (!is.null(rs$header_titles)) {
    
    tret <- get_titles_docx(rs$header_titles, rs$content_size[["width"]], rs)
    ret <- paste0(ret, rs$table_break, tret$docx)
    cnt <- cnt + tret$lines
    
  }

  res <- list(docx = ret, lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_page_footer_docx <- function(rs) {
  
  ret <- ""
  cnt <- 0

  fl <- rs$page_footer_left
  fc <- rs$page_footer_center
  fr <- rs$page_footer_right
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  

  maxf <- max(length(fl), length(fc), length(fr))

  if (maxf > 0) {

    ret <- paste0("<w:tbl> ",
                  "<w:tblPr>",
                  '<w:tblStyle w:val="TableGrid"/>',
                  '<w:tblW w:w="5000"',
                  ' w:type="pct"/>',
                  "</w:tblPr>",
                  "<w:tblGrid>",
                  '<w:gridCol w:w="1667" w:type="pct"/>',
                  '<w:gridCol w:w="1666" w:type="pct"/>',
                  '<w:gridCol w:w="1667" w:type="pct"/>',
                  "</w:tblGrid>", collapse = "")

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    width <- rs$page_footer_width
    
    total_width <- sum(width, na.rm = T)
    if (total_width > rs$content_size[["width"]]) {
      
      stop(sprintf("Total width of page footer %s %s cannot be greater than content width %s %s.",
                   total_width,
                   rs$units,
                   rs$content_size[["width"]],
                   rs$units))
      
    } else {
      na_num <- sum(is.na(width))
      imputed_width <- (rs$content_size[["width"]] - total_width) / na_num
      
      left_width <- ifelse(is.na(width[1]), imputed_width, width[1])
      center_width <- ifelse(is.na(width[2]), imputed_width, width[2])
      right_width <- ifelse(is.na(width[3]), imputed_width, width[3])
      
      left_pct <- floor(5000 * left_width/rs$content_size[["width"]])
      center_pct <- floor(5000 * center_width/rs$content_size[["width"]])
      right_pct <- floor(5000 * right_width/rs$content_size[["width"]])
      
      # Make sure the total is 5000 exactly
      pct_lst <- c(left_pct, center_pct, right_pct)
      rest_pct <- 5000 - sum(pct_lst)
      
      if (rest_pct >= 1){
        for (i in 1:rest_pct) {
          pct_lst[i%%3] <- pct_lst[i%%3] + 1
        }
      }
    }

    for (i in seq(1, maxf)) {

      tret <- ""

      if (length(fl) >= i) {

        # Split strings if they exceed width
        tmp1 <- split_string_html(fl[[i]], left_width, rs$units)
        
        
        # tret <- paste0(tret, cell_pct(tmp1$html, "left", 1667))
        tret <- paste0(tret, cell_pct(tmp1$html, "left", left_pct))
        
        lcnt <- tmp1$lines
      } else {
        tret <- paste0(tret, cell_pct(" ", "left", left_pct))
        lcnt <- 1
      }

      if (length(fc) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_html(fc[[i]], center_width, rs$units)
        
        # tret <- paste0(tret,  cell_pct(tmp2$html, "center", 1666))
        tret <- paste0(tret,  cell_pct(tmp2$html, "center", center_pct))
        ccnt <- tmp2$lines
      } else {
        tret <- paste0(tret,  cell_pct(" ", "center", center_pct))
        ccnt <- 1
      }

      if (length(fr) >= i) {

        tmp3 <- split_string_html(fr[[i]], right_width, rs$units)
        
        # tret <- paste0(tret, cell_pct(tmp3$html, "right", 1667))
        tret <- paste0(tret, cell_pct(tmp3$html, "right", right_pct))
        
        rcnt <- tmp3$lines
      } else {
        tret <- paste0(tret,  cell_pct(" ", "right", right_pct))
        rcnt <- 1
      }

      cnt <- cnt + max(lcnt, ccnt, rcnt)
      
      
      trht <- get_row_height(round(rs$row_height * max(lcnt, ccnt, rcnt) * conv)) 
      
      
      ret <- paste0(ret, '<w:tr>', trht, tret)

    }
    dev.off()

    ret <- paste0(get_page_numbers_docx(ret), "</w:tr>\n")
    ret <- paste0(ret, "</w:tbl>\n")
  }
  
  if (!is.null(rs$footer_footnotes)) {
    
    tret <- get_footnotes_docx(rs$footer_footnotes, rs$content_size[["width"]], rs)
    ret <- paste0(tret$docx, rs$table_break, ret)
    cnt <- cnt + tret$lines
    
  }


  
  res <- list(docx = paste0(ret, collapse = ""),
              lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_titles_docx <- function(ttllst, content_width, rs, talgn = "center", 
                            colspan = 0, col_widths = NULL, 
                            content_brdrs = NULL) {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  cflag <- FALSE
  
  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      cols <- ttls$columns
      
      if (ttls$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttls$width == "content") {
        width <- content_width
        cflag <- TRUE
      } else if (is.numeric(ttls$width))
        width <- ttls$width

      w <- round(width * conv)
      
      # Calculate cell widths
      cw <- round(w / cols)
      cwidth <- width / cols
      
      cwdths <- rep(cwidth, cols)
      names(cwdths) <- paste0("col", seq_len(cols))
      
      # Get grid for title block
      grd <- get_col_grid(cwdths, conv)
      
      
      if (ttls$align %in% c("centre", "center"))
        algn <- "center"
      else if (ttls$align == "right")
        algn <- "right"
      else
        algn <- "left"
      
      brd <- ttls$borders
      # If content borders are on, align indent with content
      if (!is.null(content_brdrs)) {
        
        if (any(content_brdrs %in% c("all", "outside", "body", "left")))
          brd <- "outside"
        
      }

      # Get indent codes for alignment
      ta <- get_indent_docx(talgn, rs$line_size, w, 
                           rs$base_indent, brd, conv)

      
      alcnt <- 0
      blcnt <- 0
      
      # Open device context
      pdf(NULL)
      
      # Set point size (ps) for strwidth to calculate string width
      if (!is.null(ttls$font_size)) {
        ttlfs <- ttls$font_size
      } else {
        ttlfs <- rs$font_size
      }
      par(family = get_font_family(rs$font), ps = ttlfs)
      
      
      tb <- get_table_borders_docx(ttls$borders)
      
      ret[length(ret) + 1] <- paste0("<w:tbl>",
                                     "<w:tblPr>",
                                     '<w:tblStyle w:val="TableGrid"/>',
                                     rs$cell_margin,
                                     "<w:tblW w:w=\"", w, "\"/>", ta, tb, 
                                     "</w:tblPr>\n", grd)
      
      # Get spanning for blank rows
      spn <- paste0('<w:gridSpan w:val="', cols , '"/>')
      
      al <- ""
      if (any(ttls$blank_row %in% c("above", "both"))) {
        
        alcnt <- 1
        
        al <- paste0("<w:tr>", rht, 
                     "<w:tc><w:tcPr>", spn, '</w:tcPr><w:p><w:r><w:t>', 
                     "</w:t></w:r></w:p></w:tc></w:tr>\n")
        
        ret <- append(ret, al)
        
        cnt <- cnt + 1
      }
      
      
      bb <- ""
      if (any(ttls$borders %in% c("bottom", "outside"))) {
        
        bb <- get_cell_borders_docx(2, 1, 2, 1, ttls$borders)
        
      }
      
      bl <- ""
      if (any(ttls$blank_row %in% c("below", "both"))) {
        blcnt <- 1
        

        
        bl <- paste0("<w:tr>", rht, 
                     "<w:tc>'<w:tcPr>'", bb, spn, '</w:tcPr>',
                     "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
        
        bb <- ""  # needed?
        
        cnt <- cnt + 1
      }
      
      
      i <- 1
      while (i <= length(ttls$titles)) {
        
        mxlns <- 0
        
        # Calculate current row
        rwnum <- ceiling(i / cols)
        
        rw <- ""
        
        for (j in seq_len(cols)) {
          
          # Not all cells have titles
          if (i > length(ttls$titles))
            vl <- ""
          else 
            vl <- ttls$titles[[i]]
          
          # Deal with column alignments
          if (cols == 1) {
            calgn <- algn 
          } else if (cols == 2) {
            if (j == 1)
              calgn <- "left"
            else 
              calgn <- "right"
          } else if (cols == 3) {
            if (j == 1)
              calgn <- "left"
            else if (j == 2)
              calgn <- "center"
            else if (j == 3) 
              calgn <- "right"
          }

          # Split title strings if they exceed width
          tmp <- split_string_html(vl, cwidth, rs$units)
          
          # Track max lines for counting
          if (tmp$lines > mxlns)
            mxlns <- tmp$lines
          
          # Get paragraph 
          tstr <- para(tmp$html, calgn, ttlfs, ttls$bold)

          cwdth <- paste0('<w:tcW w:w="', cw,'"/>')
          
          # Append cell
          rw <- paste0(rw, "<w:tc><w:tcPr>", cwdth, "</w:tcPr>", tstr, "</w:tc>\n")
          
          
          i <- i + 1
        }
        
        # Row height has to be determined based on max number of lines in cell
        if (is.null(ttls$font_size)) {
          
          srht <- get_row_height(round(rs$row_height * mxlns * conv))
          
        } else {
          
          srht <- get_row_height(round(get_rh(rs$font, ttls$font_size) * mxlns * conv))
          
        }
        
        # Construct row
        ret <- append(ret, paste0("<w:tr>", srht, rw, "</w:tr>\n"))
        
        # Track lines
        cnt <- cnt + mxlns

      }
      
      if (bl != "")
        ret <- append(ret, bl)
      
      if (cflag) {
        ret[length(ret) + 1] <- paste0("</w:tbl>")
      } else {
        ret[length(ret) + 1] <- paste0("</w:tbl>", rs$table_break)
      }
      
      # A flag to indicate that this block has bottom borders.  
      # Used to eliminate border duplication on subsequent blocks.
      if ("bottom" %in% get_outer_borders(ttls$borders))
        border_flag <- TRUE
      
      dev.off()
      

    }
    
  }
  

  
  res <- list(docx = paste0(ret, collapse = ""), 
              lines = cnt,
              border_flag = border_flag)
  
  return(res)
}


#' @import grDevices
#' @noRd
get_titles_docx_back <- function(ttllst, content_width, rs, talgn = "center", 
                            colspan = 0, col_widths = NULL, 
                            content_brdrs = NULL) {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  cflag <- FALSE
  
  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      
      if (ttls$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttls$width == "content") {
        width <- content_width
        cflag <- TRUE
      } else if (is.numeric(ttls$width))
        width <- ttls$width
      
      w <- round(width * conv)
      
      # Get grid for title block
      grd <- get_col_grid(c(title=width), conv)
      
      
      if (ttls$align %in% c("centre", "center"))
        algn <- "center"
      else if (ttls$align == "right")
        algn <- "right"
      else
        algn <- "left"
      
      brd <- ttls$borders
      # If content borders are on, align indent with content
      if (!is.null(content_brdrs)) {
        
        if (any(content_brdrs %in% c("all", "outside", "body", "left")))
          brd <- "outside"
        
      }
      
      # Get indent codes for alignment
      ta <- get_indent_docx(talgn, rs$line_size, w, 
                            rs$base_indent, brd, conv)
      
      
      alcnt <- 0
      blcnt <- 0
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      
      tb <- get_table_borders_docx(ttls$borders)
      
      ret[length(ret) + 1] <- paste0("<w:tbl>",
                                     "<w:tblPr>",
                                     rs$cell_margin,
                                     "<w:tblW w:w=\"", w, "\"/>", ta, tb, 
                                     "</w:tblPr>\n", grd)
      
      for (i in seq_along(ttls$titles)) {
        
        
        
        al <- ""
        if (i == 1) {
          if (any(ttls$blank_row %in% c("above", "both"))) {
            
            alcnt <- 1
            
            
            al <- paste0("<w:tr>", rht, 
                         "<w:tc>", '<w:p><w:r><w:t>', 
                         "</w:t></w:r></w:p></w:tc></w:tr>\n")
            
            
            cnt <- cnt + 1
          }
        }
        
        bb <- ""
        
        
        bl <- ""
        if (i == length(ttls$titles)) {
          
          if (any(ttls$borders %in% c("bottom", "outside"))) {
            bb <- get_cell_borders_docx(2, 1, 2, 1, ttls$borders)
            
            
            bb <- paste0('<w:tcPr>', bb, '</w:tcPr>')
            
          }
          
          if (any(ttls$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            bl <- paste0("<w:tr>", rht, 
                         "<w:tc>", bb,
                         "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            
            bb <- ""
            
            cnt <- cnt + 1
          }
          
        }
        
        
        # Split title strings if they exceed width
        tmp <- split_string_html(ttls$titles[[i]], width, rs$units)
        
        
        tstr <- para(tmp$html, algn, ttls$font_size, ttls$bold)
        
        
        
        # Concatenate title string
        if (al != "")
          ret <- append(ret, al)
        
        if (is.null(ttls$font_size)) {
          
          srht <- get_row_height(round(rs$row_height * tmp$lines * conv))
          
          ret <- append(ret, paste0("<w:tr>", srht, "<w:tc>", bb, tstr, 
                                    "</w:tc></w:tr>\n"))
        } else {
          
          srht <- get_row_height(round(get_rh(rs$font, ttls$font_size) * tmp$lines * conv))
          
          ret <- append(ret, paste0("<w:tr>", srht, "<w:tc>", bb, tstr, 
                                    "</w:tc></w:tr>\n"))
        }
        
        if (bl != "")
          ret <- append(ret, bl)
        
        cnt <- cnt + tmp$lines
        
        # A flag to indicate that this block has bottom borders.  
        # Used to eliminate border duplication on subsequent blocks.
        if ("bottom" %in% get_outer_borders(ttls$borders))
          border_flag <- TRUE
      }
      
      
      if (cflag) {
        ret[length(ret) + 1] <- paste0("</w:tbl>")
      } else {
        ret[length(ret) + 1] <- paste0("</w:tbl>", rs$table_break)
      }
      
      dev.off()
      
      
    }
    
  }
  
  
  
  res <- list(docx = paste0(ret, collapse = ""), 
              lines = cnt,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_footnotes_docx <- function(ftnlst, content_width, rs, talgn = "center", 
                               ex_brdr = FALSE, colspan = 0, col_widths = NULL,
                               content_brdrs = NULL) {
  
  ret <- c()
  cnt <- 0
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))

  u <- rs$units
  if (rs$units == "inches")
    u <- "in"
  
  
  if (length(ftnlst) > 0) {

    for (ftnts in ftnlst) {
      
      cols <- ftnts$columns

      if (ftnts$width == "page")
        width <- rs$content_size[["width"]]
      else if (ftnts$width == "content")
        width <- content_width
      else if (is.numeric(ftnts$width))
        width <- ftnts$width

      w <- round(width * conv) + 2
      
      # Calculate cell widths
      cw <- round(w / cols)
      cwidth <- width / cols
      
      cwdths <- rep(cwidth, cols)
      names(cwdths) <- paste0("col", seq_len(cols))
  
      # Get grid for footnote block
      # grd <- get_col_grid(c(all=w), 1)
      grd <- get_col_grid(cwdths, conv)
      
      if (ftnts$align %in% c("centre", "center"))
        algn <- "center"
      else if (ftnts$align == "right")
        algn <- "right"
      else
        algn <- "left"
      
      brd <- ftnts$borders
      # If content borders are on, align indent with content
      if (!is.null(content_brdrs)) {
        
        if (any(content_brdrs %in% c("all", "outside", "body", "left")))
          brd <- "outside"
        
      }
      
      # Get indent codes for alignment
      ta <- get_indent_docx(talgn, rs$line_size, w, 
                           rs$base_indent, brd, conv)

      alcnt <- 0
      blcnt <- 0


      pdf(NULL)
      
      # Set point size (ps) for strwidth to calculate string width
      if (!is.null(ftnts$font_size)) {
        ftntfs <- ftnts$font_size
      } else {
        ftntfs <- rs$font_size
      }
      par(family = get_font_family(rs$font), ps = ftntfs)
      
      tb <- get_table_borders_docx(ftnts$borders)
    
      
      ret[length(ret) + 1] <- paste0("<w:tbl>",
                                     "<w:tblPr>",
                                     rs$cell_margin,
                                     "<w:tblW w:w=\"", w, "\"/>", ta, tb,
                                     "</w:tblPr>", grd)
    
      
      # Get spanning for blank rows
      spn <- paste0('<w:gridSpan w:val="', cols , '"/>')
      
      al <- ""
      if (any(ftnts$blank_row %in% c("above", "both"))) {
        
        alcnt <- 1
        
        
        # al <- paste0("<w:tr>", rht, 
        #              "<w:tc>", bb, "<w:p><w:r><w:t>", 
        #              "</w:t></w:r></w:p></w:tc></w:tr>\n")
        al <- paste0("<w:tr>", rht, 
                     "<w:tc><w:tcPr>", spn, '</w:tcPr><w:p><w:r><w:t>', 
                     "</w:t></w:r></w:p></w:tc></w:tr>\n")

        ret <- append(ret, al)
        
        cnt <- cnt + 1
        bb <- ""
        
      }
      
      bb <- ""
      if (any(ftnts$borders %in% c("bottom", "outside"))) {
        bb <- get_cell_borders_docx(2, 1, 2, 1, ftnts$borders)
        
        
        bb <- paste0('<w:tcPr>', bb, '</w:tcPr>')
        
      }
      
      bl <- ""
      if (any(ftnts$blank_row %in% c("below", "both"))) {
        blcnt <- 1
        
        
        # bl <- paste0("<w:tr>", rht, 
        #              "<w:tc>", "<w:p><w:r><w:t>", 
        #              "</w:t></w:r></w:p></w:tc></w:tr>\n")
        bl <- paste0("<w:tr>", rht, 
                     "<w:tc>'<w:tcPr>'", bb, spn, '</w:tcPr>',
                     "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
        
        bb <- "" 
        
        cnt <- cnt + 1
      }

      i <- 1
      while (i <= length(ftnts$footnotes)) {
        
        mxlns <- 0
        
        # Calculate current row
        rwnum <- ceiling(i / cols)
        
        rw <- ""
        

        for (j in seq_len(cols)) {
          
          # Not all cells have titles
          if (i > length(ftnts$footnotes))
            vl <- ""
          else 
            vl <- ftnts$footnotes[[i]]
          
          # Deal with column alignments
          if (cols == 1) {
            calgn <- algn 
          } else if (cols == 2) {
            if (j == 1)
              calgn <- "left"
            else 
              calgn <- "right"
          } else if (cols == 3) {
            if (j == 1)
              calgn <- "left"
            else if (j == 2)
              calgn <- "center"
            else if (j == 3) 
              calgn <- "right"
          }
  
          # Split footnote strings if they exceed width
          tmp <- split_string_html(vl, cwidth, rs$units)
          
          # Track max lines for counting
          if (tmp$lines > mxlns)
            mxlns <- tmp$lines
          
          # Get paragraph 
          tstr <- para(tmp$html, calgn, ftntfs, italics = ftnts$italics)
          
          cwdth <- paste0('<w:tcW w:w="', cw,'"/>')
          
          # Deal with bottom borders for when there is no blank row
          cb <- ""
          if (!any(ftnts$blank_row %in% c("below", "both"))) {
            if (any(ftnts$borders %in% c("bottom", "outside"))) {
              cb <- get_cell_borders_docx(2, 1, 2, 1, ftnts$borders)
              
              
              cb <- paste0('<w:tcPr>', cb, '</w:tcPr>')
              
            }
          }
          
          # Append cell
          rw <- paste0(rw, "<w:tc><w:tcPr>", cwdth, cb, "</w:tcPr>", tstr, "</w:tc>\n")
          
          
          i <- i + 1
          

        }
        
        # Row height has to be determined based on max number of lines in cell
        if (is.null(ftnts$font_size)) {
          
          srht <- get_row_height(round(rs$row_height * mxlns * conv))
          
        } else {
          
          srht <- get_row_height(round(get_rh(rs$font, ftnts$font_size) * mxlns * conv))
          
        }
        
        # Need to change height depending on max lines
        # srht <- get_row_height(round(rs$row_height * mxlns * conv))
        
        # Construct row
        ret <- append(ret, paste0("<w:tr>", srht, rw, "</w:tr>\n"))
        
        # Track lines
        cnt <- cnt + mxlns
        
        
      }
      

      

      
      # ret <- append(ret, paste0("<w:tr>", trht, 
      #                           "<w:tc>", bb, para(tmp$html, algn, 
      #                                              italics = ftnts$italics), 
      #                           "</w:tc></w:tr>\n"))
      
      
      if (bl != "")
        ret <- append(ret, bl)
      
      # Close table
      ret[length(ret) + 1] <- "</w:tbl>\n"
      
      dev.off()

    }
    

    
    ret <- get_page_numbers_docx(ret)

  }

  
  res <- list(docx = paste0(paste0(ret,  collapse = ""), rs$table_break),
              lines = cnt)
  
  return(res)
}



#' @import grDevices
#' @noRd
get_footnotes_docx_back <- function(ftnlst, content_width, rs, talgn = "center", 
                               ex_brdr = FALSE, colspan = 0, col_widths = NULL,
                               content_brdrs = NULL) {
  
  ret <- c()
  cnt <- 0
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"
  
  
  if (length(ftnlst) > 0) {
    
    for (ftnts in ftnlst) {
      
      if (ftnts$width == "page")
        width <- rs$content_size[["width"]]
      else if (ftnts$width == "content")
        width <- content_width
      else if (is.numeric(ftnts$width))
        width <- ftnts$width
      
      w <- round(width * conv) + 2
      
      
      grd <- get_col_grid(c(all=w), 1)
      
      
      if (ftnts$align %in% c("centre", "center"))
        algn <- "center"
      else if (ftnts$align == "right")
        algn <- "right"
      else
        algn <- "left"
      
      brd <- ftnts$borders
      # If content borders are on, align indent with content
      if (!is.null(content_brdrs)) {
        
        if (any(content_brdrs %in% c("all", "outside", "body", "left")))
          brd <- "outside"
        
      }
      
      # Get indent codes for alignment
      ta <- get_indent_docx(talgn, rs$line_size, w, 
                            rs$base_indent, brd, conv)
      
      alcnt <- 0
      blcnt <- 0
      
      
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      tb <- get_table_borders_docx(ftnts$borders)
      
      
      ret[length(ret) + 1] <- paste0("<w:tbl>",
                                     "<w:tblPr>",
                                     rs$cell_margin,
                                     "<w:tblW w:w=\"", w, "\"/>", ta, tb,
                                     "</w:tblPr>", grd)
      
      for (i in seq_along(ftnts$footnotes)) {
        
        
        al <- ""
        bb <- ""
        if (i == 1) {
          
          if (any(ftnts$borders %in% c("top", "bottom", "outside"))) {
            bb <- get_cell_borders_docx(1, 1, 2, 1, ftnts$borders)
            
            
            bb <- paste0('<w:tcPr>', bb, '</w:tcPr>')
            
          }
          
          if (any(ftnts$blank_row %in% c("above", "both"))) {
            
            alcnt <- 1
            
            
            al <- paste0("<w:tr>", rht, 
                         "<w:tc>", bb, "<w:p><w:r><w:t>", 
                         "</w:t></w:r></w:p></w:tc></w:tr>\n")
            
            
            cnt <- cnt + 1
            bb <- ""
            
          }
        }
        
        bl <- ""
        if (i == length(ftnts$footnotes)) {
          if (any(ftnts$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            
            bl <- paste0("<w:tr>", rht, 
                         "<w:tc>", "<w:p><w:r><w:t>", 
                         "</w:t></w:r></w:p></w:tc></w:tr>\n")
            
            
            cnt <- cnt + 1
          }
          
        }
        
        
        # Split footnote strings if they exceed width
        tmp <- split_string_html(ftnts$footnotes[[i]], width, rs$units)
        
        if (al != "")
          ret <- append(ret, al)
        
        
        trht <- get_row_height(round(rs$row_height * tmp$lines * conv))
        
        ret <- append(ret, paste0("<w:tr>", trht, 
                                  "<w:tc>", bb, para(tmp$html, algn, 
                                                     italics = ftnts$italics), 
                                  "</w:tc></w:tr>\n"))
        
        
        
        if (bl != "")
          ret <- append(ret, bl)
        
        cnt <- cnt + tmp$lines
      }
      ret[length(ret) + 1] <- "</w:tbl>\n"
      dev.off()
      
      
    }
    
    ret <- get_page_numbers_docx(ret)
    
  }
  
  
  res <- list(docx = paste0(paste0(ret,  collapse = ""), rs$table_break),
              lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_title_header_docx <- function(thdrlst, content_width, rs, talgn = "center", 
                                  colspan = 0, col_widths = NULL) {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  cflag <- FALSE
  
  # ta <- "align=\"left\" "
  # if (talgn == "right")
  #   ta <- "align=\"right\" "
  # else if (talgn %in% c("center", "centre"))
  #   ta <- "align=\"center\" "
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"

  if (length(thdrlst) > 0) {

    for (ttlhdr in thdrlst) {

      if (ttlhdr$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttlhdr$width == "content") {
        width <- content_width
        cflag <- TRUE
      } else if (is.numeric(ttlhdr$width))
        width <- ttlhdr$width

      w <- round(width * conv) + 2
      wl <- round(w * .7)
      wr <- round(w * .3)
      

      grd <- get_col_grid(c(left= wl, right = wr), 1)


      mx <- max(length(ttlhdr$titles), length(ttlhdr$right))

      alcnt <- 0
      blcnt <- 0

      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      
      
      tb <- get_table_borders_docx(ttlhdr$borders)
      
      ta <- get_indent_docx(talgn, rs$line_size, w, 
                            rs$base_indent, ttlhdr$borders, conv)

      
      ret[length(ret) + 1] <- paste0("<w:tbl>", 
                                     "<w:tblPr>",
                                     '<w:tblStyle w:val="TableGrid"/>',
                                     rs$cell_margin,
                                     '<w:tblW w:w="', w, '"/>', ta, tb,
                                     "</w:tblPr>", grd, "\n")
      
                                     

      for(i in seq_len(mx)) {

        al <- ""
        if (i == 1) {
          if (any(ttlhdr$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            
            al <- paste0("<w:tr>", rht, 
                  "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc>",
                  "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            cnt <- cnt + 1

          }
        }
        
        bb <- ""

        bl <- ""
        if (i == mx) {
          
          if (any(ttlhdr$borders %in% c("bottom", "outside"))) {
            bb <- get_cell_borders_docx(2, 1, 2, 1, ttlhdr$borders)
            
            
            bb <- paste0('<w:tcPr>', bb, '</w:tcPr>')
            
          }
          
          if (any(ttlhdr$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            
            bl <- paste0("<w:tr>", rht, 
                  "<w:tc>", bb, "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc>", 
                  "<w:tc>", bb, "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            cnt <- cnt + 1
            
            bb <- ""
          }

        }

        if (length(ttlhdr$titles) >= i) {
          # Split strings if they exceed width
          tmp1 <- split_string_html(ttlhdr$titles[[i]], width * .7, rs$units)

          ttl <-  tmp1$html 
          tcnt <- tmp1$lines
        } else {
          ttl <- ""
          tcnt <- 1
        }

        if (length(ttlhdr$right) >= i) {
          tmp2 <- split_string_html(ttlhdr$right[[i]],
                                   width * .3, rs$units)

          
          hdr <- tmp2$html
          hcnt <- tmp2$lines
        } else {
          hdr <- " "
          hcnt <- 1
        }



        if (al != "")
          ret <- append(ret, al)
        
        if (bb == "") {
          ret <- append(ret, paste0("<w:tr>", rht, 
                                     cell_abs(ttl, "left", wl), 
                                     cell_abs(hdr, "right", wr), 
                                    "</w:tr>\n"))
        } else {
          
          ret <- append(ret, paste0("<w:tr>", rht, 
                                    cell_abs(ttl, "left", wl, borders = "bottom"), 
                                    cell_abs(hdr, "right", wr, borders = "bottom"), 
                                    "</w:tr>\n"))
        }
        
        
        if (bl != "")
          ret <- append(ret, bl)

        if (tcnt > hcnt)
          cnt <- cnt + tcnt
        else
          cnt <- cnt + hcnt
        
        if ("bottom" %in% get_outer_borders(ttlhdr$borders))
          border_flag <- TRUE
      }

      if (cflag) {
        ret[length(ret) + 1] <- paste0('</w:tbl>\n')
      } else {
        ret[length(ret) + 1] <- paste0('</w:tbl>\n', rs$table_break)

      }
      dev.off()
      
    }

  }
  

  
  res <- list(docx = paste0(ret, collapse = ""),
              lines = cnt,
              border_flag = border_flag)
  
  return(res)
}


#' Get page by text strings suitable for printing
#' @import stringi
#' @param titles Page by object
#' @param width The width to set the page by strings to
#' @return A vector of strings
#' @noRd
get_page_by_docx <- function(pgby, width, value, rs, talgn, 
                             ex_brdr = FALSE, pgby_cnt = NULL) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- get_pgby_value(value, pgby_cnt)
  
  ll <- width
  conv <- rs$twip_conversion
  ret <- c()
  cnt <- 0
  rht <- get_row_height(round(rs$row_height * conv))
  border_flag <- FALSE
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"


  if (!is.null(pgby)) {

    if (!any(class(pgby) == "page_by"))
      stop("pgby parameter value is not a page_by.")

    w <- round(width * conv)
    
    grd <- get_col_grid(c(pgby=w), 1)

    if (pgby$align %in% c("centre", "center"))
      algn <- "center"
    else if (pgby$align == "right")
      algn <- "right"
    else
      algn <- "left"
  
    
    tb <- get_table_borders_docx(pgby$borders)
    
    
    ta <- get_indent_docx(talgn, rs$line_size, w, 
                          rs$base_indent, pgby$borders, conv)
    
    ret[length(ret) + 1] <- paste0("<w:tbl>",
                                   "<w:tblPr>", 
                                   rs$cell_margin,
                                   "<w:tblW w:w=\"", w, "\"/>", ta, tb,
                                   "</w:tblPr>", grd)
    
    trows <- 1
    brow <- 1
    if (pgby$blank_row %in% c("above", "both")) {
      trows <- trows + 1
      brow <- 2
    }
    if (pgby$blank_row %in% c("below", "both"))
      trows <- trows + 1

    if (pgby$blank_row %in% c("above", "both")) {
      
      cb <- ""
      if (any(pgby$borders %in% c("outside", "top"))) {
        cb <- get_cell_borders_docx(1, 1, 2, 1, "top")
        
        if (cb != "")
          cb <- paste0("<w:tcPr>", cb, "</w:tcPr>")
      }


      ret[length(ret) + 1] <- paste0("<w:tr>", rht, 
                   "<w:tc>", cb, 
                   "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
      
      cnt <- cnt + 1
    }

    
    vb <- c()
    if (any(pgby$borders %in% c("outside", "top", "all")) & 
        !pgby$blank_row %in% c("above", "both")) {
      vb <- "top"
    
    }
    
    if (any(pgby$borders %in% c("outside", "bottom", "all")) & 
        !pgby$blank_row %in% c("below", "both")) {
      vb <- append(vb, "bottom")
      
    }
    
    
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    # Account for multiple pgby lines
    tmp <- split_string_html(value, width, rs$units)
    
    dev.off()
    
    vl <- tmp$html
    cnt <- cnt + tmp$lines
    vrht <- get_row_height(round(rs$row_height * conv) * tmp$lines)
    
    ret <- append(ret, paste0("<w:tr>", vrht, 
                              cell_abs(paste0(pgby$label, vl), width = w, 
                                       borders = vb), 
                              "</w:tr>\n"))
    
    # cnt <- cnt + 1

    if (pgby$blank_row %in% c("below", "both")) {
      
      cb <- ""
      if (any(pgby$borders %in% c("outside", "bottom"))) {
        cb <- get_cell_borders_docx(2, 1, 2, 1, "bottom")
        
        if (cb != "")
          cb <- paste0("<w:tcPr>", cb, "</w:tcPr>")
      }
      
      
      ret[length(ret) + 1] <- paste0("<w:tr>", rht, 
                                     "<w:tc>", cb, 
                                     "<w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
      
      cnt <- cnt + 1
    }

    # if (pgby$blank_row %in% c("below", "both")) {
    # 
    #   
    #   ret[length(ret) + 1] <- paste0("<w:tr>", rht, 
    #                 "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
    #   cnt <- cnt + 1
    #   
    # 
    # }

    ret[length(ret) + 1] <- paste0("</w:tbl>")
    
    if ("bottom" %in% get_outer_borders(pgby$borders))
      border_flag <- TRUE

  }
  
  
  res <- list(docx = paste0(ret, collapse = ""), 
              lines = cnt,
              border_flag = border_flag)
  
  return(res)
}

# Utilities ---------------------------------------------------------------



# <w:tblBorders>
#   <w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>
#   <w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>
#   <w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>
#   <w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>
#   <w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>
#   <w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>
#   </w:tblBorders>
  
#' @description Return border codes for the whole table.  
#' @noRd
get_table_borders_docx <- function(brdrs, 
                                  flag = "", exclude = NULL) {
  
  t <- ""
  b <- ""
  l <- ""
  r <- ""
  ih <- ""
  iv <- ""
  
  
  if ("all" %in% brdrs) {
    t <- '<w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    b <- '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    l <- '<w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    r <- '<w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    ih <- '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    iv <- '<w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    
    
  } else {
    
    if ("inside" %in% brdrs) {
      
      ih <- '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
      iv <- '<w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
      
      
    }
    
    if ( any(brdrs %in% c("outside", "top")))
      t <- '<w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    
    if (any(brdrs %in% c("bottom", "outside")))
      b <- '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    
    if (any(brdrs %in% c("outside", "left")))
      l <- '<w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    
    if (any(brdrs %in% c("outside", "right")))
      r <- '<w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>'
    
  }
  
  
  
  if (!is.null(exclude)) {
    if (any(exclude == "top"))
      t <- ""
    if (any(exclude == "bottom"))
      b <- ""
    if (any(exclude == "left"))
      l <- ""
    if (any(exclude == "right"))
      r <- ""
  }
  
  ret <- paste0('<w:tblBorders>',
                t, b, l, r, ih, iv, 
                '</w:tblBorders>')
  
  return(ret)
  
}

#' @noRd
get_page_numbers_docx <- function(val, tpg = TRUE) {
  
  ret <- val
  
  ret <- gsub("[pg]", get_page_number_docx(), ret, fixed = TRUE)
  
  if (tpg)
    ret <- gsub("[tpg]", get_total_pages_docx(), ret, fixed = TRUE)
  
  return(ret)
}

#' @noRd
get_row_height <- function(hgt) {
  
  ret <- paste0('<w:trPr><w:trHeight w:hRule="exact" w:val="', hgt, 
                  '"/></w:trPr>')
  
  return(ret)
  
}

#' @noRd
get_page_number_docx <- function() {
  
  ret <- paste0('</w:t></w:r><w:r><w:fldChar w:fldCharType="begin"/></w:r>
     <w:r>
        <w:instrText xml:space="preserve"> PAGE   \\* MERGEFORMAT </w:instrText>
     </w:r>
     <w:r>
        <w:fldChar w:fldCharType="separate"/>
     </w:r>
     <w:r>
        <w:t>1</w:t>
     </w:r>
     <w:r>
        <w:fldChar w:fldCharType="end"/>
     </w:r><w:r><w:t xml:space="preserve">')
  
  return(ret)
}

#' @noRd
get_total_pages_docx <- function() {
  
  ret <- paste0('</w:t></w:r><w:r><w:fldChar w:fldCharType="begin"/></w:r>
     <w:r>
        <w:instrText xml:space="preserve"> NUMPAGES   \\* MERGEFORMAT </w:instrText>
     </w:r>
     <w:r>
        <w:fldChar w:fldCharType="separate"/>
     </w:r>
     <w:r>
        <w:t>1</w:t>
     </w:r>
     <w:r>
        <w:fldChar w:fldCharType="end"/>
     </w:r><w:r><w:t xml:space="preserve">')
  
  return(ret)
}

#' @noRd
get_indent_docx <- function(talgn, line_size, width, base_indent, borders, conv) {
  
  ret <- ""
  bi <- 0
  
  if (any(borders %in% c("outside", "all", "left"))) 
    bi <- base_indent
    
    if (talgn == "right")
      aw <- round(line_size * conv) - width + bi
    else if (talgn %in% c("center", "centre"))
      aw <- round(((line_size * conv) - width) / 2) + bi
    else 
      aw <- bi
    
  if (aw < 0)
    aw <- 0
  
  ret <- paste0('<w:tblInd w:w="', aw, '" w:type="dxa"/>')
  
  
  return(ret)
  
}


#' @noRd
get_col_grid <- function(widths, conv) {
  
  nms <- names(widths) 
  ret <- "<w:tblGrid>\n"
  
  
  # Get cell widths
  for (k in seq_along(widths)) {
    if (!is.control(nms[k])) {
      if (!is.na(widths[k])) {
        ret <- paste0(ret,  "<w:gridCol w:w=\"", 
                      round(widths[k] * conv), "\"/>\n")
      }
    }
  } 
  
  ret <- paste0(ret, "</w:tblGrid>\n")
  
  
  return(ret)
  
}
