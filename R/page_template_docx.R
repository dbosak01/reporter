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
  image_path <- c()
  
  if ((!is.null(rs$header_image_left) & is.null(rs$page_header_left)) |
      (!is.null(rs$header_image_center) & is.null(rs$page_header_center)) |
      (!is.null(rs$header_image_right) & is.null(rs$page_header_right))) {
    
    stop("`page_header` must be used when using `header_image`.")
  }
  
  # Make sure the length is 3, NA will be imputed later.
  width <- c(rs$page_header_width, rep(NA, 3 - length(rs$page_header_width)))
  
  # hl <- rs$page_header_left
  # hr <- rs$page_header_right
  # maxh <- max(length(hl), length(hr))
  
  image_left <- FALSE
  image_center <- FALSE
  image_right <- FALSE
  
  if (!is.null(rs$header_image_left)) {
    image_left <- TRUE
    hl <- rs$header_image_left
  } else{
    hl <- rs$page_header_left
  }
  
  if (!is.null(rs$header_image_right)) {
    image_right <- TRUE
    hr <- rs$header_image_right
  } else {
    hr <- rs$page_header_right
  }
  
  if (!is.null(rs$header_image_center)) {
    image_center <- TRUE
    hc <- rs$header_image_center
    
    # Default center cell is not displayed, open it when picture exists
    if (width[2] == 0) {
      width[2] <- NA
    }
  } else {
    hc <- rs$page_header_center
  }
  
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  
  hl_num <- ifelse(image_left, length(hl$image_path), length(hl))
  hc_num <- ifelse(image_center, length(hc$image_path), length(hc))
  hr_num <- ifelse(image_right, length(hr$image_path) , length(hr))
  
  maxh <- max(hl_num, hc_num, hr_num)
  
  # # User controlled width of left column
  # lwdth <- rs$page_header_width
  # if (is.null(lwdth))
  #   lwdth <- rs$content_size[["width"]]/2
  # 
  # # Calculate right column width
  # rwdth <- rs$content_size[["width"]] - lwdth
  # lpct <- round(5000 * lwdth / rs$content_size[["width"]])
  # rpct <- round(5000 * rwdth / rs$content_size[["width"]])
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"

  if (maxh > 0) {
    
    # ret <- paste0("<w:tbl> ",
    #               "<w:tblPr>",
    #               '<w:tblStyle w:val="TableGrid"/>',
    #               '<w:tblW w:w="5000"',
    #               ' w:type="pct"/>',
    #               "</w:tblPr>",
    #               "<w:tblGrid>",
    #               '<w:gridCol w:w="', lpct, '" w:type="pct"/>',
    #               '<w:gridCol w:w="', rpct, '" w:type="pct"/>',
    #               "</w:tblGrid>", collapse = "")
    
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
      add_pos <- c(1, 3 ,2)
      
      if (rest_pct >= 1){
        for (i in 1:rest_pct) {
          pct_lst[add_pos[i%%3]] <- pct_lst[i%%3] + 1
        }
      }
    }
    
    width <- c(left_width, center_width, right_width)
    
    if (sum(width > 0) == 3) {
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
    } else if (sum(width > 0) == 2) {
      ret <- paste0("<w:tbl> ",
                    "<w:tblPr>",
                    '<w:tblStyle w:val="TableGrid"/>',
                    '<w:tblW w:w="5000"',
                    ' w:type="pct"/>',
                    "</w:tblPr>",
                    "<w:tblGrid>",
                    '<w:gridCol w:w="2500" w:type="pct"/>',
                    '<w:gridCol w:w="2500" w:type="pct"/>',
                    "</w:tblGrid>", collapse = "")
    } else {
      ret <- paste0("<w:tbl> ",
                    "<w:tblPr>",
                    '<w:tblStyle w:val="TableGrid"/>',
                    '<w:tblW w:w="5000"',
                    ' w:type="pct"/>',
                    "</w:tblPr>",
                    "<w:tblGrid>",
                    '<w:gridCol w:w="5000" w:type="pct"/>',
                    "</w:tblGrid>", collapse = "")
    }

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)

    # index for images in footer
    rID <- 0
    iconv <- 914400  # 1 inch = 914,400 EMUs (English Metric Units)
    
    lcnt <- 0
    ccnt <- 0
    rcnt <- 0
    
    lheight <- 0
    cheight <- 0
    rheight <- 0
    max_height <- 0
    
    for (i in seq(1, maxh)) {

      cret <- ""

      if (left_width > 0) {
        if (hl_num >= i) {
          
          if (image_left == FALSE) {
            # Split strings if they exceed width
            tmp <- split_string_html(hl[[i]], left_width, rs$units)
            
            cret <- paste0(cret, 
                           '<w:tc><w:tcPr><w:tcW w:w="', left_pct, '" w:type="pct"/></w:tcPr>', 
                           get_page_numbers_docx(para(tmp$html)),
                           "</w:tc>\n")
            
            lcnt <- tmp$lines
          } else {
            # Calculate image height & width
            if (rs$units == "inches") {
              hgth <- (hl$height) * iconv 
              wdth <- (min(hl$width, left_width)) * iconv 
              
            } else {
              hgth <- (cin(hl$height)) * iconv 
              wdth <- (cin(min(hl$width, left_width))) * iconv
              
            }
            
            # Get image DOCX codes
            rID <- rID + 1
            image_path <- c(image_path, hl$image_path[i])
            
            img <- get_image_docx(rID, "left", hgth, wdth, type = "h")
            cret <- paste0(cret, "<w:tc>", img, "</w:tc>")
            
            lheight <- hl$height
          }
        } else {
          cret <- paste0(cret, 
                         '<w:tc><w:tcPr><w:tcW w:w="', left_pct, '" w:type="pct"/></w:tcPr>', 
                         para(" "), "</w:tc>\n")
          lcnt <- 1
        }
      }
      
      if (center_width > 0) {
        if (hc_num >= i) {
          
          if (image_center == FALSE) {
            # Split strings if they exceed width
            tmp2 <- split_string_html(hc[[i]], center_width, rs$units)
            
            cret <- paste0(cret, 
                           '<w:tc><w:tcPr><w:tcW w:w="', center_pct, '" w:type="pct"/></w:tcPr>', 
                           get_page_numbers_docx(para(tmp2$html)),
                           "</w:tc>\n")
            
            ccnt <- tmp2$lines
          } else {
            # Calculate image height & width
            if (rs$units == "inches") {
              hgth <- (hc$height) * iconv 
              wdth <- (min(hc$width, center_width)) * iconv 
              
            } else {
              hgth <- (cin(hc$height)) * iconv 
              wdth <- (cin(min(hc$width, center_width))) * iconv
              
            }
            
            # Get image DOCX codes
            rID <- rID + 1
            image_path <- c(image_path, hc$image_path[i])
            
            img <- get_image_docx(rID, "center", hgth, wdth, type = "h")
            cret <- paste0(cret, "<w:tc>", img, "</w:tc>")
            
            cheight <- hc$height
          }
        } else {
          cret <- paste0(cret, 
                         '<w:tc><w:tcPr><w:tcW w:w="', center_pct, '" w:type="pct"/></w:tcPr>', 
                         para(" ", "center"), "</w:tc>")
          ccnt <- 1
        }
      }

      if (right_width > 0) {
        if (hr_num >= i) {
          
          if (image_right == FALSE) {
            # Split strings if they exceed width
            tmp3 <- split_string_html(hr[[i]], right_width, rs$units)
            
            
            cret <- paste0(cret, 
                           '<w:tc><w:tcPr><w:tcW w:w="', right_pct, '" w:type="pct"/></w:tcPr>', 
                           get_page_numbers_docx(para(tmp3$html, "right")), 
                           "</w:tc>")
            
            rcnt <- tmp3$lines 
          } else {
            # Calculate image height & width
            if (rs$units == "inches") {
              hgth <- (hr$height) * iconv 
              wdth <- (min(hr$width, right_width)) * iconv 
              
            } else {
              hgth <- (cin(hr$height)) * iconv 
              wdth <- (cin(min(hr$width, right_width))) * iconv
              
            }
            
            # Get image DOCX codes
            rID <- rID + 1
            image_path <- c(image_path, hr$image_path[i])
            
            img <- get_image_docx(rID, "right", hgth, wdth, type = "h")
            cret <- paste0(cret, "<w:tc>", img, "</w:tc>")
            
            rheight <- hr$height
          }
        } else {
          cret <- paste0(cret, 
                         '<w:tc><w:tcPr><w:tcW w:w="', right_pct, '" w:type="pct"/></w:tcPr>', 
                         para(" ", "right"), "</w:tc>")
          rcnt <- 1
        } 
      }
      
      cret <- paste0(cret, "</w:tr>\n")

      # if (lcnt > rcnt) {
      #   trht <- get_row_height(round(rs$row_height * lcnt * conv))
      #   cnt <- cnt + lcnt
      # 
      # } else {
      #   trht <- get_row_height(round(rs$row_height * rcnt * conv))
      #   cnt <- cnt + rcnt
      # }
      
      cnt <- cnt + max(lcnt, ccnt, rcnt)
      max_height <- max_height + max(lheight, cheight, rheight)
      
      trht <- get_row_height(round(max(rs$row_height * max(lcnt, ccnt, rcnt), max(lheight, cheight, rheight)) * conv)) 
      
      
      ret <- paste0(ret, '<w:tr>', trht, cret)
      
    }
    
    ret <- paste0(ret, "</w:tbl>")

    dev.off()
    
    if (max_height > 0) {
      cnt <- max(cnt, ceiling(max_height/rs$line_height))
    }
    
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

  res <- list(docx = ret, lines = cnt, image_path = image_path)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_page_footer_docx <- function(rs) {
  
  ret <- ""
  cnt <- 0
  image_path <- c()
  
  # fl <- rs$page_footer_left
  # fc <- rs$page_footer_center
  # fr <- rs$page_footer_right
  if ((!is.null(rs$footer_image_left) & is.null(rs$page_footer_left)) |
      (!is.null(rs$footer_image_right) & is.null(rs$page_footer_right)) |
      (!is.null(rs$footer_image_center) & is.null(rs$page_footer_center))) {
    
    stop("`page_footer` must be used when using `footer_image`.")
  }
  
  image_left <- FALSE
  image_center <- FALSE
  image_right <- FALSE
  
  if (!is.null(rs$footer_image_left)) {
    fl <- rs$footer_image_left
    image_left <- TRUE
  } else {
    fl <- rs$page_footer_left
  }
  
  if (!is.null(rs$footer_image_center)) {
    fc <- rs$footer_image_center
    image_center <- TRUE
  } else {
    fc <- rs$page_footer_center
  }
  
  if (!is.null(rs$footer_image_right)) {
    fr <- rs$footer_image_right
    image_right <- TRUE
  } else {
    fr <- rs$page_footer_right
  }
  
  conv <- rs$twip_conversion
  rht <- get_row_height(round(rs$row_height * conv))
  

  # maxf <- max(length(fl), length(fc), length(fr))
  fl_num <- ifelse(image_left, length(fl$image_path), length(fl))
  fc_num <- ifelse(image_center, length(fc$image_path), length(fc))
  fr_num <- ifelse(image_right, length(fr$image_path) , length(fr))
  
  maxf <- max(fl_num, fc_num, fr_num)
  

  if (maxf > 0) {

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    width <- rs$page_footer_width
    
    # Make sure the length is 3
    width <- c(width, rep(NA, 3 - length(width)))
    
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
      add_pos <- c(1, 3 ,2)
      
      if (rest_pct >= 1){
        for (i in 1:rest_pct) {
          pct_lst[add_pos[i%%3]] <- pct_lst[i%%3] + 1
        }
      }
    }
    
    width <- c(left_width, center_width, right_width)

    if (sum(width > 0) == 3) {
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
    } else if (sum(width > 0) == 2) {
      ret <- paste0("<w:tbl> ",
                    "<w:tblPr>",
                    '<w:tblStyle w:val="TableGrid"/>',
                    '<w:tblW w:w="5000"',
                    ' w:type="pct"/>',
                    "</w:tblPr>",
                    "<w:tblGrid>",
                    '<w:gridCol w:w="2500" w:type="pct"/>',
                    '<w:gridCol w:w="2500" w:type="pct"/>',
                    "</w:tblGrid>", collapse = "")
    } else {
      ret <- paste0("<w:tbl> ",
                    "<w:tblPr>",
                    '<w:tblStyle w:val="TableGrid"/>',
                    '<w:tblW w:w="5000"',
                    ' w:type="pct"/>',
                    "</w:tblPr>",
                    "<w:tblGrid>",
                    '<w:gridCol w:w="5000" w:type="pct"/>',
                    "</w:tblGrid>", collapse = "")
    }

    # index for images in footer
    rID <- 0
    iconv <- 914400  # 1 inch = 914,400 EMUs (English Metric Units)
    
    lcnt <- 0
    ccnt <- 0
    rcnt <- 0
    
    lheight <- 0
    cheight <- 0
    rheight <- 0
    max_height <- 0
    
    for (i in seq(1, maxf)) {

      tret <- ""

      if (left_width > 0) {
        if (fl_num >= i) {
          
          if (image_left == FALSE) {
            # Split strings if they exceed width
            tmp1 <- split_string_html(fl[[i]], left_width, rs$units)
            
            
            # tret <- paste0(tret, cell_pct(tmp1$html, "left", 1667))
            tret <- paste0(tret, cell_pct(tmp1$html, "left", left_pct))
            
            lcnt <- tmp1$lines
          } else {
            # Calculate image height & width
            if (rs$units == "inches") {
              hgth <- (fl$height) * iconv 
              wdth <- (min(fl$width, left_width)) * iconv 
              
            } else {
              hgth <- (cin(fl$height)) * iconv 
              wdth <- (cin(min(fl$width, left_width))) * iconv
              
            }
            
            # Get image DOCX codes
            rID <- rID + 1
            image_path <- c(image_path, fl$image_path[i])
            
            img <- get_image_docx(rID, "left", hgth, wdth, type = "f")
            tret <- paste0(tret, "<w:tc>", img, "</w:tc>")
            
            lheight <- fl$height
          }
        } else {
          tret <- paste0(tret, cell_pct(" ", "left", left_pct))
          lcnt <- 1
        }
      }
      
      if (center_width > 0) {
        if (fc_num >= i) {
          
          if (image_center == FALSE) {
            # Split strings if they exceed width
            tmp2 <- split_string_html(fc[[i]], center_width, rs$units)
            
            # tret <- paste0(tret,  cell_pct(tmp2$html, "center", 1666))
            tret <- paste0(tret,  cell_pct(tmp2$html, "center", center_pct))
            ccnt <- tmp2$lines
          } else {
            # Calculate image height & width
            if (rs$units == "inches") {
              hgth <- (fc$height) * iconv 
              wdth <- (min(fc$width, center_width)) * iconv 
              
            } else {
              hgth <- (cin(fc$height)) * iconv 
              wdth <- (cin(min(fc$width, center_width))) * iconv
              
            }
            
            # Get image DOCX codes
            rID <- rID + 1
            image_path <- c(image_path, fc$image_path[i])
            
            img <- get_image_docx(rID, "center", hgth, wdth, type = "f")
            tret <- paste0(tret, "<w:tc>", img, "</w:tc>")
            
            cheight <- fc$height
          }
        } else {
          tret <- paste0(tret,  cell_pct(" ", "center", center_pct))
          ccnt <- 1
        }
      }
      
      if (right_width > 0) {
        if (fr_num >= i) {
          
          if (image_right == FALSE) {
            tmp3 <- split_string_html(fr[[i]], right_width, rs$units)
            
            # tret <- paste0(tret, cell_pct(tmp3$html, "right", 1667))
            tret <- paste0(tret, cell_pct(tmp3$html, "right", right_pct))
            
            rcnt <- tmp3$lines
          } else {
            # Calculate image height & width
            if (rs$units == "inches") {
              hgth <- (fr$height) * iconv 
              wdth <- (min(fr$width, right_width)) * iconv 
              
            } else {
              hgth <- (cin(fr$height)) * iconv 
              wdth <- (cin(min(fr$width, right_width))) * iconv
              
            }
            
            # Get image DOCX codes
            rID <- rID + 1
            image_path <- c(image_path, fr$image_path[i])
            
            img <- get_image_docx(rID, "right", hgth, wdth, type = "f")
            tret <- paste0(tret, "<w:tc>", img, "</w:tc>")
            
            rheight <- fr$height
          }
        } else {
          tret <- paste0(tret,  cell_pct(" ", "right", right_pct))
          rcnt <- 1
        }
      }

      cnt <- cnt + max(lcnt, ccnt, rcnt)
      max_height <- max_height + max(lheight, cheight, rheight)
      
      
      trht <- get_row_height(max(round(rs$row_height * max(lcnt, ccnt, rcnt), max(lheight, cheight, rheight)) * conv)) 
      
      # <w:tr> should end here for each row
      ret <- paste0(ret, '<w:tr>', trht, tret, "</w:tr>\n")

    }
    dev.off()
    
    if (max_height > 0) {
      cnt <- max(cnt, ceiling(max_height/rs$line_height))
    }

    # ret <- paste0(get_page_numbers_docx(ret), "</w:tr>\n")
    ret <- paste0(get_page_numbers_docx(ret))
    ret <- paste0(ret, "</w:tbl>\n")
  }
  
  if (!is.null(rs$footer_footnotes)) {
    
    tret <- get_footnotes_docx(rs$footer_footnotes, rs$content_size[["width"]], rs)
    ret <- paste0(tret$docx, rs$table_break, ret)
    cnt <- cnt + tret$lines
    
  }


  
  res <- list(docx = paste0(ret, collapse = ""),
              lines = cnt,
              image_path = image_path)
  
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
    
    # Bold has two scenarios
    #   (1) All TRUE, FALSE -> Directly output as all bold or not bold
    #   (2) Only label or value is bold -> Specially process the line with label and value
    
    # if (pgby$bold %in% c(T, F)) {
    #   # Account for multiple pgby lines
    #   tmp <- split_string_html(paste0(pgby$label, value), width, rs$units)
    #   
    #   vl <- tmp$html
    #   cnt <- cnt + tmp$lines
    #   vrht <- get_row_height(round(rs$row_height * conv) * tmp$lines)
    #   
    #   ret <- append(ret, paste0("<w:tr>", vrht, 
    #                             cell_abs(vl, width = w, 
    #                                      borders = vb, bold = pgby$bold), 
    #                             "</w:tr>\n"))
    #   
    # } else if (pgby$bold %in% c("value", "label", T, F)) {
    if (pgby$bold %in% c("value", "label", T, F)) {
      
      # -----  This is for test ------- #
      # pgby <- list("label" = paste0(
      #   "This is a very long\nlabel with intentionally line change and also",
      #   " long text which should take at least three lines: "
      # ),
      #              "bold" = "label")
      # # value <- "serota"
      # 
      # # pgby <- list("label" = "Flower Type: ",
      # # "bold" = "value")
      # value <- paste0(
      #   "This is a very long value without intentionally line change and also",
      #   " long text which should take at least three lines: ",
      #   "serota"
      # )
      # width <- 5
      # rs <- list("units" = "inches")
      # conv <- 1440
      # vb <- c()
      # w <- round(width * conv)
      # cnt <- 0
      tmp_cnt <- 0
      # ------------------------------- #
      if (pgby$bold == "value") {
        bold <- c(FALSE, TRUE)
      } else if (pgby$bold == "label") {
        bold <- c(TRUE, FALSE)
      } else if (pgby$bold == TRUE) {
        bold <- c(TRUE, TRUE)
      } else if (pgby$bold == FALSE) {
        bold <- c(FALSE, FALSE)
      }
      
      # Split label
      label_buffer <- ifelse(bold[1] == TRUE, 0.5, 0.2)
      label_split <- split_string_html(pgby$label, width - label_buffer, rs$units)
      cnt <- cnt + label_split$lines
      tmp_cnt <- tmp_cnt + label_split$lines # To be deleted

      # Split value
      value_buffer <- ifelse(bold[2] == TRUE, 0.5, 0.2)
      label_last_width <- label_split$widths[length(label_split$widths)]
      remain_width <- width - label_last_width - value_buffer
      value_split <- split_string_html(value, remain_width, rs$units)

      remain_value_lines <- 0
      if (value_split$widths[1] > remain_width) {
        # If the first width is greater than remain width, it means value start a new line
        value_split <- split_string_html(value, width, rs$units)
        cnt <- cnt + value_split$lines
        remain_value_lines <- value_split$lines
        tmp_cnt <- tmp_cnt + value_split$lines # To be deleted
        value_split_txt <- value_split$html
      } else {
        # If not, calculate from second line with full width
        splt <- strsplit(value_split$html, split = "\n", fixed = TRUE)
        if (length(splt[[1]]) > 1) {
          remain_value <- trimws(sub(splt[[1]][1], "", value), which = "left")
          remain_value_split <- split_string_html(remain_value, width - value_buffer, rs$units)
          cnt <- cnt + remain_value_split$lines
          tmp_cnt <- tmp_cnt + remain_value_split$lines # To be deleted
          remain_value_lines <- remain_value_split$lines
          
          value_split_txt <- paste0(splt[[1]][1], "\n", remain_value_split$html)
        } else {
          value_split_txt <- value_split$html
        }
      }
      
      vrht <- get_row_height(round(rs$row_height * conv) * (label_split$lines + remain_value_lines))
      
      # Make sure there is blank between label and value
      label_nchar <- nchar(label_split$html)
      value_nchar <- nchar(value_split_txt)
      if (substr(label_split$html, label_nchar, label_nchar) != " "
          & substr(value_split_txt, value_nchar, value_nchar) != " "){
        value_split_txt <- paste0(" ", value_split_txt)
      }
      
      # print(paste0("w is ", w))
      # print(paste0("line is ", tmp_cnt))
      # print(paste0("label is ", label_split$html))
      # print(paste0("value is ", value_split_txt))
      
      ret <- append(ret, paste0("<w:tr>", vrht, 
                                cell_abs(c(label_split$html, value_split_txt), width = w, 
                                         borders = vb, bold = bold, multiple = TRUE), 
                                "</w:tr>\n"))
    }
    
    dev.off()
    
    
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
