# The page template is everything except the content: page header/footer,
# titles, footnotes, etc.

# Page Template RTF Functions ---------------------------------------------


#' Create a page template with header, titles, footnotes, and footer.
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_pdf<- function(rs) {
  
  pt <- structure(list(), class = c("page_template_pdf", "list"))
  
  pt$page_header <- get_page_header_pdf(rs)
  pt$title_hdr <- get_title_header_pdf(rs$title_hdr, rs$line_size, rs,
                                       ystart = pt$page_header$points)
  pt$titles <- get_titles_pdf(rs$titles, rs$line_size, rs, 
                              ystart = pt$page_header$points)
  
  pt$page_footer <- get_page_footer_pdf(rs)
  


  
  pt$footnotes <- c()
  if (!is.null(rs$footnotes)) {
    if (!is.null(rs$footnotes[[1]])) {
      if (rs$footnotes[[1]]$valign == "bottom")

  
        pt$footnotes <- get_footnotes_pdf(rs$footnotes, rs$line_size, rs, 
                                          footer_lines = pt$page_footer$lines)
    }
  }
  
  pt$lines <- sum(pt$page_header$lines, pt$page_footer$lines,
                  pt$title_hdr$lines, pt$titles$lines, pt$footnotes$lines)
  
  pt$points <- sum(pt$page_header$points, pt$page_footer$points,
                  pt$title_hdr$points, pt$titles$points, pt$footnotes$points)
  
  # Page by not here.  Messes up line counts.
  
  return(pt)
}

#' @import grDevices
#' @noRd
get_page_header_pdf <- function(rs) {
  
  ret <- list()
  cnt <- 0
  pnts <- 0
  
  hl <- rs$page_header_left
  hr <- rs$page_header_right
  lh <- rs$row_height
  
  rb <- rs$content_size[["width"]] 
  
  # User controlled width of left column
  lwdth <- rs$page_header_width
  if (is.null(lwdth))
    lwdth <- rs$content_size[["width"]]/2
  
  # Calculate right column width
  rwdth <- rs$content_size[["width"]] - lwdth
  # lpct <- round(5000 * lwdth / rs$content_size[["width"]])
  # rpct <- round(5000 * rwdth / rs$content_size[["width"]])
  

  maxh <- max(length(hl), length(hr))

  if (maxh > 0) {

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)

    lyline <- 0 
    ryline <- 0 
    
    for (i in seq(1, maxh)) {


      if (length(hl) >= i) {

        # Split strings if they exceed width
        tmp <- split_string_text(hl[[i]], lwdth, rs$units)
        
        
        for (ln in seq_len(tmp$lines)) {
        
          ret[[length(ret) + 1]] <- page_text(tmp$text[ln], rs$font_size, 
                                xpos = get_points_left(0, 
                                                       rb,
                                                       tmp$widths[ln],
                                                       units = rs$units),
                                            ypos = lyline, 
                                align = "left", 
                                alignx = 0)
          lyline <- lyline + lh
        }

        lcnt <- tmp$lines

      } else {
        
        lcnt <- 1
      }

      if (length(hr) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_text(hr[[i]], rwdth, rs$units)

        for (ln in seq_len(tmp2$lines)) {
          ret[[length(ret) + 1]] <- page_text(tmp2$text[ln], rs$font_size, 
              xpos = get_points_right(0,
                                      rb, 
                                      tmp2$widths[ln], 
                                      rs$units),
                                      ypos = ryline, 
              align = "right", 
              alignx = rb)
          ryline <- ryline + lh
        }
        
        rcnt <- tmp2$lines

      } else {
        rcnt <- 1
      }

      if (lcnt > rcnt) {
        cnt <- cnt + lcnt
      } else {
        cnt <- cnt + rcnt
      }
      
    }

    dev.off()

    if (rs$page_header_blank_row == "below") {

      cnt <- cnt + 1
    } 

  }

  pnts <- cnt * lh

  
  res <- list(pdf = ret, 
              lines = cnt,
              points = pnts)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_page_footer_pdf <- function(rs) {
  
  ret <- list()
  cnt <- 0
  pnts <- 0
  
  conv <- rs$point_conversion
  
  fl <- rs$page_footer_left
  fc <- rs$page_footer_center
  fr <- rs$page_footer_right
  lh <- rs$row_height

  maxf <- max(length(fl), length(fc), length(fr))

  if (maxf > 0) {

    rb3 <- rs$content_size[["width"]]
    rb1 <- round(rb3 / 3)
    rb2 <- round(rb1 * 2)
    
    tmp1 <- list()
    tmp2 <- list()
    tmp3 <- list()

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)


    # First get all wraps
    for (i in seq(1, maxf)) {


      if (length(fl) >= i) {

        # Split strings if they exceed width
        tmp1[[length(tmp1) + 1]] <- split_string_text(fl[[i]], 
                                    rs$content_size[["width"]]/3, rs$units)
        
        lcnt <- tmp1[[length(tmp1)]]$lines
        
      } else {

        lcnt <- 1
      }

      if (length(fc) >= i) {

        # Split strings if they exceed width
        tmp2[[length(tmp2) + 1]] <- split_string_text(fc[[i]], 
                                    rs$content_size[["width"]]/3, rs$units)
        
        ccnt <- tmp2[[length(tmp2)]]$lines
      } else {

        ccnt <- 1
      }

      if (length(fr) >= i) {

        tmp3[[length(tmp3) + 1]] <- split_string_text(fr[[i]], 
                                 rs$content_size[["width"]]/3, rs$units)
        
        rcnt <- tmp3[[length(tmp3)]]$lines
      } else {

        rcnt <- 1
      }

      cnt <- cnt + max(lcnt, ccnt, rcnt)

    }
    
    dev.off()
    
    # Have to determine wraps so we can calculate the height
    # of the page footer before creating any pdf.
    sy <- (rs$content_size[["height"]] * rs$point_conversion) - (cnt * lh )
    
    lyline <- sy
    cyline <- sy
    ryline <- sy
    
    # Then create pdf text
    for (i in seq(1, length(tmp1))) {
    
        
        
      for (ln in seq_len(tmp1[[i]]$lines)) {
        
        ret[[length(ret) + 1]] <- page_text(tmp1[[i]]$text[ln], rs$font_size, 
                                            xpos = get_points_left(0, 
                                                          rb1,
                                                          tmp1[[i]]$widths[ln],
                                                          units = rs$units),
                                            ypos = lyline, 
                                            align = "left",
                                            alignx = 0)
        lyline <- lyline + lh
      }

    }
    
    for (i in seq(1, length(tmp2))) {
      
        
        for (ln in seq_len(tmp2[[i]]$lines)) {
          
          ret[[length(ret) + 1]] <- page_text(tmp2[[i]]$text[ln], rs$font_size, 
                                              xpos = get_points_center(rb1, 
                                                            rb2,
                                                            tmp2[[i]]$widths[ln],
                                                            units = rs$units),
                                              ypos = cyline, 
                                              align = "center",
                                              alignx = rb1 + ((rb2 - rb1)/2))
          cyline <- cyline + lh
        }
    }
      
    for (i in seq(1, length(tmp3))) {
        
        
      for (ln in seq_len(tmp3[[i]]$lines)) {
        
        ret[[length(ret) + 1]] <- page_text(tmp3[[i]]$text[ln], rs$font_size, 
                                            xpos = get_points_right(rb2, 
                                                                    rb3,
                                                                    tmp3[[i]]$widths[ln],
                                                                    units = rs$units),
                                            ypos = ryline, 
                                            align = "right",
                                            alignx = rb3)
        ryline <- ryline + lh
      }
        
    }
  }
  
  if (any(rs$page_footer_blank_row == "above"))
    cnt <- cnt + 1

  res <- list(pdf = ret,
              lines = cnt,
              points = cnt * lh)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_titles_pdf <- function(ttllst, content_width, rs, 
                           talgn = "center", ystart = 0) {
  
  ret <- c()
  cnt <- 0
  pnts <- 0
  #print(ystart)
  #print("get titles")
  
  conv <- rs$point_conversion
  bs <- rs$border_spacing
  bh <- rs$border_height
  border_flag <- FALSE
  lh <- rs$line_height

  start_offset <- NULL
  
  yline <- ystart 

  if (length(ttllst) > 0) {
    
    blkcnt <- 0

    for (ttls in ttllst) {
      
      blkcnt <- blkcnt + 1
      
      cols <- ttls$columns
      

      if (ttls$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttls$width == "content")
        width <- content_width
      else if (is.numeric(ttls$width))
        width <- ttls$width
      
      cwidth <- width / cols

      # Get content alignment codes
      if (talgn == "right") {
        lb <- rs$content_size[["width"]] - width
        rb <- rs$content_size[["width"]]
      } else if (talgn %in% c("center", "centre")) {
        lb <- (rs$content_size[["width"]] - width) / 2
        rb <- width + lb
      } else {
        lb <- 0
        rb <- width
      }
      
      border_flag <- FALSE
      
      fs <- rs$font_size
      if (!is.null(ttls$font_size))
        fs <- ttls$font_size
      
      lho <- get_line_height_pdf(fs)
      lh <- lho
      if (any(ttls$borders %in% c("all", "inside", "outside")))
          lh <- lh + bh

      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = fs)
      
      brdrs <- strip_borders(ttls$borders)
      
      if (blkcnt == 1) {
        
        start_offset <- lho - rs$row_height
        
        if (any(brdrs %in% c("all", "outside", "top"))) {
          yline <- ystart + start_offset + bh
        } else {
          yline <- ystart + start_offset
        }
        
      }
      

      
      # Extra line for blank row
      if (any(ttls$blank_row %in% c("above", "both"))) {
        
        
        if (any(brdrs %in% c("all", "outside", "top"))) {
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                               yline - lh + bs + 1,
                                               (rb - lb) * conv)
          
          if (blkcnt == 1 & any(brdrs %in% c("outside")))
            pnts <- pnts + bs
        }
        
        
        
        if (any(brdrs %in% c("all", "outside", "left", "right"))) {
          ret[[length(ret) + 1]] <- page_vline(lb * conv,
                                               yline - lh + bs + 1,
                                               (1 * lh) )
          
          ret[[length(ret) + 1]] <- page_vline(rb * conv,
                                               yline - lh + bs + 1,
                                               (1 * lh))
        }
        
        yline <- yline + lh
        cnt <- cnt + 1
        pnts <- pnts + lh
        
        
        if (any(brdrs %in% c("all", "inside")) ) {

          ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                               yline - lh + bs + 1,
                                               (rb - lb) * conv)
          if (blkcnt == 1)
            pnts <- pnts + bs

        }
      } else if (any(brdrs %in% c("all", "outside", "top")) ) {

          ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                               yline - lh + bs + 1,
                                               (rb - lb) * conv)
          if (blkcnt == 1)
            pnts <- pnts + bs

      }
      
      
      i <- 1
      
      while (i <= length(ttls$titles)) {
        
        # Max lines per row
        mxlns <- 0
        
        # Calculate current row
        rw <- ceiling(i / cols)
        

        for (j in seq_len(cols)) { 
          
          # Not all cells have titles
          if (i > length(ttls$titles))
            vl <- ""
          else 
            vl <- ttls$titles[[i]]
          
          # Deal with column alignments
          if (cols == 1) {
            calgn <- ttls$align
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
          tmp <- split_string_text(vl, cwidth, rs$units)
          
          if (tmp$lines > mxlns)
            mxlns <- tmp$lines
          

          
          # Recalculate boundaries for cells
          clb <- lb + (cwidth * (j - 1))
          crb <- clb + cwidth
          
          # Row within cell pointer
          rwln <- 0
          
          for (ln in seq_len(tmp$lines)) {
            
            # print(yline) 
            ret[[length(ret) + 1]] <- page_text(tmp$text[ln], fs, 
                                                bold = ttls$bold,
                                                xpos = get_points(clb, 
                                                                  crb,
                                                                  tmp$widths[ln],
                                                                  units = rs$units,
                                                                  align = calgn),
                                                ypos = yline + rwln)
            rwln <- rwln + lh
          }
          


          
          i <- i + 1
        }
        
        
        if (any(brdrs %in% c("all", "inside")) & cols > 1) {
          
          for (z in seq_len(cols - 1)) {
            ret[[length(ret) + 1]] <- page_vline((lb + (z * cwidth)) * conv,
                                                 yline - lh + bs + 1,
                                                 mxlns * lh)
          }
        }
        
        
        # Left border
        if (any(brdrs %in% c("all", "outside", "left"))) {


          ret[[length(ret) + 1]] <- page_vline(lb * conv,
                                               yline - lh + bs + 1,
                                               (mxlns * lh) )

        }

        # Right border
        if (any(brdrs %in% c("all", "outside", "right"))) {


          ret[[length(ret) + 1]] <- page_vline(rb * conv,
                                               yline - lh + bs + 1,
                                               (mxlns * lh))

        }
        
        
        yline <- yline + (lh * mxlns)
        pnts <- pnts + (lh * mxlns)
        
        # Inside borders
        if (any(brdrs %in% c("all", "inside")) & i > 1) {
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                               yline - lh + bs + 1, 
                                               (rb - lb) * conv) 
        }

        cnt <- cnt + mxlns

      }
      
      # Extra border for blank line below
      if (any(ttls$blank_row %in% c("below", "both"))) {
        
        if (any(brdrs %in% c("all", "inside"))) {
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                               yline - lh + bs + 1, 
                                               (rb - lb) * conv) 
        }
        
        if (any(brdrs %in% c("all", "outside", "bottom"))) {
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                               yline + bs + 1, 
                                               (rb - lb) * conv) 
          
          # if (blkcnt == 1 & brdrs %in% c("outside"))
          #    pnts <- pnts + bh + bh + 1
        }
          
        if (any(brdrs %in% c("all", "outside", "left", "right"))) {
          ret[[length(ret) + 1]] <- page_vline(lb * conv,
                                               yline - lh + bs + 1,
                                               (1 * lh) )
          
          ret[[length(ret) + 1]] <- page_vline(rb * conv,
                                               yline - lh + bs + 1,
                                               (1 * lh))
          
        }
        
        
        yline <- yline + lh
        cnt <- cnt + 1
        pnts <- pnts + lh
      } else {
        #pnts <- pnts + .5 # no idea why
      }
      
      if (any(brdrs %in% c("outside", "all", "bottom")))
        border_flag <- TRUE
      
      dev.off()
      

      # if (blkcnt == 1) {
      #   ypos <-  ystart + start_offset - lho + bs + 1 
      #   lcnt <- cnt
      #   blkrws[blkcnt] <- cnt
      # } else {
      #   
      #   blkrws[blkcnt] <- cnt - lcnt
      #   ypos <- ypos + (blkrws[blkcnt - 1] * lh)
      #   lcnt <- cnt
      # }
      # 
      # 
      # badj <- 0
      # if (lh == lho & any(brdrs %in% c("bottom", "outside"))) {
      #   badj <- bh
      # }
      
      # print("ypos")
      # print(ypos)
      
      # Top border
      # if (any(brdrs %in% c("all", "outside", "top"))) {
      #   
      #   ret[[length(ret) + 1]] <- page_hline(lb * conv, 
      #                                        ypos , 
      #                                        (rb - lb) * conv) 
      #   if (blkcnt == 1)
      #     pnts <- pnts + bs
      #   
      # }
      # 
      # # Bottom border
      # if (any(brdrs %in% c("all", "outside", "bottom"))) {
      #   
      #     
      #     ret[[length(ret) + 1]] <- page_hline(lb * conv, 
      #                                          ypos + (blkrws * lh) + badj, 
      #                                          (rb - lb) * conv) 
      #     if (blkcnt == 1)
      #       pnts <- pnts + badj
      #     #pnts <- pnts + 2
      #     border_flag <- TRUE
      #   
      # }
      # 
      # # Left border
      # if (any(brdrs %in% c("all", "outside", "left"))) {
      #   
      #   
      #   ret[[length(ret) + 1]] <- page_vline(lb * conv, 
      #                                        ypos, 
      #                                        (blkrws * lh) + badj) 
      #   
      # }
      # 
      # # Right border
      # if (any(brdrs %in% c("all", "outside", "right"))) {
      #   
      #   
      #   ret[[length(ret) + 1]] <- page_vline(rb * conv, 
      #                                        ypos, 
      #                                        (blkrws * lh) + badj) 
      #   
      # }
      # 
      # if (any(brdrs %in% c("all", "inside")) & cols > 1) {
      #   
      #   
      #   for (k in seq_len(cols - 1)) {
      #   
      #     ret[[length(ret) + 1]] <- page_vline((lb + (cwidth * k)) * conv, 
      #                                          ypos, 
      #                                          (blkrws * lh) + badj) 
      #   }
      #   
      # }
      

    }
    
    


  }

  # pnts <- (cnt * lh) + start_offset - .5
  
  #pnts <- pnts + start_offset
  #print(pnts)
  cnts <- pnts / rs$row_height
  
  res <- list(pdf = ret, 
              lines = cnts,
              points = pnts,
              border_flag = border_flag)
  
  return(res)
}


#' @import grDevices
#' @noRd
get_titles_pdf_back <- function(ttllst, content_width, rs, 
                           talgn = "center", ystart = 0) {
  
  ret <- c()
  cnt <- 0
  pnts <- 0
  #print(ystart)
  #print("get titles")
  
  conv <- rs$point_conversion
  bs <- rs$border_spacing
  bh <- rs$border_height
  border_flag <- FALSE
  lh <- rs$line_height
  
  
  start_offset <- NULL
  
  yline <- ystart 
  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      if (ttls$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttls$width == "content")
        width <- content_width
      else if (is.numeric(ttls$width))
        width <- ttls$width
      
      # Get content alignment codes
      if (talgn == "right") {
        lb <- rs$content_size[["width"]] - width
        rb <- rs$content_size[["width"]]
      } else if (talgn %in% c("center", "centre")) {
        lb <- (rs$content_size[["width"]] - width) / 2
        rb <- width + lb
      } else {
        lb <- 0
        rb <- width
      }
      
      border_flag <- FALSE
      
      fs <- rs$font_size
      if (!is.null(ttls$font_size))
        fs <- ttls$font_size
      
      lho <- get_line_height_pdf(fs)
      lh <- lho
      if (any(ttls$borders %in% c("all", "inside")))
        lh <- lh + bh
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = fs)
      
      for (i in seq_along(ttls$titles)) {
        
        brdrs <- strip_borders(ttls$borders)
        
        if (i == 1) {
          
          if (is.null(start_offset)) {
            
            start_offset <- lho - rs$row_height
            
            if (any(brdrs %in% c("all", "outside", "top"))) {
              yline <- ystart + start_offset + bh
            } else {
              yline <- ystart + start_offset
            }
            
          }
          
          # Extra line for blank row
          if (any(ttls$blank_row %in% c("above", "both"))) {
            
            
            if (any(brdrs %in% c("all", "inside"))) {
              
              ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                                   yline + bs,
                                                   (rb - lb) * conv)
            }
            
            yline <- yline + lh
            cnt <- cnt + 1
            pnts <- pnts + lh
          }
        }
        
        
        # Split title strings if they exceed width
        tmp <- split_string_text(ttls$titles[[i]], width, rs$units)
        
        # Inside borders
        if (any(brdrs %in% c("all", "inside")) & i > 1) {
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                               yline - lh + bs + 1, 
                                               (rb - lb) * conv) 
        }
        
        for (ln in seq_len(tmp$lines)) {
          
          # print(yline) 
          ret[[length(ret) + 1]] <- page_text(tmp$text[ln], fs, 
                                              bold = ttls$bold,
                                              xpos = get_points(lb, 
                                                                rb,
                                                                tmp$widths[ln],
                                                                units = rs$units,
                                                                align = ttls$align),
                                              ypos = yline)
          yline <- yline + lh
          pnts <- pnts + lh
        }
        
        
        if (i == length(ttls$titles)) {
          
          # Extra border for blank line below
          if (any(ttls$blank_row %in% c("below", "both"))) {
            
            if (any(brdrs %in% c("all", "inside"))) {
              
              ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                                   yline - lh + bs + 1, 
                                                   (rb - lb) * conv) 
            }
            
            yline <- yline + lh
            cnt <- cnt + 1
            pnts <- pnts + lh
          }
          
          if (any(brdrs %in% c("outside", "all", "bottom")))
            border_flag <- TRUE
        }
        
        
        
        
        cnt <- cnt + tmp$lines
        
      }
      dev.off()
      
      ypos <-  ystart + start_offset - lho + bs + 1
      
      badj <- 0
      if (lh == lho & any(brdrs %in% c("bottom", "outside"))) {
        badj <- bh
      }
      
      # print("ypos")
      # print(ypos)
      
      # Top border
      if (any(brdrs %in% c("all", "outside", "top"))) {
        
        ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                             ypos , 
                                             (rb - lb) * conv) 
        pnts <- pnts + bs
        
      }
      
      # Bottom border
      if (any(brdrs %in% c("all", "outside", "bottom"))) {
        
        
        ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                             ypos + (cnt * lh) + badj, 
                                             (rb - lb) * conv) 
        
        pnts <- pnts + badj
        #pnts <- pnts + 2
        border_flag <- TRUE
        
      }
      
      # Left border
      if (any(brdrs %in% c("all", "outside", "left"))) {
        
        
        ret[[length(ret) + 1]] <- page_vline(lb * conv, 
                                             ypos, 
                                             (cnt * lh) + badj) 
        
      }
      
      # Right border
      if (any(brdrs %in% c("all", "outside", "right"))) {
        
        
        ret[[length(ret) + 1]] <- page_vline(rb * conv, 
                                             ypos, 
                                             (cnt * lh) + badj) 
        
      }
      
    }
    
    
    
    
  }
  
  # pnts <- (cnt * lh) + start_offset - .5
  
  #pnts <- pnts + start_offset
  #print(pnts)
  cnts <- pnts / rs$row_height
  
  res <- list(pdf = ret, 
              lines = cnts,
              points = pnts,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_footnotes_pdf <- function(ftnlst, content_width, rs, 
                              talgn = "center", ystart = NULL, footer_lines = 0,
                              brdr_flag = FALSE) {
  
  ret <- c()
  cnt <- 0
  pnts <- 0
  border_flag <- FALSE
  
  olh <- rs$row_height
  lh <- olh
  conv <- rs$point_conversion
  bs <- rs$border_spacing
  bh <- rs$border_height
  pnts <- 0
  
  if (!is.null(ystart)) {
    if (brdr_flag) {
      yline <- ystart + bh - 2
      pnts <- pnts + bh - 2
    } else 
      yline <- ystart
  } else 
    yline <- 0


  if (length(ftnlst) > 0) {
    
    for (ftnts in ftnlst) {
      
      tmp <- list()

      if (ftnts$width == "page")
        width <- rs$content_size[["width"]]
      else if (ftnts$width == "content")
        width <- content_width
      else if (is.numeric(ftnts$width))
        width <- ftnts$width
      
      # Get content alignment codes
      if (talgn == "right") {
        lb <- rs$content_size[["width"]] - width
        rb <- rs$content_size[["width"]]
      } else if (talgn %in% c("center", "centre")) {
        lb <- (rs$content_size[["width"]] - width) / 2
        rb <- width + lb
      } else {
        lb <- 0
        rb <- width
      }

      alcnt <- 0
      blcnt <- 0
      border_flag <- FALSE
      brdrs <- strip_borders(ftnts$borders)


      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)

      for (i in seq_along(ftnts$footnotes)) {

        # If all borders on, change line height to account for extra points 
        # needed for border
        if (any(brdrs %in% c("all", "inside")))
          lh <- olh + bs
        else
          lh <- olh

        al <- ""
        if (i == 1) {
          if (any(ftnts$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            yline <- yline + lh
            
            cnt <- cnt + 1
            pnts <- pnts + lh

          }
        }

        bl <- ""
        if (i == length(ftnts$footnotes)) {
          if (any(ftnts$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            cnt <- cnt + 1
            pnts <- pnts + lh
          }
          
          if (any(brdrs %in% c("outside", "all", "top")))
            border_flag <- TRUE
          
        }

        # b <- get_cell_borders(i + alcnt, 1,
        #                       length(ftnts$footnotes) + alcnt + blcnt,
        #                       1, ftnts$borders)



        # Split footnote strings if they exceed width
        t <- split_string_text(ftnts$footnotes[[i]], width, rs$units)
        
        # Capture alignment for this footnote block
        t$align <- ftnts$align

        # Count number of lines
        cnt <- cnt + t$lines

        
        # Assign strings to temp variable for now
        tmp[[length(tmp) + 1]] <- t
      }
      
      dev.off()

      if (is.null(ystart)) {
        
        yline <- (rs$content_size[["height"]] * rs$point_conversion) - 
          ((cnt + footer_lines - alcnt) * lh )
      }
      
      
      
      # Now get pdf text for each temp variable
      for (i in seq(1, length(tmp))) {
        for (ln in seq_len(tmp[[i]]$lines)) {
          
          ret[[length(ret) + 1]] <- page_text(tmp[[i]]$text[ln], rs$font_size, 
                                              xpos = get_points(lb, 
                                                          rb,
                                                          tmp[[i]]$widths[ln],
                                                          units = rs$units,
                                                          align = tmp[[i]]$align),
                                              ypos = yline, 
                                              italics = ftnts$italics)
          
          

          
          yline <- yline + lh
          pnts <- pnts + lh
        }
        
        if (any(brdrs %in% c("all", "inside")) & i < length(tmp)) {
          
          
          if (brdr_flag)
            ypos <- yline - lh + bs 
          else 
            ypos <- yline + bh - lh + bs 
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                               ypos, 
                                               (rb - lb) * conv) 
        }
        
      }
      
      
      if (is.null(ystart)) {
        
        ypos <- (rs$content_size[["height"]] * rs$point_conversion) - 
          ((cnt + footer_lines - alcnt) * lh ) - (alcnt * lh) - lh 
      } else {
      
      
        if (brdr_flag)
          ypos <- ystart - olh  
        else 
          ypos <- ystart + bh - olh + bs 
      }
      
      badj <- 0
      if (!any(brdrs %in% c("all", "inside")))
        badj <- bs
      
      if (any(brdrs %in% c("all", "outside", "top"))) {
        
        ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                             ypos, 
                                             (rb - lb) * conv) 
      }
      
      if (any(brdrs %in% c("all", "outside", "bottom"))) {
        
        ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                             ypos + (cnt * lh) + badj, 
                                             (rb - lb) * conv) 
        
        pnts <- pnts + badj
      }
      
      if (any(brdrs %in% c("all", "outside", "left"))) {
        
        ret[[length(ret) + 1]] <- page_vline(lb * conv, 
                                             ypos, 
                                             (cnt * lh) + badj ) 
      }
      
      if (any(brdrs %in% c("all", "outside", "right"))) {
      
        ret[[length(ret) + 1]] <- page_vline(rb * conv, 
                                             ypos, 
                                             (cnt * lh) + badj) 
      }
      
    }
    
  }

  res <- list(pdf = ret,
              lines = pnts / olh,
              points = pnts,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_title_header_pdf <- function(thdrlst, content_width, rs, 
                                 talgn = "center", ystart = 0) {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE

  lh <- rs$row_height
  conv <- rs$point_conversion
  bs <- rs$border_spacing
  bh <- rs$border_height
  startpos <- ystart
  tcnt <- 0
  hcnt <- 0
  pnts <- 0
  counter <- 0


  if (length(thdrlst) > 0) {

    for (ttlhdr in thdrlst) {
      
      counter <- counter + 1

      if (ttlhdr$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttlhdr$width == "content")
        width <- content_width
      else if (is.numeric(ttlhdr$width))
        width <- ttlhdr$width
      
      # Get content alignment codes
      if (talgn == "right") {
        lb <- rs$content_size[["width"]] - width
        rb1 <- rs$content_size[["width"]]
      } else if (talgn %in% c("center", "centre")) {
        lb <- (rs$content_size[["width"]] - width) / 2
        rb1 <- width + lb
      } else {
        lb <- 0
        rb1 <- width
      }
      rb2 <- rb1 * .7
      splitx <- (lb + ((rb1 - lb) * .7))  * conv
      brdrs <- strip_borders(ttlhdr$borders)
      
      if (counter == 1) {
        if (any(brdrs %in% c("all", "top", "outside"))) {
          
          startpos <- ystart + bs - 1
          
        } else {
          
          startpos <- ystart
          
        }
        lyline <- startpos
        ryline <- startpos
      }
      
      
      lh <- rs$line_height
      if (any(ttlhdr$borders %in% c("all", "inside")))
        lh <- lh + bh


      mx <- max(length(ttlhdr$titles), length(ttlhdr$right))

      alcnt <- 0
      blcnt <- 0
      border_flag <- FALSE


      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      for(i in seq_len(mx)) {

        al <- ""
        if (i == 1) {
          
          
          if (any(ttlhdr$blank_row %in% c("above", "both"))) {

            alcnt <- 1
            
            if (any(brdrs %in% c("all", "inside"))) {
              
              ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                                   startpos + bs, 
                                                   (rb1 - lb) * conv) 
            }

            lyline <- lyline + lh
            ryline <- ryline + lh

            cnt <- cnt + 1

          }
        }

        if (length(ttlhdr$titles) >= i) {
          # Split strings if they exceed width
          tmp1 <- split_string_text(ttlhdr$titles[[i]], width * .7, rs$units)
          
        
          
          for (ln in seq_len(tmp1$lines)) {
            
            ret[[length(ret) + 1]] <- page_text(tmp1$text[ln], rs$font_size, 
                                                xpos = get_points(lb, 
                                                                  rb2,
                                                                  tmp1$widths[ln],
                                                                  units = rs$units,
                                                                  align = "left"),
                                                ypos = lyline, 
                                                align = "left",
                                                alignx = 0)
            lyline <- lyline + lh
          }
          
          if (any(brdrs %in% c("all", "inside")) ) {
            
            ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                                 lyline - lh + bs + 1, 
                                                 splitx - (lb * conv)) 
          }
          
          tcnt <- tcnt + tmp1$lines
        } else {

          tcnt <- tcnt + 0
        }

        if (length(ttlhdr$right) >= i) {
          tmp2 <- split_string_text(ttlhdr$right[[i]],
                                   width * .3, rs$units)
          
          
          for (ln in seq_len(tmp2$lines)) {
            
            ret[[length(ret) + 1]] <- page_text(tmp2$text[ln], rs$font_size, 
                                                xpos = get_points(rb2, 
                                                                  rb1,
                                                                  tmp2$widths[ln],
                                                                  units = rs$units,
                                                                  align = "right"),
                                                ypos = ryline, 
                                                align = "right",
                                                alignx = rb1)
            ryline <- ryline + lh
          }
          
          if (any(brdrs %in% c("all", "inside"))) {

            ret[[length(ret) + 1]] <- page_hline(splitx,
                                                 ryline - lh + bs + 1,
                                                 (rb1* conv) - splitx)
          }
          
          hcnt <- hcnt + tmp2$lines
        } else {

          hcnt <- hcnt + 0
        }
        
      }
      
      if (tcnt > hcnt)
        cnt <- cnt + tcnt
      else
        cnt <- cnt + hcnt
      
    
      if (any(ttlhdr$blank_row %in% c("below", "both"))) {
        blcnt <- 1
        
        if (any(brdrs %in% c("all", "inside"))) {
          
          
          ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                               (startpos + (cnt * lh) - lh) + bs + 1,
                                               (rb1 - lb) * conv)
        }
        
        lyline <- lyline + lh
        ryline <- ryline + lh
        
        cnt <- cnt + 1
      }
      
      if (any(brdrs %in% c("all", "outside", "bottom")))
        border_flag <- TRUE
      

      dev.off()
      
      if (any(brdrs %in% "bottom"))
        pnts <- (cnt * lh) + bh
      else
        pnts <- cnt * lh
      
     pnts <- pnts + (startpos - ystart) + 1
     
     ypos <-  ystart - rs$line_height + bs + 1
     
     badj <- 0
     if (lh == rs$line_height & any(brdrs %in% c("bottom", "outside"))) {
       badj <- bh
     }
     

      # Top border
      if (any(brdrs %in% c("all", "outside", "top"))) {
        
        ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                             ypos, 
                                             (rb1 - lb) * conv) 
        
      }
      
      #  Bottom border
      if (any(brdrs %in% c("all", "outside", "bottom"))) {
        
        ret[[length(ret) + 1]] <- page_hline(lb * conv, 
                                             ypos + (cnt * lh) + badj, 
                                             (rb1 - lb) * conv) 
        
        pnts <- pnts + badj
        
      }
      
      # Left border
      if (any(brdrs %in% c("all", "outside", "left"))) {
        
        
        ret[[length(ret) + 1]] <- page_vline(lb * conv, 
                                             ypos, 
                                             (cnt * lh) + badj) 
        
      }
      
      # Right border
      if (any(brdrs %in% c("all", "outside", "right"))) {
        
        
        ret[[length(ret) + 1]] <- page_vline(rb1 * conv, 
                                             ypos, 
                                             (cnt * lh) + badj) 
        
      }
      
     # Middle border
      if (any(brdrs %in% c("all", "inside"))) {
        
        bldiff <- blcnt - alcnt
        if (bldiff < 0)
          bldiff <- 0
        
        ret[[length(ret) + 1]] <- page_vline(splitx, 
                                             ypos + (alcnt * lh), 
                                             ((cnt - alcnt - blcnt) * lh) ) 
        
      }
      
    }

  }

  res <- list(pdf = ret,
              lines = cnt,
              points = pnts,
              border_flag = border_flag)

  
  return(res)
}


#' Get page by text strings suitable for printing
#' @import stringi
#' @param titles Page by object
#' @param width The width to set the page by strings to
#' @return A vector of strings
#' @noRd
get_page_by_pdf <- function(pgby, width, value, rs, talgn, ystart = 0, 
                            brdr_flag = FALSE) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- ""

  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  lh <- rs$line_height
  conv <- rs$point_conversion
  bs <- rs$border_spacing
  bh <- rs$border_height
  pnts <- 0

  if (!is.null(pgby)) {

    if (!any(class(pgby) == "page_by"))
      stop("pgby parameter value is not a page_by.")
    
    # Get content alignment codes
    if (talgn == "right") {
      lb <- rs$content_size[["width"]] - width
      rb <- rs$content_size[["width"]]
    } else if (talgn %in% c("center", "centre")) {
      lb <- (rs$content_size[["width"]] - width) / 2
      rb <- width + lb
    } else {
      lb <- 0
      rb <- width
    }
    
    brdrs <- strip_borders(pgby$borders)
    
    if (any(brdrs %in% c("all", "inside"))) {
      lh <- lh + bh 
    }
  
    if (any(brdrs %in% c("all", "outside", "top")) & !brdr_flag) {
      yline <- ystart + bh
      pnts <- pnts + bh
    } else {
      
      yline <- ystart
    }
    
    # Open device context
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)


    if (pgby$blank_row %in% c("above", "both")) {

      if (any(brdrs %in% c("all", "inside"))) {
        ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                             yline + bs,
                                             (rb - lb) * conv)
      }
      
      yline <- yline + lh
      cnt <- cnt + 1
      pnts <- pnts + lh
    }


    vl <- paste0( pgby$label, " ", value)

    tmp <- split_string_text(vl, width, rs$units)
    
    dev.off()
    
    for (ln in seq_len(tmp$lines)) {
      
      ret[[length(ret) + 1]] <- page_text(tmp$text[ln], rs$font_size, 
                                          bold = FALSE,
                                          xpos = get_points(lb, 
                                                            rb,
                                                            tmp$widths[ln],
                                                            units = rs$units,
                                                            align = pgby$align),
                                          ypos = yline)
      yline <- yline + lh
      cnt <- cnt + 1
      pnts <- pnts + lh
    }
    

    
    if (pgby$blank_row %in% c("below", "both")) {
      
      
      if (any(brdrs %in% c("all", "inside"))) {
        ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                             yline - lh + bs,
                                             (rb - lb) * conv)
      }
      
      yline <- yline + lh
      cnt <- cnt + 1
      pnts <- pnts + lh
    }
    
    if (any(brdrs %in% c("all", "inside"))) {
      if (brdr_flag)
        ypos <-  ystart - lh + 3 
      else 
        ypos <- ystart - lh + bs + 2
      
    } else 
      ypos <-  ystart - lh + bs + 1
    
    # Top border
    if (any(brdrs %in% c("all", "outside", "top"))) {

      ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                           ypos,
                                           (rb - lb) * conv)

    }

    # Bottom border
    if (any(brdrs %in% c("all", "outside", "bottom"))) {

      ret[[length(ret) + 1]] <- page_hline(lb * conv,
                                           ypos  + (cnt * lh),
                                           (rb - lb) * conv)
      border_flag <- TRUE
    }

    # Left border
    if (any(brdrs %in% c("all", "outside", "left"))) {


      ret[[length(ret) + 1]] <- page_vline(lb * conv,
                                           ypos,
                                           (cnt * lh)) 

    }

    # Right border
    if (any(brdrs %in% c("all", "outside", "right"))) {


      ret[[length(ret) + 1]] <- page_vline(rb * conv,
                                           ypos,
                                           (cnt * lh) )

    }

  }

  
  res <- list(pdf = ret, 
              lines = pnts / lh,
              points = pnts,
              border_flag = border_flag)
  
  return(res)
}

# Utilities ---------------------------------------------------------------



get_page_numbers_pdf <- function(txt, pg, tpg) {
  
  ret <- txt
  
  ret <- gsub("[pg]", pg, ret, fixed = TRUE)
  
  ret <- gsub("[tpg]", tpg, ret, fixed = TRUE)
  
  return(ret)
}


#' Function to temporarily minimize border specs for PDF
#' @noRd
strip_borders <- function(bspec) {
  
 ret <- bspec
 
 # if ("all" %in% ret)
 #   ret <- append(ret[ret != "all"], c("top", "bottom"))
 # if ("inside" %in% ret)
 #   ret <- ret[ret != "inside"]
 # if ("outside" %in% ret)
 #   ret <- append(ret[ret != "outside"], c("top", "bottom"))
 # if ("body" %in% ret)
 #   ret <- append(ret[ret != "body"], c("top", "bottom"))
 # if ("left" %in% ret)
 #   ret <- ret[ret != "left"]
 # if ("right" %in% ret)
 #   ret <- ret[ret != "right"]
  
 return(ret)
}
