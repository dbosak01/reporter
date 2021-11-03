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

  maxh <- max(length(hl), length(hr))

  if (maxh > 0) {

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)

    lyline <- 0 
    ryline <- 0 
    
    for (i in seq(1, maxh)) {


      if (length(hl) >= i) {

        # Split strings if they exceed width
        tmp <- split_string_text(hl[[i]], rs$content_size[["width"]]/2, rs$units)
        
        
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
        tmp2 <- split_string_text(hr[[i]], rs$content_size[["width"]]/2, rs$units)

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
                                              alignx = rb1 / rb2)
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
  
  lh <- rs$row_height
  border_flag <- FALSE
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

      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = fs)

      for (i in seq_along(ttls$titles)) {


        if (i == 1) {
          if (any(ttls$blank_row %in% c("above", "both"))) {

            yline <- yline + lh
            cnt <- cnt + 1

          }
        }


        # 
        # b <- get_cell_borders(i + alcnt, 1,
        #                       length(ttls$titles) + alcnt + blcnt,
        #                       1, ttls$borders)

        # Split title strings if they exceed width
        tmp <- split_string_text(ttls$titles[[i]], width, rs$units)

        
        for (ln in seq_len(tmp$lines)) {
          
          ret[[length(ret) + 1]] <- page_text(tmp$text[ln], fs, 
                                              bold = ttls$bold,
                                              xpos = get_points(lb, 
                                                                rb,
                                                                tmp$widths[ln],
                                                                units = rs$units,
                                                                align = ttls$align),
                                              ypos = yline)
          yline <- yline + lh
        }
        
        
        if (i == length(ttls$titles)) {
          if (any(ttls$blank_row %in% c("below", "both"))) {
            #blcnt <- 1
            yline <- yline + lh
            cnt <- cnt + 1
          }
          
          # if (any(ttls$borders %in% c("outside", "all", "bottom")))
          #   border_flag <- TRUE
        }
        

        cnt <- cnt + tmp$lines

      }
      dev.off()

    }

  }


  
  res <- list(pdf = ret, 
              lines = cnt,
              points = cnt * lh,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_footnotes_pdf <- function(ftnlst, content_width, rs, 
                              talgn = "center", ystart = NULL, footer_lines = 0) {
  
  ret <- c()
  cnt <- 0
  pnts <- 0
  border_flag <- FALSE
  
  lh <- rs$row_height
  
  if (!is.null(ystart))
    yline <- ystart
  else 
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


      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)

      for (i in seq_along(ftnts$footnotes)) {


        al <- ""
        if (i == 1) {
          if (any(ftnts$blank_row %in% c("above", "both"))) {

            alcnt <- 1
            
            yline <- yline + lh

            cnt <- cnt + 1

          }
        }

        bl <- ""
        if (i == length(ftnts$footnotes)) {
          if (any(ftnts$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            cnt <- cnt + 1
          }
          
          if (any(ftnts$borders %in% c("outside", "all", "top")))
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
                                              ypos = yline)
          yline <- yline + lh
        }
        
      }
      
    }
    
  }

  res <- list(pdf = ret,
              lines = cnt,
              points = cnt * lh,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_title_header_pdf <- function(thdrlst, content_width, rs, 
                                 talgn = "center", ystart = 0) {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  border_flag <- FALSE

  lh <- rs$row_height
  lyline <- ystart
  ryline <- ystart

  if (length(thdrlst) > 0) {

    for (ttlhdr in thdrlst) {

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
                                                ypos = lyline)
            lyline <- lyline + lh
          }
          
          tcnt <- tmp1$lines
        } else {

          tcnt <- 1
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
                                                ypos = ryline)
            ryline <- ryline + lh
          }
          

          hcnt <- tmp2$lines
        } else {

          hcnt <- 1
        }
        
        bl <- ""
        if (i == mx) {
          if (any(ttlhdr$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            lyline <- lyline + lh
            ryline <- ryline + lh
            
            cnt <- cnt + 1
          }
          
          if (any(ttlhdr$borders %in% c("all", "outside", "bottom")))
            border_flag <- TRUE
        }

        # b1 <- get_cell_borders(i + alcnt, 1, mx + alcnt + blcnt, 2, ttlhdr$borders)
        # b2 <- get_cell_borders(i + alcnt, 2, mx+ alcnt + blcnt, 2, ttlhdr$borders)


        if (tcnt > hcnt)
          cnt <- cnt + tcnt
        else
          cnt <- cnt + hcnt
      }

      dev.off()

    }

  }

  res <- list(pdf = ret,
              lines = cnt,
              points = cnt * lh,
              border_flag = border_flag)

  
  return(res)
}


#' Get page by text strings suitable for printing
#' @import stringi
#' @param titles Page by object
#' @param width The width to set the page by strings to
#' @return A vector of strings
#' @noRd
get_page_by_pdf <- function(pgby, width, value, rs, talgn, ystart = 0) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- ""

  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  lh <- rs$line_height

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
  
    yline <- ystart
    
    # Open device context
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)


    if (pgby$blank_row %in% c("above", "both")) {

      yline <- yline + lh
      cnt <- cnt + 1
    }

    # tb <- get_cell_borders(brow, 1 , trows, 1, pgby$borders)

    vl <- paste0( pgby$label, ": ", value)

    tmp <- split_string_text(vl, width, rs$units)
    
    
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
    }
    
    if (pgby$blank_row %in% c("below", "both")) {
      yline <- yline + lh
      cnt <- cnt + 1
      
    }
  }

  
  res <- list(pdf = ret, 
              lines = cnt,
              points = cnt * lh,
              border_flag = FALSE)
  
  return(res)
}

# Utilities ---------------------------------------------------------------


get_cell_borders_pdf <- function(startx, starty, col_widths, row_heights, brdrs, 
                                 flag = "", exclude = NULL) {
  
  t <- ""
  b <- ""
  l <- ""
  r <- ""
  
  
  # if ("all" %in% brdrs) {
  #   t <- "\\clbrdrt\\brdrs"
  #   b <- "\\clbrdrb\\brdrs"
  #   l <- "\\clbrdrl\\brdrs"
  #   r <- "\\clbrdrr\\brdrs"
  # } else {
  #   
  #   if ("inside" %in% brdrs) {
  #     
  #     t <- "\\clbrdrt\\brdrs"
  #     b <- "\\clbrdrb\\brdrs"
  #     l <- "\\clbrdrl\\brdrs"
  #     r <- "\\clbrdrr\\brdrs"
  #     
  #     if (col == 1) 
  #       l <- ""
  #     
  #     if (col == ncol)
  #       r <- ""
  #     
  #     if (row == nrow)
  #       b <- ""
  #     
  #     if (row == 1)
  #       t <- ""
  #     
  #   }
  #   
  #   if (row == 1 & any(brdrs %in% c("outside", "top")))
  #     t <- "\\clbrdrt\\brdrs"
  #   
  #   if (row == nrow & any(brdrs %in% c("bottom", "outside")))
  #     b <- "\\clbrdrb\\brdrs"
  #   
  #   if (col == 1 & any(brdrs %in% c("outside", "left")))
  #     l <- "\\clbrdrl\\brdrs"
  #   
  #   if (col == ncol & any(brdrs %in% c("outside", "right")))
  #     r <- "\\clbrdrr\\brdrs"
  #   
  # }
  # 
  # # Deal with flag
  # if (!is.na(flag)) {
  #   if (flag %in% c("L", "B")) {
  #     
  #     if (col != ncol)
  #       r <- ""
  #     
  #     if (col != 1)
  #       l <- ""
  #   }
  # }
  # 
  # if (!is.null(exclude)) {
  #   if (any(exclude == "top"))
  #     t <- ""
  #   if (any(exclude == "bottom"))
  #     b <- ""
  #   if (any(exclude == "left"))
  #     l <- ""
  #   if (any(exclude == "right"))
  #     r <- ""
  # }
  # 
  # ret <- paste0(t, b, l, r)
  
  ret <- ""
  
  return(ret)
  
}

get_page_numbers_pdf <- function(txt, pg, tpg) {
  
  ret <- txt
  
  ret <- gsub("[pg]", pg, ret, fixed = TRUE)
  
  ret <- gsub("[tpg]", tpg, ret, fixed = TRUE)
  
  return(ret)
}
