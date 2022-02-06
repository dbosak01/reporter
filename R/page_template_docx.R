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
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"

  if (maxh > 0) {


    # ret <- paste0("<w:tbl> ",
    #               "<w:tblPr>",
    #               '<w:tblStyle w:val="TableGrid"/>',
    #               '<w:tblW w:w="', rs$content_size[["width"]] * conv, '"',
    #               ' w:type="pct"/>',
    #               "</w:tblPr>",
    #               "<w:tblGrid>",
    #               '<w:gridCol w:w="', rs$content_size[["width"]] * conv/ 2, '"/>',
    #               '<w:gridCol w:w="', rs$content_size[["width"]] * conv/ 2, '"/>',
    #               "</w:tblGrid>", collapse = "")
    
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

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)


    for (i in seq(1, maxh)) {
      ret <- paste0(ret, "<w:tr>")

      if (length(hl) >= i) {

        # Split strings if they exceed width
        tmp <- split_string_html(hl[[i]], rs$content_size[["width"]]/2, rs$units)
        
        ret <- paste0(ret, 
                      '<w:tc><w:tcPr><w:tcW w:w="2500" w:type="pct"/></w:tcPr>', 
                      para(tmp$html),
                           "</w:tc>\n")
        
        lcnt <- tmp$lines  

      } else {
        ret <- paste0(ret, 
                      '<w:tc><w:tcPr><w:tcW w:w="2500" w:type="pct"/></w:tcPr>', 
                      para(" "), "</w:tc>\n")
        lcnt <- 1
      }

      if (length(hr) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_html(hr[[i]], rs$content_size[["width"]]/2, rs$units)

        
        ret <- paste0(ret, 
                      '<w:tc><w:tcPr><w:tcW w:w="2500" w:type="pct"/></w:tcPr>', 
                      para(tmp2$html, "right"), 
                      "</w:tc></w:tr>\n")
        
        rcnt <- tmp2$lines 

      } else {
        ret <- paste0(ret, 
                      '<w:tc><w:tcPr><w:tcW w:w="2500" w:type="pct"/></w:tcPr>', 
                      para(" ", "right"), "</w:tc></w:tr>\n")
        rcnt <- 1
      }

      if (lcnt > rcnt)
        cnt <- cnt + lcnt
      else
        cnt <- cnt + rcnt
    }
    
    ret <- paste0(ret, "</w:tbl>")

    dev.off()

    if (rs$page_header_blank_row == "below") {
      ret <- paste0(ret, "<br>")
      cnt <- cnt + 1
    }
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

    for (i in seq(1, maxf)) {

      ret <- paste0(ret, "<w:tr>")

      if (length(fl) >= i) {

        # Split strings if they exceed width
        tmp1 <- split_string_html(fl[[i]], rs$content_size[["width"]]/3, rs$units)
        
        
        ret <- paste0(ret, cell_pct(tmp1$html, "left", 1667))
        
        lcnt <- tmp1$lines
      } else {
        ret <- paste0(ret, cell_pct(" ", "left", 1667))
        lcnt <- 1
      }

      if (length(fc) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_html(fc[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret,  cell_pct(tmp2$html, "center", 1666))
        ccnt <- tmp2$lines
      } else {
        ret <- paste0(ret,  cell_pct(" ", "center", 1666))
        ccnt <- 1
      }

      if (length(fr) >= i) {

        tmp3 <- split_string_html(fr[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, cell_pct(tmp3$html, "right", 1667))
        
        rcnt <- tmp3$lines
      } else {
        ret <- paste0(ret,  cell_pct(" ", "right", 1667))
        rcnt <- 1
      }

      cnt <- cnt + max(lcnt, ccnt, rcnt)

    }
    dev.off()

    ret <- paste0(ret, "</w:tr>\n")
    ret <- paste0(ret, "</w:tbl>\n")
  }


  
  res <- list(docx = paste0(ret, collapse = ""),
              lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_titles_docx <- function(ttllst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE

  # ta <- "align=\"left\" "
  # if (talgn == "right")
  #   ta <- "align=\"right\" "
  # else if (talgn %in% c("center", "centre"))
  #   ta <- "align=\"center\" "
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"
  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      
      if (ttls$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttls$width == "content")
        width <- content_width
      else if (is.numeric(ttls$width))
        width <- ttls$width

      w <- round(width, 3)
      
      if (ttls$align %in% c("centre", "center"))
        algn <- "text-align: center;"
      else if (ttls$align == "right")
        algn <- "text-align: right;"
      else
        algn <- "text-align: left;"
      
      alcnt <- 0
      blcnt <- 0
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      ret[length(ret) + 1] <- paste0("<table ",
                                     "style=\"width:", w, u, ";", 
                                     algn, 
                                     "\">\n")
      
      for (i in seq_along(ttls$titles)) {
        
        
        
        al <- ""
        if (i == 1) {
          if (any(ttls$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            tb <- get_cell_borders_html(i, 1, length(ttls$titles) + alcnt, 
                                   1, ttls$borders)
            
            if (tb == "")
              al <- "<tr><td>&nbsp;</td></tr>\n"
            else 
              al <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }
        }
        
        bl <- ""
        if (i == length(ttls$titles)) {
          if (any(ttls$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            tb <- get_cell_borders_html(i + alcnt + blcnt, 1,
                                   length(ttls$titles) + alcnt + blcnt,
                                   1, ttls$borders)

            if (tb == "")
              bl <- "<tr><td>&nbsp;</td></tr>\n"
            else 
              bl <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }

        }
        
        b <- get_cell_borders_html(i + alcnt, 1,
                              length(ttls$titles) + alcnt + blcnt,
                              1, ttls$borders)
        
        # Split title strings if they exceed width
        tmp <- split_string_html(ttls$titles[[i]], width, rs$units)
        
        if (ttls$bold)
          tstr <- paste0("<b>", encodeHTML(tmp$html), "</b>")
        else 
          tstr <- encodeHTML(tmp$html)
        
        fz <- ""
        if (!is.null(ttls$font_size)){
          
          fz <- paste0("font-size:", ttls$font_size, "pt;") 
        }
        
        
        # Concatenate title string
        if (al != "")
          ret <- append(ret, al)
        
        if (b == "" & fz == "") {
          ret <- append(ret, paste0("<tr><td>", tstr, 
                                    "</td></tr>\n"))
        } else {
          
          ret <- append(ret, paste0("<tr><td style=\"", b, fz, "\">", 
                                    tstr, 
                                    "</td></tr>\n"))
        }
          
        if (bl != "")
          ret <- append(ret, bl)
        
        cnt <- cnt + tmp$lines
        
        # A flag to indicate that this block has bottom borders.  
        # Used to eliminate border duplication on subsequent blocks.
        if ("bottom" %in% get_outer_borders(ttls$borders))
          border_flag <- TRUE
      }
      
      ret[length(ret) + 1] <- "</table>"
      dev.off()
      

    }
    
  }
  

  
  res <- list(html = paste0(ret, collapse = ""), 
              lines = cnt,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_footnotes_docx <- function(ftnlst, content_width, rs, talgn = "center", 
                               ex_brdr = FALSE) {
  
  ret <- c()
  cnt <- 0
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"

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

      w <- round(width, 3)


      if (ftnts$align %in% c("centre", "center"))
        algn <- "text-align: center;"
      else if (ftnts$align == "right")
        algn <- "text-align: right;"
      else
        algn <- "text-align: left;"

      alcnt <- 0
      blcnt <- 0


      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      ret[length(ret) + 1] <- paste0("<table ",
                                     "style=\"width:", w, u, ";",
                                     algn,
                                     "\">\n")

      for (i in seq_along(ftnts$footnotes)) {


        al <- ""
        if (i == 1) {
          if (any(ftnts$blank_row %in% c("above", "both"))) {

            alcnt <- 1
          
            tb <- get_cell_borders_html(i, 1, length(ftnts$footnotes) + alcnt,
                                   1, ftnts$borders, exclude = exclude_top)
            
            if (tb == "")
              al <- "<tr><td>&nbsp;</td></tr>\n"
            else 
              al <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")

            cnt <- cnt + 1

          }
        }

        bl <- ""
        if (i == length(ftnts$footnotes)) {
          if (any(ftnts$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            tb <- get_cell_borders_html(i + alcnt + blcnt, 1,
                                   length(ftnts$footnotes) + alcnt + blcnt,
                                   1, ftnts$borders)
            
            if (tb == "")
              bl <- "<tr><td>&nbsp;</td></tr>\n"
            else 
              bl <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }

        }

        b <- get_cell_borders_html(i + alcnt, 1,
                              length(ftnts$footnotes) + alcnt + blcnt,
                              1, ftnts$borders, exclude = exclude_top)



        # Split footnote strings if they exceed width
        tmp <- split_string_html(ftnts$footnotes[[i]], width, rs$units)

        if (al != "")
          ret <- append(ret, al)

        if (b == "")
          ret <- append(ret, paste0("<tr><td>", encodeHTML(tmp$html), 
                                    "</td></tr>\n"))
        else {
          ret <- append(ret, paste0("<tr><td style=\"", b, "\">", 
                                    encodeHTML(tmp$html), 
                                    "</td></tr>\n"))
        }


        if (bl != "")
          ret <- append(ret, bl)

        cnt <- cnt + tmp$lines
      }
      ret[length(ret) + 1] <- "</table>"
      dev.off()


    }

  }

  
  res <- list(html = paste0(ret, collapse = ""),
              lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_title_header_docx <- function(thdrlst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  
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
      else if (ttlhdr$width == "content")
        width <- content_width
      else if (is.numeric(ttlhdr$width))
        width <- ttlhdr$width

      w <- round(width, 3)

      mx <- max(length(ttlhdr$titles), length(ttlhdr$right))

      alcnt <- 0
      blcnt <- 0

      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      ret[length(ret) + 1] <- paste0("<table ",
                                   #  ta,
                                     "style=\"width:", w, u, ";", 
                                     "\">\n", 
                                     "<colgroup><col style=\"width:70%;\">\n",
                                     "<col style=\"width:30%;\"></colgroup>\n")
                                     

      for(i in seq_len(mx)) {

        al <- ""
        if (i == 1) {
          if (any(ttlhdr$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            tb1 <- get_cell_borders_html(i, 1, mx + alcnt,
                                   2, ttlhdr$borders)
            tb2 <- get_cell_borders_html(i, 2, mx + alcnt,
                                        2, ttlhdr$borders)

            al <- paste0("<tr><td style=\"text-align:left;", tb1, "\">&nbsp;</td>", 
                         "<td style=\"text-align:right;", tb2, 
                         "\">&nbsp;</td></tr>\n")
            cnt <- cnt + 1

          }
        }

        bl <- ""
        if (i == mx) {
          if (any(ttlhdr$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            tb1 <- get_cell_borders_html(i + alcnt + blcnt, 1,
                                   mx + alcnt + blcnt,
                                   2, ttlhdr$borders)
            tb2 <- get_cell_borders_html(i + alcnt + blcnt, 2,
                                        mx + alcnt + blcnt,
                                        2, ttlhdr$borders)
            
            bl <- paste0("<tr><td style=\"text-align:left;", tb1, "\">&nbsp;</td>", 
                         "<td style=\"text-align:right;", tb2, 
                         "\">&nbsp;</td></tr>\n")
            cnt <- cnt + 1
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
          hdr <- get_page_numbers_html(tmp2$html, FALSE)
          
          hdr <- tmp2$html
          hcnt <- tmp2$lines
        } else {
          hdr <- ""
          hcnt <- 1
        }

        b1 <- get_cell_borders_html(i + alcnt, 1, mx + alcnt + blcnt, 
                                    2, ttlhdr$borders)
        b2 <- get_cell_borders_html(i + alcnt, 2, mx+ alcnt + blcnt, 
                                    2, ttlhdr$borders)


        if (al != "")
          ret <- append(ret, al)
        
        ret <- append(ret, paste0("<tr><td style=\"text-align:left;", b1, "\">",
                                  encodeHTML(ttl), 
                                  "</td><td style=\"text-align:right;", b2, "\">", 
                                  encodeHTML(hdr), 
                                  "</td></tr>\n"))
        
        if (bl != "")
          ret <- append(ret, bl)

        if (tcnt > hcnt)
          cnt <- cnt + tcnt
        else
          cnt <- cnt + hcnt
        
        if ("bottom" %in% get_outer_borders(ttlhdr$borders))
          border_flag <- TRUE
      }

      ret[length(ret) + 1] <- "</table>\n"
      dev.off()
      


    }

  }
  

  
  res <- list(html = paste0(ret, collapse = ""),
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
get_page_by_docx <- function(pgby, width, value, rs, talgn, ex_brdr = FALSE) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- ""
  
  ll <- width
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"


  if (!is.null(pgby)) {

    if (!any(class(pgby) == "page_by"))
      stop("pgby parameter value is not a page_by.")

    w <- paste0("width:", round(width, 3), units_html(rs$units), ";")

    if (pgby$align %in% c("centre", "center"))
      algn <- "text-align: center;"
    else if (pgby$align == "right")
      algn <- "text-align: right;"
    else
      algn <- "text-align: left;"

    ret[length(ret) + 1] <- paste0("<table style=\"", algn, w, "\">\n")
    
    trows <- 1
    brow <- 1
    if (pgby$blank_row %in% c("above", "both")) {
      trows <- trows + 1
      brow <- 2
    }
    if (pgby$blank_row %in% c("below", "both"))
      trows <- trows + 1

    if (pgby$blank_row %in% c("above", "both")) {

      tb <- get_cell_borders_html(1, 1, trows, 1, pgby$borders, 
                                  exclude = exclude_top)

      ret[length(ret) + 1] <- paste0("<tr><td style=\"", tb, 
                                     "\">&nbsp;</td></tr>\n")
      cnt <- cnt + 1
    }

    tb <- get_cell_borders_html(brow, 1 , trows, 1, pgby$borders, 
                                exclude = exclude_top)

    ret[length(ret) + 1] <- paste0("<tr><td style=\"", tb, "\">",
                                   pgby$label, encodeHTML(value), "</td></tr>\n")
    cnt <- cnt + 1

    # cnt <- cnt + get_lines_rtf(paste0( pgby$label, ": ", value), width,
    #                            rs$font, rs$font_size, rs$units)


    if (pgby$blank_row %in% c("below", "both")) {

      tb <- get_cell_borders_html(trows, 1, trows, 1, pgby$borders)

      ret[length(ret) + 1] <- paste0("<tr><td style=\"", tb, 
                                     "\">&nbsp;</td></tr>\n")
      cnt <- cnt + 1
      

    }

    ret[length(ret) + 1] <- "</table>"
    
    if ("bottom" %in% get_outer_borders(pgby$borders))
      border_flag <- TRUE

  }
  
  
  res <- list(html = paste0(ret, collapse = ""), 
              lines = cnt,
              border_flag = border_flag)
  
  return(res)
}

# Utilities ---------------------------------------------------------------

#' @description Return border code for a particular cell.  Idea is 
#' you pass in the size of the table and the particular cell position,
#' and this function will return the correct border codes.  System works 
#' great.
#' @noRd
get_cell_borders_docx <- function(row, col, nrow, ncol, brdrs, 
                                  flag = "", exclude = NULL) {
  
  t <- ""
  b <- ""
  l <- ""
  r <- ""
  
  
  if ("all" %in% brdrs) {
    t <- "border-top:thin solid;"
    b <- "border-bottom:thin solid;"
    l <- "border-left:thin solid;"
    r <- "border-right:thin solid;"
    
    if (row > 1)
      t <- ""
    
    if (col < ncol)
      r <- ""
    
  } else {
    
    if ("inside" %in% brdrs) {
      
      t <- ""
      b <- "border-bottom:thin solid;"
      l <- "border-left:thin solid;"
      r <- ""
      
      if (col == 1) 
        l <- ""
      
      if (col == ncol)
        r <- ""
      
      if (row == nrow)
        b <- ""
      
      if (row == 1)
        t <- ""
      
    }
    
    if (row == 1 & any(brdrs %in% c("outside", "top")))
      t <- "border-top:thin solid;"
    
    if (row == nrow & any(brdrs %in% c("bottom", "outside")))
      b <- "border-bottom:thin solid;"
    
    if (col == 1 & any(brdrs %in% c("outside", "left")))
      l <- "border-left:thin solid;"
    
    if (col == ncol & any(brdrs %in% c("outside", "right")))
      r <- "border-right:thin solid;"
    
  }
  
  # Deal with flag
  # Flag is for special rows like blanks or labels
  if (!is.null(flag)) {
    if (flag %in% c("L", "B")) {
      
      if (col != ncol)
        r <- ""
      
      if (col != 1)
        l <- ""
    }
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
  
  ret <- paste0(t, b, l, r)
  
  return(ret)
  
}

get_page_numbers_docx <- function(val, tpg = TRUE) {
  
  ret <- val
  
  ret <- gsub("[pg]", "\\chpgn ", ret, fixed = TRUE)
  
  if (tpg)
    ret <- gsub("[tpg]", "{\\field{\\*\\fldinst  NUMPAGES }}", ret, fixed = TRUE)
  
  return(ret)
}
