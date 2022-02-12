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
      ret <- paste0(ret, 
            '<w:tr><w:trPr><w:trHeight w:hRule="exact" w:val="288"/></w:trPr>')

      if (length(hl) >= i) {

        # Split strings if they exceed width
        tmp <- split_string_html(hl[[i]], rs$content_size[["width"]]/2, rs$units)
        
        ret <- paste0(ret, 
                      '<w:tc><w:tcPr><w:tcW w:w="2500" w:type="pct"/></w:tcPr>', 
                      get_page_numbers_docx(para(tmp$html)),
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
                      get_page_numbers_docx(para(tmp2$html, "right")), 
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
      ret <- paste0(ret, "<w:p/>")
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

      ret <- paste0(ret, 
            '<w:tr><w:trPr><w:trHeight w:hRule="exact" w:val="288"/></w:trPr>')

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

    ret <- paste0(get_page_numbers_docx(ret), "</w:tr>\n")
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
  conv <- rs$twip_conversion
  rht <- get_row_height(288)

  
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

      w <- round(width, 3) * conv
      
      if (ttls$align %in% c("centre", "center"))
        algn <- "center"
      else if (ttls$align == "right")
        algn <- "right"
      else
        algn <- "left"
      
      alcnt <- 0
      blcnt <- 0
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      
      tb <- get_table_borders_docx(ttls$borders)
      
      ret[length(ret) + 1] <- paste0("<w:tbl>",
                                     "<w:tblPr>",
                                     "<w:tblW w:w=\"", w, "\"/>", tb,
                                     "</w:tblPr>")
      
      for (i in seq_along(ttls$titles)) {
        
        
        
        al <- ""
        if (i == 1) {
          if (any(ttls$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            # tb <- get_cell_borders_html(i, 1, length(ttls$titles) + alcnt, 
            #                        1, ttls$borders)
            
            # if (tb == "")
              al <- paste0("<w:tr>", rht, 
                    "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            # else 
            #   al <- paste0("<w:tr><w:tc style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }
        }
        
        bl <- ""
        if (i == length(ttls$titles)) {
          if (any(ttls$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            # tb <- get_cell_borders_html(i + alcnt + blcnt, 1,
            #                        length(ttls$titles) + alcnt + blcnt,
            #                        1, ttls$borders)

            # if (tb == "")
              bl <- paste0("<w:tr>", rht, 
                    "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            # else 
            #   bl <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }

        }
        
        # b <- get_cell_borders_html(i + alcnt, 1,
        #                       length(ttls$titles) + alcnt + blcnt,
        #                       1, ttls$borders)
        
        # Split title strings if they exceed width
        tmp <- split_string_html(ttls$titles[[i]], width, rs$units)
        
        # if (ttls$bold)
        #   tstr <- paste0("<b>", encodeHTML(tmp$html), "</b>")
        # else 
          tstr <- para(tmp$html, algn)
        
        # fz <- ""
        # if (!is.null(ttls$font_size)){
        #   
        #   fz <- paste0("font-size:", ttls$font_size, "pt;") 
        # }
        
        
        # Concatenate title string
        if (al != "")
          ret <- append(ret, al)
        
        # if (b == "" & fz == "") {
          ret <- append(ret, paste0("<w:tr>", rht, "<w:tc>", tstr, 
                                    "</w:tc></w:tr>\n"))
        # } else {
        #   
        #   ret <- append(ret, paste0("<tr><td style=\"", b, fz, "\">", 
        #                             tstr, 
        #                             "</td></tr>\n"))
        # }
          
        if (bl != "")
          ret <- append(ret, bl)
        
        cnt <- cnt + tmp$lines
        
        # A flag to indicate that this block has bottom borders.  
        # Used to eliminate border duplication on subsequent blocks.
        if ("bottom" %in% get_outer_borders(ttls$borders))
          border_flag <- TRUE
      }
      
      ret[length(ret) + 1] <- paste0("</w:tbl>", rs$table_break)
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
                               ex_brdr = FALSE) {
  
  ret <- c()
  cnt <- 0
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"
  
  conv <- rs$twip_conversion
  rht <- get_row_height(288)

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

      w <- round(width, 3) * conv


      if (ftnts$align %in% c("centre", "center"))
        algn <- "center"
      else if (ftnts$align == "right")
        algn <- "right"
      else
        algn <- "left"

      alcnt <- 0
      blcnt <- 0


      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      tb <- get_table_borders_docx(ftnts$borders)
      
      ret[length(ret) + 1] <- paste0("<w:tbl>",
                                     "<w:tblPr>",
                                     "<w:tblW w:w=\"", w, "\"/>", tb,
                                     "</w:tblPr>")

      for (i in seq_along(ftnts$footnotes)) {


        al <- ""
        if (i == 1) {
          if (any(ftnts$blank_row %in% c("above", "both"))) {

            alcnt <- 1
          
            # tb <- get_cell_borders_html(i, 1, length(ftnts$footnotes) + alcnt,
            #                        1, ftnts$borders, exclude = exclude_top)
            
            # if (tb == "")
              al <- paste0("<w:tr>", rht, 
                    "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            # else 
            #   al <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")

            cnt <- cnt + 1

          }
        }

        bl <- ""
        if (i == length(ftnts$footnotes)) {
          if (any(ftnts$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            # tb <- get_cell_borders_html(i + alcnt + blcnt, 1,
            #                        length(ftnts$footnotes) + alcnt + blcnt,
            #                        1, ftnts$borders)
            
            # if (tb == "")
              bl <- paste0("<w:tr>", rht, 
                    "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            # else 
            #   bl <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }

        }
# 
#         b <- get_cell_borders_html(i + alcnt, 1,
#                               length(ftnts$footnotes) + alcnt + blcnt,
#                               1, ftnts$borders, exclude = exclude_top)



        # Split footnote strings if they exceed width
        tmp <- split_string_html(ftnts$footnotes[[i]], width, rs$units)

        if (al != "")
          ret <- append(ret, al)

        # if (b == "")
          ret <- append(ret, paste0("<w:tr>", rht, 
                                   "<w:tc>", para(tmp$html, algn), 
                                    "</w:tc></w:tr>\n"))
        # else {
        #   ret <- append(ret, paste0("<tr><td style=\"", b, "\">", 
        #                             encodeHTML(tmp$html), 
        #                             "</td></tr>\n"))
        # }


        if (bl != "")
          ret <- append(ret, bl)

        cnt <- cnt + tmp$lines
      }
      ret[length(ret) + 1] <- "</w:tbl>"
      dev.off()


    }
    
    ret <- get_page_numbers_docx(ret)

  }

  
  res <- list(docx = paste0(ret, collapse = ""),
              lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_title_header_docx <- function(thdrlst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  conv <- rs$twip_conversion
  rht <- get_row_height(288)
  
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

      w <- round(width, 3) * conv

      mx <- max(length(ttlhdr$titles), length(ttlhdr$right))

      alcnt <- 0
      blcnt <- 0

      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      
      
      tb <- get_table_borders_docx(ttlhdr$borders)
      
      # ret[length(ret) + 1] <- paste0("<w:tbl>",
      #                                "<w:tblPr>",
      #                                "<w:tblW w:w=\"", w, "\"/>", tb,
      #                                "</w:tblPr>")
      
      ret[length(ret) + 1] <- paste0("<w:tbl>", 
                                     "<w:tblPr>",
                              '<w:tblStyle w:val="TableGrid"/>',
                              '<w:tblW w:w="', w, '"/>', tb,
                              "</w:tblPr>",
                              "<w:tblGrid>",
                              '<w:gridCol w:w="3500" w:type="pct"/>',
                              '<w:gridCol w:w="1500" w:type="pct"/>',
                              "</w:tblGrid>")
      
      
      # ret[length(ret) + 1] <- paste0("<table ",
      #                              #  ta,
      #                                "style=\"width:", w, u, ";", 
      #                                "\">\n", 
      #                                "<colgroup><col style=\"width:70%;\">\n",
      #                                "<col style=\"width:30%;\"></colgroup>\n")
                                     

      for(i in seq_len(mx)) {

        al <- ""
        if (i == 1) {
          if (any(ttlhdr$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            # tb1 <- get_cell_borders_html(i, 1, mx + alcnt,
            #                        2, ttlhdr$borders)
            # tb2 <- get_cell_borders_html(i, 2, mx + alcnt,
            #                             2, ttlhdr$borders)

            # al <- paste0("<tr><td style=\"text-align:left;", tb1, "\">&nbsp;</td>", 
            #              "<td style=\"text-align:right;", tb2, 
            #              "\">&nbsp;</td></tr>\n")
            
            al <- paste0("<w:tr>", rht, 
                  "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
            cnt <- cnt + 1

          }
        }

        bl <- ""
        if (i == mx) {
          if (any(ttlhdr$blank_row %in% c("below", "both"))) {
            blcnt <- 1

            # tb1 <- get_cell_borders_html(i + alcnt + blcnt, 1,
            #                        mx + alcnt + blcnt,
            #                        2, ttlhdr$borders)
            # tb2 <- get_cell_borders_html(i + alcnt + blcnt, 2,
            #                             mx + alcnt + blcnt,
            #                             2, ttlhdr$borders)
            
            # bl <- paste0("<tr><td style=\"text-align:left;", tb1, "\">&nbsp;</td>", 
            #              "<td style=\"text-align:right;", tb2, 
            #              "\">&nbsp;</td></tr>\n")
            
            bl <- paste0("<w:tr>", rht, 
                  "<w:tc><w:p><w:r><w:t></w:t></w:r></w:p></w:tc></w:tr>\n")
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
          #hdr <- get_page_numbers_html(tmp2$html, FALSE)
          #hdr <- tmp2$html
          
          hdr <- tmp2$html
          hcnt <- tmp2$lines
        } else {
          hdr <- ""
          hcnt <- 1
        }

        # b1 <- get_cell_borders_html(i + alcnt, 1, mx + alcnt + blcnt, 
        #                             2, ttlhdr$borders)
        # b2 <- get_cell_borders_html(i + alcnt, 2, mx+ alcnt + blcnt, 
        #                             2, ttlhdr$borders)


        if (al != "")
          ret <- append(ret, al)
        
        
        ret <- append(ret, paste0("<w:tr>", rht, 
                                   cell_pct(ttl, "left", 3500), 
                                   cell_pct(hdr, "right", 1500), 
                                  "</w:tr>\n"))
        
        # ret <- append(ret, paste0("<tr><td style=\"text-align:left;", b1, "\">",
        #                           encodeHTML(ttl), 
        #                           "</td><td style=\"text-align:right;", b2, "\">", 
        #                           encodeHTML(hdr), 
        #                           "</td></tr>\n"))
        
        if (bl != "")
          ret <- append(ret, bl)

        if (tcnt > hcnt)
          cnt <- cnt + tcnt
        else
          cnt <- cnt + hcnt
        
        if ("bottom" %in% get_outer_borders(ttlhdr$borders))
          border_flag <- TRUE
      }

      ret[length(ret) + 1] <- paste0('</w:tbl>\n', rs$table_break)
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

get_page_numbers_docx <- function(val, tpg = TRUE) {
  
  ret <- val
  
  ret <- gsub("[pg]", get_page_number_docx(), ret, fixed = TRUE)
  
  if (tpg)
    ret <- gsub("[tpg]", get_total_pages_docx(), ret, fixed = TRUE)
  
  return(ret)
}


get_row_height <- function(hgt) {
  
  ret <- paste0('<w:trPr><w:trHeight w:hRule="exact" w:val="', hgt, 
                  '"/></w:trPr>')
  
  return(ret)
  
}


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
