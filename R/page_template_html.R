# The page template is everything except the content: page header/footer,
# titles, footnotes, etc.

# Page Template HTML Functions ---------------------------------------------


#' Create a page template with header, titles, footnotes, and footer.
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_html <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_html", "list"))
  
  pt$page_header <- get_page_header_html(rs)
  pt$title_hdr <- get_title_header_html(rs$title_hdr, rs$line_size, rs)
  pt$titles <- get_titles_html(rs$titles, rs$line_size, rs)
  pt$footnotes <- c()
  if (!is.null(rs$footnotes)) {
    if (!is.null(rs$footnotes[[1]])) {
      if (rs$footnotes[[1]]$valign == "bottom")
        pt$footnotes <- get_footnotes_html(rs$footnotes, rs$line_size, rs)
    }
    
  }
  pt$page_footer <- get_page_footer_html(rs)
  
  pt$lines <- sum(pt$page_header$lines, pt$page_footer$lines,
                  pt$title_hdr$lines, pt$titles$lines, pt$footnotes$lines)
  
  
  # Page by not here.  Messes up line counts.
  
  return(pt)
}

#' @import grDevices
#' @noRd
get_page_header_html <- function(rs) {
  
  ret <- ""
  cnt <- 0
  
  hl <- rs$page_header_left
  hr <- rs$page_header_right
  maxh <- max(length(hl), length(hr))
  
  u <- rs$units
  if (rs$units == "inches")
    u <- "in"

  if (maxh > 0) {


    ret <- paste0("<table ",
                  "style=\"width:100%\">",
                  "<colgroup>\n<col style=\"width:50%\">\n",
                  "<col style=\"width:50%\">\n</colgroup>\n")

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)


    for (i in seq(1, maxh)) {
      ret <- paste0(ret, "<tr>")

      if (length(hl) >= i) {

        # Split strings if they exceed width
        tmp <- split_string_html(hl[[i]], rs$content_size[["width"]]/2, rs$units)
        
        ret <- paste0(ret, "<td style=\"text-align:left\">", encodeHTML(tmp$html),
                           "</td>\n")
        
        lcnt <- tmp$lines  

      } else {
        ret <- paste0(ret, "<td style=\"text-align:left\">&nbsp</td>\n")
        lcnt <- 1
      }

      if (length(hr) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_html(hr[[i]], rs$content_size[["width"]]/2, rs$units)

        
        ret <- paste0(ret, "<td style=\"text-align:right\">", encodeHTML(tmp2$html), 
                           "</td></tr>\n")
        
        rcnt <- tmp2$lines 

      } else {
        ret <- paste0(ret, "<td style=\"text-align:right\">&nbsp</td></tr>\n")
        rcnt <- 1
      }

      if (lcnt > rcnt)
        cnt <- cnt + lcnt
      else
        cnt <- cnt + rcnt
    }
    
    ret <- paste0(ret, "</table>")

    dev.off()

    if (rs$page_header_blank_row == "below") {
      ret <- paste0(ret, "<br>")
      cnt <- cnt + 1
    }
  }

  
  res <- list(html = ret, lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_page_footer_html <- function(rs) {
  
  ret <- ""
  cnt <- 1

  fl <- rs$page_footer_left
  fc <- rs$page_footer_center
  fr <- rs$page_footer_right

  maxf <- max(length(fl), length(fc), length(fr))

  if (maxf > 0) {

    ret <- paste0("<br>\n<table ",
                  "style=\"width:100%\">\n",
                  "<colgroup>\n<col style=\"width:33.3%\">\n",
                  "<col style=\"width:33.3%\">\n",
                  "<col style=\"width:33.3%\">\n</colgroup>\n")

    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)

    for (i in seq(1, maxf)) {

      ret <- paste0(ret, "<tr>")

      if (length(fl) >= i) {

        # Split strings if they exceed width
        tmp1 <- split_string_html(fl[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, "<td style=\"text-align:left\">", encodeHTML(tmp1$html),
                           "</td>")
        lcnt <- tmp1$lines
      } else {
        ret <- paste0(ret, "<td style=\"text-align:left\">&nbsp;</td>")
        lcnt <- 1
      }

      if (length(fc) >= i) {

        # Split strings if they exceed width
        tmp2 <- split_string_html(fc[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, "<td style=\"text-align:center\">", encodeHTML(tmp2$html),
                           "</td>")
        ccnt <- tmp2$lines
      } else {
        ret <- paste0(ret, "<td style=\"text-align:center\">&nbsp;</td>")
        ccnt <- 1
      }

      if (length(fr) >= i) {

        tmp3 <- split_string_html(fr[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, "<td style=\"text-align:right\">", encodeHTML(tmp3$html),
                      "</td>")
        
        rcnt <- tmp3$lines
      } else {
        ret <- paste0(ret, "<td style=\"text-align:right\">&nbsp;</td>")
        rcnt <- 1
      }

      cnt <- cnt + max(lcnt, ccnt, rcnt)

    }
    dev.off()

    ret <- paste0(ret, "</tr>\n")
  }

  ret <- paste0(ret, "</table>\n")
  
  res <- list(html = paste0(ret, collapse = ""),
              lines = cnt)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_titles_html <- function(ttllst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE

  # ta <- "align=\"left\" "
  # if (talgn == "right")
  #   ta <- "align=\"right\" "
  # else if (talgn %in% c("center", "centre"))
  #   ta <- "align=\"center\" "
  
  sty <- paste0(get_style_html(rs, "title_font_color"),
                get_style_html(rs, "title_background"),
                get_style_html(rs, "title_font_bold"),
                get_style_html(rs, "title_font_size"))
  
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
                                     algn, sty,
                                     "\">\n")
      
      for (i in seq_along(ttls$titles)) {
        
        
        
        al <- ""
        if (i == 1) {
          if (any(ttls$blank_row %in% c("above", "both"))) {

            alcnt <- 1

            tb <- get_cell_borders_html(i, 1, length(ttls$titles) + alcnt, 
                                   1, ttls$borders, 
                                   border_color = get_style(rs, "border_color"))
            
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
                                   1, ttls$borders,
                                   border_color = get_style(rs, "border_color"))

            if (tb == "")
              bl <- "<tr><td>&nbsp;</td></tr>\n"
            else 
              bl <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }

        }
        
        b <- get_cell_borders_html(i + alcnt, 1,
                              length(ttls$titles) + alcnt + blcnt,
                              1, ttls$borders,
                              border_color = get_style(rs, "border_color"))
        
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
get_footnotes_html <- function(ftnlst, content_width, rs, talgn = "center", 
                               ex_brdr = FALSE) {
  
  ret <- c()
  cnt <- 0
  exclude_top <- NULL
  if (ex_brdr)
    exclude_top <- "top"

  u <- rs$units
  if (rs$units == "inches")
    u <- "in"
  
  sty <- paste0(get_style_html(rs, "footnote_font_color"),
                get_style_html(rs, "footnote_background"),
                get_style_html(rs, "footnote_font_bold"))
  
  
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
                                     algn, sty,
                                     "\">\n")

      for (i in seq_along(ftnts$footnotes)) {


        al <- ""
        if (i == 1) {
          if (any(ftnts$blank_row %in% c("above", "both"))) {

            alcnt <- 1
          
            tb <- get_cell_borders_html(i, 1, length(ftnts$footnotes) + alcnt,
                                   1, ftnts$borders, exclude = exclude_top,
                                   border_color = get_style(rs, "border_color"))
            
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
                                   1, ftnts$borders,
                                   border_color = get_style(rs, "border_color"))
            
            if (tb == "")
              bl <- "<tr><td>&nbsp;</td></tr>\n"
            else 
              bl <- paste0("<tr><td style=\"", tb, "\">&nbsp;</td></tr>\n")
            
            cnt <- cnt + 1
          }

        }

        b <- get_cell_borders_html(i + alcnt, 1,
                              length(ftnts$footnotes) + alcnt + blcnt,
                              1, ftnts$borders, exclude = exclude_top,
                              border_color = get_style(rs, "border_color"))



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
get_title_header_html <- function(thdrlst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  border_flag <- FALSE
  
  
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
                                   2, ttlhdr$borders,
                                   border_color = get_style(rs, "border_color"))
            tb2 <- get_cell_borders_html(i, 2, mx + alcnt,
                                        2, ttlhdr$borders,
                                        border_color = get_style(rs, "border_color"))

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
                                   2, ttlhdr$borders,
                                   border_color = get_style(rs, "border_color"))
            tb2 <- get_cell_borders_html(i + alcnt + blcnt, 2,
                                        mx + alcnt + blcnt,
                                        2, ttlhdr$borders,
                                        border_color = get_style(rs, "border_color"))
            
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
                                    2, ttlhdr$borders,
                                    border_color = get_style(rs, "border_color"))
        b2 <- get_cell_borders_html(i + alcnt, 2, mx+ alcnt + blcnt, 
                                    2, ttlhdr$borders,
                                    border_color = get_style(rs, "border_color"))


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
#' @return A vector of strings
#' @noRd
get_page_by_html <- function(pgby, width, value, rs, talgn, ex_brdr = FALSE) {
  
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
                                  exclude = exclude_top, 
                                  border_color = get_style(rs, "border_color"))

      ret[length(ret) + 1] <- paste0("<tr><td style=\"", tb, 
                                     "\">&nbsp;</td></tr>\n")
      cnt <- cnt + 1
    }

    tb <- get_cell_borders_html(brow, 1 , trows, 1, pgby$borders, 
                                exclude = exclude_top, 
                                border_color = get_style(rs, "border_color"))

    ret[length(ret) + 1] <- paste0("<tr><td style=\"", tb, "\">",
                                   pgby$label, encodeHTML(value), "</td></tr>\n")
    cnt <- cnt + 1


    if (pgby$blank_row %in% c("below", "both")) {

      tb <- get_cell_borders_html(trows, 1, trows, 1, pgby$borders, 
                                  border_color = get_style(rs, "border_color"))

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
 get_cell_borders_html <- function(row, col, nrow, ncol, brdrs, 
                                  flag = "", exclude = NULL, 
                                  border_color = "",
                                  stub_flag = FALSE) {
  
  t <- ""
  b <- ""
  l <- ""
  r <- ""
  
  if (border_color == "")
    border_color <- "black"
  
  
  if ("all" %in% brdrs) {
    t <- paste0("border-top:thin solid ", border_color, ";")
    b <- paste0("border-bottom:thin solid ", border_color, ";")
    l <- paste0("border-left:thin solid ", border_color, ";")
    r <- paste0("border-right:thin solid ", border_color, ";")
    
    if (row > 1)
      t <- ""

    if (col < ncol & stub_flag == FALSE)
      r <- ""
    
    
  } else {
    
    if ("inside" %in% brdrs) {
      
      t <- ""
      b <- paste0("border-bottom:thin solid ", border_color, ";")
      l <- paste0("border-left:thin solid ", border_color, ";")
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
      t <- paste0("border-top:thin solid ", border_color, ";")
    
    if (row == nrow & any(brdrs %in% c("bottom", "outside")))
      b <- paste0("border-bottom:thin solid ", border_color, ";")
    
    if (col == 1 & any(brdrs %in% c("outside", "left")))
      l <- paste0("border-left:thin solid ", border_color, ";")
    
    if (col == ncol & any(brdrs %in% c("outside", "right")))
      r <- paste0("border-right:thin solid ", border_color, ";")
    
  }
  
  # Deal with flag
  # Flag is for special rows like blanks or labels
  if (!is.null(flag)) {
    if (flag %in% c("L", "B")) {
      
      if (stub_flag == FALSE & col == 1 & any(brdrs %in% c("outside", "all", "right")))
        r <- paste0("border-right:thin solid ", border_color, ";")
      else if (col != ncol & stub_flag == FALSE)
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

get_page_numbers_html <- function(val, tpg = TRUE) {
  
  ret <- val
  
  ret <- gsub("[pg]", "\\chpgn ", ret, fixed = TRUE)
  
  if (tpg)
    ret <- gsub("[tpg]", "{\\field{\\*\\fldinst  NUMPAGES }}", ret, fixed = TRUE)
  
  return(ret)
}
