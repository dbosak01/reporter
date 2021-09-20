# The page template is everything except the content: page header/footer,
# titles, footnotes, etc.

# Page Template RTF Functions ---------------------------------------------


#' Create a page template with header, titles, footnotes, and footer
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_rtf<- function(rs) {
  
  pt <- structure(list(), class = c("page_template_rtf", "list"))
  
  pt$page_header <- get_page_header_rtf(rs)
  pt$title_hdr <- get_title_header_rtf(rs$title_hdr, rs$line_size, rs)
  pt$titles <- get_titles_rtf(rs$titles, rs$line_size, rs)
  pt$footnotes <- c()
  if (!is.null(rs$footnotes)) {
    if (!is.null(rs$footnotes[[1]])) {
      if (rs$footnotes[[1]]$valign == "bottom")
        pt$footnotes <- get_footnotes_rtf(rs$footnotes, rs$line_size, rs)
    }
    
  }
  pt$page_footer <- get_page_footer_rtf(rs)
  
  pt$lines <- sum(pt$page_header$lines, pt$page_footer$lines,
                  pt$title_hdr$lines, pt$titles$lines, pt$footnotes$lines)
  
  pt$twips <- sum(pt$page_header$twips, pt$page_footer$twips,
                  pt$title_hdr$twips, pt$titles$twips, pt$footnotes$twips)
  
  # Page by not here.  Messes up line counts.
  
  return(pt)
}

#' @noRd
get_page_header_rtf <- function(rs) {
  
  ret <- ""
  cnt <- 0
  twps <- 0
  
  hl <- rs$page_header_left
  hr <- rs$page_header_right
  conv <- rs$twip_conversion
  lh <- rs$row_height
  
  maxh <- max(length(hl), length(hr))
  
  if (maxh > 0) {
    
    fs <- rs$font_size * 2
    
    c2 <- round(rs$content_size[["width"]] * conv)
    c1 <- round(c2 / 2)
    
    ret <- paste0("{\\header \\fs", fs)
    
    cnt <- maxh
    
    for (i in seq(1, maxh)) {
      ret <- paste0(ret, "\\trowd\\trgaph0\\trrh", lh, 
                    "\\cellx", c1, "\\cellx", c2 , " ")
      
      if (length(hl) >= i) {
        ret <- paste0(ret, get_page_numbers_rtf(hl[[i]]), "\\cell")
        cnt <- cnt + get_excess_lines(hl[[i]], rs$content_size[["width"]]/2, 
                                      rs$font, 
                                      rs$font_size, rs$units)
        
      } else 
        ret <- paste0(ret, "\\cell")
      
      if (length(hr) >= i) {
        ret <- paste0(ret, "\\qr ", get_page_numbers_rtf(hr[[i]]), "\\cell\\row\n")
        cnt <- cnt + get_excess_lines(hr[[i]], rs$content_size[["width"]]/2, 
                                      rs$font, 
                                      rs$font_size, rs$units)
      } else 
        ret <- paste0(ret, "\\qr \\cell\\row\n")
      
    }
    
    if (rs$page_header_blank_row == "below") {
      ret <- paste0(ret, "\\par\\pard")
      cnt <- cnt + 1
    }
    
    ret <- paste0(ret, "}")
  }
  
  twps <- cnt * lh
  
  res <- list(rtf = ret, lines = cnt, twips = twps)
  
  return(res)
}


#' @noRd
get_page_footer_rtf <- function(rs) {
  
  ret <- ""
  cnt <- 0
  twps <- 0
  
  fl <- rs$page_footer_left
  fc <- rs$page_footer_center
  fr <- rs$page_footer_right
  conv <- rs$twip_conversion
  lh <- rs$row_height
  
  maxf <- max(length(fl), length(fc), length(fr))
  
  if (maxf > 0) {
    
    fs <- rs$font_size * 2
    
    c3 <- round(rs$content_size[["width"]] * conv)
    c1 <- round(c3 / 3)
    c2 <- round(c1 * 2)
    
    cnt <- maxf
    
    ret <- paste0("{\\footer \\fs", fs)
    
    for (i in seq(1, maxf)) {
      
      ret <- paste0(ret, "\\trowd\\trgaph0\\cellx", c1, 
                    "\\cellx", c2 , "\\cellx", c3, " ")
      
      if (length(fl) >= i) {
        ret <- paste0(ret, get_page_numbers_rtf(fl[[i]]), "\\cell")
        cnt <- cnt + get_excess_lines(fl[[i]], rs$content_size[["width"]] / 3,
                                      rs$font, rs$font_size, rs$units)
      } else 
        ret <- paste0(ret, "\\cell")
      
      if (length(fc) >= i) {
        ret <- paste0(ret, "\\qc ", get_page_numbers_rtf(fc[[i]]), "\\cell")
        cnt <- cnt + get_excess_lines(fc[[i]], rs$content_size[["width"]] / 3,
                                      rs$font, rs$font_size, rs$units)
      } else 
        ret <- paste0(ret, "\\qc \\cell")
      
      if (length(fr) >= i) {
        ret <- paste0(ret, "\\qr ", get_page_numbers_rtf(fr[[i]]), "\\cell\\row\n")
        cnt <- cnt + get_excess_lines(fr[[i]], rs$content_size[["width"]] / 3,
                                      rs$font, rs$font_size, rs$units)
      } else 
        ret <- paste0(ret, "\\qr \\cell\\row\n")
      
    }
    
    ret <- paste0(ret, "}")
  }
  
  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt, 
              twips = cnt * lh)
  
  return(res)
}

#' @noRd
get_titles_rtf <- function(ttllst, width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  
  conv <- rs$twip_conversion
  lh <- rs$row_height
  
  w <- round(width * conv)
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"

  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      if (any(ttls$blank_row %in% c("above", "both"))) {
        ret <- append(ret, "\\line\n")
        cnt <- cnt + 1 
      }
      
      if (ttls$align == "center")
        algn <- "\\qc"
      else if (ttls$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      for (i in seq_along(ttls$titles)) {
        
        b <- get_cell_borders(i, 1, length(ttls$titles), 1, ttls$borders)
        
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b, "\\cellx", w, 
                                  algn, " ", ttls$titles[[i]], "\\cell\\row\n"))
        cnt <- cnt + 1
        cnt <- cnt + get_excess_lines(ttls$titles[[i]], width, rs$font, 
                                      rs$font_size, rs$units)
      }
      
      
      if (any(ttls$blank_row %in% c("below", "both"))) {
        ret <- append(ret, "\\par\n")
        cnt <- cnt + 1
      }
        
      
    }
    
    ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  }
  
  res <- list(rtf = paste0(ret, collapse = ""), 
              lines = cnt, 
              twips = cnt * lh)
  
  return(res)
}

#' @noRd
get_footnotes_rtf <- function(ftnlst, width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  
  w <- round(width * rs$twip_conversion)
  lh <- rs$row_height
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  if (length(ftnlst) > 0) {
    
    for (ftnts in ftnlst) {
      
      
      if (any(ftnts$blank_row %in% c("above", "both"))) {
        ret <- append(ret, "\\line\n")
        cnt <- cnt + 1 
      }
      
      if (ftnts$align == "center")
        algn <- "\\qc"
      else if (ftnts$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      for (i in seq_along(ftnts$footnotes)) {
        
        b <- get_cell_borders(i, 1, length(ftnts$footnotes), 1, ftnts$borders)
        
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b, "\\cellx", w, 
                                  algn, " ", ftnts$footnotes[[i]], "\\cell\\row\n"))
        cnt <- cnt + 1
        cnt <- cnt + get_excess_lines(ftnts$footnotes[[i]], w, rs$font, 
                                      rs$font_size, rs$units)
      }
      
      if (any(ftnts$blank_row %in% c("below", "both"))) {
        ret <- append(ret, "\\line\n")
        cnt <- cnt + 1
      }
    }
    
    ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  }
  
  
  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt, 
              twips = cnt * lh)
  
  return(res)
}


get_title_header_rtf <- function(thdrlst, width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  
  w1 <- round(width * rs$twip_conversion)
  w2 <- round(width * .7 * rs$twip_conversion)
  lh <- rs$row_height
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  if (length(thdrlst) > 0) {
    
    for (ttlhdr in thdrlst) {
      
      mx <- max(length(ttlhdr$titles), length(ttlhdr$right))
      
      if (any(ttlhdr$blank_row %in% c("above", "both"))) {
        ret <- append(ret, "\\par\n")
        cnt <- cnt + 1
      }
      
      for(i in seq_len(mx)) {
      
        if (length(ttlhdr$titles) >= i)
          ttl <- ttlhdr$titles[[i]]
        else 
          ttl <- ""
        
        if (length(ttlhdr$right) >= i)
          hdr <- get_page_numbers_rtf(ttlhdr$right[[i]], FALSE)
        else 
          hdr <- ""
        
        b1 <- get_cell_borders(i, 1, mx, 2, ttlhdr$borders)
        b2 <- get_cell_borders(i, 2, mx, 2, ttlhdr$borders)
        
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b1, "\\cellx", w2, 
                                  b2, "\\cellx", w1,
                                  "\\ql ", ttl, "\\cell\\qr ",
                                  hdr, "\\cell\\row\n"))
        cnt <- cnt + 1
        cnt <- cnt + get_excess_lines(ttl, width * .7, 
                                      rs$font, rs$font_size, rs$units)
        cnt <- cnt + get_excess_lines(hdr, width * .3, 
                                      rs$font, rs$font_size, rs$units)
      }
      
      if (any(ttlhdr$blank_row %in% c("below", "both"))) {
        ret <- append(ret, "\\par\n")
        cnt <- cnt + 1
      }
    }
    
    ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  }
  
  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt,
              twips = cnt * lh)
  
  return(res)
}


#' Get page by text strings suitable for printing
#' @import stringi
#' @param titles Page by object
#' @param width The width to set the page by strings to
#' @return A vector of strings
#' @noRd
get_page_by_rtf <- function(pgby, width, value, rs) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- ""
  
  ll <- width
  ret <- c()
  cnt <- 0
  
  if (!is.null(pgby)) { 
    
    if (!any(class(pgby) == "page_by"))
      stop("pgby parameter value is not a page_by.")
    
    if (pgby$blank_row %in% c("above", "both")) {
      ret[length(ret) + 1] <- "\\par\n"
      cnt <- cnt + 1 
    }
    
    algn <- "\\ql"
    if (pgby$align == "right")
      algn <- "\\qr"
    else if (pgby$align %in% c("center", "centre"))
      algn <- "\\qc"
    
    
    w1 <- round(width * rs$twip_conversion)
    
    ret[length(ret) + 1] <- paste0("\\trowd\\trgaph0\\cellx", w1, algn, " ",
                              pgby$label, value, "\\cell\\row\n")
    
    
    cnt <- cnt + get_lines_rtf(paste0( pgby$label, ": ", value), width,
                               rs$font, rs$font_size, rs$units)
    
  
    if (pgby$blank_row %in% c("below", "both")) {
      ret[length(ret) + 1] <- "\\par\n"
      cnt <- cnt + 1 
    }
    
    
  }
  
  res <- list(rtf = paste0(ret, collapse = ""), 
              lines = cnt, 
              twips = cnt * rs$line_height)
  
  return(res)
}

# Utilities ---------------------------------------------------------------


get_cell_borders <- function(row, col, nrow, ncol, brdrs) {
  
  t <- ""
  b <- ""
  l <- ""
  r <- ""
  
  if ("all" %in% brdrs) {
    t <- "\\clbrdrt\\brdrs"
    b <- "\\clbrdrb\\brdrs"
    l <- "\\clbrdrl\\brdrs"
    r <- "\\clbrdrr\\brdrs"
  } else {
    
    if ("inside" %in% brdrs) {
      
      t <- "\\clbrdrt\\brdrs"
      b <- "\\clbrdrb\\brdrs"
      l <- "\\clbrdrl\\brdrs"
      r <- "\\clbrdrr\\brdrs"
      
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
      t <- "\\clbrdrt\\brdrs"
    
    if (row == nrow & any(brdrs %in% c("bottom", "outside")))
      b <- "\\clbrdrb\\brdrs"
    
    if (col == 1 & any(brdrs %in% c("outside", "left")))
      l <- "\\clbrdrl\\brdrs"
    
    if (col == ncol & any(brdrs %in% c("outside", "right")))
      r <- "\\clbrdrr\\brdrs"
    
  }
  
  ret <- paste0(t, b, l, r)
  
  return(ret)
  
}

get_page_numbers_rtf <- function(val, tpg = TRUE) {
  
  ret <- val
  
  ret <- gsub("[pg]", "\\chpgn ", ret, fixed = TRUE)
  
  if (tpg)
    ret <- gsub("[tpg]", "{\\field{\\*\\fldinst  NUMPAGES }}", ret, fixed = TRUE)

  return(ret)
}
