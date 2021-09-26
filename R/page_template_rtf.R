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

#' @import grDevices
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
    
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    
    for (i in seq(1, maxh)) {
      ret <- paste0(ret, "\\trowd\\trgaph0\\trrh", lh, 
                    "\\cellx", c1, "\\cellx", c2)
      
      if (length(hl) >= i) {
        
        # Split strings if they exceed width
        tmp <- split_string_rtf(hl[[i]], rs$content_size[["width"]]/2, rs$units)
        
        ret <- paste0(ret, "\\ql ", get_page_numbers_rtf(tmp$rtf), "\\cell")
        lcnt <- tmp$lines
        
      } else {
        ret <- paste0(ret, "\\ql \\cell")
        lcnt <- 1 
      }
      
      if (length(hr) >= i) {
        
        # Split strings if they exceed width
        tmp2 <- split_string_rtf(hr[[i]], rs$content_size[["width"]]/2, rs$units)
        
        ret <- paste0(ret, "\\qr ", get_page_numbers_rtf(tmp2$rtf), "\\cell\\row\n")
        rcnt <- tmp2$lines
        
      } else {
        ret <- paste0(ret, "\\qr \\cell\\row\n")
        rcnt <- 1
      }
      
      if (lcnt > rcnt)
        cnt <- cnt + lcnt
      else 
        cnt <- cnt + rcnt
    }
    
    dev.off()
    
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

#' @import grDevices
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
    
    ret <- paste0("{\\footer \\fs", fs)
    
    pdf(NULL)
    par(family = get_font_family(rs$font), ps = rs$font_size)
    
    for (i in seq(1, maxf)) {
      
      ret <- paste0(ret, "\\trowd\\trgaph0\\cellx", c1, 
                    "\\cellx", c2 , "\\cellx", c3)
      
      if (length(fl) >= i) {
        
        # Split strings if they exceed width
        tmp1 <- split_string_rtf(fl[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, "\\ql ", get_page_numbers_rtf(tmp1$rtf), "\\cell")
        lcnt <- tmp1$lines
      } else {
        ret <- paste0(ret, "\\ql \\cell")
        lcnt <- 1
      }
      
      if (length(fc) >= i) {
        
        # Split strings if they exceed width
        tmp2 <- split_string_rtf(fc[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, "\\qc ", get_page_numbers_rtf(tmp2$rtf), "\\cell")
        ccnt <- tmp2$lines
      } else {
        ret <- paste0(ret, "\\qc \\cell")
        ccnt <- 1
      }
      
      if (length(fr) >= i) {
        
        tmp3 <- split_string_rtf(fr[[i]], rs$content_size[["width"]]/3, rs$units)
        
        ret <- paste0(ret, "\\qr ", get_page_numbers_rtf(tmp3$rtf), "\\cell\\row\n")
        rcnt <- tmp3$lines
      } else {
        ret <- paste0(ret, "\\qr \\cell\\row\n")
        rcnt <- 1
      }
      
      cnt <- cnt + max(lcnt, ccnt, rcnt)
      
    }
    dev.off()
    
    ret <- paste0(ret, "}")
  }
  
  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt, 
              twips = cnt * lh)
  
  return(res)
}

#' @import grDevices
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
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      for (i in seq_along(ttls$titles)) {
        
        b <- get_cell_borders(i, 1, length(ttls$titles), 1, ttls$borders)
        
        # Split title strings if they exceed width
        tmp <- split_string_rtf(ttls$titles[[i]], width, rs$units)
        
        # Concatenate title string
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b, "\\cellx", w, 
                                  algn, " ", tmp$rtf, "\\cell\\row\n"))

        cnt <- cnt + tmp$lines
      }
      dev.off()
      
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

#' @import grDevices
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
      
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      for (i in seq_along(ftnts$footnotes)) {
        
        b <- get_cell_borders(i, 1, length(ftnts$footnotes), 1, ftnts$borders)
        
        # Split footnote strings if they exceed width
        tmp <- split_string_rtf(ftnts$footnotes[[i]], width, rs$units)
        
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b, "\\cellx", w, 
                                  algn, " ", tmp$rtf, "\\cell\\row\n"))
        cnt <- cnt + tmp$lines
      }
      dev.off()
      
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

#' @import grDevices
#' @noRd
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
      
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      for(i in seq_len(mx)) {
      
        if (length(ttlhdr$titles) >= i) {
          # Split strings if they exceed width
          tmp1 <- split_string_rtf(ttlhdr$titles[[i]], width * .7, rs$units)
          ttl <- tmp1$rtf
          tcnt <- tmp1$lines
        } else {
          ttl <- ""
          tcnt <- 1 
        }
        
        if (length(ttlhdr$right) >= i) {
          tmp2 <- split_string_rtf(ttlhdr$right[[i]],
                                   width * .3, rs$units)
          hdr <- get_page_numbers_rtf(tmp2$rtf, FALSE)
          hcnt <- tmp2$lines
        } else {
          hdr <- ""
          hcnt <- 1
        }
        
        b1 <- get_cell_borders(i, 1, mx, 2, ttlhdr$borders)
        b2 <- get_cell_borders(i, 2, mx, 2, ttlhdr$borders)
        
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b1, "\\cellx", w2, 
                                  b2, "\\cellx", w1,
                                  "\\ql ", ttl, "\\cell\\qr ",
                                  hdr, "\\cell\\row\n"))
        if (tcnt > hcnt)
          cnt <- cnt + tcnt
        else 
          cnt <- cnt + hcnt
      }
      
      dev.off()
      
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
get_page_by_rtf <- function(pgby, width, value, rs, talgn) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- ""
  
  ll <- width
  ret <- c()
  cnt <- 0
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
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
    
    ret[length(ret) + 1] <- paste0("\\trowd\\trgaph0", ta, "\\cellx", w1, algn, " ",
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
