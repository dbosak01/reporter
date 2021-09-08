
# Page Template RTF Functions ---------------------------------------------


#' @noRd
get_page_header_rtf <- function(rs) {
  
  ret <- ""
  cnt <- 0
  twps <- 0
  
  hl <- rs$page_header_left
  hr <- rs$page_header_right
  conv <- rs$twip_conversion
  lh <- rs$line_height
  
  maxh <- max(length(hl), length(hr))
  
  if (maxh > 0) {
    
    fs <- rs$font_size * 2
    
    c2 <- rs$content_size[["width"]] * conv
    c1 <- c2 / 2
    
    ret <- paste0("{\\header \\fs", fs)
    
    cnt <- maxh
    
    for (i in seq(1, maxh)) {
      ret <- paste0(ret, "\\trowd\\trgaph0\\trrh", lh, 
                    "\\cellx", c1, "\\cellx", c2 , " ")
      
      if (length(hl) >= i) {
        ret <- paste0(ret, hl[[i]], "\\cell")
        cnt <- cnt + get_excess_lines(hl[[i]], rs$content_size[["width"]], 
                                      rs$font, 
                                      rs$font_size, rs$units)
        
      } else 
        ret <- paste0(ret, "\\cell")
      
      if (length(hr) >= i) {
        ret <- paste0(ret, "\\qr ", hr[[i]], "\\cell\\row\n")
        cnt <- cnt + get_excess_lines(hr[[i]], rs$content_size[["width"]], 
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
  
  fl <- rs$page_footer_left
  fc <- rs$page_footer_center
  fr <- rs$page_footer_right
  conv <- rs$twip_conversion
  
  maxf <- max(length(fl), length(fc), length(fr))
  
  if (maxf > 0) {
    
    fs <- rs$font_size * 2
    
    c3 <- rs$content_size[["width"]] * conv
    c1 <- c3 / 3
    c2 <- c1 * 2
    
    ret <- paste0("{\\footer \\fs", fs)
    
    for (i in seq(1, maxf)) {
      
      ret <- paste0(ret, "\\trowd\\trgaph0\\cellx", c1, 
                    "\\cellx", c2 , "\\cellx", c3, " ")
      
      if (length(fl) >= i)
        ret <- paste0(ret, fl[[i]], "\\cell")
      else 
        ret <- paste0(ret, "\\cell")
      
      if (length(fc) >= i)
        ret <- paste0(ret, "\\qc ", fc[[i]], "\\cell")
      else 
        ret <- paste0(ret, "\\qc \\cell")
      
      if (length(fr) >= i)
        ret <- paste0(ret, "\\qr ", fr[[i]], "\\cell\\row\n")
      else 
        ret <- paste0(ret, "\\qr \\cell\\row\n")
      
    }
    
    ret <- paste0(ret, "}")
  }
  
  return(ret)
}

#' @noRd
get_titles_rtf <- function(ttllst, width, rs) {
  
  ret <- c()
  cnt <- 0
  
  conv <- rs$twip_conversion
  
  w <- width * conv
  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      if (any(ttls$blank_row %in% c("above", "both")))
        ret <- append(ret, "\\line\n")
      
      if (ttls$align == "center")
        algn <- "\\qc"
      else if (ttls$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      for (ttl in ttls$titles) {
        
        ret <- append(ret, paste0("\\trowd\\trgaph0\\cellx", w, 
                                  algn, " ", ttl, "\\cell\\row\n"))
      }
      
      
      if (any(ttls$blank_row %in% c("below", "both")))
        ret <- append(ret, "\\line\n")
      
    }
    
    ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  }
  
  res <- list(rtf = paste0(ret, collapse = ""), lines = cnt)
  
  return(ret)
}

#' @noRd
get_footnotes_rtf <- function(ftnlst, width, conv) {
  
  ret <- c()
  
  w <- width * conv
  
  if (length(ftnlst) > 0) {
    
    for (ftnts in ftnlst) {
      
      
      if (any(ftnts$blank_row %in% c("above", "both")))
        ret <- append(ret, "\\line\n")
      
      if (ftnts$align == "center")
        algn <- "\\qc"
      else if (ftnts$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      for (ftn in ftnts$footnotes) {
        
        ret <- append(ret, paste0("\\trowd\\trgaph0\\cellx", w, 
                                  algn, " ", ftn, "\\cell\\row\n"))
      }
      
      if (any(ftnts$blank_row %in% c("below", "both")))
        ret <- append(ret, "\\line\n")
    }
    
    ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  }
  
  return(ret)
}


get_title_header_rtf <- function(thdrlst, width, conv) {
  
  ret <- c()
  
  w <- width * conv
  
  if (length(thdrlst) > 0) {
    
    for (ttlhdr in thdrlst) {
      
      mx <- max(length(ttlhdr$titles), length(ttlhdr$right))
      
      if (any(ttlhdr$blank_row %in% c("above", "both")))
        ret <- append(ret, "\\line\n")
      
      for(i in seq_len(mx)) {
      
      
        if (!is.null(ttlhdr$titles[[i]]))
          ttl <- ttlhdr$titles[[i]]
        else 
          ttl <- ""
        
        if (!is.null(ttlhdr$right[[i]]))
          hdr <- ttlhdr$right[[i]]
        else 
          hdr <- ""
        
        ret <- append(ret, paste0("\\trowd\\trgaph0\\cellx", w, 
                                  "\\ql ", ttl, "\\cell\\qr ",
                                  hdr, "\\cell\\row\n"))
      }
      
      if (any(ttlhdr$blank_row %in% c("below", "both")))
        ret <- append(ret, "\\line\n")
    }
    
    ret[length(ret)] <- paste0(ret[length(ret)], "\\pard")
  }
  
  return(ret)
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
