# The page template is everything except the content: page header/footer,
# titles, footnotes, etc.

# Page Template RTF Functions ---------------------------------------------


#' Create a page template with header, titles, footnotes, and footer.
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
  
  # User controlled width of left column
  lwdth <- rs$page_header_width
  if (is.null(lwdth))
    lwdth <- rs$content_size[["width"]]/2
  
  # Calculate right column width
  rwdth <- rs$content_size[["width"]] - lwdth
  
  maxh <- max(length(hl), length(hr))
  
  if (maxh > 0 | length(rs$header_titles) > 0) {
    
    fs <- rs$font_size * 2
    
    c1 <- round(lwdth * conv) 
    c2 <- round(rwdth * conv) + c1
    
    ret <- paste0("{\\header \\f0\\fs", fs)
    
    if (maxh > 0) {
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      for (i in seq(1, maxh)) {
        ret <- paste0(ret, "\\trowd\\trgaph0\\trrh", lh, 
                      "\\cellx", c1, "\\cellx", c2)
        
        if (length(hl) >= i) {
          
          # Split strings if they exceed width
          tmp <- split_string_rtf(hl[[i]], lwdth, rs$units)
          
          ret <- paste0(ret, "\\ql ", get_page_numbers_rtf(tmp$rtf), "\\cell")
          lcnt <- tmp$lines
          
        } else {
          ret <- paste0(ret, "\\ql \\cell")
          lcnt <- 1 
        }
        
        if (length(hr) >= i) {
          
          # Split strings if they exceed width
          tmp2 <- split_string_rtf(hr[[i]], rwdth, rs$units)
          
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
        ret <- paste0(ret, "\\par\\pard", rs$font_rtf, rs$spacing_multiplier)
        cnt <- cnt + 1
      } else {
        
        ret <- paste0(ret, "\\fs1\\sl0\\par\\pard", rs$font_rtf, 
                      rs$spacing_multiplier) 
      }
    }
    
    htitles <- ""
    if (!is.null(rs$header_titles)) {
    
      tret <- get_titles_rtf(rs$header_titles, rs$content_size[["width"]], rs)
      htitles <- paste0(tret$rtf, "\\fs1\\sl0\\par\\pard", rs$font_rtf, 
                                              rs$spacing_multiplier) 
      cnt <- cnt + tret$lines
    
    }
    
    ret <- paste0(ret, htitles, "}")
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
  
  if (maxf > 0 | length(rs$footer_footnotes) > 0) {
    
    fs <- rs$font_size * 2
    
    c3 <- round(rs$content_size[["width"]] * conv)
    c1 <- round(c3 / 3)
    c2 <- round(c1 * 2)
    
    ret <- paste0("{\\footer \\f0\\fs", fs, "[ff]")
    
    if (maxf > 0) {
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
    }
    
    
    ffootnotes <- ""
    if (!is.null(rs$footer_footnotes)) {
      
      tret <- get_footnotes_rtf(rs$footer_footnotes, rs$content_size[["width"]], rs)
      ffootnotes <- paste0(tret$rtf, "\\fs1\\sl0\\par\\pard", rs$font_rtf, 
                        rs$spacing_multiplier) 
      cnt <- cnt + tret$lines
      
    }
    
    ret <- sub("[ff]", ffootnotes, ret, fixed = TRUE)
    
    ret <- paste0(ret, "\\fs1\\sl0\\par\\pard", 
                  rs$font_rtf, rs$spacing_multiplier, "}")
  }
  

  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt, 
              twips = cnt * lh)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_titles_rtf <- function(ttllst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  
  conv <- rs$twip_conversion
  lh <- rs$row_height
  border_flag <- FALSE

  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"

  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      cols <- ttls$columns
      
      
      if (ttls$width == "page")
        width <- rs$content_size[["width"]] 
      else if (ttls$width == "content") 
        width <- content_width 
      else if (is.numeric(ttls$width))
        width <- ttls$width 
        
      w <- round(width * conv)
      
      cwidth <- width / cols
      cw <- round(w / cols)
      

      if (ttls$align == "center")
        algn <- "\\qc"
      else if (ttls$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      border_flag <- FALSE
      alcnt <- 0
      blcnt <- 0
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      
      al <- ""
      # Get blank row above
      if (any(ttls$blank_row %in% c("above", "both"))) {
        
        alcnt <- 1
        
        tb <- get_cell_borders(1, 1, length(ttls$titles) + alcnt, 1, ttls$borders)
        
        al <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w, 
                     algn, "\\cell\\row\n")
        cnt <- cnt + 1 
        
      }
      
      bl <- ""
      # Get blank row below
      if (any(ttls$blank_row %in% c("below", "both"))) {
        blcnt <- 1
        
        tb <- get_cell_borders(length(ttls$titles) + alcnt + blcnt, 1, 
                               length(ttls$titles) + alcnt + blcnt, 
                               1, ttls$borders)
        
        sm <- get_spacing_multiplier(rs$font_size)
        
        bl <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w, 
                     algn, sm, "\\cell\\row\n")
        cnt <- cnt + 1
      }
      
      # Append blank row above
      if (al != "")
        ret <- append(ret, al)
      
      fz <- ""
      fs <- ""
      # Get font size
      if (!is.null(ttls$font_size)) {
        
        fz <- paste0("\\fs", ttls$font_size * 2, 
                     get_spacing_multiplier(ttls$font_size)) 
        fs <- paste0("\\fs", rs$font_size * 2)
      }
      
      # Reset column width accumulation
      cwa <- 0
      
      # Calculate total number of rows in this block
      rws <- ceiling(length(ttls$titles) / cols) + alcnt + blcnt

      i <- 1
      while (i <= length(ttls$titles)) {    
        
        mxlns <- 0
        
        # Calculate current row
        rw <- ceiling(i / cols)

        # Start row
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta))
        
        for (j in seq_len(cols)) {
          
          # Get border specs for this cell
          b <- get_cell_borders(rw + alcnt, j, 
                                rws, 
                                cols, ttls$borders)
          
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
             calgn <- "\\ql"
           else 
             calgn <- "\\qr"
          } else if (cols == 3) {
           if (j == 1)
             calgn <- "\\ql"
           else if (j == 2)
             calgn <- "\\qc"
           else if (j == 3) 
             calgn <- "\\qr"
          }
          
          if (j == 1) 
            cwa <- 0
        
          # RTF cell widths are absolute ending points
          cwa <- cwa + cw
          
          # Split title strings if they exceed width
          tmp <- split_string_rtf(vl, cwidth, rs$units)
          
          # Track max lines for counting
          if (tmp$lines > mxlns)
            mxlns <- tmp$lines

          # Add bold if needed
          tb <- tmp$rtf
          if (ttls$bold)
            tb <- paste0("\\b ", tmp$rtf, "\\b0")
          
          # Contruct cell from constituent parts
          ret <- append(ret, paste0(b, "\\cellx", cwa, 
                                    calgn, fz, " ", tb, fs, "\\cell"))
          
          i <- i + 1
          
        }
        
        # End row
        ret <- append(ret, "\\row\n")

        # Track lines
        cnt <- cnt + mxlns
      }
      
      # Append blank row below
      if (bl != "")
        ret <- append(ret, bl)
      
      if (any(ttls$borders %in% c("outside", "all", "bottom")))
        border_flag <- TRUE
      
      dev.off()
    
    }
    
  }
  
  res <- list(rtf = paste0(ret, collapse = ""), 
              lines = cnt, 
              twips = cnt * lh,
              border_flag = border_flag)
  
  return(res)
}



#' @import grDevices
#' @noRd
get_titles_rtf_back <- function(ttllst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  
  conv <- rs$twip_conversion
  lh <- rs$row_height
  border_flag <- FALSE
  
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  if (length(ttllst) > 0) {
    
    for (ttls in ttllst) {
      
      
      if (ttls$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttls$width == "content")
        width <- content_width
      else if (is.numeric(ttls$width))
        width <- ttls$width
      
      w <- round(width * conv)
      
      
      if (ttls$align == "center")
        algn <- "\\qc"
      else if (ttls$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
      border_flag <- FALSE
      alcnt <- 0
      blcnt <- 0
      
      # Open device context
      pdf(NULL)
      par(family = get_font_family(rs$font), ps = rs$font_size)
      
      for (i in seq_along(ttls$titles)) {
        
        
        
        al <- ""
        
        if (i == 1) {
          if (any(ttls$blank_row %in% c("above", "both"))) {
            
            alcnt <- 1
            
            tb <- get_cell_borders(i, 1, length(ttls$titles) + alcnt, 1, ttls$borders)
            
            al <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w, 
                         algn, "\\cell\\row\n")
            cnt <- cnt + 1 
            
          }
        }
        
        bl <- ""
        if (i == length(ttls$titles)) {
          if (any(ttls$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            tb <- get_cell_borders(i + alcnt + blcnt, 1, 
                                   length(ttls$titles) + alcnt + blcnt, 
                                   1, ttls$borders)
            
            sm <- get_spacing_multiplier(rs$font_size)
            
            bl <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w, 
                         algn, sm, "\\cell\\row\n")
            cnt <- cnt + 1
          }
          
          if (any(ttls$borders %in% c("outside", "all", "bottom")))
            border_flag <- TRUE
        }
        
        b <- get_cell_borders(i + alcnt, 1, 
                              length(ttls$titles) + alcnt + blcnt, 
                              1, ttls$borders)
        
        # Split title strings if they exceed width
        tmp <- split_string_rtf(ttls$titles[[i]], width, rs$units)
        
        fz <- ""
        fs <- ""
        if (!is.null(ttls$font_size)) {
          
          fz <- paste0("\\fs", ttls$font_size * 2, 
                       get_spacing_multiplier(ttls$font_size)) 
          fs <- paste0("\\fs", rs$font_size * 2)
        }
        
        
        tb <- tmp$rtf
        if (ttls$bold)
          tb <- paste0("\\b ", tmp$rtf, "\\b0")
        
        # Concatenate title string
        if (al != "")
          ret <- append(ret, al)
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b, "\\cellx", w, 
                                  algn, fz, " ", tb, fs, "\\cell\\row\n"))
        if (bl != "")
          ret <- append(ret, bl)
        
        cnt <- cnt + tmp$lines
      }
      dev.off()
      
    }
    
  }
  
  res <- list(rtf = paste0(ret, collapse = ""), 
              lines = cnt, 
              twips = cnt * lh,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_footnotes_rtf <- function(ftnlst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  border_flag <- FALSE
  
  conv <- rs$twip_conversion
  lh <- rs$row_height
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  if (length(ftnlst) > 0) {
    
    for (ftnts in ftnlst) {
      
      if (ftnts$width == "page")
        width <- rs$content_size[["width"]]
      else if (ftnts$width == "content")
        width <- content_width
      else if (is.numeric(ftnts$width))
        width <- ftnts$width
      
      w <- round(width * conv)
      
      
      if (ftnts$align == "center")
        algn <- "\\qc"
      else if (ftnts$align == "right")
        algn <- "\\qr"
      else 
        algn <- "\\ql"
      
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
            
            tb <- get_cell_borders(i, 1, length(ftnts$footnotes) + alcnt, 
                                   1, ftnts$borders)
            
            al <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w, 
                         algn, "\\cell\\row\n")
            cnt <- cnt + 1 
            
          }
        }
        
        bl <- ""
        if (i == length(ftnts$footnotes)) {
          if (any(ftnts$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            tb <- get_cell_borders(i + alcnt + blcnt, 1, 
                                   length(ftnts$footnotes) + alcnt + blcnt, 
                                   1, ftnts$borders)
            
            bl <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w, 
                         algn, "\\cell\\row\n")
            cnt <- cnt + 1
          }
          if (any(ftnts$borders %in% c("outside", "all", "top")))
            border_flag <- TRUE
        }
        
        b <- get_cell_borders(i + alcnt, 1, 
                              length(ftnts$footnotes) + alcnt + blcnt, 
                              1, ftnts$borders)
        
        
        
        # Split footnote strings if they exceed width
        tmp <- split_string_rtf(ftnts$footnotes[[i]], width, rs$units)
        
        if (al != "")
          ret <- append(ret, al)
        
        # Concat footnote row
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b, "\\cellx", w, 
                                  algn, " ", get_page_numbers_rtf(tmp$rtf, FALSE), 
                                  "\\cell\\row\n"))
        if (bl != "")
          ret <- append(ret, bl)
        
        cnt <- cnt + tmp$lines
      }
      dev.off()
      

    }
    
  }
  
  
  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt, 
              twips = cnt * lh,
              border_flag = border_flag)
  
  return(res)
}

#' @import grDevices
#' @noRd
get_title_header_rtf <- function(thdrlst, content_width, rs, talgn = "center") {
  
  ret <- c()
  cnt <- 0
  twps <- 0
  border_flag <- FALSE 
  
  conv <- rs$twip_conversion

  lh <- rs$row_height
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  if (length(thdrlst) > 0) {
    
    for (ttlhdr in thdrlst) {
      
      if (ttlhdr$width == "page")
        width <- rs$content_size[["width"]]
      else if (ttlhdr$width == "content")
        width <- content_width
      else if (is.numeric(ttlhdr$width))
        width <- ttlhdr$width
      
      w1 <- round(width * conv)
      w2 <- round(width * .7 * conv)
      
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
            
            tb <- get_cell_borders(i, 1, mx + alcnt, 
                                   1, ttlhdr$borders)
            
            al <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w1, 
                         "\\ql\\cell\\row\n")
            cnt <- cnt + 1 
            
          }
        }
        
        bl <- ""
        if (i == mx) {
          if (any(ttlhdr$blank_row %in% c("below", "both"))) {
            blcnt <- 1
            
            tb <- get_cell_borders(i + alcnt + blcnt, 1, 
                                   mx + alcnt + blcnt, 
                                   1, ttlhdr$borders)
            
            bl <- paste0("\\trowd\\trgaph0", ta, tb, "\\cellx", w1, 
                         "\\ql\\cell\\row\n")
            cnt <- cnt + 1
          }
          
          if (any(ttlhdr$borders %in% c("all", "outside", "bottom")))
            border_flag <- TRUE
        }
        
        
      
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
        
        b1 <- get_cell_borders(i + alcnt, 1, mx + alcnt + blcnt, 2, ttlhdr$borders)
        b2 <- get_cell_borders(i + alcnt, 2, mx+ alcnt + blcnt, 2, ttlhdr$borders)
        
        
        if (al != "")
          ret <- append(ret, al)
        
        ret <- append(ret, paste0("\\trowd\\trgaph0", ta, b1, "\\cellx", w2, 
                                  b2, "\\cellx", w1,
                                  "\\ql ", ttl, "\\cell\\qr ", 
                                  hdr, "\\cell\\row\n"))
        if (bl != "")
          ret <- append(ret, bl)
        
        if (tcnt > hcnt)
          cnt <- cnt + tcnt
        else 
          cnt <- cnt + hcnt
      }
      
      dev.off()
      
    }

  }
  
  res <- list(rtf = paste0(ret, collapse = ""),
              lines = cnt,
              twips = cnt * lh,
              border_flag = border_flag)
  
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
  border_flag <- FALSE
  
  ta <- "\\trql"
  if (talgn == "right")
    ta <- "\\trqr"
  else if (talgn %in% c("center", "centre"))
    ta <- "\\trqc"
  
  if (!is.null(pgby)) { 
    
    if (!any(class(pgby) == "page_by"))
      stop("pgby parameter value is not a page_by.")
    
    
    w1 <- round(width * rs$twip_conversion)
    
    algn <- "\\ql"
    if (pgby$align == "right")
      algn <- "\\qr"
    else if (pgby$align %in% c("center", "centre"))
      algn <- "\\qc"
    
    trows <- 1
    brow <- 1
    if (pgby$blank_row %in% c("above", "both")) {
      trows <- trows + 1
      brow <- 2
    }
    if (pgby$blank_row %in% c("below", "both"))
      trows <- trows + 1
    
    if (pgby$blank_row %in% c("above", "both")) {
      
      tb <- get_cell_borders(1, 1, trows, 1, pgby$borders)

      ret[length(ret) + 1] <- paste0("\\trowd\\trgaph0", ta, tb, 
                                     "\\cellx", w1, algn, 
                                  "\\cell\\row\n")
      cnt <- cnt + 1 
    }
    
    tb <- get_cell_borders(brow, 1 , trows, 1, pgby$borders)
    
    ret[length(ret) + 1] <- paste0("\\trowd\\trgaph0", ta, tb, 
                                   "\\cellx", w1, algn, " ",
                              pgby$label, value, "\\cell\\row\n")
    
    
    cnt <- cnt + get_lines_rtf(paste0( pgby$label, ": ", value), width,
                               rs$font, rs$font_size, rs$units)
    
  
    if (pgby$blank_row %in% c("below", "both")) {
      
      tb <- get_cell_borders(trows, 1, trows, 1, pgby$borders)
      
      ret[length(ret) + 1] <- paste0("\\trowd\\trgaph0", ta, tb, 
                                     "\\cellx", w1, algn, 
                                  "\\cell\\row\n")
      cnt <- cnt + 1 
    }
    
    if (any(pgby$borders %in% c("all", "outside", "bottom")))
      border_flag <- TRUE
    
  }
  
  res <- list(rtf = paste0(ret, collapse = ""), 
              lines = cnt, 
              twips = cnt * rs$line_height,
              border_flag = border_flag)
  
  return(res)
}

# Utilities ---------------------------------------------------------------


get_cell_borders <- function(row, col, nrow, ncol, brdrs, flag = "", exclude = NULL) {
  
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
  
  # Deal with flag
  if (!is.na(flag)) {
    if (flag %in% c("L", "B")) {
      
      if (col == 1 & any(brdrs %in% c("outside", "right", "all")))
        r <- "\\clbrdrr\\brdrs"
      
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

get_page_numbers_rtf <- function(val, tpg = TRUE) {
  
  ret <- val
  
  ret <- gsub("[pg]", "\\chpgn ", ret, fixed = TRUE)
  
  if (tpg)
    ret <- gsub("[tpg]", "{\\field{\\*\\fldinst  NUMPAGES }}", ret, fixed = TRUE)

  return(ret)
}
