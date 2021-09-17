
# Page Template Functions ----------------------------------------------

#' Create a page template with header, titles, footnotes, and footer
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_text <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_text", "list"))
  
  pt$page_header <- get_page_header(rs)
  pt$title_hdr <- get_title_header(rs$title_hdr, rs$line_size, rs$uchar)
  pt$titles <- get_titles(rs$titles, rs$line_size, rs$uchar)
  pt$footnotes <- c()
  if (!is.null(rs$footnotes)) {
    if (!is.null(rs$footnotes[[1]])) {
      if (rs$footnotes[[1]]$valign == "bottom")
        pt$footnotes <- get_footnotes(rs$footnotes, rs$line_size, rs$uchar)
    }
  
  }
  pt$page_footer <- get_page_footer(rs)
  # Page by not here.  Messes up line counts.
  
  return(pt)
}

#' Get page header text strings suitable for printing
#' @param rs The report spec
#' @return A vector of strings
#' @noRd
get_page_header <- function(rs) {
  
  if (is.null(rs$line_size)) {
    stop("line_size cannot be null.") 
    
  }
  
  phdrr <- rs$page_header_right
  phdrl <- rs$page_header_left
  phdr <- rs$page_header_left
  if(length(phdrl) < length(phdrr))
    phdr <- phdrr
  
  ret <- c()
  
  if (!is.null(phdr)) {
    for (i in seq_along(phdr)) {
      
      hl <- ""
      hr <- ""
      
      if (length(phdrl) >= i)
        hl <- phdrl[[i]]
      
      if (length(phdrr) >= i)
        hr <- phdrr[[i]]
      
      gp <- rs$line_size - (nchar(hl) + nchar(hr))
      
      #print("header")
      if (gp >= 0) {
        
        lw <- rs$line_size - nchar(hr)
        ln <- paste0(pad_right(hl, lw), hr) 
      }
      
      else {
        stop(paste0("Page header exceeds available width\n", 
                    "Header Left: ", hl, "\n", 
                    "Header Right: ", hr, "\n",
                    "Header length: ", nchar(hl) + nchar(hr), "\n",
                    "Line length: ", rs$line_size, "\n"))
        ln <- ""
      }
      
      ret[i] <- ln
    }
    
    if (length(ret) > 0 & rs$page_header_blank_row == "below") 
      ret[[length(ret) + 1]] <- "" 
    
  }
  
  return(ret)
}

#' Get title text strings suitable for printing
#' @import stringi
#' @param titles A list of title objects
#' @param width The width to set the title strings to
#' @return A vector of strings
#' @noRd
get_titles <- function(titles, width, uchar = "-") {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  

  ll <- width
  ret <- c()
  
  if (!is.null(titles)) { 
    
    for (ttl in titles) {
      
      if (!any(class(ttl) == "title_spec")) {
        stop("titles parameter value is not a title spec.")
      }
          
      if (ttl$blank_row %in% c("above", "both") & length(ttl$titles) > 0)
        ret[length(ret) + 1] <- ""
      
      if (any(ttl$borders %in% c("top", "all")) & length(ttl$titles) > 0)
        ret[length(ret) + 1] <-  paste0(rep(uchar, ll), 
                                               collapse = "")
      
      for (i in seq_along(ttl$titles)) {
      
        t <- ttl$titles[i]
        
        gp <- ll - nchar(t)
        
        #print("titles")
        if (gp > 0) {
          
          if (ttl$align == "left")
            ln <- pad_right(t, ll)
          else if (ttl$align == "right")
            ln <- pad_left(t, ll)
          else if (ttl$align == "center" | ttl$align == "centre")
            ln <- pad_both(t, ll)
          
        } else {
          warning(paste0("Title exceeds available width.",
                      "\nTitle: ", t,
                      "\nTitle width: ", nchar(t),
                      "\nLine length: ", ll))
          
          tgp <- ll - 3

          if (tgp >= 0) {
            if (ttl$align == "left") {
              ln <- paste0(substr(pad_right(t, ll), 1, tgp), "...")
            } else if (ttl$align == "right") {
              ln <- paste0("...", substr(pad_left(t, ll), 1, tgp))
            } else if (ttl$align == "center" | ttl$align == "centre") {
              ln <- paste0(substr(pad_both(t, ll), 1, tgp), "...")
            }
            
            
          } else ln <- ""
          
        }
        
        
        ret[length(ret) + 1] <- ln
      }
      
      if (any(ttl$borders %in% c("bottom", "all")) & length(ttl$titles) > 0)
        ret[length(ret) + 1] <- paste0(rep(uchar, ll ), 
                                              collapse = "")
      
      if (ttl$blank_row %in% c("below", "both") & length(ttl$titles) > 0)
        ret[length(ret) + 1] <- ""
    }
    
  }
  
  
  return(ret)
}


#' Get page by text strings suitable for printing
#' @import stringi
#' @param titles Page by object
#' @param width The width to set the page by strings to
#' @return A vector of strings
#' @noRd
get_page_by <- function(pgby, width, value) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  if (is.null(value))
    value <- ""
  
  ll <- width
  ret <- c()
  
  if (!is.null(pgby)) { 
    
    if (!any(class(pgby) == "page_by"))
      stop("pgby parameter value is not a page_by.")
    
    if (pgby$blank_row %in% c("above", "both"))
      ret[length(ret) + 1] <- ""
    

    pb <- paste0(pgby$label, value)
    
    gp <- ll - nchar(pb) 
    

    if (gp > 0) {
      
      if (pgby$align == "left")
        ln <- pad_right(pb, ll)
      else if (pgby$align == "right")
        ln <- pad_left(pb, ll)
      else if (pgby$align == "center" | pgby$align == "centre")
        ln <- pad_both(pb, ll)
      
    } else 
      stop("Page by exceeds available width.")
    
    
    ret[length(ret) + 1] <- ln
    
    
    if (pgby$blank_row %in% c("below", "both"))
      ret[length(ret) + 1] <- ""
    
    
  }
  
  
  return(ret)
}

#' Get title header text strings suitable for printing
#' @import stringi
#' @param title_hdr A title_hdr object
#' @param width The width to set the title header to
#' @return A vector of strings
#' @noRd
get_title_header <- function(title_hdr, width, uchar = "-") {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  
  ll <- width
  ret <- c()
  
  if (!is.null(title_hdr)) { 
    
    for (ttl_hdr in title_hdr) {
      
      if (!any(class(ttl_hdr) == "title_hdr"))
        stop("title header parameter value is not a title header.")
      
      if (ttl_hdr$blank_row %in% c("above", "both") & length(ttl_hdr$titles) > 0)
        ret[length(ret) + 1] <- ""
      
      if (any(ttl_hdr$borders %in% c("top", "all")) & length(ttl_hdr$titles) > 0)
        ret[length(ret) + 1] <-  paste0(paste0(rep(uchar, ll), collapse = ""), " ")
      
      maxlen <- length(ttl_hdr$titles)
      if (length(ttl_hdr$right) > maxlen)
        maxlen <- length(ttl_hdr$right)
      
      hdr <- ttl_hdr$right
      
      for (i in seq_len(maxlen)) {
        
        if (i <= length(ttl_hdr$titles))
          t <- ttl_hdr$titles[i]
        else 
          t <- ""
        
        if (i <= length(hdr))
          h <- hdr[i]
        else 
          h <- ""
        
        gp <- ll - nchar(t) - nchar(h)
        
        #print("titles")
        if (gp >= 0) {
          
  
            ln <- paste0(pad_right(t, ll - nchar(h)), h, " ")
  
          
        } else {
          warning(paste0("Title header exceeds available width.\n",
                      "Title: ", t, "\n",
                      "Header: ", h, "\n",
                      "Title length: ", nchar(t), "\n",
                      "Header length: ", nchar(h), "\n",
                      "Line length: ", ll, "\n"))
          
          tgp <- ll - 3
          if (tgp >= 0) {
            
            ln <- paste0(substr(paste0(pad_right(t, ll - nchar(h)), h, " "), 
                                1, tgp), "...")
            
          } else ln <- ""
        }
        
        
        ret[length(ret) + 1] <- ln
      }
      
      if (any(ttl_hdr$borders %in% c("bottom", "all")) & 
          length(ttl_hdr$titles) > 0)
        ret[length(ret) + 1] <-  paste0(paste0(rep(uchar, ll), collapse = ""), " ")
      
      if (ttl_hdr$blank_row %in% c("below", "both") & 
          length(ttl_hdr$titles) > 0)
        ret[length(ret) + 1] <- ""
  
    
    }
  }
  
  
  return(ret)
}



#' Get title header text strings suitable for printing
#' @import stringi
#' @param title_hdr A title_hdr object
#' @param width The width to set the title header to
#' @return A vector of strings
#' @noRd
get_title_header_back <- function(title_hdr, width, uchar = "-") {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  
  ll <- width
  ret <- c()
  
  if (!is.null(title_hdr)) { 
    
    if (!any(class(title_hdr) == "title_hdr"))
      stop("title header parameter value is not a title header.")
    
    if (title_hdr$blank_row %in% c("above", "both") & length(title_hdr$titles) > 0)
      ret[length(ret) + 1] <- ""
    
    if (any(title_hdr$borders %in% c("top", "all")) & length(title_hdr$titles) > 0)
      ret[length(ret) + 1] <-  paste0(paste0(rep(uchar, ll), collapse = ""), " ")
    
    maxlen <- length(title_hdr$titles)
    if (length(title_hdr$right) > maxlen)
      maxlen <- length(title_hdr$right)
    
    hdr <- title_hdr$right
    
    for (i in seq_len(maxlen)) {
      
      if (i <= length(title_hdr$titles))
        t <- title_hdr$titles[i]
      else 
        t <- ""
      
      if (i <= length(hdr))
        h <- hdr[i]
      else 
        h <- ""
      
      gp <- ll - nchar(t) - nchar(h)
      
      #print("titles")
      if (gp >= 0) {
        
        
        ln <- paste0(pad_right(t, ll - nchar(h)), h, " ")
        
        
      } else {
        warning(paste0("Title header exceeds available width.\n",
                       "Title: ", t, "\n",
                       "Header: ", h, "\n",
                       "Title length: ", nchar(t), "\n",
                       "Header length: ", nchar(h), "\n",
                       "Line length: ", ll, "\n"))
        
        tgp <- ll - 3
        if (tgp >= 0) {
          
          ln <- paste0(substr(paste0(pad_right(t, ll - nchar(h)), h, " "), 
                              1, tgp), "...")
          
        } else ln <- ""
      }
      
      
      ret[length(ret) + 1] <- ln
    }
    
    if (any(title_hdr$borders %in% c("bottom", "all")) & 
        length(title_hdr$titles) > 0)
      ret[length(ret) + 1] <-  paste0(paste0(rep(uchar, ll), collapse = ""), " ")
    
    if (title_hdr$blank_row %in% c("below", "both") & 
        length(title_hdr$titles) > 0)
      ret[length(ret) + 1] <- ""
    
    
  }
  
  
  return(ret)
}


#' Get footnote text strings suitable for printing
#' @param rs The report spec
#' @return A vector of strings
#' @noRd
get_footnotes <- function(footnotes, width, uchar = "-") {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  
  ll <- width
  ret <- c()
  
  if (!is.null(footnotes)) {
    for (ftn in footnotes) {
      
      if (!any(class(ftn) == "footnote_spec"))
        stop("footnotes parameter value is not a footnote spec.")
      
      if (ftn$blank_row %in% c("above", "both") & length(ftn$footnotes) > 0)
        ret[length(ret) + 1] <- ""
      
      if (any(ftn$borders %in% c("top", "all")) & length(ftn$footnotes) > 0)
        ret[length(ret) + 1] <- paste0(rep(uchar, ll), 
                                              collapse = "")
      
      for (i in seq_along(ftn$footnotes)) {
        
        f <- ftn$footnotes[i]
        
        gp <- ll - nchar(f)
        
        #print("footnotes")
        if (gp > 0) {
          
          if (ftn$align == "left")
            ln <- pad_right(f, ll)
          else if (ftn$align == "right")
            ln <- pad_left(f, ll)
          else if (ftn$align == "center" | ftn$align == "centre")
            ln <- pad_both(f, ll)
          
        } else {
          warning(paste0("Footnote exceeds available width.",
                      "\nFootnote: ", f,
                      "\nFootnote length: ", nchar(f), 
                      "\nLine Length: ", ll))
          
          tln <- ll - 3
          
          if (tln >= 0) {
            
            if (ftn$align == "left") {
              ln <- paste0(substr(pad_right(f , ll), 1, tln), "...")
            } else if (ftn$align == "right") {
              ln <- paste0("...", substr(pad_left(f, ll), 1, tln))
            } else if (ftn$align == "center" | ftn$align == "centre") {
              ln <- paste0(substr(pad_both(f, ll), 1, tln), "...")
            }
          } else ln <- "" 
        }
        
        
        ret[length(ret) + 1] <- ln
      }
      
      if (any(ftn$borders %in% c("bottom", "all")) & length(ftn$footnotes) > 0)
        ret[length(ret) + 1] <- paste0(rep(uchar, ll), 
                                              collapse = "")
      
      if (ftn$blank_row %in% c("below", "both") & length(ftn$footnotes) > 0)
        ret[length(ret) + 1] <- ""
    }
  }
  
  return(ret)
}

#' Get page footer text strings suitable for printing
#' @param rs The report spec
#' @return A vector of strings
#' @noRd
get_page_footer <- function(rs) {
  
  if (is.null(rs$line_size)) {
    stop("line_size cannot be null.") 
    
  }
  
  pftrr <- rs$page_footer_right
  pftrl <- rs$page_footer_left
  pftrc <- rs$page_footer_center
  
  mx <- max(c(length(pftrr), length(pftrl), length(pftrc)))
  
  if (length(pftrr) < mx)
    pftrr <- c(rep("", mx - length(pftrr)), pftrr)
  
  if (length(pftrl) < mx)
    pftrl <- c(rep("", mx - length(pftrl)), pftrl)
  
  if (length(pftrc) < mx)
    pftrc <- c(rep("", mx - length(pftrc)), pftrc)
  
  # Put blank space above page footer by default
  ret <- NULL
  
  if (mx != 0) {
    
    if (rs$page_footer_blank_row == "above") 
      ret <- c("")
    else 
      ret <- c()
    
    for (i in 1:mx) {
      
      fl <- ""
      fc <- ""
      fr <- ""
      
      if (length(pftrl) >= i)
        fl <- as.character(pftrl[[i]])
      
      if (length(pftrr) >= i)
        fr <- as.character(pftrr[[i]])
      
      if (length(pftrc) >= i)
        fc <- as.character(pftrc[[i]])
      
      l_sz <- if (is.null(fl)) 0 else nchar(fl)
      r_sz <- if (is.null(fr)) 0 else nchar(fr)
      c_sz <- if (is.null(fc)) 0 else nchar(fc)
      
      gp <- rs$line_size - (l_sz + r_sz + c_sz)
      
      #print("footer")
      if (gp >= 0) {
        if (l_sz > r_sz)
          fr <- pad_left(fr, l_sz)
        else
          fl <- pad_right(fl, r_sz)
        
        lw <- rs$line_size - nchar(fr) - nchar(fl)
        ln <- paste0(fl, pad_both(fc, lw), fr)
      }
      else {
        
        stop(paste0("Page footer exceeds available width\n", 
                    "Footer Left: ", fl, "\n", 
                    "Footer Center: ", fc, "\n",
                    "Footer Right: ", fr, "\n",
                    "Footer length: ", nchar(fl) + nchar(fc) + nchar(fr), "\n",
                    "Line length: ", rs$line_size, "\n"))
        ln <- ""
      }
      
      ret[length(ret) + 1] <- ln
    }
    

  }
  
  
  return(ret)
}



# Page Info ---------------------------------------------------------------

#' @description A internal class to collect data about a report page.  This 
#' object is created in create_table_pages_text and passed around to 
#' different functions so they can make decisions based on the information
#' contained in this object.
#' @noRd
page_info <- function(data, keys, font_name, col_width, col_align,
                      label, label_align, page_by = NULL, table_align = NULL) {
  
  ret <- structure(list(), class = c("page_info", "list"))
  
  ret$data <- data
  ret$keys <- keys
  ret$font_name <- font_name
  ret$col_width <- col_width
  ret$col_align <- col_align
  ret$label <- label
  ret$label_align <- label_align
  ret$total_pages <- 0
  ret$page_number <- 0
  ret$page_by <- page_by
  ret$table_align <- table_align
  
  return(ret)
  
}




# Utilities ---------------------------------------------------------------

#' @noRd
pad_right <- Vectorize(function(s, w) {
  
  l <- w - nchar(s)
  
  if (l < 0)
    ret <- s
  else
    ret <- paste0(s, paste0(rep_len(" ", length.out = w - nchar(s)), collapse = ""))

  return(ret)
}, USE.NAMES = FALSE)

#' @noRd
pad_left <- Vectorize(function(s, w) {
  
  l <- w - nchar(s)
  
  if (l < 0)
    ret <- s
  else 
    ret <- paste0(paste0(rep_len(" ", length.out =l), collapse = ""), s)
  
  
  return(ret)
  
}, USE.NAMES = FALSE)

#' @noRd
pad_both <- Vectorize(function(s, w) {
  
  
  l <- w - nchar(s)
  
  if (l < 0)
    ret <- s
  else {
  
    lp <- floor(l / 2)
    rp <- ceiling(l / 2)
    
    ret <- paste0(paste0(rep_len(" ", length.out = lp), collapse = ""), 
                s, paste0(rep_len(" ", length.out = rp), collapse = ""))
  
  }
  
  return(ret)
  
  
}, USE.NAMES = FALSE)

pad_any <- function(s, w, j) {
  
  if (j == "left")
    ret <- pad_right(s, w)
  else if (j == "right")
    ret <- pad_left(s, w)
  else if (j %in% c("center", "centre"))
    ret <- pad_both(s, w)
  else 
    ret <- pad_right(s, w)
  
  return(ret)
  
}

