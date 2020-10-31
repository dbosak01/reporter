
# Page Template Functions ----------------------------------------------

#' Create a page template with header, titles, footnotes, and footer
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_text <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_text", "list"))
  
  pt$page_header <- get_page_header(rs)
  pt$title_hdr <- get_title_header(rs$title_hdr, rs$line_size)
  pt$titles <- get_titles(rs$titles, rs$line_size)
  pt$footnotes <- get_footnotes(rs$footnotes, rs$line_size)
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
        ln <- paste0(stri_pad_right(hl, width = lw), hr) 
      }
      
      else
        stop("Page header exceeds available width")
      
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
get_titles <- function(titles, width) {
  
  if (is.null(width)) {
    stop("width cannot be null.") 
    
  }
  

  ll <- width
  ret <- c()
  
  if (!is.null(titles)) { 
    
    for (ttl in titles) {
      
      if (!any(class(ttl) == "title_spec"))
        stop("titles parameter value is not a title spec.")
          
      if (ttl$blank_row %in% c("above", "both") & length(ttl$titles) > 0)
        ret[length(ret) + 1] <- ""
      
      for (i in seq_along(ttl$titles)) {
      
        t <- ttl$titles[i]
        
        gp <- ll - nchar(t)
        
        #print("titles")
        if (gp > 0) {
          
          if (ttl$align == "left")
            ln <- stri_pad_right(t, ll)
          else if (ttl$align == "right")
            ln <- stri_pad_left(t, ll)
          else if (ttl$align == "center" | ttl$align == "centre")
            ln <- stri_pad_both(t, ll)
          
        } else 
          stop(paste0("Title exceeds available width.",
                      "\nTitle: ", t,
                      "\nTitle width: ", nchar(t),
                      "\nLine length: ", ll))
        
        
        ret[length(ret) + 1] <- ln
      }
      
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
        ln <- stri_pad_right(pb, ll)
      else if (pgby$align == "right")
        ln <- stri_pad_left(pb, ll)
      else if (pgby$align == "center" | pgby$align == "centre")
        ln <- stri_pad_both(pb, ll)
      
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
get_title_header <- function(title_hdr, width) {
  
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
        

          ln <- paste0(stri_pad_right(t, ll - nchar(h)), h, " ")

        
      } else 
        stop(paste0("Title header exceeds available width.\n",
                    "Title: ", t, "\n",
                    "Header: ", h, "\n",
                    "Title length: ", nchar(t), "\n",
                    "Header length: ", nchar(h), "\n",
                    "Line length: ", ll, "\n"))
      
      
      ret[length(ret) + 1] <- ln
    }
    
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
get_footnotes <- function(footnotes, width) {
  
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
      
      for (i in seq_along(ftn$footnotes)) {
        
        f <- ftn$footnotes[i]
        
        gp <- ll - nchar(f)
        
        #print("footnotes")
        if (gp > 0) {
          
          if (ftn$align == "left")
            ln <- stri_pad_right(f, ll)
          else if (ftn$align == "right")
            ln <- stri_pad_left(f, ll)
          else if (ftn$align == "center" | ftn$align == "centre")
            ln <- stri_pad_both(f, ll)
          
        } else 
          stop(paste0("Footnote exceeds available width.",
                      "\nFootnote: ", f,
                      "\nFootnote length: ", nchar(f), 
                      "\nLine Length: ", ll))
        
        
        ret[length(ret) + 1] <- ln
      }
      
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
          fr <- stri_pad_left(fr, l_sz)
        else
          fl <- stri_pad_right(fl, r_sz)
        
        lw <- rs$line_size - nchar(fr) - nchar(fl)
        ln <- paste0(fl, stri_pad_both(fc, width = lw), fr)
      }
      else
        stop("Page header exceeds available width")
      
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
                      label, label_align, page_by = NULL) {
  
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
  
  return(ret)
  
}



