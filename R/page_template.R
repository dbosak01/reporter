
# Page Template Functions ----------------------------------------------

#' Create a page template with header, titles, footnotes, and footer
#' @param rs The report spec
#' @return The page template object
#' @noRd
page_template_text <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_text", "list"))
  
  
  pt$page_header <- get_page_header(rs)
  pt$titles <- get_titles(rs)
  pt$footnotes <- get_footnotes(rs)
  pt$page_footer <- get_page_footer(rs)
  
  
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
  }
  
  return(ret)
}

#' Get title text strings suitable for printing
#' @import stringi
#' @param rs The report spec
#' @return A vector of strings
#' @noRd
get_titles <- function(rs) {
  
  if (is.null(rs$line_size)) {
    stop("line_size cannot be null.") 
    
  }
  
  ll <- rs$line_size
  ret <- c()
  
  if (!is.null(rs$titles)) { 
    
    for (i in seq_along(rs$titles)) {
      
      t <- rs$titles[i]
      
      gp <- ll - nchar(t)
      
      #print("titles")
      if (gp > 0) {
        
        if (rs$titles_align == "left")
          ln <- stri_pad_right(t, ll)
        else if (rs$titles_align == "right")
          ln <- stri_pad_left(t, ll)
        else if (rs$titles_align == "center")
          ln <- stri_pad_both(t, ll)
        
      } else 
        stop("Title exceeds available width.")
      
      
      ret[i] <- ln
    }
    
  }
  
  
  return(ret)
}

#' Get footnote text strings suitable for printing
#' @param rs The report spec
#' @return A vector of strings
#' @noRd
get_footnotes <- function(rs) {
  
  if (is.null(rs$line_size)) {
    stop("line_size cannot be null.") 
    
  }
  
  ll <- rs$line_size
  ret <- c()
  
  if (!is.null(rs$footnotes)) {
    for (i in seq_along(rs$footnotes)) {
      
      t <- rs$footnotes[i]
      
      gp <- ll - nchar(t)
      
      #print("footnotes")
      if (gp > 0) {
        
        if (rs$footnotes_align == "left")
          ln <- stri_pad_right(t, ll)
        else if (rs$footnotes_align == "right")
          ln <- stri_pad_left(t, ll)
        else if (rs$footnotes_align == "center")
          ln <- stri_pad_both(t, ll)
        
      } else 
        stop("Footnote exceeds available width.")
      
      
      ret[i] <- ln
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
  
  # Put blank space above page footer by default
  ret <- NULL
  
  if (mx != 0) {
    
    ret <- c("")
    
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







