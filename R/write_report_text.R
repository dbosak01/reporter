

# Write Text Driver Function ----------------------------------------------


#' @title
#' Write a text report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_text <- function(rs) {
  
  ret <- ""
  
  # Kill existing file
  if (file.exists(rs$file_path))
    file.remove(rs$file_path)
  
  # Calculate available space
  rs$content_size <- get_content_size(rs)
  rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width)
  rs$body_size <- get_body_size(rs)
  rs$body_line_count <- floor(rs$body_size[["height"]] / rs$line_height)
  
  # Get page template
  pt <- page_template_text(rs)
  

  ls <- rs$content

  counter <- 1

  # Write out content
  for(o in ls){
    if (class(o)[1] == "table_spec"){

      ttx <- create_table_text(rs, o)
      print(ttx)
      rs <- write_table_text(rs, ttx, pt)
    } else if (class(o)[1] == "character" & o == "page_break"){
      
      if (counter < length(ls))
        rs <- write_page_break(rs)
    }
    
    counter <- counter + 1
  }

  
  invisible(rs)
}



write_table_text <- function(rs, ttx, pt) {
  

  
  f <- file(rs$file_path, open="a")
  
  writeLines(pt$page_header, con = f)
  writeLines(pt$titles, con = f)
  
  writeLines(ttx, con = f)
  
  writeLines(pt$footnotes, con = f)
  writeLines(pt$page_footer, con = f)
  
  close(f)
  
  return(rs)
}

write_page_break <- function(rs) {
  
  f <- file(rs$file_path, open="a")
  
  writeLines("", con = f, sep = "\f")
  
  close(f)
  
  return(rs)

}

# Page Template Functions ----------------------------------------------


page_template_text <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_text", "list"))
  
  
  pt$page_header <- get_page_header(rs)
  pt$titles <- get_titles(rs)
  pt$footnotes <- get_footnotes(rs)
  pt$page_footer <- get_page_footer(rs)
  
  
  return(pt)
}


get_page_header <- function(rs) {
  
  phdrr <- rs$page_header_right
  phdrl <- rs$page_header_left
  phdr <- rs$page_header_left
  if(length(phdrl) < length(phdrr))
    phdr <- phdrr
  
  ret <- c()
  
  for (i in seq_along(phdr)) {
    
    hl <- ""
    hr <- ""
    
    if (length(phdrl) >= i)
      hl <- phdrl[[i]]
    
    if (length(phdrr) >= i)
      hr <- phdrr[[i]]
    
    gp <- rs$line_size - (nchar(hl) + nchar(hr))

    if (gp >= 0) {
      
      lw <- rs$line_size - nchar(hr)
      ln <- paste0(stri_pad_right(hl, width = lw), hr) 
    }
 
    else
      stop("Page header exceeds available width")
    
    ret[i] <- ln
  }

  
  return(ret)
}



get_titles <- function(rs) {
  
  ll <- rs$line_size
  ret <- c()
  
  for (i in seq_along(rs$titles)) {
    
    t <- rs$titles[i]
    
    gp <- ll - nchar(t)
    
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
  
  
  return(ret)
}


get_footnotes <- function(rs) {
  
  ll <- rs$line_size
  ret <- c()
  
  for (i in seq_along(rs$footnotes)) {
    
    t <- rs$footnotes[i]
    
    gp <- ll - nchar(t)
    
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
  
  return(ret)
}


get_page_footer <- function(rs) {
  
  pftrr <- rs$page_footer_right
  pftrl <- rs$page_footer_left
  pftrc <- rs$page_footer_center
  
  mx <- max(c(length(pftrr), length(pftrl), length(pftrc)))
  
  # Put blank space above page footer by default
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
  
    l_sz <- nchar(fl)
    r_sz <- nchar(fr)
    c_sz <- nchar(fc)
  
    gp <- rs$line_size - (l_sz + r_sz + c_sz)
  
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
  
  
  return(ret)
}









asciify <- function(df, pad = 1, ...) {
  ## error checking
  stopifnot(is.data.frame(df))
  ## internal functions
  SepLine <- function(n, pad = 1) {
    tmp <- lapply(n, function(x, pad) paste(rep("-", x + (2* pad)),
                                            collapse = ""),
                  pad = pad)
    paste0("+", paste(tmp, collapse = "+"), "+")
  }
  Row <- function(x, n, pad = 1) {
    foo <- function(i, x, n) {
      fmt <- paste0("%", n[i], "s")
      sprintf(fmt, as.character(x[i]))
    }
    rowc <- sapply(seq_along(x), foo, x = x, n = n)
    paste0("|", paste(paste0(rep(" ", pad), rowc, rep(" ", pad)),
                      collapse = "|"),
           "|")
  }
  ## convert everything to characters
  df <- as.matrix(df)
  ## nchar in data
  mdf <- apply(df, 2, function(x) max(nchar(x)))
  ## nchar in names
  cnames <- nchar(colnames(df))
  ## max nchar of name+data per elements
  M <- pmax(mdf, cnames)
  ## write the header
  sep <- SepLine(M, pad = pad)
  writeLines(sep)
  writeLines(Row(colnames(df), M, pad = pad))
  writeLines(sep)
  ## write the rows
  for(i in seq_len(nrow(df))) {
    ## write a row
    writeLines(Row(df[i,], M, pad = pad))
    ## write separator
    writeLines(sep)
  }
  invisible(df)
}
