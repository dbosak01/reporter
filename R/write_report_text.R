

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
write_report_text <- function(x) {
  
  ret <- ""
  
  pt <- page_template_text(x)
  
  
  #print(pt$page_header)
  #print(pt$titles)
  print(pt)
  # 
  # ls <- x$content
  # 
  # counter <- 1
  # 
  # for(o in ls){
  #   
  #   if (class(o)[1] == "table_spec"){
  #     # change to flextables
  #     fts <- create_table_text(o, body_size = bs)  
  #     my_doc <- body_add_texttables(my_doc, fts)
  #   }
  # }
  
  
  invisible(ret)
}



# Utilities ---------------------------------------------------------------


page_template_text <- function(rs) {
  
  pt <- structure(list(), class = c("page_template_text", "list"))
  
  cs <- get_content_size(rs)
  
  pt$content_size <- cs
  rs$line_size <- floor(cs[["width"]] / rs$char_width)
  pt$body_size <- get_body_size(rs, cs)
  pt$page_header <- get_page_header(rs)
  pt$titles <- get_titles(rs)
  pt$footnotes <- get_footnotes(rs)
  pt$page_footer <- get_page_footer(rs)


  return(pt)
}


# Write Text Write Functions ----------------------------------------------



get_page_header <- function(x) {
  
  phdrr <- x$page_header_right
  phdrl <- x$page_header_left
  phdr <- x$page_header_left
  if(length(phdrl) < length(phdrr))
    phdr <- phdrr
  
  ret <- c()
  
  for (i in seq_along(phdr)) {
    
    ls <- ""
    rs <- ""
    
    if (length(phdrl) >= i)
      ls <- phdrl[[i]]
    
    if (length(phdrr) >= i)
      rs <- phdrr[[i]]
    
    gp <- x$line_size - (nchar(ls) + nchar(rs))

    if (gp >= 0) {
      
      lw <- x$line_size - nchar(rs)
      ln <- paste0(stri_pad_right(ls, width = lw), rs) 
    }
 
    else
      stop("Page header exceeds available width")
    
    ret[i] <- ln
  }

  
  return(ret)
}



get_titles <- function(x) {
  
  ll <- x$line_size
  ret <- c()
  
  for (i in seq_along(x$titles)) {
    
    t <- x$titles[i]
    
    gp <- ll - nchar(t)
    
    if (gp > 0) {
      
      if (x$titles_align == "left")
        ln <- stri_pad_right(t, ll)
      else if (x$titles_align == "right")
        ln <- stri_pad_left(t, ll)
      else if (x$titles_align == "center")
        ln <- stri_pad_both(t, ll)
      
    } else 
      stop("Title exceeds available width.")
    
    
    ret[i] <- ln
  }
  
  
  return(ret)
}

get_table_header <- function(x) {
  
  ret <- "Here is a the table header"
  
  return(ret)
}

get_table_body <- function(x) {
  
  ret <- "Here is the body"
  
  return(ret)
}

get_footnotes <- function(x) {
  
  ll <- x$line_size
  ret <- c()
  
  for (i in seq_along(x$footnotes)) {
    
    t <- x$footnotes[i]
    
    gp <- ll - nchar(t)
    
    if (gp > 0) {
      
      if (x$footnotes_align == "left")
        ln <- stri_pad_right(t, ll)
      else if (x$footnotes_align == "right")
        ln <- stri_pad_left(t, ll)
      else if (x$footnotes_align == "center")
        ln <- stri_pad_both(t, ll)
      
    } else 
      stop("Footnote exceeds available width.")
    
    
    ret[i] <- ln
  }
  
  return(ret)
}

get_page_footer <- function(x) {
  
  pftrr <- x$page_footer_right
  pftrl <- x$page_footer_left
  pftrc <- x$page_footer_center

  mx <- max(c(length(pftrr), length(pftrl), length(pftrc)))

    ret <- c()
  
  for (i in 1:mx) {
    
    ls <- ""
    cs <- ""
    rs <- ""
    
    if (length(pftrl) >= i)
      ls <- as.character(pftrl[[i]])
    
    if (length(pftrr) >= i)
      rs <- as.character(pftrr[[i]])
    
    if (length(pftrc) >= i)
      cs <- as.character(pftrc[[i]])
    
    l_sz <- nchar(ls)
    r_sz <- nchar(rs)
    c_sz <- nchar(cs)
    
    gp <- x$line_size - (l_sz + r_sz + c_sz)

    if (gp >= 0) {
      if (l_sz > r_sz)
        rs <- stri_pad_left(rs, l_sz)
      else 
        ls <- stri_pad_right(ls, r_sz)
        
      lw <- x$line_size - nchar(rs) - nchar(ls)
      ln <- paste0(ls, stri_pad_both(cs, width = lw), rs) 
    }
    else
      stop("Page header exceeds available width")
    
    ret[i] <- ln
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
