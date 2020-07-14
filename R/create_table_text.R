


create_table_text <- function(rs, ts) {
  

  ret <- get_table_body(rs, ts)
  
  
  return(ret)
  
}


get_table_header <- function(x) {
  
  ret <- "Here is a the table header"
  
  return(ret)
}

get_table_body <- function(rs, ts) {
  
  df <- ts$data
  pad <- 1
  
  ret <- c()
  
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
  ret[1] <- sep
  ret[2] <- Row(colnames(df), M, pad = pad)
  ret[3] <- sep
  ## write the rows
  for(i in seq_len(nrow(df))) {
    ## write a row
      ret[length(ret) + 1] <- Row(df[i,], M, pad = pad)
      ## write separator
      ret[length(ret) + 1] <- sep
  }
  
  
  # Set alignment and body height
  for (i in 1:rs$body_line_count) {
    if (!is.na(ret[i])) {
      if (ts$align == "center") {
        ret[i] <- stri_pad_both(ret[i], width = rs$line_size) 
      } else if (ts$align == "right") {
        ret[i] <- stri_pad_left(ret[i], width = rs$line_size)
      }
    } else {
      ret[i] <- ""
    }
  }
  
  return(ret)
}
