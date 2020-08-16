
# Create Text Functions ---------------------------------------------------


create_text <- function(rs, txt) {
  
  a <- stri_wrap(unlist(
    strsplit(txt, split = "\n", fixed = TRUE)), 
    width = rs$line_size, normalize = FALSE)
  
  ret <- list()
  tmp <- c()
  for (i in seq_along(a)) {
    if (length(tmp) < rs$body_line_count) {
      
      tmp[length(tmp) + 1] <- a[i]
      
    } else {

      ret[[length(ret) + 1]] <- tmp
      tmp <- c(a[i])
    }
    
  }
  

  if (length(tmp) > 0 ) {
    
    tmp <- append(tmp, rep("", rs$body_line_count - length(tmp)))
    ret[[length(ret) + 1]] <- tmp
    
  }
  
  
  return(ret)
}
