library(scales)

cnt_pct_internal <- function(x, func, low_threshold = .01, low_label="< 1%", zero_label = "0.0%"){
  
  
  if (is.na(x)){
    ret = zero_label
  } else if(x > 0 & x < low_threshold){
    ret = low_label
  } else {
    ret = func(x)
  }
  
  return(ret)
}


cnt_pct <- function(a, b, precision = 0.1, low_threshold = .01, low_label="< 1%"){
  
  # Find length of precision
  p_length <- 0
  pos <- regexpr(pattern="\\.", as.character(precision))
  if (pos > 0)
    p_length <- nchar(substring(as.character(precision), pos + 1)) + 1
  
  zero_label = "0%"
  if (p_length > 1)
    zero_label = paste("0.", rep("0", p_length), "%", sep="")
  
  # Format Percentage column
  p <- format(sapply(b, cnt_pct_internal,
                     func=label_percent(accuracy=precision),
                     zero_label=zero_label, 
                     low_label = low_label),
              width=p_length + 4, justify = "right")
  
  # Concatenate count and percent
  ret <- paste(a, " (", p, ")", sep="")
  
  return(ret)
}

sex_levels <- c("M", "F", "O", "UNK")

#' sex_fmt 
#' 
sex_fmt <- Vectorize(function(x){
  
  ret <- ""
  if (is.na(x)){
    ret <- "Unknown"
  } else {
    if (x == "F")
      ret <- "Female"
    else if(x == "M")
      ret <- "Male"
    else if(x == "O")
      ret <- "Other"
    else if(x == "UNK")
      ret <- "Unknown"
  }
  
  return(ret)
})


age_group_levels <- c("< 18", "18 to 24", "24 to 38", "38 to 55", "55 to 72", "> 72", "Unknown")

age_group <- function(x){
  
  ret <- case_when(
    x < 18            ~ age_group_levels[1],
    x >= 18 & x <= 24 ~ age_group_levels[2],
    x > 24 & x <= 38  ~ age_group_levels[3],
    x > 38 & x <= 55  ~ age_group_levels[4],
    x > 55 & x <= 72  ~ age_group_levels[5],
    x > 72            ~ age_group_levels[6], 
    TRUE              ~ age_group_levels[7]
  )
  
  return(ret)
}

range_fmt <- Vectorize(function(a, b){
  ret <- paste(as.character(a), " to ", as.character(b), sep="")
  return(ret)
})



race_levels <- c("W", "B", "A", "N", "UNK")

#' race_fmt 
#' 
race_fmt <- Vectorize(function(x){
  
  ret <- ""
  if (is.na(x)){
    ret <- "Unknown"
  } else {
    if (x == "W")
      ret <- "White"
    else if(x == "B")
      ret <- "Black"
    else if(x == "A")
      ret <- "Asian or Pacific Islander"
    else if(x == "N")
      ret <- "Native American"
    else if(x == "UNK")
      ret <- "Unknown"
  }
  
  return(ret)
})




