

# Formatting functions ----------------------------------------------------



#' @title
#' Helper Function for cnt_pct
#'
#' @description
#' Internal helper function for cnt_pct
#'
#' @noRd
cnt_pct_internal <- function(x, low_threshold = .01, low_label="< 1%",
                             zero_label = "0.0%", precision = 1){


  if (is.na(x)){
    ret = zero_label
  } else if(x > 0 & x < low_threshold){
    ret = low_label
  } else {
    ret = paste0(format(x * 100, nsmall = precision), "%")
  }

  return(ret)
}

cnt_pct_internal(.5, precision = 2)

#' @title
#' Counts and percent formatting function
#'
#' @description
#' A function to format counts and percents.
#'
#' @param a The vector that contains the count values.
#' @param b The vector that contains the percent values.
#' @param precision The precision specified for the percent format.
#' @param low_threshold The threshold at which the low level applies.
#' @param low_label The low label to be used for percents that fall below the
#' low_threshold.  Default is "< 1\%".
#' @export
cnt_pct <- function(a, b, precision = 0.1, low_threshold = .01,
                    low_label="< 1%"){

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
                     precision=p_length,
                     zero_label=zero_label,
                     low_label = low_label),
              width=p_length + 4, justify = "right")

  # Concatenate count and percent
  ret <- paste(a, " (", p, ")", sep="")

  return(ret)
}

cnt <- c(1, 3, 4, 2)
pct <- c(.01, .02, .3, .4)

j <- cnt_pct(cnt, pct)

#' @title
#' Sex levels
#'
#' @description
#' The unique levels for the sex variable.
#'
#' @export
sex_levels <- c("M", "F", "O", "UNK")

#' @title
#' Function for formatting Sex variable
#'
#' @description
#' A vectorized function to format the sex variable.
#'
#' @param x The vector to apply formatting to.
#' @export
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


#' @title
#' Age groups
#'
#' @description
#' Unique values for age group levels
#'
#' @export
age_group_levels <- c("< 18",
                      "18 to 24",
                      "24 to 38",
                      "38 to 55",
                      "55 to 72",
                      "> 72", "Unknown")

#' @title
#' Function to format age groups
#' @description
#' A function to format age groups.
#'
#' @param x The vector to apply the age format to.
#' @export
age_group <- Vectorize(function(x){

  ret <- ""

  if (x < 18) {
    ret <- age_group_levels[1]
  } else if (x >= 18 & x <= 24) {
    ret <- age_group_levels[2]
  } else if (x > 24 & x <= 38) {
    ret <- age_group_levels[3]
  } else if (x > 38 & x <= 55) {
    ret <- age_group_levels[4]
  } else if (x > 55 & x <= 72) {
    ret <- age_group_levels[5]
  } else if (x > 72) {
    ret <- age_group_levels[6]
  } else {
    ret <- age_group_levels[7]
  }


  return(ret)
})

#' @title
#' Function to format ranges
#'
#' @description
#' A vectorized function to format the range values.
#'
#' @param a The vector to apply the range format to.
#' @param b The vector to apply the range format to.
#' @export
range_fmt <- Vectorize(function(a, b){
  ret <- paste(as.character(a), " to ", as.character(b), sep="")
  return(ret)
})


#' Race levels
#' @export
race_levels <- c("W", "B", "A", "N", "UNK")

#' @title
#' Function to format race
#'
#' @description
#' A vectorized function to format the race variable.
#'
#' @param x The vector to apply the race format to.
#' @export
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




