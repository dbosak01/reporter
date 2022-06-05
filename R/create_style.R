
# Style class -------------------------------------------------------------

#' @noRd
create_style <- function(font_name = NULL,
                         font_size = NULL,
                         text_color = NULL,
                         background_color = NULL,
                         title_font_bold = NULL,
                         title_font_color = NULL,
                         title_background = NULL,
                         border_color = NULL,
                         table_header_background = NULL,
                         table_header_font_bold = NULL,
                         table_header_font_color = NULL,
                         table_body_background = NULL,
                         table_body_background_even = NULL,
                         table_body_background_odd = NULL,
                         table_stub_background = NULL,
                         table_stub_font_color = NULL,
                         table_stub_font_bold = NULL) {


  ret <- structure(list(), class = c("style_spec", "list"))
  
  ret$font_name <- font_name
  ret$font_size <- font_size
  ret$text_color <- text_color
  ret$background_color <- background_color
  ret$title_font_bold <- title_font_bold
  ret$title_font_color <- title_font_color
  ret$title_background <- title_background
  ret$border_color <- border_color
  ret$table_header_background <- table_header_background
  ret$table_header_font_bold <- table_header_font_bold
  ret$table_header_font_color <- table_header_font_color
  ret$table_body_background <- table_body_background
  ret$table_body_background_even <- table_body_background_even 
  ret$table_body_background_odd <- table_body_background_odd
  ret$table_stub_background <- table_stub_background
  ret$table_stub_font_color <- table_stub_font_color
  ret$table_stub_font_bold <- table_stub_font_bold
  
  
  return(ret)

}

#' @noRd
add_style <- function(rpt, style = NULL, theme = NULL) {
  
  if (is.null(rpt)) {
    stop("Report object cannot be NULL.") 
    
  }
  
  if (is.null(style) & is.null(theme)) {
    stop("Style object and theme cannot both be NULL.") 
    
  }
  
  if (!any(class(rpt) %in% c("report_spec"))) {
    
    stop("First parameter to add_style() must be a report spec.") 
  }
  
  if (!is.null(style)) {
    if (!any(class(style) %in% c("style_spec"))) {
      
     stop("Style parameter to add_style() must be a style spec.") 
    }
  }
  
  if (!is.null(theme)) {
    if (!any(class(theme) %in% c("character"))) {
      
      stop("Theme parameter to add_style() must be a valid theme name.") 
    }
  }
  
  
  if (!is.null(theme)) {
    
    style <- theme_lookup(theme)
  } 
  
  if (!is.null(style)) {
    
    rpt$style <- style
    
    if (!is.null(style$font_name))
      rpt$font <- style$font_name
    
    if (!is.null(style$font_size))
      rpt$font_size <- style$font_size
  }
  
  
    
  return(rpt)
  
}

#' @noRd
theme_lookup <- function(theme_name) {
  
  ret <- NULL
  
  if (theme_name == "basic1") {
    
    ret <- create_style(font_name = "Arial",
                        font_size = 10,
                        text_color = "DarkGrey",
                        background_color = "#FAFBFE",
                        title_font_color = "MidnightBlue",
                        title_font_bold = TRUE,
                        border_color = "Grey",
                        table_header_background = "#EDF2F9",
                        table_header_font_bold = TRUE,
                        table_header_font_color = "MidnightBlue",
                        table_body_background = "White",
                        table_stub_background = "LightSteelBlue",
                        table_stub_font_bold = TRUE,
                        table_stub_font_color = "MidnightBlue")
                        
    
  } else {
    
   stop("Theme name is not valid.") 
    
  }
  
  return(ret)
  
}


#' @noRd
has_style <- function(rs, style_name) {
 
  ret <- FALSE
  
  if (!is.null(rs$style)) {
    if (!is.null(rs$style[[style_name]])) {
      
      ret <- TRUE 
    }
  }
  
  return(ret)
  
}

#' @noRd
get_style <- function(rs, style_name) {
  
  ret <- ""
  
  if (!is.null(rs$style)) {
    if (!is.null(rs$style[[style_name]])) {
      
      ret <- rs$style[[style_name]]
    }
  }
  
  return(ret)
}

#' @noRd
get_style_html <- function(rs, style_name, default = NULL) {
  
  ret <- ""
  
  val <- ""
  
  if (!is.null(default))
    val <- default
  if (has_style(rs, style_name))
    val <- get_style(rs, style_name)
  

  if (val != "") {
    
    if (style_name == "title_font_color")
      ret <- paste0("color: ", val, ";")
    
    if (style_name == "title_background")
      ret <- paste0("background-color: ", val, ";")
    
    if (style_name == "title_font_bold") {
      if (val == TRUE)
        ret <- paste0("font-weight: bold;")
      else 
        ret <- paste0("font-weight: normal;")
      
    }
    
    if (style_name == "background_color")
      ret <- paste0("background-color: ", val, ";")
    
    if (style_name == "table_body_background")
      ret <- paste0("background-color: ", val, ";")
    
    if (style_name == "table_header_background") 
      ret <- paste0("background-color: ", val, ";")
    
    if (style_name == "table_header_font_bold") {
      if (val == TRUE)
        ret <- paste0("font-weight: bold;")
      else 
        ret <- paste0("font-weight: normal;")
    }
    
    if (style_name == "table_header_font_color")
      ret <- paste0("color: ", val, ";")
    
    if (style_name == "table_stub_font_color")
      ret <- paste0("color: ", val, ";")
    
    if (style_name == "table_stub_background")
      ret <- paste0("background-color: ", val, ";")
    
    if (style_name == "table_stub_font_bold") {
      if (val == TRUE)
        ret <- paste0("font-weight: bold;")
      else 
        ret <- paste0("font-weight: normal;")
      
    }
    
        
  }
  
  
  return(ret)
}
