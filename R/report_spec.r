library(readr)
library(magrittr)
library(glue)
library(stringi)
library(sjlabelled)
library(zip)

# Report Spec Assembly Functions -----------------------------------------------


#'##############################################################################
#' @title 
#' Create a docx report
#' 
#' @description 
#' This function is the constructor for the report_spec object.  The report_spec
#' object contains information needed to construct a docx report. The object is 
#' defined as an S3 object, and is inherited from a list. 
#' 
#' @param file_path The output path of the desired report. Either a full path or
#' a relative path is acceptable.  This parameter is not required to create the 
#' report_spec object, but will be required to print the report.
#' @param orientation The page orientation of the desired report.  Valid values 
#' are "landscape" or "portrait".  The default page orientation is "landscape".
#' @param font_name The font name to use on the report.  The specified font 
#' will be used on the entire report.  Valid values are "Courier New", "Arial",
#' "Calibri", or "Times New Roman".  The default font is "Courier New".
#' @return A new report_spec S3 object.
#' @examples 
#' create_report("mtcars.docx", orientation="portrait") %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#
#' file.show("mtcars.docx")
#' @export
#'##############################################################################
create_report <- function(file_path = "", orientation="landscape", 
                          font_name="Courier New", font_size=10) {
  
  x <- structure(list(), class = c("report_spec", "list"))
  
  # Trap missing or invalid orientation parameter.
  if (!orientation %in% c("landscape", "portrait")) {
    
    stop(paste0("ERROR: orientation parameter on ",
                "create_report() function is invalid: '", orientation,
                "'\n\tValid values are: 'landscape' or 'portrait'."))
  } 
  
  # Tramp missing or invalid font_name parameter.
  if (!font_name %in% c("Courier New", "Times New Roman", "Arial", "Calibri")) {
    
    stop(paste0("ERROR: font_name parameter on create_report() ", 
                "function is invalid: '", font_name, 
                "'\n\tValid values are: 'Arial', 'Calibri', 'Courier New', ",
                "and 'Times New Roman'."))
  } 
  
  # Populate report_spec fields
  x$file_path <- file_path
  x$font_size <-font_size
  x$orientation <- orientation
  x$font_name <- font_name
  x$font_family <- get_font_family(font_name)
  x$content <- list()
  
  # Set default margins
  x <- set_margins(x)
  
  return(x)
  
}

#'##############################################################################
#' @title 
#' Set page margins.
#' @description Set the page margins on the report spec object. 
#' @details 
#' The margins will be used for the entire report.  Units for the margins 
#' are inches.  The default margins are 1 inch on the left and right, and 
#' .5 inch on the top and bottom.  
#' @param margin_top The top margin.
#' @param margin_bottom The bottom margin.
#' @param margin_left The left margin.
#' @param margin_right The right margin.
#' @return The report_spec with margins set as desired.
#' @example 
#' create_report("mtcars.docx", orientation="portrait") %>%
#' set_margins(margin_top = 1, margin_bottom =1)  %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#
#' file.show("mtcars.docx")
#' @export
#'##############################################################################
set_margins <- function(x, margin_top=.5, margin_bottom=.5, 
                           margin_left=1, margin_right=1) {
  
  
  if (is.na(margin_top) | margin_top < 0 | !is.numeric(margin_top)){
    stop("ERROR: invalid value for margin_top.")
  }
  if (is.na(margin_bottom) | margin_bottom < 0| !is.numeric(margin_bottom)){
    stop("ERROR: invalid value for margin_bottom.")
  }
  if (is.na(margin_left) | margin_left < 0| !is.numeric(margin_left)){
    stop("ERROR: invalid value for margin_left.")
  }
  if (is.na(margin_right) | margin_right < 0| !is.numeric(margin_right)){
    stop("ERROR: invalid value for margin_right.")
  }
  
  # Populate margin value fields
  x$margin_top = margin_top
  x$margin_bottom = margin_bottom
  x$margin_left = margin_left
  x$margin_right = margin_right

 
  return(x) 
}

#'##############################################################################
#' @title 
#' Add a page header to the report
#' 
#' @description 
#' This function adds a page header to the report.  The page header will appear
#' on each page of the report.
#' 
#' @details
#' The page header may contain text on the left or right. Use the appropriate
#' parameters to specify the desired text.  The page header may also contain 
#' titles for the report, if the location of the 
#' the titles has been specified as "header".  See \code{titles()} function
#' for additional details.
#' @param left The left page header text.  May be a single string or a vector
#' of strings.
#' @param right The right page header text.  May be a single string or a vector
#' of strings.
#' @example 
#' create_report("mtcars.docx", orientation="portrait") %>%
#' page_header(left = "Cars Data", right = "Study ABC")  %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#
#' file.show("mtcars.docx")
#' @export
#'##############################################################################
page_header <- function(x, left="", right=""){
  
  x$page_header_left <- left
  x$page_header_right <- right
  
  return(x)
}


#'##############################################################################
#' @title 
#' Add titles to the report
#' 
#' @description 
#' This function adds one or more to the report.  The titles will be added to
#' the page template, and thus appear on each page of the report.
#' 
#' @details
#' The titles function accepts a set of strings of the desired title text.  
#' The titles may be aligned center, left or right using the align parameter.  
#' The titles may be located in the header, page body, or table according to 
#' the location parameter.   
#' @param x The report specification object.
#' @param ... A set of title strings.
#' @param location The location to place the titles in the report.  Valid values
#' are "header", "body", or "table". 
#' @param align The position to align the titles.  Valid values are: "left", 
#' "right", or "center".
#' @example 
#' create_report("mtcars.docx", orientation="portrait") %>%
#' titles("Cars Table Title", "All Cars")  %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#
#' file.show("mtcars.docx")
#' @export
#'##############################################################################
titles <- function(x, ..., location = "header", align="center"){
  
  
  tl <- c(...)
  
  if (length(tl) > 5){
    stop("ERROR: titles function is limited to a maximum of five (5) titles.")
  }
  
  x$titles <- tl
  x$titles_location <- location
  x$titles_align <- align
  
  return(x)
  
}


#'##############################################################################
#' @title 
#' Add footnotes to the report
#' 
#' @description 
#' This function adds one or more footnotes to the report.  The footnotes will 
#' be added to the page template, and thus appear on each page of the report.
#' 
#' @details
#' The footnotes function accepts a set of strings of the desired footnote text.  
#' The footnotes may be aligned center, left or right using the align parameter.  
#' The footnotes may be located in the footer, page body, or table according to 
#' the location parameter. The user is responsible for adding desired symbols. 
#' Footnote symbols will not be generated automatically.   
#' @param x The report specification object.
#' @param ... A set of footnotes strings.
#' @param location The location to place the footnotes in the report.  
#' Valid values are "header", "body", or "table". 
#' @param align The position to align the titles.  Valid values are: "left", 
#' "right", or "center".
#' @example 
#' create_report("mtcars.docx", orientation="portrait") %>%
#' titles("Cars Table Title", "All Cars")  %>%
#' footnotes("Source: 1974 Motor Trend US magazine") %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#
#' file.show("mtcars.docx")
#' @export
#'##############################################################################
footnotes <- function(x, ..., location = "footer", align = "left"){
  
  ft <- c(...)
  
  x$footnotes <- ft
  x$footnotes_location <- location
  x$footnotes_align <- align
  
  return(x)
  
}

#'##############################################################################
#' @title 
#' Add a page footer to the report
#' 
#' @description 
#' This function adds a page footer to the report.  The page footer will appear
#' on each page of the report.
#' 
#' @details
#' The page footer may contain text on the left, right, or center. 
#' Use the appropriate parameters to specify the desired text.  The page footer 
#' may also contain footnotes for the report, if the location of the 
#' the footnotes has been specified as "footer".  See \code{footnotes()} 
#' function for additional details.
#' @param left The left page footer text.  May be a single string or a vector
#' of strings.
#' @param right The right page footer text.  May be a single string or a vector
#' of strings.
#' @example 
#' create_report("mtcars.docx", orientation="portrait") %>%
#' page_header(left = "Cars Data", right = "Study ABC")  %>%
#' page_footer(left = Sys.time())  %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#
#' file.show("mtcars.docx")
#' @export
#'##############################################################################
page_footer <- function(x, left="", right="", center=""){
  
  x$page_footer_left <- left
  x$page_footer_right <- right
  x$page_footer_center <- center
  
  return(x)
}



#'##############################################################################
#' @title 
#' Print the report parameters.
#' 
#' @description 
#' A function to print the report parameters.  This function is an S3 generic
#' function.  The class printed is "report_spec".
#' 
#' @examples 
#' #' create_report("mtcars.docx", orientation="portrait") %>%
#' page_header(left = "Cars Data", right = "Study ABC")  %>%
#' page_footer(left = Sys.time())  %>%
#' add_content(create_table(mtcars)) %>%
#' print()
#' @export
#'##############################################################################
print.report_spec <- function(x, full=FALSE){
  
  if (full)
    print.listof(x)
  else
    print.simple.list(x)
  
  invisible(x)
}

#'##############################################################################
#' @title 
#' Add content to a report
#' 
#' @description 
#' This function adds an object to the report content list. Valid objects
#' are a table_spec, a flextable, or a plot from ggplot.  Objects will be
#' appended to the report in order they are added.  By default, a page break
#' is added after the content.
#' 
#' @param x a report_spec to append content to
#' @param object the object to append
#' @param page_break whether to add a page break. Value values are "before",
#' "after", or "none"
#' @return The modified report_spec
#' @example 
#' create_report("listing_3_0.docx") %>%
#' add_content(create_table(mtcars)) %>%
#' write_report()
#' 
#' @export
#' #'##############################################################################
add_content <- function(x, object, page_break="after") {
  
  # Add page break before if requested
  if (page_break == "before")
    x$content[[length(x$content) + 1]] <- "page_break"
  
  # Add object to the content list
  x$content[[length(x$content) + 1]] <- object
  
  # Add page break after if requested, and by default
  if (page_break == "after")
    x$content[[length(x$content) + 1]] <- "page_break"
  
  return(x)
}


# Utilities --------------------------------------------------------------------

get_font_family <- function(font_name) {
  
  
  # mono, serif, sans
  fam <- case_when(font_name == "Courier New" ~ "mono",
                   font_name == "Arial" ~ "sans",
                   font_name == "Times New Roman" ~ "serif",
                   font_name == "Calibri" ~ "sans")
  
  return(fam)
  
}



# Report Spec Write Functions --------------------------------------------------

#'##############################################################################
#' Generate unique 8 digit keys for use as tag ids
#' @param count The number of ids to generate
#' @return A character vector of unique ids
#' @noRd
#'##############################################################################
gen_keys <- function(count = 1){
  
  if(count < 1 | is.na(count)){
    stop("ERROR: count parameter must be an integer value greater than zero.")  
  }
  
  ret <- stri_rand_strings(count, 8, pattern = "[A-Z0-9]")
  
  return(ret)
}


#'##############################################################################
#' @description 
#' The docx directory contains the Content Types.xml file.  The Content Types 
#' file is created by this function.  This files exists in the top level folder.
#' @param docx_path The path to the docx_directory
#' @return None
#' @noRd
#'##############################################################################
create_content_type <- function(docx_path){
  
  if (!file.exists(docx_path)) {
    
    stop(paste("Folder does not exist:", docx_path))
  }
  
  path <- file.path(docx_path, "[Content_Types].xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
     <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
     <Default Extension="xml" ContentType="application/xml"/>
     <Override PartName="/word/document.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
     <Override PartName="/word/styles.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>
     <Override PartName="/word/settings.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/>
     <Override PartName="/word/webSettings.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml"/>
     <Override PartName="/word/footnotes.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"/>
     <Override PartName="/word/endnotes.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"/>
     <Override PartName="/word/header1.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>
     <Override PartName="/word/footer1.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"/>
     <Override PartName="/word/fontTable.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml"/>
     <Override PartName="/word/theme/theme1.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>
     <Override PartName="/docProps/core.xml" 
      ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
     <Override PartName="/docProps/app.xml" 
      ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
  </Types>')
  
  cat(xml, file=f)
  
  close(f)
  
  
}

#'##############################################################################
#' @description 
#' The _rels subdirectory contains the .rels file.  The _rels 
#' subdirectory and .rels file is created by this function.
#' @param docx_path The path to the docx_directory
#' @return None
#' @noRd
#'##############################################################################
create_rels <- function(docx_path){
  
  if (!file.exists(docx_path)) {
    stop(paste("Folder does not exist:", docx_path))
  }
  
  # Create _rels subdirectory
  path_rels <- file.path(docx_path, "_rels")
  dir.create(path_rels)
  
  path <- file.path(path_rels, ".rels")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  xml <- gsub("[\r\n]\\s+", "", 
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
     <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/extended-properties" 
     Target="docProps/app.xml"/>
     <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/
      package/2006/relationships/metadata/core-properties" 
     Target="docProps/core.xml"/>
     <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/officeDocument" 
     Target="word/document.xml"/>
  </Relationships>')
  
  cat(xml, file=f)
  
  close(f)
  
  
}

#'##############################################################################
#' @description 
#' The docProps subdirectory contains the app.xml file.  The docProps 
#' subdirectory and app.xml file is created by this function.
#' @param docx_path The path to the docx_directory
#' @return None
#' @noRd
#'##############################################################################
create_docProps <- function(docx_path){
  
  if (!file.exists(docx_path)) {
    stop(paste("Folder does not exist:", docx_path))
  }
  
  # Create docProps subdirectory
  path_docProps <- file.path(docx_path, "docProps")
  dir.create(path_docProps)
  
  path <- file.path(path_docProps, "app.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" 
    xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
     <Template>Normal.dotm</Template>
     <TotalTime>36</TotalTime>
     <Pages>1</Pages>
     <Words>0</Words>
     <Characters>0</Characters>
     <Application>Microsoft Office Word</Application>
     <DocSecurity>0</DocSecurity>
     <Lines>0</Lines>
     <Paragraphs>0</Paragraphs>
     <ScaleCrop>false</ScaleCrop>
     <Company></Company>
     <LinksUpToDate>false</LinksUpToDate>
     <CharactersWithSpaces>0</CharactersWithSpaces>
     <SharedDoc>false</SharedDoc>
     <HyperlinksChanged>false</HyperlinksChanged>
     <AppVersion>16.0000</AppVersion>
  </Properties>')
  
  cat(xml, file=f)
  
  
  close(f)
  
  path <- file.path(path_docProps, "core.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  xml <- gsub("[\r\n]\\s+", "", 
  '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" 
    xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" 
    xmlns:dcterms="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <dc:title></dc:title>
    <dc:subject></dc:subject>
    <dc:creator>{user}</dc:creator>
    <cp:keywords></cp:keywords>
    <dc:description></dc:description>
    <cp:lastModifiedBy>{user}</cp:lastModifiedBy>
    <cp:revision>0</cp:revision>
    <dcterms:created xsi:type="dcterms:W3CDTF">{datetime_now}</dcterms:created>
    <dcterms:modified xsi:type="dcterms:W3CDTF">{datetime_now}</dcterms:modified>
  </cp:coreProperties>')
  
  xml <- glue(xml, datetime_now = format(Sys.time(), "%Y-%m-%dT%H:%M%:%SZ"), 
              user=Sys.info()["user"])
  
  cat(xml, file=f)
  
  close(f)
  
  
}

# Twips per inch #
# This setting is fixed, but used in several locations in 
# the functions below.
tpi <- 1440

#'##############################################################################
#' @description 
#' This function calculates the header table widths based on the page 
#' orientation specified in the page_template.  
#' @param x The page_template
#' @return A named vector of widths.
#' @noRd
#'##############################################################################
get_header_widths <- function(x){
  
  pg_sz <- if (x$orientation == "landscape") 11 else 8.5
  w <- (pg_sz * tpi) - (x$margin_left * tpi) - (x$margin_right * tpi)
  ret <- c(header_width = w, 
           header_left_width = w/2,
           header_right_width = w/2)
  
  return(ret)
}


#'##############################################################################
#' @description 
#' This function calculates the footer table widths based on the page 
#' orientation specified in the page_template.  
#' @param x The page_template
#' @return A named vector of widths.
#' @noRd
#'##############################################################################
get_footer_widths <- function(x){
  
  pg_sz <- if (x$orientation == "landscape") 11 else 8.5
  w <- (pg_sz * tpi) - (x$margin_left * tpi) - (x$margin_right * tpi)
  ret <- c(footer_width = w, 
           footer_left_width = w/3,
           footer_right_width = w/3,
           footer_center_width = w/3)
  
  return(ret)
}


#'##############################################################################
#' @description 
#' The header.xml file contains the information that appears on the page header.
#' This document is dynamically constructed based on user selections such as 
#' desired titles and page header.  The file is placed in the word
#' subdirectory.
#' @param x The page_template object
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_header <- function(x, word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  
  path <- file.path(word_path, "header1.xml")
  
  f <- file(path, open="w")
  
  hw <- get_header_widths(x)
  
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  header_start <- gsub("[\r\n]\\s+", "", 
  '<w:hdr 
            xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
            xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink" 
            xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d" 
            xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex" 
            xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex" 
            xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex" 
            xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex" 
            xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex" 
            xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex" 
            xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex" 
            xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex" 
            xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex" 
            xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" 
            xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
            xmlns:o="urn:schemas-microsoft-com:office:office" 
            xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
            xmlns:v="urn:schemas-microsoft-com:vml" 
            xmlns:w10="urn:schemas-microsoft-com:office:word" 
            xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
            xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
            xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
            xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
            xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
            xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
            xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" 
            xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" 
            xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" 
            xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" 
            xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" 
            xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" 
            xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
            mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
    <w:tbl>
    <w:tblPr>
       <w:tblStyle w:val="TableGrid"/>
       <w:tblW w:w="{header_width}" w:type="dxa"/>
       <w:tblBorders>
          <w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>
          <w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>
          <w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>
          <w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>
          <w:insideH w:val="none" w:sz="0" w:space="0" w:color="auto"/>
          <w:insideV w:val="none" w:sz="0" w:space="0" w:color="auto"/>
       </w:tblBorders>
       <w:tblLook w:val="04A0" w:firstRow="1" w:lastRow="0" w:firstColumn="1" w:lastColumn="0" w:noHBand="0" w:noVBand="1"/>
    </w:tblPr>
    <w:tblGrid>
       <w:gridCol w:w="{header_left_width}"/>
       <w:gridCol w:w="{header_right_width}"/>
    </w:tblGrid>')
  
  
  
  header_row <- gsub("[\r\n]\\s+", "",
  '<w:tr w:rsidR="00415629" w:rsidRPr="0072030C" w14:paraId="{paraid_row}" 
    w14:textId="77777777" w:rsidTr="00987FC6">
     <w:tc>
        <w:tcPr>
           <w:tcW w:w="{header_left_width}" w:type="dxa"/>
        </w:tcPr>
        <w:p w14:paraId="{paraid_left}" w14:textId="{textid_left}" w:rsidR="00415629" 
          w:rsidRPr="0072030C" w:rsidRDefault="00415629" w:rsidP="00415629">
           <w:pPr>
              <w:pStyle w:val="Header"/>
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
           </w:pPr>
           <w:proofErr w:type="spellStart"/>
           <w:r w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:t>{header_left}</w:t>
           </w:r>
           <w:proofErr w:type="spellEnd"/>
        </w:p>
     </w:tc>
     <w:tc>
        <w:tcPr>
           <w:tcW w:w="{header_right_width}" w:type="dxa"/>
        </w:tcPr>
        <w:p w14:paraId="{paraid_right}" w14:textId="{textid_right}" w:rsidR="00415629" 
          w:rsidRPr="0072030C" w:rsidRDefault="00415629" w:rsidP="00415629">
           <w:pPr>
              <w:pStyle w:val="Header"/>
              <w:jc w:val="right"/>
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
           </w:pPr>
           <w:proofErr w:type="spellStart"/>
           <w:r w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:t>{header_right}</w:t>
           </w:r>
           <w:proofErr w:type="spellEnd"/>
        </w:p>
     </w:tc>
  </w:tr>')
  
  
  header_strings <- c("")
  max_header_rows <- max(c(length(x$page_header_left), 
                           length(x$page_header_right)))
  
  for(i in 1:max_header_rows){
    hl <- if (length(x$page_header_left) >= i) x$page_header_left[i] else ""
    hr <- if (length(x$page_header_right) >= i) x$page_header_right[i] else ""
    k <- gen_keys(5)
    header_strings[i] <- glue(header_row, paraid_row=k[1],
                              paraid_left=k[2], textid_left=k[3], 
                              paraid_right=k[4], textid_right=k[5], 
                              header_left=hl, header_right=hr, 
                              header_left_width=hw["header_left_width"],
                              header_right_width=hw["header_right_width"],
                              font_name=x$font_name)
  }
  
  title_block <- gsub("[\r\n]\\s+", "", 
  '<w:p w14:paraId="{paraid}" w14:textId="{textid}" w:rsidR="002050AC" 
    w:rsidRDefault="002050AC" w:rsidP="00281DD7">
    <w:pPr>
       <w:pStyle w:val="Header"/>
       <w:jc w:val="center"/>
       <w:rPr>
          <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
       </w:rPr>
    </w:pPr>
    <w:r>
       <w:rPr>
          <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
       </w:rPr>
       <w:t>{title}</w:t>
    </w:r>
  </w:p>')
  
  # Prepare title blocks
  title_strings <- c("")
  for(i in seq_along(x$titles)){
    k <- gen_keys(2)
    title_strings[i] <- glue(title_block, paraid=k[1], textid=k[2], 
                             title=x$titles[i], font_name=x$font_name)
  }
  
  
  header_end <- gsub("[\r\n]\\s+", "",
  '<w:p w14:paraId="5658A9DF" w14:textId="77777777" w:rsidR="002050AC" 
    w:rsidRPr="0072030C" w:rsidRDefault="002050AC" w:rsidP="00281DD7">
    <w:pPr>
    <w:pStyle w:val="Header"/>
    <w:jc w:val="center"/>
    <w:rPr>
    <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
    </w:rPr>
    </w:pPr>
    </w:p>
    </w:hdr>')
  
  
  
  xml <- glue(header_start, 
              paste(header_strings, sep="", collapse=""), 
              '</w:tbl>',
              paste(title_strings, sep="", collapse=""), 
              header_end, 
              header_width = hw["header_width"],
              header_left_width=hw["header_left_width"],
              header_right_width=hw["header_right_width"],
              font_name=x$font_name)
  
  
  cat(xml, file=f)
  
  
  close(f)
  
}


#'##############################################################################
#' @description 
#' The footer.xml file contains the information that appears on the page footer.
#' This document is dynamically constructed based on user selections such as 
#' desired footnotes and page footers.  The file is placed in the word
#' subdirectory.
#' @param x The page_template object
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_footer <- function(x, word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "footer1.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  
  fw <- get_footer_widths(x)
  
  footer_start <- gsub("[\r\n]\\s+", "", 
  '<w:ftr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink" 
    xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d" 
    xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex" 
    xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex" 
    xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex" 
    xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex" 
    xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex" 
    xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex" 
    xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex" 
    xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex" 
    xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex" 
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:o="urn:schemas-microsoft-com:office:office" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:v="urn:schemas-microsoft-com:vml" 
    xmlns:w10="urn:schemas-microsoft-com:office:word" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" 
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" 
    xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" 
    xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" 
    xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" 
    xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" 
    xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">')
  
  
  
  footnote_block <- gsub("[\r\n]\\s+", "",
  '<w:p w14:paraId="{paraid}" w14:textId="{textid}" w:rsidR="00C478E4" 
    w:rsidRPr="00C478E4" w:rsidRDefault="00C478E4">
    <w:pPr>
       <w:pStyle w:val="Footer" />
       <w:rPr>
          <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" />
       </w:rPr>
    </w:pPr>
    <w:r w:rsidRPr="00C478E4">
       <w:rPr>
          <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" />
       </w:rPr>
       <w:t>{footnote}</w:t>
    </w:r>
  </w:p>')
  
  footnote_strings <- c()
  for(i in seq_along(x$footnotes)){
    k <- gen_keys(2)
    footnote_strings[i] <- glue(footnote_block, paraid=k[1],
                              textid=k[2], footnote=x$footnotes[i], 
                              font_name=x$font_name)
  }
  
  
  table_start <-gsub("[\r\n]\\s+", "", 
  '<w:tbl>
  <w:tblPr>
     <w:tblStyle w:val="TableGrid"/>
     <w:tblW w:w="{footer_width}" w:type="dxa"/>
     <w:tblBorders>
        <w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>
        <w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>
        <w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>
        <w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>
        <w:insideH w:val="none" w:sz="0" w:space="0" w:color="auto"/>
        <w:insideV w:val="none" w:sz="0" w:space="0" w:color="auto"/>
     </w:tblBorders>
     <w:tblCellMar>
        <w:left w:w="0" w:type="dxa"/>
        <w:right w:w="0" w:type="dxa"/>
     </w:tblCellMar>
     <w:tblLook w:val="04A0" w:firstRow="1" w:lastRow="0" w:firstColumn="1" 
      w:lastColumn="0" w:noHBand="0" w:noVBand="1"/>
  </w:tblPr>
  <w:tblGrid>
     <w:gridCol w:w="{footer_left_width}"/>
     <w:gridCol w:w="{footer_center_width}"/>
     <w:gridCol w:w="{footer_right_width}"/>
  </w:tblGrid>')
  
  
  footer_row <- gsub("[\r\n]\\s+", "", 
  '<w:tr w:rsidR="00415629" w:rsidRPr="0072030C" w14:paraId="{paraid_row}" 
    w14:textId="77777777" w:rsidTr="00987FC6">
     <w:tc>
        <w:tcPr>
           <w:tcW w:w="{footer_left_width}" w:type="dxa"/>
        </w:tcPr>
        <w:p w14:paraId="{paraid_left}" w14:textId="{textid_left}" w:rsidR="00415629" 
          w:rsidRPr="0072030C" w:rsidRDefault="00415629">
           <w:pPr>
              <w:pStyle w:val="Footer"/>
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
           </w:pPr>
           <w:proofErr w:type="spellStart"/>
           <w:r w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:t>{footer_left}</w:t>
           </w:r>
           <w:proofErr w:type="spellEnd"/>
        </w:p>
     </w:tc>
     <w:tc>
        <w:tcPr>
           <w:tcW w:w="{footer_center_width}" w:type="dxa"/>
        </w:tcPr>
        <w:p w14:paraId="{paraid_center}" w14:textId="{textid_center}" w:rsidR="00415629" 
          w:rsidRPr="0072030C" w:rsidRDefault="00415629" w:rsidP="00415629">
           <w:pPr>
              <w:pStyle w:val="Footer"/>
              <w:jc w:val="center"/>
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
           </w:pPr>
           <w:proofErr w:type="spellStart"/>
           <w:r w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:t>{footer_center}</w:t>
           </w:r>
           <w:proofErr w:type="spellEnd"/>
        </w:p>
     </w:tc>
     <w:tc>
        <w:tcPr>
           <w:tcW w:w="{footer_right_width}" w:type="dxa"/>
        </w:tcPr>
        <w:p w14:paraId="{paraid_right}" w14:textId="{textid_right}" w:rsidR="00415629" 
          w:rsidRPr="0072030C" w:rsidRDefault="00104915" w:rsidP="00415629">
           <w:pPr>
              <w:pStyle w:val="Footer"/>
              <w:jc w:val="right"/>
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
           </w:pPr>
           <w:r w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:t xml:space="preserve">{footer_right} Page </w:t>
           </w:r>
           <w:r w:rsidR="002B143C" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:fldChar w:fldCharType="begin"/>
           </w:r>
           <w:r w:rsidR="002B143C" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:instrText xml:space="preserve"> PAGE   \\* MERGEFORMAT </w:instrText>
           </w:r>
           <w:r w:rsidR="002B143C" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:fldChar w:fldCharType="separate"/>
           </w:r>
           <w:r w:rsidR="002B143C" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
                 <w:noProof/>
              </w:rPr>
              <w:t>1</w:t>
           </w:r>
           <w:r w:rsidR="002B143C" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:fldChar w:fldCharType="end"/>
           </w:r>
           <w:r w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:t xml:space="preserve"> of </w:t>
           </w:r>
           <w:r w:rsidR="00E452DE" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:fldChar w:fldCharType="begin"/>
           </w:r>
           <w:r w:rsidR="00E452DE" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:instrText xml:space="preserve"> NUMPAGES   \\* MERGEFORMAT </w:instrText>
           </w:r>
           <w:r w:rsidR="00E452DE" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
              <w:fldChar w:fldCharType="separate"/>
           </w:r>
           <w:r w:rsidR="002B143C" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
                 <w:noProof/>
              </w:rPr>
              <w:t>1</w:t>
           </w:r>
           <w:r w:rsidR="00E452DE" w:rsidRPr="0072030C">
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
                 <w:noProof/>
              </w:rPr>
              <w:fldChar w:fldCharType="end"/>
           </w:r>
        </w:p>
     </w:tc>
  </w:tr>')
  
  
  footer_strings <- c()
  max_footer_rows <- max(c(length(x$page_footer_left), 
                           length(x$page_footer_right), 
                           length(x$page_footer_center)))
  
  for(i in 1:max_footer_rows){
    fl <- if (length(x$page_footer_left) >= i) x$page_footer_left[i] else ""
    fr <- if (length(x$page_footer_right) >= i) x$page_footer_right[i] else ""
    fc <- if (length(x$page_footer_center) >= i) x$page_footer_center[i] else ""
    k <- gen_keys(7)
    footer_strings[i] <- glue(footer_row, paraid_row=k[1],
                              paraid_left=k[2], textid_left=k[3], 
                              paraid_right=k[4], textid_right=k[5], 
                              paraid_center=k[4], textid_center=k[5], 
                              footer_left=fl, footer_right=fr, footer_center=fc, 
                              footer_left_width=fw["footer_left_width"],
                              footer_center_width=fw["footer_center_width"],
                              footer_right_width=fw["footer_right_width"],
                              font_name=x$font_name)
  }
  
  
  footer_end <- gsub("[\r\n]\\s+", "", 
  '</w:tbl>
     <w:p w14:paraId="74C661ED" w14:textId="77777777" w:rsidR="00415629" 
      w:rsidRDefault="00415629">
        <w:pPr>
           <w:pStyle w:val="Footer"/>
        </w:pPr>
     </w:p>
  </w:ftr>')
  
  xml <- glue(footer_start,
              paste(footnote_strings, sep="", collapse=""),
              table_start,
              paste(footer_strings, sep="", collapse=""),
              footer_end,
              footer_left_width=fw["footer_left_width"],
              footer_center_width=fw["footer_center_width"],
              footer_right_width=fw["footer_right_width"],
              footer_width=fw["footer_width"],
              font_name=x$font_name)
  
  cat(xml, file = f)
  
  
  close(f)
  
}

#'##############################################################################
#' @description 
#' The fontTable.xml file contains a list of available fonts for the document.
#' This function creates the file.  For purposes of reporting, there are only
#' four fonts available: Arial, Calibri, Times New Roman, and Courier New.
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_fontTable <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "fontTable.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:fonts xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex">
     <w:font w:name="Calibri">
        <w:panose1 w:val="020F0502020204030204"/>
        <w:charset w:val="00"/>
        <w:family w:val="swiss"/>
        <w:pitch w:val="variable"/>
        <w:sig w:usb0="E4002EFF" w:usb1="C000247B" w:usb2="00000009" 
          w:usb3="00000000" w:csb0="000001FF" w:csb1="00000000"/>
     </w:font>
     <w:font w:name="Times New Roman">
        <w:panose1 w:val="02020603050405020304"/>
        <w:charset w:val="00"/>
        <w:family w:val="roman"/>
        <w:pitch w:val="variable"/>
        <w:sig w:usb0="E0002EFF" w:usb1="C000785B" w:usb2="00000009" 
          w:usb3="00000000" w:csb0="000001FF" w:csb1="00000000"/>
     </w:font>
     <w:font w:name="Courier New">
        <w:panose1 w:val="02070309020205020404"/>
        <w:charset w:val="00"/>
        <w:family w:val="modern"/>
        <w:pitch w:val="fixed"/>
        <w:sig w:usb0="E0002EFF" w:usb1="C0007843" w:usb2="00000009" 
          w:usb3="00000000" w:csb0="000001FF" w:csb1="00000000"/>
     </w:font>
     <w:font w:name="Arial">
        <w:panose1 w:val="020B0604020202020204"/>
        <w:charset w:val="00"/>
        <w:family w:val="swiss"/>
        <w:pitch w:val="variable"/>
        <w:sig w:usb0="E0002EFF" w:usb1="C000785B" w:usb2="00000009" 
          w:usb3="00000000" w:csb0="000001FF" w:csb1="00000000"/>
     </w:font>
  </w:fonts>')
  
  cat(xml, file = f)
  
  
  close(f)
  
}


#'##############################################################################
#' @description 
#' The footnotes.xml file contains the footnotes for the document.  This 
#' function creates the footnotes file.  But for purposes of the report, 
#' footnotes are added to the footer file instead of this file so they appear
#' on every page.
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_footnotes <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "footnotes.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:footnotes xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink" 
    xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d" 
    xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex" 
    xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex" 
    xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex" 
    xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex" 
    xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex" 
    xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex" 
    xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex" 
    xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex" 
    xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex" 
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:o="urn:schemas-microsoft-com:office:office" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:v="urn:schemas-microsoft-com:vml" 
    xmlns:w10="urn:schemas-microsoft-com:office:word" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" 
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" 
    xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" 
    xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" 
    xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" 
    xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" 
    xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
     <w:footnote w:type="separator" w:id="-1">
        <w:p w14:paraId="4F9B79D5" w14:textId="77777777" w:rsidR="00E6281B" 
          w:rsidRDefault="00E6281B" w:rsidP="00415629">
           <w:pPr>
              <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
           </w:pPr>
           <w:r>
              <w:separator/>
           </w:r>
        </w:p>
     </w:footnote>
     <w:footnote w:type="continuationSeparator" w:id="0">
        <w:p w14:paraId="31BB53F9" w14:textId="77777777" w:rsidR="00E6281B" 
          w:rsidRDefault="00E6281B" w:rsidP="00415629">
           <w:pPr>
              <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
           </w:pPr>
           <w:r>
              <w:continuationSeparator/>
           </w:r>
        </w:p>
     </w:footnote>
  </w:footnotes>')
  
  cat(xml, file = f)
  
  
  close(f)
  
  
}

#'##############################################################################
#' @description 
#' The webSettings.xml file is a file in the word subdirectory.  This function
#' creates the webSettings.xml file.
#' @param x 
#' @return 
#' @noRd
#'##############################################################################
create_webSettings <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "webSettings.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:webSettings xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex">
             <w:optimizeForBrowser/>
             <w:relyOnVML/>
             <w:allowPNG/>
          </w:webSettings>')
  
  cat(xml, file = f)
  
  
  close(f)
  
  
}

#'##############################################################################
#' @description 
#' The document.xml file contains the document body.  The function creates
#' an empty document body that will be populated when content is added to the 
#' page template.
#' @param x The page_template object
#' @param word_path The path to the word subdirectory
#' @return None
#' @noRd
#'##############################################################################
create_document <- function(x, word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "document.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
      xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink" 
      xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d" 
      xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex" 
      xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex" 
      xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex" 
      xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex" 
      xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex" 
      xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex" 
      xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex" 
      xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex" 
      xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex" 
      xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" 
      xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
      xmlns:o="urn:schemas-microsoft-com:office:office" 
      xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
      xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w10="urn:schemas-microsoft-com:office:word" 
      xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
      xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
      xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
      xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
      xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
      xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
      xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" 
      xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" 
      xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" 
      xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" 
      xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" 
      xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" 
      xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
      mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
     <w:body>
        <w:p w14:paraId="61F53360" w14:textId="042DEE32" w:rsidR="001F5D69" w:rsidRPr="0072030C" 
             w:rsidRDefault="001F5D69" w:rsidP="001F5D69">
           <w:pPr>
              <w:rPr>
                 <w:rFonts w:ascii="{font_name}" w:hAnsi="{font_name}" w:cs="{font_name}"/>
              </w:rPr>
           </w:pPr>
        </w:p>
        <w:sectPr w:rsidR="001F5D69" w:rsidRPr="0072030C" w:rsidSect="00987FC6">
           <w:headerReference w:type="default" r:id="rId6"/>
           <w:footerReference w:type="default" r:id="rId7"/>
           <w:pgSz w:w="{page_width}" w:h="{page_height}" w:orient="{orientation}"/>
           <w:pgMar w:top="{margin_top}" w:right="{margin_right}" w:bottom="{margin_bottom}" w:left="{margin_left}" 
              w:header="{margin_top}" w:footer="{margin_bottom}" w:gutter="0"/>
           <w:cols w:space="720"/>
           <w:docGrid w:linePitch="360"/>
        </w:sectPr>
     </w:body>
  </w:document>')
  
  pw <- 11 * tpi
  ph <- 8.5 * tpi
  if (x$orientation == "portrait"){
    pw <- 8.5 * tpi
    ph <- 11 * tpi
  }
  
  xml <- glue(xml, 
              orientation=x$orientation,
              page_height=ph,
              page_width=pw,
              font_name=x$font_name,
              margin_top=x$margin_top * tpi,
              margin_bottom=x$margin_bottom * tpi, 
              margin_left=x$margin_left * tpi,
              margin_right=x$margin_right * tpi)
  
  cat(xml, file = f)
  
  
  close(f)
}


#'##############################################################################
#' @description
#' The styles.xml file is a file in the word subdirectory.  This function 
#' creates styles.xml.  The styles file does not change based on content or 
#' user parameters.  
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_styles <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "styles.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex">
     <w:docDefaults>
        <w:rPrDefault>
           <w:rPr>
              <w:rFonts w:asciiTheme="minorHAnsi" w:eastAsiaTheme="minorHAnsi" w:hAnsiTheme="minorHAnsi" w:cstheme="minorBidi"/>
              <w:sz w:val="22"/>
              <w:szCs w:val="22"/>
              <w:lang w:val="en-US" w:eastAsia="en-US" w:bidi="ar-SA"/>
           </w:rPr>
        </w:rPrDefault>
        <w:pPrDefault>
           <w:pPr>
              <w:spacing w:after="160" w:line="259" w:lineRule="auto"/>
           </w:pPr>
        </w:pPrDefault>
     </w:docDefaults>
     <w:latentStyles w:defLockedState="0" w:defUIPriority="99" w:defSemiHidden="0" w:defUnhideWhenUsed="0" w:defQFormat="0" w:count="376">
        <w:lsdException w:name="Normal" w:uiPriority="0" w:qFormat="1"/>
        <w:lsdException w:name="heading 1" w:uiPriority="9" w:qFormat="1"/>
        <w:lsdException w:name="heading 2" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 3" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 4" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 5" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 6" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 7" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 8" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="heading 9" w:semiHidden="1" w:uiPriority="9" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="index 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index 9" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 1" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 2" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 3" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 4" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 5" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 6" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 7" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 8" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toc 9" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Normal Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="footnote text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="annotation text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="header" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="footer" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="index heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="caption" w:semiHidden="1" w:uiPriority="35" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="table of figures" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="envelope address" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="envelope return" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="footnote reference" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="annotation reference" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="line number" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="page number" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="endnote reference" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="endnote text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="table of authorities" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="macro" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="toa heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Bullet" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Number" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Bullet 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Bullet 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Bullet 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Bullet 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Number 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Number 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Number 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Number 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Title" w:uiPriority="10" w:qFormat="1"/>
        <w:lsdException w:name="Closing" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Signature" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Default Paragraph Font" w:semiHidden="1" w:uiPriority="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Continue" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Continue 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Continue 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Continue 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="List Continue 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Message Header" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Subtitle" w:uiPriority="11" w:qFormat="1"/>
        <w:lsdException w:name="Salutation" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Date" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text First Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text First Indent 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Note Heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text Indent 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Body Text Indent 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Block Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Hyperlink" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="FollowedHyperlink" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Strong" w:uiPriority="22" w:qFormat="1"/>
        <w:lsdException w:name="Emphasis" w:uiPriority="20" w:qFormat="1"/>
        <w:lsdException w:name="Document Map" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Plain Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="E-mail Signature" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Top of Form" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Bottom of Form" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Normal (Web)" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Acronym" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Address" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Cite" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Code" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Definition" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Keyboard" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Preformatted" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Sample" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Typewriter" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="HTML Variable" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Normal Table" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="annotation subject" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="No List" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Outline List 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Outline List 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Outline List 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Simple 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Simple 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Simple 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Classic 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Classic 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Classic 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Classic 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Colorful 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Colorful 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Colorful 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Columns 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Columns 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Columns 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Columns 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Columns 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table List 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table 3D effects 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table 3D effects 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table 3D effects 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Contemporary" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Elegant" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Professional" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Subtle 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Subtle 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Web 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Web 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Web 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Balloon Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Table Grid" w:uiPriority="39"/>
        <w:lsdException w:name="Table Theme" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Placeholder Text" w:semiHidden="1"/>
        <w:lsdException w:name="No Spacing" w:uiPriority="1" w:qFormat="1"/>
        <w:lsdException w:name="Light Shading" w:uiPriority="60"/>
        <w:lsdException w:name="Light List" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1" w:uiPriority="65"/>
        <w:lsdException w:name="Medium List 2" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid" w:uiPriority="73"/>
        <w:lsdException w:name="Light Shading Accent 1" w:uiPriority="60"/>
        <w:lsdException w:name="Light List Accent 1" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid Accent 1" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1 Accent 1" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2 Accent 1" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1 Accent 1" w:uiPriority="65"/>
        <w:lsdException w:name="Revision" w:semiHidden="1"/>
        <w:lsdException w:name="List Paragraph" w:uiPriority="34" w:qFormat="1"/>
        <w:lsdException w:name="Quote" w:uiPriority="29" w:qFormat="1"/>
        <w:lsdException w:name="Intense Quote" w:uiPriority="30" w:qFormat="1"/>
        <w:lsdException w:name="Medium List 2 Accent 1" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1 Accent 1" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2 Accent 1" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3 Accent 1" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List Accent 1" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading Accent 1" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List Accent 1" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid Accent 1" w:uiPriority="73"/>
        <w:lsdException w:name="Light Shading Accent 2" w:uiPriority="60"/>
        <w:lsdException w:name="Light List Accent 2" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid Accent 2" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1 Accent 2" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2 Accent 2" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1 Accent 2" w:uiPriority="65"/>
        <w:lsdException w:name="Medium List 2 Accent 2" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1 Accent 2" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2 Accent 2" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3 Accent 2" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List Accent 2" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading Accent 2" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List Accent 2" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid Accent 2" w:uiPriority="73"/>
        <w:lsdException w:name="Light Shading Accent 3" w:uiPriority="60"/>
        <w:lsdException w:name="Light List Accent 3" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid Accent 3" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1 Accent 3" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2 Accent 3" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1 Accent 3" w:uiPriority="65"/>
        <w:lsdException w:name="Medium List 2 Accent 3" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1 Accent 3" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2 Accent 3" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3 Accent 3" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List Accent 3" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading Accent 3" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List Accent 3" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid Accent 3" w:uiPriority="73"/>
        <w:lsdException w:name="Light Shading Accent 4" w:uiPriority="60"/>
        <w:lsdException w:name="Light List Accent 4" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid Accent 4" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1 Accent 4" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2 Accent 4" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1 Accent 4" w:uiPriority="65"/>
        <w:lsdException w:name="Medium List 2 Accent 4" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1 Accent 4" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2 Accent 4" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3 Accent 4" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List Accent 4" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading Accent 4" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List Accent 4" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid Accent 4" w:uiPriority="73"/>
        <w:lsdException w:name="Light Shading Accent 5" w:uiPriority="60"/>
        <w:lsdException w:name="Light List Accent 5" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid Accent 5" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1 Accent 5" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2 Accent 5" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1 Accent 5" w:uiPriority="65"/>
        <w:lsdException w:name="Medium List 2 Accent 5" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1 Accent 5" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2 Accent 5" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3 Accent 5" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List Accent 5" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading Accent 5" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List Accent 5" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid Accent 5" w:uiPriority="73"/>
        <w:lsdException w:name="Light Shading Accent 6" w:uiPriority="60"/>
        <w:lsdException w:name="Light List Accent 6" w:uiPriority="61"/>
        <w:lsdException w:name="Light Grid Accent 6" w:uiPriority="62"/>
        <w:lsdException w:name="Medium Shading 1 Accent 6" w:uiPriority="63"/>
        <w:lsdException w:name="Medium Shading 2 Accent 6" w:uiPriority="64"/>
        <w:lsdException w:name="Medium List 1 Accent 6" w:uiPriority="65"/>
        <w:lsdException w:name="Medium List 2 Accent 6" w:uiPriority="66"/>
        <w:lsdException w:name="Medium Grid 1 Accent 6" w:uiPriority="67"/>
        <w:lsdException w:name="Medium Grid 2 Accent 6" w:uiPriority="68"/>
        <w:lsdException w:name="Medium Grid 3 Accent 6" w:uiPriority="69"/>
        <w:lsdException w:name="Dark List Accent 6" w:uiPriority="70"/>
        <w:lsdException w:name="Colorful Shading Accent 6" w:uiPriority="71"/>
        <w:lsdException w:name="Colorful List Accent 6" w:uiPriority="72"/>
        <w:lsdException w:name="Colorful Grid Accent 6" w:uiPriority="73"/>
        <w:lsdException w:name="Subtle Emphasis" w:uiPriority="19" w:qFormat="1"/>
        <w:lsdException w:name="Intense Emphasis" w:uiPriority="21" w:qFormat="1"/>
        <w:lsdException w:name="Subtle Reference" w:uiPriority="31" w:qFormat="1"/>
        <w:lsdException w:name="Intense Reference" w:uiPriority="32" w:qFormat="1"/>
        <w:lsdException w:name="Book Title" w:uiPriority="33" w:qFormat="1"/>
        <w:lsdException w:name="Bibliography" w:semiHidden="1" w:uiPriority="37" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="TOC Heading" w:semiHidden="1" w:uiPriority="39" w:unhideWhenUsed="1" w:qFormat="1"/>
        <w:lsdException w:name="Plain Table 1" w:uiPriority="41"/>
        <w:lsdException w:name="Plain Table 2" w:uiPriority="42"/>
        <w:lsdException w:name="Plain Table 3" w:uiPriority="43"/>
        <w:lsdException w:name="Plain Table 4" w:uiPriority="44"/>
        <w:lsdException w:name="Plain Table 5" w:uiPriority="45"/>
        <w:lsdException w:name="Grid Table Light" w:uiPriority="40"/>
        <w:lsdException w:name="Grid Table 1 Light" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful" w:uiPriority="52"/>
        <w:lsdException w:name="Grid Table 1 Light Accent 1" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2 Accent 1" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3 Accent 1" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4 Accent 1" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark Accent 1" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful Accent 1" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful Accent 1" w:uiPriority="52"/>
        <w:lsdException w:name="Grid Table 1 Light Accent 2" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2 Accent 2" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3 Accent 2" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4 Accent 2" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark Accent 2" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful Accent 2" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful Accent 2" w:uiPriority="52"/>
        <w:lsdException w:name="Grid Table 1 Light Accent 3" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2 Accent 3" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3 Accent 3" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4 Accent 3" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark Accent 3" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful Accent 3" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful Accent 3" w:uiPriority="52"/>
        <w:lsdException w:name="Grid Table 1 Light Accent 4" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2 Accent 4" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3 Accent 4" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4 Accent 4" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark Accent 4" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful Accent 4" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful Accent 4" w:uiPriority="52"/>
        <w:lsdException w:name="Grid Table 1 Light Accent 5" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2 Accent 5" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3 Accent 5" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4 Accent 5" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark Accent 5" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful Accent 5" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful Accent 5" w:uiPriority="52"/>
        <w:lsdException w:name="Grid Table 1 Light Accent 6" w:uiPriority="46"/>
        <w:lsdException w:name="Grid Table 2 Accent 6" w:uiPriority="47"/>
        <w:lsdException w:name="Grid Table 3 Accent 6" w:uiPriority="48"/>
        <w:lsdException w:name="Grid Table 4 Accent 6" w:uiPriority="49"/>
        <w:lsdException w:name="Grid Table 5 Dark Accent 6" w:uiPriority="50"/>
        <w:lsdException w:name="Grid Table 6 Colorful Accent 6" w:uiPriority="51"/>
        <w:lsdException w:name="Grid Table 7 Colorful Accent 6" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light Accent 1" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2 Accent 1" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3 Accent 1" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4 Accent 1" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark Accent 1" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful Accent 1" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful Accent 1" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light Accent 2" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2 Accent 2" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3 Accent 2" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4 Accent 2" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark Accent 2" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful Accent 2" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful Accent 2" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light Accent 3" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2 Accent 3" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3 Accent 3" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4 Accent 3" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark Accent 3" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful Accent 3" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful Accent 3" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light Accent 4" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2 Accent 4" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3 Accent 4" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4 Accent 4" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark Accent 4" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful Accent 4" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful Accent 4" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light Accent 5" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2 Accent 5" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3 Accent 5" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4 Accent 5" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark Accent 5" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful Accent 5" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful Accent 5" w:uiPriority="52"/>
        <w:lsdException w:name="List Table 1 Light Accent 6" w:uiPriority="46"/>
        <w:lsdException w:name="List Table 2 Accent 6" w:uiPriority="47"/>
        <w:lsdException w:name="List Table 3 Accent 6" w:uiPriority="48"/>
        <w:lsdException w:name="List Table 4 Accent 6" w:uiPriority="49"/>
        <w:lsdException w:name="List Table 5 Dark Accent 6" w:uiPriority="50"/>
        <w:lsdException w:name="List Table 6 Colorful Accent 6" w:uiPriority="51"/>
        <w:lsdException w:name="List Table 7 Colorful Accent 6" w:uiPriority="52"/>
        <w:lsdException w:name="Mention" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Smart Hyperlink" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Hashtag" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Unresolved Mention" w:semiHidden="1" w:unhideWhenUsed="1"/>
        <w:lsdException w:name="Smart Link" w:semiHidden="1" w:unhideWhenUsed="1"/>
     </w:latentStyles>
     <w:style w:type="paragraph" w:default="1" w:styleId="Normal">
        <w:name w:val="Normal"/>
        <w:qFormat/>
     </w:style>
     <w:style w:type="character" w:default="1" w:styleId="DefaultParagraphFont">
        <w:name w:val="Default Paragraph Font"/>
        <w:uiPriority w:val="1"/>
        <w:semiHidden/>
        <w:unhideWhenUsed/>
     </w:style>
     <w:style w:type="table" w:default="1" w:styleId="TableNormal">
        <w:name w:val="Normal Table"/>
        <w:uiPriority w:val="99"/>
        <w:semiHidden/>
        <w:unhideWhenUsed/>
        <w:tblPr>
           <w:tblInd w:w="0" w:type="dxa"/>
           <w:tblCellMar>
              <w:top w:w="0" w:type="dxa"/>
              <w:left w:w="108" w:type="dxa"/>
              <w:bottom w:w="0" w:type="dxa"/>
              <w:right w:w="108" w:type="dxa"/>
           </w:tblCellMar>
        </w:tblPr>
     </w:style>
     <w:style w:type="numbering" w:default="1" w:styleId="NoList">
        <w:name w:val="No List"/>
        <w:uiPriority w:val="99"/>
        <w:semiHidden/>
        <w:unhideWhenUsed/>
     </w:style>
     <w:style w:type="paragraph" w:styleId="Header">
        <w:name w:val="header"/>
        <w:basedOn w:val="Normal"/>
        <w:link w:val="HeaderChar"/>
        <w:uiPriority w:val="99"/>
        <w:unhideWhenUsed/>
        <w:rsid w:val="00415629"/>
        <w:pPr>
           <w:tabs>
              <w:tab w:val="center" w:pos="4680"/>
              <w:tab w:val="right" w:pos="9360"/>
           </w:tabs>
           <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
        </w:pPr>
     </w:style>
     <w:style w:type="character" w:customStyle="1" w:styleId="HeaderChar">
        <w:name w:val="Header Char"/>
        <w:basedOn w:val="DefaultParagraphFont"/>
        <w:link w:val="Header"/>
        <w:uiPriority w:val="99"/>
        <w:rsid w:val="00415629"/>
     </w:style>
     <w:style w:type="paragraph" w:styleId="Footer">
        <w:name w:val="footer"/>
        <w:basedOn w:val="Normal"/>
        <w:link w:val="FooterChar"/>
        <w:uiPriority w:val="99"/>
        <w:unhideWhenUsed/>
        <w:rsid w:val="00415629"/>
        <w:pPr>
           <w:tabs>
              <w:tab w:val="center" w:pos="4680"/>
              <w:tab w:val="right" w:pos="9360"/>
           </w:tabs>
           <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
        </w:pPr>
     </w:style>
     <w:style w:type="character" w:customStyle="1" w:styleId="FooterChar">
        <w:name w:val="Footer Char"/>
        <w:basedOn w:val="DefaultParagraphFont"/>
        <w:link w:val="Footer"/>
        <w:uiPriority w:val="99"/>
        <w:rsid w:val="00415629"/>
     </w:style>
     <w:style w:type="table" w:styleId="TableGrid">
        <w:name w:val="Table Grid"/>
        <w:basedOn w:val="TableNormal"/>
        <w:uiPriority w:val="39"/>
        <w:rsid w:val="00415629"/>
        <w:pPr>
           <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
        </w:pPr>
        <w:tblPr>
           <w:tblBorders>
              <w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>
              <w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>
              <w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>
              <w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>
              <w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>
              <w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>
           </w:tblBorders>
        </w:tblPr>
     </w:style>
  </w:styles>')
  
  
  cat(xml,file = f)
  
  
  close(f)
}


#'##############################################################################
#' @description 
#' The endNotes.xml is a file in the word subdirectory.  This function creates
#' the endNotes file.
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_endNotes <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "endNotes.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:endnotes xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink" 
    xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d" 
    xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex" 
    xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex" 
    xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex" 
    xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex" 
    xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex" 
    xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex" 
    xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex" 
    xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex" 
    xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex" 
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:o="urn:schemas-microsoft-com:office:office" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:v="urn:schemas-microsoft-com:vml" 
    xmlns:w10="urn:schemas-microsoft-com:office:word" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" 
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" 
    xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" 
    xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" 
    xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" 
    xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" 
    xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
     <w:endnote w:type="separator" w:id="-1">
        <w:p w14:paraId="301A2E20" w14:textId="77777777" w:rsidR="00E6281B" 
          w:rsidRDefault="00E6281B" w:rsidP="00415629">
           <w:pPr>
              <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
           </w:pPr>
           <w:r>
              <w:separator/>
           </w:r>
        </w:p>
     </w:endnote>
     <w:endnote w:type="continuationSeparator" w:id="0">
        <w:p w14:paraId="761FDD37" w14:textId="77777777" w:rsidR="00E6281B" 
          w:rsidRDefault="00E6281B" w:rsidP="00415629">
           <w:pPr>
              <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
           </w:pPr>
           <w:r>
              <w:continuationSeparator/>
           </w:r>
        </w:p>
     </w:endnote>
  </w:endnotes>')
  
  cat(xml, file=f)
  
  
  close(f)
  
  
}


#'##############################################################################
#' @description 
#' The word subdirectory has itself two more subdirectories: _rels and _theme.
#' This function creates those subdirectories and the associated files. These 
#' files do not change based on content or user parameters.
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_word_subdirectories <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  # Create _rels subdirectory
  path_rels <- file.path(word_path, "_rels")
  dir.create(path_rels)
  
  path <- file.path(path_rels, "document.xml.rels")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
     <Relationship Id="rId8" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/fontTable" Target="fontTable.xml"/>
     <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/webSettings" Target="webSettings.xml"/>
     <Relationship Id="rId7" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/footer" Target="footer1.xml"/>
     <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/settings" Target="settings.xml"/>
     <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/styles" Target="styles.xml"/>
     <Relationship Id="rId6" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/header" Target="header1.xml"/>
     <Relationship Id="rId5" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/endnotes" Target="endnotes.xml"/>
     <Relationship Id="rId4" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/footnotes" Target="footnotes.xml"/>
     <Relationship Id="rId9" Type="http://schemas.openxmlformats.org/
      officeDocument/2006/relationships/theme" Target="theme/theme1.xml"/>
  </Relationships>')
  
  cat(xml, file = f)
  
  close(f)
  
  
  # Create theme subdirectory
  path_theme <- file.path(word_path, "theme")
  dir.create(path_theme)
  
  
  path <- file.path(path_theme, "theme1.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<a:theme xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" name="Office Theme">
     <a:themeElements>
        <a:clrScheme name="Office">
           <a:dk1>
              <a:sysClr val="windowText" lastClr="000000"/>
           </a:dk1>
           <a:lt1>
              <a:sysClr val="window" lastClr="FFFFFF"/>
           </a:lt1>
           <a:dk2>
              <a:srgbClr val="44546A"/>
           </a:dk2>
           <a:lt2>
              <a:srgbClr val="E7E6E6"/>
           </a:lt2>
           <a:accent1>
              <a:srgbClr val="4472C4"/>
           </a:accent1>
           <a:accent2>
              <a:srgbClr val="ED7D31"/>
           </a:accent2>
           <a:accent3>
              <a:srgbClr val="A5A5A5"/>
           </a:accent3>
           <a:accent4>
              <a:srgbClr val="FFC000"/>
           </a:accent4>
           <a:accent5>
              <a:srgbClr val="5B9BD5"/>
           </a:accent5>
           <a:accent6>
              <a:srgbClr val="70AD47"/>
           </a:accent6>
           <a:hlink>
              <a:srgbClr val="0563C1"/>
           </a:hlink>
           <a:folHlink>
              <a:srgbClr val="954F72"/>
           </a:folHlink>
        </a:clrScheme>
        <a:fontScheme name="Office">
           <a:majorFont>
              <a:latin typeface="Calibri Light" panose="020F0302020204030204"/>
              <a:ea typeface=""/>
              <a:cs typeface=""/>

           </a:majorFont>
           <a:minorFont>
              <a:latin typeface="Calibri" panose="020F0502020204030204"/>
              <a:ea typeface=""/>
              <a:cs typeface=""/>

           </a:minorFont>
        </a:fontScheme>
        <a:fmtScheme name="Office">
           <a:fillStyleLst>
              <a:solidFill>
                 <a:schemeClr val="phClr"/>
              </a:solidFill>
              <a:gradFill rotWithShape="1">
                 <a:gsLst>
                    <a:gs pos="0">
                       <a:schemeClr val="phClr">
                          <a:lumMod val="110000"/>
                          <a:satMod val="105000"/>
                          <a:tint val="67000"/>
                       </a:schemeClr>
                    </a:gs>
                    <a:gs pos="50000">
                       <a:schemeClr val="phClr">
                          <a:lumMod val="105000"/>
                          <a:satMod val="103000"/>
                          <a:tint val="73000"/>
                       </a:schemeClr>
                    </a:gs>
                    <a:gs pos="100000">
                       <a:schemeClr val="phClr">
                          <a:lumMod val="105000"/>
                          <a:satMod val="109000"/>
                          <a:tint val="81000"/>
                       </a:schemeClr>
                    </a:gs>
                 </a:gsLst>
                 <a:lin ang="5400000" scaled="0"/>
              </a:gradFill>
              <a:gradFill rotWithShape="1">
                 <a:gsLst>
                    <a:gs pos="0">
                       <a:schemeClr val="phClr">
                          <a:satMod val="103000"/>
                          <a:lumMod val="102000"/>
                          <a:tint val="94000"/>
                       </a:schemeClr>
                    </a:gs>
                    <a:gs pos="50000">
                       <a:schemeClr val="phClr">
                          <a:satMod val="110000"/>
                          <a:lumMod val="100000"/>
                          <a:shade val="100000"/>
                       </a:schemeClr>
                    </a:gs>
                    <a:gs pos="100000">
                       <a:schemeClr val="phClr">
                          <a:lumMod val="99000"/>
                          <a:satMod val="120000"/>
                          <a:shade val="78000"/>
                       </a:schemeClr>
                    </a:gs>
                 </a:gsLst>
                 <a:lin ang="5400000" scaled="0"/>
              </a:gradFill>
           </a:fillStyleLst>
           <a:lnStyleLst>
              <a:ln w="6350" cap="flat" cmpd="sng" algn="ctr">
                 <a:solidFill>
                    <a:schemeClr val="phClr"/>
                 </a:solidFill>
                 <a:prstDash val="solid"/>
                 <a:miter lim="800000"/>
              </a:ln>
              <a:ln w="12700" cap="flat" cmpd="sng" algn="ctr">
                 <a:solidFill>
                    <a:schemeClr val="phClr"/>
                 </a:solidFill>
                 <a:prstDash val="solid"/>
                 <a:miter lim="800000"/>
              </a:ln>
              <a:ln w="19050" cap="flat" cmpd="sng" algn="ctr">
                 <a:solidFill>
                    <a:schemeClr val="phClr"/>
                 </a:solidFill>
                 <a:prstDash val="solid"/>
                 <a:miter lim="800000"/>
              </a:ln>
           </a:lnStyleLst>
           <a:effectStyleLst>
              <a:effectStyle>
                 <a:effectLst/>
              </a:effectStyle>
              <a:effectStyle>
                 <a:effectLst/>
              </a:effectStyle>
              <a:effectStyle>
                 <a:effectLst>
                    <a:outerShdw blurRad="57150" dist="19050" dir="5400000" 
                      algn="ctr" rotWithShape="0">
                       <a:srgbClr val="000000">
                          <a:alpha val="63000"/>
                       </a:srgbClr>
                    </a:outerShdw>
                 </a:effectLst>
              </a:effectStyle>
           </a:effectStyleLst>
           <a:bgFillStyleLst>
              <a:solidFill>
                 <a:schemeClr val="phClr"/>
              </a:solidFill>
              <a:solidFill>
                 <a:schemeClr val="phClr">
                    <a:tint val="95000"/>
                    <a:satMod val="170000"/>
                 </a:schemeClr>
              </a:solidFill>
              <a:gradFill rotWithShape="1">
                 <a:gsLst>
                    <a:gs pos="0">
                       <a:schemeClr val="phClr">
                          <a:tint val="93000"/>
                          <a:satMod val="150000"/>
                          <a:shade val="98000"/>
                          <a:lumMod val="102000"/>
                       </a:schemeClr>
                    </a:gs>
                    <a:gs pos="50000">
                       <a:schemeClr val="phClr">
                          <a:tint val="98000"/>
                          <a:satMod val="130000"/>
                          <a:shade val="90000"/>
                          <a:lumMod val="103000"/>
                       </a:schemeClr>
                    </a:gs>
                    <a:gs pos="100000">
                       <a:schemeClr val="phClr">
                          <a:shade val="63000"/>
                          <a:satMod val="120000"/>
                       </a:schemeClr>
                    </a:gs>
                 </a:gsLst>
                 <a:lin ang="5400000" scaled="0"/>
              </a:gradFill>
           </a:bgFillStyleLst>
        </a:fmtScheme>
     </a:themeElements>
     <a:objectDefaults/>
     <a:extraClrSchemeLst/>
     <a:extLst>
        <a:ext uri="{05A4C25C-085E-4340-85A3-A5531E510DB2}">
           <thm15:themeFamily xmlns:thm15="http://schemas.microsoft.com/office/thememl/2012/main" 
           name="Office Theme" id="{62F939B6-93AF-4DB8-9C6B-D6C7DFDC589F}" 
           vid="{4A3C46E8-61CC-4603-A589-7422A47A8E4A}"/>
        </a:ext>
     </a:extLst>
  </a:theme>')
  
  cat(xml, file = f)
  
  close(f)
  
}

#'##############################################################################
#' Create the settings file for the page template.  The settings file will
#' be written to the word subdirectory.
#' @param word_path The path to the word subdirectory.
#' @return None
#' @noRd
#'##############################################################################
create_settings <- function(word_path){
  
  if (!file.exists(word_path)) {
    stop(paste("Folder does not exist:", word_path))
  }
  
  path <- file.path(word_path, "settings.xml")
  
  f <- file(path, open="w")
  
  write_lines('<?xml version="1.0" encoding="UTF-8"?>', f)
  
  xml <- gsub("[\r\n]\\s+", "", 
  '<w:settings xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" 
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" 
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
    xmlns:o="urn:schemas-microsoft-com:office:office" 
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" 
    xmlns:sl="http://schemas.openxmlformats.org/schemaLibrary/2006/main" 
    xmlns:v="urn:schemas-microsoft-com:vml" 
    xmlns:w10="urn:schemas-microsoft-com:office:word" 
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" 
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" 
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" 
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" 
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
    mc:Ignorable="w14 w15 w16se w16cid w16 w16cex">
     <w:zoom w:percent="100"/>
     <w:proofState w:spelling="clean" w:grammar="clean"/>
     <w:defaultTabStop w:val="720"/>
     <w:characterSpacingControl w:val="doNotCompress"/>
     <w:footnotePr>
        <w:footnote w:id="-1"/>
        <w:footnote w:id="0"/>
     </w:footnotePr>
     <w:endnotePr>
        <w:endnote w:id="-1"/>
        <w:endnote w:id="0"/>
     </w:endnotePr>
     <w:compat>
        <w:compatSetting w:name="compatibilityMode" 
          w:uri="http://schemas.microsoft.com/office/word" w:val="15"/>
        <w:compatSetting w:name="overrideTableStyleFontSizeAndJustification" 
          w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
        <w:compatSetting w:name="enableOpenTypeFeatures" 
          w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
        <w:compatSetting w:name="doNotFlipMirrorIndents" 
          w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
        <w:compatSetting w:name="differentiateMultirowTableHeaders" 
          w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
        <w:compatSetting w:name="useWord2013TrackBottomHyphenation" 
          w:uri="http://schemas.microsoft.com/office/word" w:val="0"/>
     </w:compat>
     <w:rsids>
        <w:rsidRoot w:val="00650D23"/>
        <w:rsid w:val="00104915"/>
        <w:rsid w:val="001338B7"/>
        <w:rsid w:val="00191A36"/>
        <w:rsid w:val="001A1B43"/>
        <w:rsid w:val="001A61C5"/>
        <w:rsid w:val="001F5D69"/>
        <w:rsid w:val="002050AC"/>
        <w:rsid w:val="0024449D"/>
        <w:rsid w:val="00281DD7"/>
        <w:rsid w:val="002A43E1"/>
        <w:rsid w:val="002B143C"/>
        <w:rsid w:val="002F5677"/>
        <w:rsid w:val="00316977"/>
        <w:rsid w:val="00330EFE"/>
        <w:rsid w:val="00415629"/>
        <w:rsid w:val="00650D23"/>
        <w:rsid w:val="007136DE"/>
        <w:rsid w:val="0072030C"/>
        <w:rsid w:val="007B508B"/>
        <w:rsid w:val="00806F3D"/>
        <w:rsid w:val="00853F33"/>
        <w:rsid w:val="00861FB4"/>
        <w:rsid w:val="008624F0"/>
        <w:rsid w:val="009333F4"/>
        <w:rsid w:val="00987FC6"/>
        <w:rsid w:val="00BC1080"/>
        <w:rsid w:val="00BE54AD"/>
        <w:rsid w:val="00C04596"/>
        <w:rsid w:val="00C050A6"/>
        <w:rsid w:val="00C422D7"/>
        <w:rsid w:val="00CA6524"/>
        <w:rsid w:val="00D85987"/>
        <w:rsid w:val="00E1560A"/>
        <w:rsid w:val="00E452DE"/>
        <w:rsid w:val="00E6281B"/>
        <w:rsid w:val="00F075BF"/>
     </w:rsids>
     <m:mathPr>
        <m:mathFont m:val="Cambria Math"/>
        <m:brkBin m:val="before"/>
        <m:brkBinSub m:val="--"/>
        <m:smallFrac m:val="0"/>
        <m:dispDef/>
        <m:lMargin m:val="0"/>
        <m:rMargin m:val="0"/>
        <m:defJc m:val="centerGroup"/>
        <m:wrapIndent m:val="1440"/>
        <m:intLim m:val="subSup"/>
        <m:naryLim m:val="undOvr"/>
     </m:mathPr>
     <w:themeFontLang w:val="en-US"/>
     <w:clrSchemeMapping w:bg1="light1" w:t1="dark1" w:bg2="light2" w:t2="dark2" 
      w:accent1="accent1" w:accent2="accent2" w:accent3="accent3" 
      w:accent4="accent4" w:accent5="accent5" w:accent6="accent6" 
      w:hyperlink="hyperlink" w:followedHyperlink="followedHyperlink"/>
     <w:shapeDefaults>
        <o:shapedefaults v:ext="edit" spidmax="1026"/>
        <o:shapelayout v:ext="edit">
           <o:idmap v:ext="edit" data="1"/>
        </o:shapelayout>
     </w:shapeDefaults>
     <w:decimalSymbol w:val="."/>
     <w:listSeparator w:val=","/>
     <w14:docId w14:val="5DF16A4E"/>
     <w15:chartTrackingRefBased/>
     <w15:docId w15:val="{5835AB14-5A94-48A4-87D7-84244E3CB846}"/>
  </w:settings>')
  
  cat(xml, file = f)
  
  
  close(f)
  
}

#'##############################################################################
#' @description
#' Creates the Word subdirectory for the page template.  This directory contains
#' the document body, the header file, and footer file.  The word subdirectory
#' is where all the document content exist.  
#' @param x The page_template object
#' @param docx_path The path to the docx directory.  Typically a temp location.
#' @return None
#' @noRd
#'##############################################################################
create_word <- function(x, docx_path){
  
  if (!file.exists(docx_path)) {
    stop(paste("Folder does not exist:", docx_path))
  }
  
  # Create Word subdirectory
  path_word <- file.path(docx_path, "word")
  dir.create(path_word)
  
  # Create subdirectories and files
  create_word_subdirectories(path_word)
  
  # Create word doc main files
  create_document(x, path_word)
  create_endNotes(path_word)
  create_fontTable(path_word)
  create_footer(x, path_word)
  create_footnotes(path_word)
  create_header(x, path_word)
  create_settings(path_word)
  create_styles(path_word)
  create_webSettings(path_word)
  
  
}


#'##############################################################################
#' @description
#' Creates a combined docx file from separate xml files and folder structure.
#' Basically this zips everything up and renames according to the target path.
#' @param source_path The folder path with the files to zip
#' @param target_path The file path (.docx) for the resulting zip operation.   
#' @return None.
#' @noRd
#'##############################################################################
create_docx_file <- function(source_path, target_path){
  
  
  # Create temporary zip file path
  zip_path <- gsub(".docx", ".zip", target_path)
  
  
  # Clear out any left over files
  if (file.exists(zip_path))
    file.remove(zip_path)
  
  if (file.exists(target_path))
    file.remove(target_path)
  
  
  # Get list of files/folders from source (temp) directory
  file_list <- list.files(path = source_path, 
                          full.names = TRUE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = TRUE, no.. = TRUE)
  
  
  # Zip up file list
  zipr(zip_path, include_directories= FALSE, files=file_list, recurse = TRUE)
  

  # Put docx extention on zip file
  file.rename(zip_path, target_path)  
  
  
}


#'##############################################################################
#' Writes the page template to the files system.  The page template is used
#' by officer to start the document creation process.  The page template
#' holds the page header/footer, and any titles/footnotes in the page 
#' header/footer.
#' @param x The report spec object to write.
#' @param file_path The path to write to.
#' @return The report spec (x) is returned invisibly.
#' @noRd
#'##############################################################################
write_page_template <- function(x, file_path = ""){
  
  
  if(file_path != "")
    x$file_path <- file_path
  
  # Create temp directory
  path_dir <- tempdir()
  if (!dir.exists(path_dir))
    dir.create(path_dir)
  
  #print(path_dir)
  
  # Create docx directory for document container
  path_docx <- file.path(path_dir, "docx")
  if (dir.exists(path_docx))
    unlink(path_docx, recursive = TRUE, force = TRUE)
  dir.create(path_docx)
  
  # Create content type file
  create_content_type(path_docx)
  
  # Create _rels directory and documents
  create_rels(path_docx)
  
  # Create docProps directory and documents
  create_docProps(path_docx)
  
  # Create word subdirectory
  create_word(x, path_docx)
  
  # Create zip/docx file in desired location
  create_docx_file(target_path = x$file_path, source_path = path_docx)
  
  # Delete temp directory/files
  unlink(path_docx, recursive =  TRUE, force = TRUE)
  
  invisible(x)
}




