
# Globals -----------------------------------------------------------------

# pointsize <- 1/72
# inchsize <- 72
# in2cm <- 2.54



# Write DOCX --------------------------------------------------------------

#' @import zip
#' @import withr
#' @noRd
write_docx <- function(src, pth) {

  # print(src)
  # print(file.exists(src))
  # print(pth)
  # Sys.sleep(1)
  
  if (file.exists(src)) {
    
    
    if (file.exists(pth))
      file.remove(pth)
    
    if (!file.exists(dirname(pth)))
      dir.create(dirname(pth))
    
    
    fls <- list.files(src, recursive = TRUE, all.files = TRUE)
    
    # 
    # tmp <- getwd()
    # print(tmp)
    withr::with_dir(src,
   # setwd(src)
    zip::zip(pth, fls, mode = "mirror"))
    # setwd(src)
    # utils::zip(pth, fls)
    
  #  setwd(tmp)
    
  
    
  } else {
    
   print("Nothing") 
  }
  
}



# Create DOCX --------------------------------------------------------------

#' @noRd
create_new_docx <- function(font, font_size, imageCount) {
  
  tdd <- file.path(tempdir(), stri_rand_strings(1, length = 6))
  
  dir.create(tdd)
  dir.create(file.path(tdd, "_rels"))
  dir.create(file.path(tdd, "docProps"))
  dir.create(file.path(tdd, "word"))
  dir.create(file.path(tdd, "word/_rels"))
  dir.create(file.path(tdd, "word/media"))
  
  create_content_types(tdd)
  create_web_settings(tdd)
  create_font_table(tdd)
  create_styles(tdd, font, font_size)
  create_app(tdd)
  create_core(tdd)
  create_endnotes(tdd)
  create_footnotes(tdd)
  create_document_rels(tdd, imageCount)
  create_rels(tdd)
  
  # Temporary
 # create_document(tdd) 
  # create_header(tdd)
   create_footer(tdd)

  
  
  return(tdd)
}

#' @noRd
create_content_types <- function(pth) {
  
  
cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types
	xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
	<Default Extension="jpeg" ContentType="image/jpeg"/>
	<Default Extension="rels" 
	ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
	<Default Extension="xml" 
	ContentType="application/xml"/>
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
	<Override PartName="/docProps/core.xml" 
	ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
	<Override PartName="/docProps/app.xml" 
	ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
</Types>'


  nm <- file.path(pth, "[Content_Types].xml")

  f <- file(nm, open="w", encoding = "native.enc")

  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
  
}


create_web_settings <- function(pth) {
  
  
  cnt <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
'<w:webSettings ',
'xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" ',
'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
'xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" ',
'xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" ',
'xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex" ',
'xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid" ',
'xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml" ',
'xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash" ',
'xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" ', 
'mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh">',
'<w:optimizeForBrowser/>',
'<w:allowPNG/>',
'</w:webSettings>')
  
  
  nm <- file.path(pth, "word/webSettings.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
  
}

create_font_table <- function(pth) {
  
  cnt <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<w:fonts
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh">
	<w:font w:name="Times New Roman">
		<w:panose1 w:val="02020603050405020304"/>
		<w:charset w:val="00"/>
		<w:family w:val="roman"/>
		<w:pitch w:val="variable"/>
		<w:sig w:usb0="E0002EFF" w:usb1="C000785B" w:usb2="00000009" 
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
	<w:font w:name="Courier New">
		<w:panose1 w:val="02070309020205020404"/>
		<w:charset w:val="00"/>
		<w:family w:val="modern"/>
		<w:pitch w:val="fixed"/>
		<w:sig w:usb0="E0002EFF" w:usb1="C0007843" w:usb2="00000009" 
		w:usb3="00000000" w:csb0="000001FF" w:csb1="00000000"/>
	</w:font>
</w:fonts>')
  
  
  nm <- file.path(pth, "word/fontTable.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
}

create_settings <- function(pth) {
  
 cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<w:settings
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
	xmlns:sl="http://schemas.openxmlformats.org/schemaLibrary/2006/main" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh">
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
		<w:rsidRoot w:val="005D1180"/>
		<w:rsid w:val="00046D2A"/>
		<w:rsid w:val="00444C49"/>
		<w:rsid w:val="005D1180"/>
		<w:rsid w:val="00BC1857"/>
		<w:rsid w:val="00F10374"/>
		<w:rsid w:val="00FC21CA"/>
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
	w:accent1="accent1" w:accent2="accent2" w:accent3="accent3" w:accent4="accent4" 
	w:accent5="accent5" w:accent6="accent6" w:hyperlink="hyperlink" 
	w:followedHyperlink="followedHyperlink"/>
	<w:shapeDefaults>
		<o:shapedefaults v:ext="edit" spidmax="1026"/>
		<o:shapelayout v:ext="edit">
			<o:idmap v:ext="edit" data="1"/>
		</o:shapelayout>
	</w:shapeDefaults>
	<w:decimalSymbol w:val="."/>
	<w:listSeparator w:val=","/>
	<w14:docId w14:val="0322C6E4"/>
	<w15:chartTrackingRefBased/>
	<w15:docId w15:val="{C1FAABC8-AE44-436C-B3A7-4CB20B70DB45}"/>
</w:settings>'
 
 
 nm <- file.path(pth, "word/settings.xml")
 
 f <- file(nm, open="w", encoding = "native.enc")
 
 writeLines(cnt, f,  useBytes = TRUE) 
 
 close(f)
 
}

create_styles <- function(pth, font, font_size) {
  
  fnt <- font
  if (toupper(font) == "TIMES")
    fnt <- "Times New Roman"
  else if (toupper(font) == "COURIER")
    fnt <- "Courier New"
    
  
  fs <- font_size * 2
  
  cnt <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<w:styles
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh">
	<w:docDefaults>
		<w:rPrDefault>
			<w:rPr>
			  <w:rFonts w:ascii="', fnt, '" w:hAnsi="', fnt, 
			  '" w:cs="', fnt, '"/>
				<w:sz w:val="', fs, '"/>
				<w:szCs w:val="', fs, '"/>
				<w:lang w:val="en-US" w:eastAsia="en-US" w:bidi="ar-SA"/>
			</w:rPr>
		</w:rPrDefault>
		<w:pPrDefault>
			<w:pPr>
				<w:spacing w:after="160" w:line="259" w:lineRule="auto"/>
			</w:pPr>
		</w:pPrDefault>
	</w:docDefaults>
	<w:latentStyles w:defLockedState="0" w:defUIPriority="99" w:defSemiHidden="0" 
	w:defUnhideWhenUsed="0" w:defQFormat="0" w:count="376">
		<w:lsdException w:name="Normal" w:uiPriority="0" w:qFormat="1"/>
		<w:lsdException w:name="heading 1" w:uiPriority="9" w:qFormat="1"/>
		<w:lsdException w:name="heading 2" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 3" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 4" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 5" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 6" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 7" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 8" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="heading 9" w:semiHidden="1" w:uiPriority="9" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
		<w:lsdException w:name="index 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index 9" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 1" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 2" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 3" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 4" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 5" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 6" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 7" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 8" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="toc 9" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="Normal Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="footnote text" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="annotation text" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="header" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="footer" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="index heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
		<w:lsdException w:name="caption" w:semiHidden="1" w:uiPriority="35" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
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
		<w:lsdException w:name="Default Paragraph Font" w:semiHidden="1" 
		w:uiPriority="1" w:unhideWhenUsed="1"/>
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
		<w:lsdException w:name="Bibliography" w:semiHidden="1" w:uiPriority="37" 
		w:unhideWhenUsed="1"/>
		<w:lsdException w:name="TOC Heading" w:semiHidden="1" w:uiPriority="39" 
		w:unhideWhenUsed="1" w:qFormat="1"/>
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
		<w:rsid w:val="00FC21CA"/>
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
		<w:rsid w:val="00FC21CA"/>
	</w:style>
	<w:style w:type="paragraph" w:styleId="Footer">
		<w:name w:val="footer"/>
		<w:basedOn w:val="Normal"/>
		<w:link w:val="FooterChar"/>
		<w:uiPriority w:val="99"/>
		<w:unhideWhenUsed/>
		<w:rsid w:val="00FC21CA"/>
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
		<w:rsid w:val="00FC21CA"/>
	</w:style>
</w:styles>')
  
  nm <- file.path(pth, "word/styles.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
}


create_app <- function(pth) {
  
  cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<Properties
	xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
	xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
	<Template>Normal.dotm</Template>
	<TotalTime>1</TotalTime>
	<Pages>1</Pages>
	<Words>2</Words>
	<Characters>16</Characters>
	<Application>Microsoft Office Word</Application>
	<DocSecurity>0</DocSecurity>
	<Lines>1</Lines>
	<Paragraphs>1</Paragraphs>
	<ScaleCrop>false</ScaleCrop>
	<Company></Company>
	<LinksUpToDate>false</LinksUpToDate>
	<CharactersWithSpaces>17</CharactersWithSpaces>
	<SharedDoc>false</SharedDoc>
	<HyperlinksChanged>false</HyperlinksChanged>
	<AppVersion>16.0000</AppVersion>
</Properties>'
  
  nm <- file.path(pth, "docProps/app.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
}


create_core <- function(pth) {
  
  

  ts <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
  
  cnt <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<cp:coreProperties
	xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
	xmlns:dc="http://purl.org/dc/elements/1.1/"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:dcmitype="http://purl.org/dc/dcmitype/"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<dc:title></dc:title>
	<dc:subject></dc:subject>
	<dc:creator>',  Sys.info()[["user"]], '</dc:creator>
	<cp:keywords></cp:keywords>
	<dc:description></dc:description>
	<cp:lastModifiedBy>',  Sys.info()[["user"]], '</cp:lastModifiedBy>
	<cp:revision>1</cp:revision>
	<dcterms:created xsi:type="dcterms:W3CDTF">', ts, '</dcterms:created>
	<dcterms:modified xsi:type="dcterms:W3CDTF">', ts, '</dcterms:modified>
</cp:coreProperties>')
  
  nm <- file.path(pth, "docProps/core.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
}

# create_document <- function(pth) {
#   
#  cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
# <w:document
# 	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
# 	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
# 	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
# 	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
# 	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
# 	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
# 	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
# 	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
# 	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
# 	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
# 	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
# 	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
# 	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
# 	xmlns:o="urn:schemas-microsoft-com:office:office"
# 	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
# 	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
# 	xmlns:v="urn:schemas-microsoft-com:vml"
# 	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
# 	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
# 	xmlns:w10="urn:schemas-microsoft-com:office:word"
# 	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
# 	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
# 	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
# 	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
# 	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
# 	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
# 	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
# 	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
# 	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
# 	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
# 	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
# 	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
# 	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">
# 	<w:body>
# 		<w:p w14:paraId="1BC7DC64" w14:textId="00AA6D14" w:rsidR="00046D2A" 
# 		w:rsidRPr="00444C49" w:rsidRDefault="00BC1857">
# 			<w:pPr>
# 				<w:rPr>
# 					<w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>
# 				</w:rPr>
# 			</w:pPr>
# 			<w:r w:rsidRPr="00444C49">
# 				<w:rPr>
# 					<w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>
# 				</w:rPr>
# 				<w:t>Fork</w:t>
# 			</w:r>
# 		</w:p>
# 		<w:p w14:paraId="25273E55" w14:textId="6D8F00D8" w:rsidR="00444C49" 
# 		w:rsidRDefault="00444C49"/>
# 		<w:p w14:paraId="22204C21" w14:textId="4434A694" w:rsidR="00444C49" 
# 		w:rsidRPr="00444C49" w:rsidRDefault="00444C49">
# 			<w:pPr>
# 				<w:rPr>
# 					<w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman" 
# 					w:cs="Times New Roman"/>
# 				</w:rPr>
# 			</w:pPr>
# 			<w:r w:rsidRPr="00444C49">
# 				<w:rPr>
# 					<w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman" 
# 					w:cs="Times New Roman"/>
# 				</w:rPr>
# 				<w:t>Bork</w:t>
# 			</w:r>
# 		</w:p>
# 		<w:p w14:paraId="32E76245" w14:textId="71B1B71F" w:rsidR="00444C49" 
# 		w:rsidRDefault="00444C49"/>
# 		<w:p w14:paraId="49CB1A18" w14:textId="256FDEDD" w:rsidR="00444C49" 
# 		w:rsidRPr="00444C49" w:rsidRDefault="00444C49">
# 			<w:pPr>
# 				<w:rPr>
# 					<w:rFonts w:ascii="Courier New" w:hAnsi="Courier New" w:cs="Courier New"/>
# 				</w:rPr>
# 			</w:pPr>
# 			<w:r w:rsidRPr="00444C49">
# 				<w:rPr>
# 					<w:rFonts w:ascii="Courier New" w:hAnsi="Courier New" w:cs="Courier New"/>
# 				</w:rPr>
# 				<w:t>Spork</w:t>
# 			</w:r>
# 		</w:p>
# 		<w:sectPr w:rsidR="00444C49" w:rsidRPr="00444C49">
# 			<w:headerReference w:type="default" r:id="rId6"/>
# 			<w:footerReference w:type="default" r:id="rId7"/>
# 			<w:pgSz w:w="12240" w:h="15840"/>
# 			<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" 
# 			w:header="720" w:footer="720" w:gutter="0"/>
# 			<w:cols w:space="720"/>
# 			<w:docGrid w:linePitch="360"/>
# 		</w:sectPr>
# 	</w:body>
# </w:document>'
#  
#  nm <- file.path(pth, "word/document.xml")
#  
#  f <- file(nm, open="w", encoding = "native.enc")
#  
#  writeLines(cnt, f,  useBytes = TRUE) 
#  
#  close(f)
#  
# }

create_header <- function(pth, cnt = "") {
 
  hdr <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:hdr
	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
	xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">\n'
  

  
  nm <- file.path(pth, "word/header1.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(paste0(hdr, cnt, '\n</w:hdr>\n', collapse = ""), f,  useBytes = TRUE) 
  
  close(f)
  
  
}

# 
# create_footer <- function(pth) {
#   
#   cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
# <w:ftr
# 	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
# 	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
# 	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
# 	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
# 	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
# 	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
# 	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
# 	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
# 	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
# 	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
# 	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
# 	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
# 	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
# 	xmlns:o="urn:schemas-microsoft-com:office:office"
# 	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
# 	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
# 	xmlns:v="urn:schemas-microsoft-com:vml"
# 	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
# 	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
# 	xmlns:w10="urn:schemas-microsoft-com:office:word"
# 	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
# 	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
# 	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
# 	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
# 	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
# 	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
# 	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
# 	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
# 	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
# 	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
# 	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
# 	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
# 	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">
# 	<w:p w14:paraId="65A3354A" w14:textId="3C459A2F" w:rsidR="00FC21CA" 
# 	w:rsidRDefault="00FC21CA">
# 		<w:pPr>
# 			<w:pStyle w:val="Footer"/>
# 		</w:pPr>
# 		<w:r>
# 			<w:t>Footer</w:t>
# 		</w:r>
# 	</w:p>
# </w:ftr>' 
#   
#   
#   nm <- file.path(pth, "word/footer1.xml")
#   
#   f <- file(nm, open="w", encoding = "native.enc")
#   
#   writeLines(cnt, f,  useBytes = TRUE) 
#   
#   close(f)
# }

create_footer <- function(pth, cnt = "") {
 
  hdr <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<w:ftr
	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
	xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">' 
  
  
  nm <- file.path(pth, "word/footer1.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(paste0(hdr, cnt, '\n</w:ftr>\n', collapse = ""), f,  useBytes = TRUE) 
  
  close(f)
}


create_document_rels <- function(pth, imgCnt) {
  
  imgs <- ""
  
  for (i in seq_len(imgCnt)) {
    imgs <- paste0(imgs, '<Relationship Target="media/image', i, '.jpeg"', 
' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"',
 ' Id="rId', 8 + i, '"/>\n')
    
  }
  
 
  cnt <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n
<Relationships
	xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
	<Relationship Id="rId8" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable" 
	Target="fontTable.xml"/>
	<Relationship Id="rId3" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings" 
	Target="webSettings.xml"/>
	<Relationship Id="rId7" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" 
	Target="footer1.xml"/>
	<Relationship Id="rId2" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings" 
	Target="settings.xml"/>
	<Relationship Id="rId1" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" 
	Target="styles.xml"/>
	<Relationship Id="rId6" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" 
	Target="header1.xml"/>
	<Relationship Id="rId5" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes" 
	Target="endnotes.xml"/>
	<Relationship Id="rId4" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes" 
	Target="footnotes.xml"/>', imgs, '</Relationships>')
  
  
  nm <- file.path(pth, "word/_rels/document.xml.rels")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
}

create_rels <- function(pth) {
 
  cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships
	xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
	<Relationship Id="rId3" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" 
	Target="docProps/app.xml"/>
	<Relationship Id="rId2" 
	Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" 
	Target="docProps/core.xml"/>
	<Relationship Id="rId1" 
	Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" 
	Target="word/document.xml"/>
</Relationships>' 
  
  
  nm <- file.path(pth, "_rels/.rels")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
}

create_endnotes <- function(pth) {
 
  cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:endnotes
	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
	xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">
	<w:endnote w:type="separator" w:id="-1">
		<w:p w14:paraId="11DB1783" w14:textId="77777777" w:rsidR="00F10374" 
		w:rsidRDefault="00F10374" w:rsidP="00FC21CA">
			<w:pPr>
				<w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
			</w:pPr>
			<w:r>
				<w:separator/>
			</w:r>
		</w:p>
	</w:endnote>
	<w:endnote w:type="continuationSeparator" w:id="0">
		<w:p w14:paraId="304CAF46" w14:textId="77777777" w:rsidR="00F10374" w:rsidRDefault="00F10374" w:rsidP="00FC21CA">
			<w:pPr>
				<w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
			</w:pPr>
			<w:r>
				<w:continuationSeparator/>
			</w:r>
		</w:p>
	</w:endnote>
</w:endnotes>'
  
  
  nm <- file.path(pth, "word/endnotes.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
  
}

create_footnotes <- function(pth) {
 
  cnt <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:footnotes
	xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
	xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
	xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
	xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
	xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
	xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
	xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
	xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
	xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
	xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
	xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
	xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
	xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
	xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
	xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
	xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
	xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
	xmlns:w16sdtdh="http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"
	xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
	xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
	xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" 
	mc:Ignorable="w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14">
	<w:footnote w:type="separator" w:id="-1">
		<w:p w14:paraId="32C960DE" w14:textId="77777777" w:rsidR="00F10374" 
		w:rsidRDefault="00F10374" w:rsidP="00FC21CA">
			<w:pPr>
				<w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
			</w:pPr>
			<w:r>
				<w:separator/>
			</w:r>
		</w:p>
	</w:footnote>
	<w:footnote w:type="continuationSeparator" w:id="0">
		<w:p w14:paraId="7E2E8024" w14:textId="77777777" w:rsidR="00F10374" w:rsidRDefault="00F10374" w:rsidP="00FC21CA">
			<w:pPr>
				<w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
			</w:pPr>
			<w:r>
				<w:continuationSeparator/>
			</w:r>
		</w:p>
	</w:footnote>
</w:footnotes>' 
  
  
  
  nm <- file.path(pth, "word/footnotes.xml")
  
  f <- file(nm, open="w", encoding = "native.enc")
  
  writeLines(cnt, f,  useBytes = TRUE) 
  
  close(f)
}





# DOCX Construction -------------------------------------------------------



cell_pct <- function(txt, align = "left", width = NULL) {
  
  if (is.null(width)) {
    ret <- paste0('<w:tc>', 
                  para(txt, align),
                  "</w:tc>\n", collapse = "")
    
  } else {
    ret <- paste0('<w:tc><w:tcPr><w:tcW w:w="', width,'" w:type="pct"/></w:tcPr>', 
           para(txt, align),
           "</w:tc>\n", collapse = "")
  }
  
  return(ret)
}

#' Bottom border means it is a header row
#' @noRd
cell_abs <- function(txt, align = "left", width = NULL, bborder = FALSE) {
  
  al <- '<w:vAlign w:val="bottom"/>'
  bb <- '<w:tcBorders>
    <w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>
      </w:tcBorders>'
  if (bborder == FALSE) {
    bb <- ""
    al <- ""
  }
  
  
  if (is.null(width)) {
    ret <- paste0('<w:tc><w:tcPr>', bb, al, '</w:tcPr>', 
                  para(txt, align),
                  "</w:tc>\n", collapse = "")
    
  } else {
    ret <- paste0('<w:tc><w:tcPr><w:tcW w:w="', width,'"/>', bb, al, '</w:tcPr>', 
                  para(txt, align),
                  "</w:tc>\n", collapse = "")
  }
  
  return(ret)
}

para <- function(txt, align = "left", font_size = NULL, bold = FALSE) {
  
  ret <- ""
  
  vl <- txt
  if (nchar(txt) == 0)
    vl <- " "

  splt <- strsplit(vl, split = "\n", fixed = TRUE)
  
  for (i in seq_len(length(splt))) {
    
    
    for (j in seq_len(length(splt[[i]]))) {
    
      b <- ""
      if (bold == TRUE)
        b <-  '<w:b/><w:bCs/>'
      
      fs <- ""
      if (!is.null(font_size))
        fs <- paste0('<w:sz w:val="', font_size * 2, 
               '"/><w:szCs w:val="', font_size * 2, '"/>')
      
      rpr <- ""
      if (!is.null(font_size) | bold == TRUE)
        rpr <- paste0('<w:rPr>', b, fs, '</w:rPr>')
            
            
      if (align == "centre")
        align <- "center"
      
     ret <- paste0(ret, '<w:p>',
                   '<w:pPr><w:jc w:val="', align, '"/>',
                   '<w:spacing w:after="0"/>', '</w:pPr>',
                   '<w:r>', rpr, '<w:t xml:space="preserve">', splt[[i]][j], 
                   '</w:t></w:r></w:p>\n')
    }
  }
 
 return(ret)
  
}

run <- function(txt) {
  
  ret <- paste0('<w:r><w:t xml:space="preserve">', txt, '</w:t></w:r>', collapse = "")
  
  return(ret)
  
}


