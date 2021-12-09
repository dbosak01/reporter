# reporter 1.2.7

* Added Arial, Times, and Courier fonts to PDF output type.  
* Fixed bug on RTF preventing _first_row_blank_ parameter from working properly.
* Added FAQ and Complete Examples.

# reporter 1.2.6

* Added 'HTML' output type to `create_report()` and `write_report()` functions.
This was a major change that allows the user to output reports in HTML. 
By default, the HTML is in a 'paged' format, meaning it is suitable for printing.
If you want unpaged HTML, set the _paper_size_ parameter on `create_report()`
to 'none'.  This configuration will create a standard HTML web page, with all
content dumped to a single page and no page breaks.
* Added _bold_ and _font_size_ parameters to the `titles()` function. This change
allows the user to create titles that are bold and a larger font size
than the page body.  
* Added 9pt and 11pt font to all output types that accept a font size.
* Added 'body' option to _borders_ parameter on `create_table()` function
so the user can get borders only on the table body.
* Various bug fixes and documentation updates.


# reporter 1.2.0

* Added Arial, Times, and Courier fonts to RTF output type.  This change required a
significant rewrite to RTF report creation functions.  Font is controlled using
the _font_ parameter on the `create_report()` function.  Old text-style RTF
output is still available using the "fixed" font.
* Added _font_size_ parameter to `create_report()` function.
* Added _valign_ parameter to `footnotes()` function.
* Added _width_ paremeter to `titles()`, `title_header()`, and `footnotes()`
functions.
* Added _borders_ parameter to `create_table()`, `create_text()`, and 
`create_plot()` functions.

# reporter 1.1.6

* Added support for covr and codecov.
* Fixed overflow bug on RTF on Linux when using 12pt font.
* Fixed line length/alignment issues on titles and footnotes.

# reporter 1.1.3

* Made package compatible back to R 3.6.
* Added GitHub Actions for prior R version checks.
* Figured out how to generate PDF files directly, instead of using 
Latex/Rmarkdown/MikTex/Pandoc.  Will greatly reduce dependencies and make
the **reporter** package much easier to install and test.  Also makes it much
easier to accurately render the PDF file.
* Allow user to add more than one `title header()`.
* Made _output_type_ parameter case insensitive.
* Added _standard_eval_ parameter to `define()`, `spanning_header()`,
and `column_defaults()` functions.  These parameters will replace curly brace 
escape on those functions.
Curly brace escape will remain active for backward compatibility, but is 
no longer documented.
* Fixed bug in RTF that was causing page overflows in LibreOffice Writer.
* Various other bug fixes.

# reporter 1.1.2

* Changed column width calculation to make sum of widths equal the total width of
the table.  Previously it was excluding the column gutter from the calculation.
* Removed error on title/footnote width being too long.  Changed to warning
and truncated text instead, so the user can at least see what is going on.
* Integrated logging functionality with **logr** package.  `write_report()` will
now log results automatically when logr autolog is enabled.
* Added _underline_ parameter to `spanning_header()` to turn it on or off
* Added 8pt font
* Added _border_ property to `titles()`, `footnotes()`, and `title_header()` 
functions.
* Added {{}} double curly escape on parameters accepting unquoted parameters, 
so that users could pass them as variables inside a function.
* Added pkgdown site
* Fixed bug on deleting image files in the temp directory.
* Various small bugs and documentation fixes/improvements

# reporter 1.0.6

* Added support for **survminer** graphics in `create_plot()`.

# reporter 1.0.5

A package to create statistical reports (TFLs). Contains the following features:

* Outputs reports in TXT, RTF, and PDF file format
* Titles, footnotes, page header, and page footer are repeated on each page
* Supports header labels and spanning headers
* Calculates default columns widths automatically
* Includes automatic wrapping and splitting of wide and long tables
* Integrates with the fmtr package to format numeric, date, and character data
* Plots from the popular ggplot2 package can be added to RTF and PDF reports
* Allows appending multiple tables to a report, multiple tables to a page, 
and intermingling of text, tables, and plots
* Supports in-report date/time stamps and "Page X of Y" page numbering

