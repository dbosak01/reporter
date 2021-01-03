# reporter 1.1.0

* Removed error on title/footnote width being too long.  Changed to warning
and truncated text instead, so the user can at least see what is going on.
* Integrated logging functionality with logr package.  write_report() will
now log results automatically when logr autolog is enabled.
* Added underline parameter to spanning_header to turn it on or off
* Added 8pt font
* Added border property to titles, footnotes, and title_header functions
* Added {{}} double curly escape on parameters accepting unquoted parameters, 
so that users could pass them as variables inside a function.
* Added pkgdown site
* Fixed bug on deleting image files in the temp directory.
* Various small bugs and documentation fixes/improvements

# reporter 1.0.6

* Added support for survminer graphics in create_plot()

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

