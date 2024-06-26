# biometrics v1.2.1
* Added a number of new functions, and updated/improved help documentation for others.

# biometrics v1.1.0
* Refactored Markdown templates so that supporting files are sourced from the
    installed package instead of copying them into the RStudio project, this
    will substantially reduce the size of the package
* Tidied up the repository
* Added an RMarkdown template to create .docx files

# biometrics v1.0.5
* further updates to markdown templates
* Added TCRC Biostatistics report template
* Updates to biometrics HTML report vignette
* Added some helper functions
* Added quick_text function to format model lists for in-text reference
* Updated default table options in hooks.R for all templates

# biometrics v1.0.4
* Updated HTML report template to better align with current workflow
  - created analysis plan
  - added markdown hooks to template
  - plotly works 'out of the box'
  - added 'recoder' to the templates
* Added format_model function
* Added excel_ref package data
* Moved telethonkids_colours and telethonkids_palletes in to package data
* Documentation updates

# biometrics v1.0.3
* Changed the logic behind group_interval, which should now be more robust
* Added abbreviator function that creates Bootstrap 4 tooltip with an abbreviation and its definition
* Added recoder function that reads a text files and saves with UTF-8 encoding
* Added round_df function that rounds data frames to a specified number of decimal points
* Some minor updates to html_report

# biometrics v1.0.2
* Fixed issue that prevented HTML widgets from working in the HTML report template.

# biometrics v1.0.1

* Updated html report css heading styles (h1 - h3) - issue #1 closed
* Updatated html report and ioslides selecton.Rmd to include vignette lines in front matter - issue #2 closed
* Changed how the html report template applies kable table formatting and css style to figures - issue #3 closed
* fixed typo in html_report vignette

# biometrics v1.0.0

* Initial release
