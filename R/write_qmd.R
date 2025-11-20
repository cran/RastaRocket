#' Write a Quarto Markdown (.qmd) file
#'
#' This function generates a Quarto Markdown (.qmd) file with predefined metadata and a sample table.
#'
#' @param path Character string specifying the output file path for the .qmd file.
#' @param path_html Character string specifying the path to an HTML file to be included before the body of the document.
#' @param path_css Character string specifying the path to a CSS file for styling the document.
#'
#' @details
#' The function creates a Quarto Markdown file with metadata fields such as title, author, date, and format settings.
#' The HTML file specified in `path_html` is included before the body, and the CSS file specified in `path_css`
#' is used for styling. The generated document includes an example of a table with a caption.
#'
#' @return None. The function writes a .qmd file to the specified `path`.
#'
#' @importFrom glue glue
#' @export
write_qmd <- function(path,
                      path_html,
                      path_css){
  qmd_string <- glue::glue(
  '---
  format:
    html:
      toc: true
      toc-location: left
      toc-depth: 3
      toc-expand: true
      number-sections: true # Numérotation des sections
      tbl-cap-location: top # Met les titres de tableaux au-dessus
      table-numbering: true
      df-print: paged
      html-table-processing: none
      embed-resources: true
      theme: cerulean
      include-before-body: {path_html}
  css: {path_css}
  echo: false
  warning: false
  message: false
  editor: source
  ---
  ')
  
  qmd_string <- paste0(
    qmd_string,
"

We start simple 

```\U007Br\U007D
#| tbl-cap: 'A caption'
#| label: tbl-firsttable
iris |>
  RastaRocket::desc_var()
```

")
  
  write.table(qmd_string, 
              file = path, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
    
}