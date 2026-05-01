#' Write a Quarto Markdown (.qmd) file
#'
#' This function generates a Quarto Markdown (.qmd) file with predefined metadata and a sample table.
#'
#' @param path Character string specifying the output file path for the .qmd file.
#' @param path_html Character string specifying the path to an HTML file to be included before the body of the document.
#' @param path_css Character string specifying the path to a CSS file for styling the document.
#' @param study_abbreviation Character string providing the abbreviation of the study.
#' @param name The name of the files
#'
#' @details
#' The function creates a Quarto Markdown file with metadata fields such as title, author, date, and format settings.
#' The HTML file specified in `path_html` is included before the body, and the CSS file specified in `path_css`
#' is used for styling. The generated document includes an example of a start of report.
#'
#' @return None. The function writes a .qmd file to the specified `path`.
#'
#' @importFrom glue glue
#' @import here
#' @export
write_qmd <- function(path,
                      path_html,
                      path_css,
                      study_abbreviation,
                      name){
  path_user <- here::here()
  qmd_string <- glue::glue(
  "---
  params:
    analyse: finale        # D\u00E9faut mais ce qui sera pris en compte c'est ce qui est not\u00E9 dans rendercopy
  format:
    html:
      toc: true
      toc-location: left
      number-sections: true # Num\u00E9rotation des sections
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
  
```{{r}}
#| echo: false
#| output: asis

titre <- dplyr::case_when(
  params$analyse == 'RDD'    ~ 'Revue des donn\u00E9es',
  params$analyse == 'final'  ~ 'Analyse finale',
  params$analyse == 'prelim' ~ 'Analyse pr\u00E9liminaire',
  params$analyse == 'compl'  ~ 'Analyse compl\u00E9mentaire',
  TRUE ~ 'Analyse'
)

cat(sprintf(\"
<script>
document.addEventListener('DOMContentLoaded', function() {{
  const el = document.getElementById('titre-rapport');
  if (el && '%s' !== '') {{
    el.textContent = '%s';
  }}
}});
</script>
\", titre, titre))

```  

```{{r ana}}
bool_grp <- params$analyse != 'RDD'
```

::: custom-table
| VERSION | DATE                               | RAISON   |
|---------|------------------------------------|----------|
| 01      | `r format(Sys.Date(), '%d/%m/%Y')` | Cr\u00E9ation |
  
: **TABLEAU DES EVOLUTIONS**
:::
  
  <!-- Non obligatoire si pas de probl\u00E8me de chemin au Render --> 
```{{r init}}
setwd('{path_user}')
here::i_am('reporting/{name}.qmd')
``` 

```{{r prog_principal, results='hide', fig.show='hide'}}
source(file = here::here('script/00_Programme_principal_{study_abbreviation}.R'), encoding = 'UTF-8')
```


# ABREVIATIONS

| Abr\u00E9viations | D\u00E9finitions                                             |
|:-------------|:--------------------------------------------------------|
| d.m.         | donn\u00E9es manquantes                                      |
| IC           | Intervalle de confiance                                 |
| USMR         | Unit\u00E9 de Soutien m\u00E9thodologique \u00E0 la recherche clinique | 
  
  
# INTRODUCTION
  
Ce rapport d'analyse statistique contient les r\u00E9sultats de l'\u00E9tude conform\u00E9ment aux analyses et m\u00E9thodes d\u00E9finies 
dans le plan d'analyse statistique version XX du XX/XX/XXXX.

`r if (params$analyse == 'RDD') '::: {{.content-hidden}}'`
Les donn\u00E9es sont issues de la base de donn\u00E9es fig\u00E9e le XX/XX/XXXX.
`r if (params$analyse == 'RDD') ':::'`

`r if (params$analyse != 'RDD') '::: {{.content-hidden}}'`
Les donn\u00E9es sont issues de la base de donn\u00E9es gel\u00E9e le XX/XX/XXXX.
`r if (params$analyse != 'RDD') ':::'` 


# Description de la randomisation, des inclusions et des suivis

## V\u00E9rification des crit\u00E8res d'\u00E9ligibilit\u00E9

```{{r}}
#| label: tbl-ci
#| tbl-cap: \"Description des crit\u00E8res d'\u00E9ligibilit\u00E9\"
desc_CI 
``` 
  
  ")

writeLines(qmd_string, con = path)
}