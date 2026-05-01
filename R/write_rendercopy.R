#' write_rendercopy
#'
#' A function to write a R file rendercopy
#'
#' @param output_folder The output folder
#' @param path The path of the R script
#' @param name The name of the files
#'
#' @return Nothing
#' 
#' @importFrom glue glue
write_rendercopy <- function(output_folder,
                             path,
                             name){
  
  r_string <- glue::glue(
    'library(quarto)
    
# Type d\'analyse. Au choix : RDD, prelim, compl ou final
type_analyse <- "final" # A modifier

# Date du jour (AAAAMMJJ)
string_time <- format(Sys.time(), "%Y%m%d")

# Version du rapport - A modifier
version <- "V1.0"

# Nom du fichier HTML généré
output_name <- paste0("Rapport_", type_analyse, "_", version, "_", string_time, ".html")

# Render Quarto
quarto::quarto_render(
  "Reporting/{name}.qmd",
  output_file = output_name,
  execute_params = list(analyse = type_analyse)
)

# Dossier de copie. Pensez à modifier le chemin entre RDD et Analyse finale
output_copy <- "{output_folder}"

if (!dir.exists(output_copy)) {{
  dir.create(output_copy, recursive = TRUE)
}}

# Copie du fichier
file.copy(
  from = file.path("Reporting", output_name),
  to = file.path(output_copy, output_name),
  overwrite = TRUE
)

# Ouverture
browseURL(file.path(output_copy, output_name))
'
  )

writeLines(r_string, con = path)

invisible()
}
