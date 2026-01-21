#' Add "n (dm ; %dm)" to Variable Labels
#'
#' This function appends the text "n (dm ; %dm)" to the labels of all variables in a dataset.
#' It uses the `labelled` package to modify and update variable labels in-place.
#'
#' @param data A data frame containing the dataset whose variable labels need to be updated.
#' @param col_to_skip A column to skip when adding `"n (dm ; %dm)"`. Default is `NULL`.
#'
#' @details
#' The function iterates over all columns in the dataset and performs the following steps:
#' 1. Retrieves the current label of each variable using `labelled::var_label`.
#' 2. Creates a new label by appending the text `"n (dm ; %dm)"` to the existing label.
#' 3. Updates the variable's label using `labelled::set_variable_labels`.
#'
#' This is useful when preparing a dataset for descriptive analysis, where it is helpful to display
#' missing data statistics (`n`, `dm`, and `%dm`) alongside variable labels in summary tables.
#'
#' @return A data frame with updated variable labels.
#'
#' @examples
#' # Example usage:
#' library(labelled)
#'
#' # Create a sample dataset
#' data <- data.frame(
#'   var1 = c(1, 2, NA),
#'   var2 = c("A", "B", NA)
#' )
#'
#' # Assign initial labels
#' data <- labelled::set_variable_labels(
#'   data,
#'   var1 = "Variable 1",
#'   var2 = "Variable 2"
#' )
#'
#' # Add "n (dm ; %dm)" to labels
#' data <- ajouter_label_ndm(data)
#'
#' # Check updated labels
#' labelled::var_label(data)
#'
#' @importFrom labelled var_label set_variable_labels
#' @export



# Fonction pour ajouter une indication "n (dm ; %dm)" aux labels des variables
ajouter_label_ndm <- function(data,
                              col_to_skip = NULL) {

  data |>
    dplyr::mutate(
      dplyr::across(
        all_of(names(data)), ~ {
          # Obtenir le nom de la colonne courante
          nom_col <- dplyr::cur_column()
          # Obtenir le label actuel de la variable (si défini)
          label_actuel <- labelled::var_label(.x)

          # set variable name as label if NULL
          if (is.null(label_actuel) || is.na(label_actuel)) {
            label_actuel <- nom_col
          }

          # si colonne à exclure, on garde le label tel quel
          if (nom_col %in% col_to_skip) {
            nouveau_label <- label_actuel
          } else {
            # Créer un nouveau label en ajoutant "n (d.m.)" au label existant
            nouveau_label <- paste0(label_actuel, " n (d.m.)")
          }
          # Appliquer le nouveau label à la variable en utilisant la fonction set_variable_labels
          labelled::set_variable_labels(.x, nouveau_label)
        }
      )
    )
}
