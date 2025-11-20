utils::globalVariables(c("nb_NA", "by_col"))

#' Add Counts and Percentages of Missing Data by Group
#'
#' This function calculates and summarizes the counts and percentages of missing and non-missing values
#' for a specified variable, grouped by another variable. It provides formatted output for integration
#' into summary tables.
#'
#' @param data A data frame containing the dataset to analyze.
#' @param variable A character string specifying the target variable for which missing data statistics
#' will be computed.
#' @param by A character string specifying the grouping variable. The data will be grouped by this variable
#' before calculating the statistics.
#' @param tbl Not used in the current implementation but retained for compatibility with the `gtsummary` framework.
#' @param ... Additional arguments (not used in the current implementation).
#'
#' @details
#' The function performs the following steps:
#' 1. Groups the data by the variable specified in `by`.
#' 2. Computes the number of non-missing values (`nb`), the number of missing values (`nb_NA`),
#' and the percentage of missing values (`nb_percent`) for the specified `variable`.
#' 3. Renames and formats the output columns for clarity and readability.
#' 4. Converts the data into a wide format suitable for integration into summary tables,
#' with calculated statistics included in formatted strings (e.g., "value (missing_count ; missing_percent%)").
#'
#' The output is designed for use with summary tools, such as `gtsummary`, to display detailed missing
#' data statistics alongside descriptive statistics.
#'
#' @return A data frame in wide format, where each row represents a group (as defined by `by`), and columns
#' include statistics for the target variable (`variable`) in a formatted string.
#'
#' @examples
#' # Example usage:
#' library(dplyr)
#' library(tidyr)
#' data(mtcars)
#'
#' # Add missing data statistics grouped by 'cyl'
#' add_by_n(
#'   data = mtcars,
#'   variable = "mpg",
#'   by = "cyl"
#' )
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data set_names
#' @export
#' 
add_by_n <- function(data, variable, by, tbl, ...) {

  data %>%
    # Sélection des colonnes pertinentes : la variable cible et la colonne de regroupement
    dplyr::select(all_of(c(variable, by))) %>%

    # Regrouper les données en fonction de la colonne spécifiée par "by"
    dplyr::group_by(.data[[by]], .drop = FALSE) %>%

    # Calcul des statistiques pour chaque groupe
    dplyr::summarise_all(
      list(
        nb = ~ sum(!is.na(.)),           # Nombre de valeurs non manquantes
        nb_NA = ~ sum(is.na(.))        # Nombre de valeurs manquantes
      ), 1 # Marginalisation pour appliquer les fonctions
    ) %>%

    # Renommage des colonnes pour une meilleure lisibilité
    rlang::set_names(c("by", "variable", "nb_NA")) %>%

    # Création de nouvelles colonnes pour la sortie formatée
    dplyr::mutate(
      by_col = paste0("add_n_stat_", dplyr::row_number()), # ID unique basé sur le numéro de ligne
      variable = gtsummary::style_number(variable),                  # Formatage éventuel de la variable (fonction externe)
      variable = paste0(variable, " ", "(", nb_NA, ")") # Concaténation des stats dans une chaîne lisible
    ) %>%

    # Suppression des colonnes intermédiaires inutiles
    dplyr::select(-c(by, nb_NA)) %>%

    # Transformation des données en format large
    tidyr::pivot_wider(
      names_from = by_col,         # Utiliser "by_col" comme noms de colonnes
      values_from = variable       # Les valeurs proviennent de la colonne "variable"
    )
}
