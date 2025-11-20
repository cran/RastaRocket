
#' Reorder Levels of Qualitative Variables by Frequency
#'
#' This function reorders the levels of all qualitative (factor) variables in a dataset based on their frequency,
#' in descending order. It ensures that the most frequent levels appear first when analyzing or visualizing the data.
#'
#' @param data A data frame containing the dataset with qualitative variables to reorder.
#'
#' @details
#' The function applies the following transformations:
#' - Identifies all columns of type `factor` in the dataset.
#' - Reorders the levels of each factor variable using the `forcats::fct_infreq()` function,
#' which orders levels by decreasing frequency.
#'
#' This is particularly useful for preparing datasets for visualization or analysis, where it can be helpful
#' to have the most common levels displayed first.
#'
#' @return A data frame with reordered levels for all factor variables. Non-factor variables remain unchanged.
#'
#' @examples
#' # Example usage:
#' library(dplyr)
#' library(forcats)
#'
#' # Create a sample dataset
#' data <- data.frame(
#'   var1 = factor(c("A", "B", "A", "C", "B", "B")),
#'   var2 = factor(c("X", "Y", "X", "Y", "X", "Z")),
#'   var3 = c(1, 2, 3, 4, 5, 6)  # Non-factor variable
#' )
#'
#' # Reorder qualitative variables by frequency
#' data <- ordonner_variables_qualitatives(data)
#'
#' # Check the new order of levels
#' levels(data$var1) # Output: "B" "A" "C"
#' levels(data$var2) # Output: "X" "Y" "Z"
#'
#' @importFrom dplyr mutate across where
#' @importFrom forcats fct_infreq
#' @export

# Fonction pour ordonner les variables qualitatives dans un jeu de données

ordonner_variables_qualitatives <- function(data) {
  # Appliquer des transformations aux colonnes du data.frame
  data %>%
    dplyr::mutate(
      # Sélectionner les colonnes de type "factor" et les réordonner
      dplyr::across(
        where(is.factor),                       # Appliquer l'opération uniquement sur les colonnes de type facteur
        ~ forcats::fct_infreq(.)               # Réordonner les niveaux des facteurs par fréquence décroissante
      )
    )
}
