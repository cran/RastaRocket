
#' Modify Table Body
#'
#' This function modifies a table by updating the "stat_*" columns with corresponding
#' "add_n_stat_*" columns if they exist. If "stat_*" columns contain missing values
#' (`NA`), the function replaces them with the respective "add_n_stat_*" column values.
#' Extra "add_n_stat_*" columns are removed after processing.
#'
#' @param data A data frame that contains columns named `stat_*` and optionally
#' `add_n_stat_*`. The function assumes that `stat_*` columns are to be updated using
#' `add_n_stat_*` columns when necessary.
#'
#' @return A modified data frame where:
#'   - `stat_*` columns are updated to replace `NA` values with the corresponding values
#'     from `add_n_stat_*`.
#'   - `add_n_stat_*` columns are removed after processing.
#'   - If no `add_n_stat_*` columns exist, these columns are simply removed.
#'
#' @details
#' - The function identifies columns starting with "add_n_stat_" and attempts to use them
#'   to fill missing values in columns matching the pattern "^stat_\\d+$".
#' - If all required "add_n_stat_*" columns exist in `data`, they are utilized for this
#'   replacement; otherwise, the `add_n_stat_*` columns are removed without modifications
#'   to the `stat_*` columns.
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   n = c(1, 2, 3),
#'   stat_1 = c(NA, 5, 6),
#'   stat_2 = c(7, NA, 9),
#'   add_n_stat_1 = c(10, 11, 12),
#'   add_n_stat_2 = c(13, 14, 15)
#' )
#'
#' # Apply the function
#' modified_data <- modify_table_body_func(data)
#' print(modified_data)
#'
#' @importFrom dplyr rename mutate across select
#' @importFrom dplyr starts_with matches
#' @export



# Fonction pour modifier le corps du tableau
modify_table_body_func <- function(data) {
  add_n_stat_cols <- grep("^add_n_stat_", colnames(data), value = TRUE)

  if (all(add_n_stat_cols %in% colnames(data))) {
    data <- data %>%
      dplyr::rename("add_n_stat_0" = n) %>%
      dplyr::mutate(dplyr::across(
        matches("^stat_\\d+$"),  # Utilise une expression régulière pour sélectionner 'stat_' suivi d'un ou plusieurs chiffres
        ~ ifelse(is.na(.), get(paste0("add_n_stat_", sub("^stat_", "", cur_column()))), .)
      )) %>%
      dplyr::select(-starts_with("add_n_stat_"))
  } else {
    data <- data %>% dplyr::select(-starts_with("add_n_stat"))
  }

  return(data)
}

