#' Column selection with optional grouping variable
#'
#' This function extends `dplyr::select()` by allowing the dynamic addition of one or more grouping
#' variables (`var_group`) to the selection.
#'
#' It is especially useful when switching between an ungrouped analysis (e.g., all observations together)
#' and a grouped analysis (e.g., stratified or including interaction terms), without rewriting code.
#'
#' For instance, this allows you to write a single analysis command for both the RDD (Rapport de Démarrage des Données)
#' and the final report, simply by changing the `.qmd` file, without modifying the core analysis code.
#'
#' @param .data A data frame.
#' @param ... Columns to select (as in `dplyr::select()`).
#' @param var_group A character string or vector of column names to additionally include,
#' typically one or more grouping variables. Can be `NULL`.
#'
#' @return A data frame with the selected columns, including `var_group` if specified.
#'
#' @importFrom dplyr select all_of
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- tibble(x = 1:3, y = 4:6, z = 7:9)
#'
#' # Simple selection
#' select_plus(df, x, y)
#'
#' # Selection with grouping variable
#' select_plus(df, x, var_group = "z")


select_plus <- function(.data, ..., var_group = NULL) {
  if (is.null(var_group)) {
    dplyr::select(.data, ...)
  } else {
    dplyr::select(.data, ..., dplyr::all_of(var_group))
  }
}