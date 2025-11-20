
#' Customize Table Body
#'
#' This function modifies a data frame by updating the `stat_0` column. If any values in
#' `stat_0` are missing (`NA`), they are replaced by the values from the `n` column.
#' After the replacement, the `n` column is removed from the data frame.
#'
#' @param data A data frame that must contain at least two columns:
#'   - `stat_0`: A column whose missing (`NA`) values are to be replaced.
#'   - `n`: A column providing replacement values for `stat_0` when its values are missing.
#'
#' @return A modified data frame with:
#'   - Updated `stat_0` values (replaced with `n` values where `NA` is found).
#'   - The `n` column removed after integration.
#'
#' @details
#' - The function uses `dplyr::case_when` to conditionally update the `stat_0` column.
#' - After the replacement process, the `n` column is dropped using `dplyr::select(-n)`.
#' - This function is particularly useful for cleaning and preparing table data.
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   stat_0 = c(NA, "B", "C"),
#'   n = c(10, 20, 30)
#' )
#'
#' # Apply the function
#' modified_data <- customize_table_body(data)
#' print(modified_data)
#'
#' @importFrom dplyr mutate case_when select
#' @export


customize_table_body <- function(data) {
  data <- data %>%
    dplyr::mutate(stat_0 = dplyr::case_when(
      is.na(.data$stat_0) ~ paste0(.data$n),
      TRUE ~ .data$stat_0
    )) %>%
    dplyr::select(-n) # Supprime la colonne 'n' après son intégration dans 'stat_0'
  return(data)
}
