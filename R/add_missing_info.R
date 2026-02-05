utils::globalVariables(c("variable"))

#' Add missing value information to a gtsummary table
#'
#' This function merge missing data row into label row of gtsummary object
#'
#' @param base_table A `gtsummary` table object.
#' @return A `gtsummary` table object with missing value information and modifications applied.
#' @export

add_missing_info <- function(base_table) {
  
  ## merge missing data row with label row
  base_table$table_body <- base_table$table_body %>%
    group_by(variable) %>%
    mutate(
      across(
        starts_with("stat_"),
        ~ if_else(
          row_type == "label" & is.na(.x),
          .x[row_type == "missing"],
          .x
        )
      )
    ) %>%
    ungroup() %>%
    filter(row_type != "missing")
  
  ## modify footnote
  base_table_missing <- base_table %>%
    gtsummary::modify_footnote(everything() ~ NA)
  
  return(base_table_missing)
}
