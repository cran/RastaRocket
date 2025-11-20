#' Add missing value information to a gtsummary table
#'
#' This function adds information about missing and non-missing data counts to a gtsummary table.
#' It can also apply custom statistics by group and modify the table body with an external function.
#'
#' @param base_table A `gtsummary` table object.
#' @param show_missing_data Logical. If `TRUE`, shows the number of non-missing and missing values with percentages.
#' If `FALSE`, shows only non-missing values.
#' @param var_group Optional. A grouping variable name. If not `NULL`, additional stats are added by group.
#' @param by_group A boolean (default is FALSE) to analyse by group.
#' @return A `gtsummary` table object with missing value information and modifications applied.
#' @export

add_missing_info <- function(base_table,
                             show_missing_data,
                             var_group = NULL,
                             by_group = FALSE) {
  
  # Add missing data summary
  if (show_missing_data) {
    base_table_missing <- base_table %>%
      gtsummary::add_n("{N_nonmiss} ({N_miss})")
  } else {
    base_table_missing <- base_table %>%
      gtsummary::add_n("{N_nonmiss}")
  }
  
  # Add grouped stats if applicable
  if (by_group) {
    if (show_missing_data) {
      base_table_missing <- base_table_missing %>%
        gtsummary::add_stat(fns = everything() ~ add_by_n)
    } else {
      base_table_missing <- base_table_missing %>%
        gtsummary::add_stat(fns = everything() ~ add_by_n_noNA)
    }
  }
  
  # Modify table body and footnotes
  if(by_group){
    base_table_missing_2 <- base_table_missing %>%
      gtsummary::modify_table_body(~ modify_table_body_func(.)) %>%
      gtsummary::modify_footnote(everything() ~ NA)
  } else {
    base_table_missing_2 <- base_table_missing %>%
      gtsummary::modify_footnote(everything() ~ NA)
  }
  
  return(base_table_missing_2)
}
