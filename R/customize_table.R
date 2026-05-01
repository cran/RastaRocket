#' Customize a Summary Table with Grouping, Missing Data, and Custom Titles
#'
#' This function customizes a `gtsummary` summary table by adding an overall column,
#' handling missing data, applying group-specific statistics, and updating headers
#' and captions. It provides flexible options for grouping, displaying missing data,
#' and customizing table titles.
#'
#'
#' @param base_table A `gtsummary` table object, typically generated using functions like `gtsummary::tbl_summary`.
#' @inheritParams desc_var
#'
#' @return A customized `gtsummary` table object with added columns, headers, captions,
#'         and modifications based on the provided arguments.
#'
#' @details
#' - The `show_missing_data` parameter determines whether missing data counts and
#'   percentages are displayed:
#'   - If `TRUE`, missing data columns are added.
#'   - If `FALSE`, only non-missing data counts are displayed.
#' - Headers for columns and spanning headers are customized using the `group_title`,
#'   `table_title`, and `var_title` arguments.
#'
#' @examples
#' # Example usage with a sample gtsummary table
#' library(gtsummary)
#' base_table <- trial %>%
#'   gtsummary::tbl_summary(
#'     type = list(
#'       gtsummary::all_continuous() ~ "continuous2"
#'     ),
#'     by = "trt",
#'     missing = "always",
#'     missing_stat = "{N_nonmiss} ({N_miss})",
#'     statistic = list(
#'       gtsummary::all_continuous2() ~ c("{mean} ({sd})",
#'                                        "{median} ({p25} ; {p75})",
#'                                        "{min} ; {max}")
#'     ))
#'
#' customize_table(
#'   base_table,
#'   var_group = "trt",
#'   add_total = TRUE,
#'   show_missing_data = TRUE,
#'   show_n_per_group = FALSE,
#'   group_title = "Treatment Group",
#'   table_title = "Summary Statistics",
#'   var_title = "Variables",
#'   var_tot = "Total"
#' )
#'
#' @import gtsummary
#' @import dplyr
#' @export
customize_table <- function(base_table,
                            by_group = FALSE,
                            var_group,
                            add_total,
                            show_missing_data,
                            show_n_per_group,
                            group_title,
                            table_title,
                            var_title,
                            var_tot = NULL,
                            var_characteristic = NULL){

  ### Add Overall column if specified
  if(by_group & add_total){
    base_table <- base_table %>%
      gtsummary::add_overall()
  }

  ### Add missing values
  base_table_missing <- add_missing_info(base_table = base_table)

  ### header
  res <- custom_headers(base_table_missing = base_table_missing,
                        var_characteristic = var_characteristic,
                        show_missing_data = show_missing_data,
                        show_n_per_group = show_n_per_group,
                        var_tot = var_tot,
                        var_group = if(by_group) var_group else NULL,
                        group_title = if(by_group) group_title else NULL,
                        table_title = table_title)

  return(res)
}
