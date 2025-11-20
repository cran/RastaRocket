utils::globalVariables(c("row_type"))

#' Custom formatting for `gtsummary` tables
#'
#' This function takes a `gt` table and applies custom formatting. It allows you to align columns,
#' apply bold text to certain rows, and adjust column widths if specified.
#'
#' @param gt_table A `gt` table object (also handles gtsummary tables by converting them).
#' @param align A character string defining the alignment of specific columns. Passed to the
#'   `gt::cols_align()` function (e.g., "left", "right", "center"). Default is "right".
#' @param column_size A named list or vector defining the width of columns (optional). The list should specify
#'   the width for one or more columns. If not provided, column widths will not be modified.
#'
#' @return A `gt` table object with the specified formatting applied.
#'   The table will have columns aligned according to the `align` parameter, 
#'   and cells in the "label" rows will have bold text. If `column_size` is provided,
#'   the column widths will be adjusted accordingly.
#'
#' @examples
#' # Example usage
#' tbl <- RastaRocket::desc_var(iris,
#'   table_title = "test",
#'   group = TRUE,
#'   var_group = "Species")
#' formatted_tbl <- custom_format(tbl,
#'   align = "center",
#'   column_size = list(label ~ gt::pct(50)))
#' formatted_tbl
#'
#' @export
custom_format <- function(gt_table,
                          align = "right",
                          column_size = NULL) {
  
  if("gtsummary" %in% class(gt_table)){
    gt_table <- gtsummary::as_gt(gt_table)
  }
  # Convert gtsummary table to gt table and align columns
  res <- gt_table %>%
    gt::cols_align(align = align, columns = gt::starts_with("stat")) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = row_type == "label")
    )
  
  # If column_size is provided, apply column widths
  if (!is.null(column_size)) {
    res <- res %>%
      gt::cols_width(.list = column_size)
  }
  
  return(res)
}
