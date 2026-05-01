#' Text indentation for `gtsummary` tables
#'
#' @param g_table A `gt` table object
#' @param indent A numerical value corresponding to the pixel value, which defines the text indentation (default = 0 corresponding to px(0)). 30 ~ px(30) should be a good compromise.
#'
#' @returns A `gt` table object with indentation applied.
#' @export
#'
#' @examples
#' tbl_bis <- RastaRocket::desc_var(
#' iris,
#' table_title = "test",
#' quali = "Species")
#'
#' tbl_bis |> indent_table(indent = 30)
#' tbl_bis |> indent_table(indent = 60)
#'
indent_table <- function(g_table, indent = 0){


  # Convert gtsummary table to gt table
  if("gtsummary" %in% class(g_table)){
    g_table <- gtsummary::as_gt(g_table)
  }
  # indent the “label” columns (gtsummary tables))
  g_table <- g_table |>
    tab_style(
      style = cell_text(indent = gt::px(indent)),
      locations = cells_body(columns = label))


  return(g_table)
}



