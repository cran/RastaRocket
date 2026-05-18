#' Text indentation for `gtsummary` tables
#'
#' @param gts_table A `gtsummary` table object
#' @param indent A numerical value indicating how many space to indent text (default = 4). A value of 8 should be a good compromise.
#'
#' @returns A `gtsummary` table object with indentation applied.
#' @export
#'
#' @examples
#' tbl <- iris |>  dplyr::select(Species, Sepal.Length) |> RastaRocket::desc_var(
#' table_title = "test",
#' quali = "Species")
#' tbl_1 <- tbl |> indent_gtsummary_table(indent = 4)
#' tbl_2 <- tbl |> indent_gtsummary_table(indent = 8)
#' tbl_3 <- tbl |> indent_gtsummary_table(indent = 16)
#'
indent_gtsummary_table <- function(gts_table, indent = 4){

  gtsu_table <- gts_table |>  gtsummary::modify_indent(columns = label, rows = row_type %in% c("level", "missing"), indent = indent)

  if(indent > 4){
    ind <- indent - 4
    gtsu_table <- gtsu_table |> gtsummary::modify_indent(columns = label, rows = row_type == "label", indent = ind)
  }

  return(gtsu_table)
}
