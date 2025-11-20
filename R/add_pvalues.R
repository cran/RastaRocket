#' Add p-values and separate footnotes to a gtsummary object
#'
#' This function adds p-values to a gtsummary table using the specified tests
#' and separates the p-value footnotes.
#'
#' @param res A `gtsummary` table object.
#' @param tests A list of tests to pass to `gtsummary::add_p()`, or `TRUE` to use default tests.
#'
#' @return A `gtsummary` table object with p-values added and footnotes separated.
#'
#' @examples
#' library(gtsummary)
#' tbl <- trial %>% tbl_summary(by = trt)
#' tbl <- add_pvalues(tbl, tests = TRUE)
#' 
#' @export
add_pvalues <- function(res, tests) {
  if (is.list(tests)) {
    res <- res %>%
      gtsummary::add_p(test = tests) %>%
      gtsummary::separate_p_footnotes()
  } else if (isTRUE(tests)) {
    res <- res %>%
      gtsummary::add_p() %>%
      gtsummary::separate_p_footnotes()
  }
  return(res)
}
