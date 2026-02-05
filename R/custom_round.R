#' Custom Round and Format
#'
#' Rounds a numeric value to a specified number of decimal places and formats it
#' to always show the specified number of decimal places, including trailing zeros.
#'
#' @param x A numeric vector to be rounded and formatted.
#' @param digits An integer indicating the number of decimal places to round to. Defaults to 1.
#'
#' @return A character vector with the rounded and formatted numbers.
#'
#' @examples
#' RastaRocket::custom_round(3.14159)      # "3.1"
#' RastaRocket::custom_round(3.14159, 3)   # "3.142"
#' RastaRocket::custom_round(c(2, 2.5), 2) # "2.00" "2.50"
#'
#' @export
custom_round <- function(x, digits = 1) {
  res <- x |>
    round(digits = digits) |>
    format(nsmall = digits)

  return(res)
}
