#' Reverse Log Transformation
#'
#' Creates a transformation object for a reverse log scale, which can be used in ggplot2 scales.
#'
#' @param base A numeric value specifying the logarithm base. Default is the natural logarithm (`exp(1)`).
#'
#' @return A transformation object compatible with ggplot2 scales.
#'
#' @details 
#' This function defines a reverse logarithmic transformation, where the transformation function is 
#' \deqn{-\log(x, \text{base})} and the inverse function is \deqn{\text{base}^{-x}}.
#' It is useful for cases where a decreasing log scale is needed.
#'
#' @examples
#' library(scales)
#' rev_log <- reverselog_trans(10)
#' rev_log$trans(100)  # -2
#' rev_log$inverse(-2) # 100
#'
#' @importFrom scales trans_new log_breaks
#' @export
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv, 
                    scales::log_breaks(base = base), 
                    domain = c(1e-100, Inf))
}
