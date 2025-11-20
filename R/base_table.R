#' Create a Summary Table with Grouping and Custom Formatting
#'
#' This function generates a summary table from a data frame with specified 
#' grouping and variable types. It uses the `gtsummary` package to create 
#' descriptive statistics for categorical and continuous variables, with 
#' options for customizing the rounding and labels.
#'
#' @param data1 A data frame containing the data to summarize.
#' @param by_group A boolean (default is FALSE) to analyse by group.
#' @param var_group A string or NULL, the variable to group by (optional). 
#'        If NULL, no grouping will be applied.
#' @param quali A character vector, the names of categorical variables to 
#'        treat as categorical in the summary table.
#' @param quanti A character vector, the names of continuous variables to 
#'        treat as continuous in the summary table.
#' @param digits A list, the number of decimal places to round categorical and
#'        continuous variable. Default is list(mean_sd = 1,
#'        median_q1_q3_min_max = 1, pct = 1).
#' @return A `gtsummary` table summarizing the specified variables, 
#'         grouped by `var_group` if provided, with customizable statistics 
#'         and rounding options.
#'
#' @examples
#' # Example usage with the iris dataset
#' base_table(iris, var_group = "Species")
#'
#' @import gtsummary
#' @import dplyr
#' @import rlang
#' @export
base_table <- function(data1,
                       by_group = FALSE,
                       var_group,
                       quali = NULL,
                       quanti = NULL,
                       digits = list(mean_sd = 1,
                                     median_q1_q3_min_max = 1,
                                     pct = 1)){
  
  ##### check digits list
  if(!is.list(digits)) stop("digits argument must be a list")
  
  vec_check <- c("mean_sd", "median_q1_q3_min_max", "pct")
  string_check <- paste(vec_check, collapse = " ")
  if(!dplyr::setequal(names(digits), vec_check)) stop(glue::glue("digits names must be {string_check}"))
  
  ##### clean formating
  
  vec_round_quanti <- c(rep(digits$mean_sd, 2),
                        rep(digits$median_q1_q3_min_max, 5))
  vec_round_quali <- c(0, digits$pct)
  
  if(by_group){
    col_1 <- rlang::ensym(var_group)  
  } else {
    col_1 <- NULL
  }
  
  base_table <- data1 %>%
    gtsummary::tbl_summary(
      type = list(
        where(is.factor) ~ "categorical",
        ## type des var (qualitatives)
        gtsummary::all_continuous() ~ "continuous2",
        ## type des var (continus)
        quali ~ "categorical",
        quanti ~ "continuous2"
      ),
      by = !!col_1,
      ## Pour degrouper les tables
      missing = "no",
      ## Ne pas afficher les NA
      statistic = list(
        gtsummary::all_continuous2() ~ c("{mean} ({sd})", "{median} ({p25} ; {p75})","{min} ; {max}"),
        ## Stat à afficher pour les VAR (quantitatives)
        gtsummary::all_categorical() ~ "{n} ({p}%)" ## Stat à afficher pour les VAR (categorielles)
      ),
      digits = list(all_continuous() ~ vec_round_quanti,
                    all_categorical() ~ vec_round_quali) ## le nbre de décimale pour les variables.
    ) %>%
    gtsummary::bold_labels()  ## Variables en gras.
  
  return(base_table)
}