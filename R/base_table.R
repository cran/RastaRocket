#' Create a Summary Table with Grouping and Custom Formatting
#'
#' This function generates a summary table from a data frame with specified
#' grouping and variable types. It uses the `gtsummary` package to create
#' descriptive statistics for categorical and continuous variables, with
#' options for customizing the rounding and labels.
#'
#' @inheritParams desc_var
#'
#' @return A `gtsummary` table summarizing the specified variables,
#'         grouped by `var_group` if provided, with customizable statistics
#'         and rounding options.
#'
#' @examples
#' # Example usage with the iris dataset
#' base_table(iris, var_group = "Species", show_missing_data = TRUE)
#'
#' @import gtsummary
#' @import dplyr
#' @import rlang
#' @export
base_table <- function(data1,
                       show_missing_data,
                       by_group = FALSE,
                       var_group,
                       quali = NULL,
                       quanti = NULL,
                       stat_var_quanti = c("{mean} ({sd})", "{median} ({p25} ; {p75})", "{min} ; {max}"),
                       digits = list(r_quanti = 1, r_quali = 1),
                       freq_relevel = FALSE){

  ##### check digits list
  if(!is.list(digits)) stop("digits argument must be a list")

  vec_check <- c("r_quanti", "r_quali")
  string_check <- paste(vec_check, collapse = " ")
  if(!dplyr::setequal(names(digits), vec_check)) stop(glue::glue("digits names must be {string_check}"))

  ####### check stat_var_quanti vector ##########
  vec_stat <- c("{mean} ({sd})", "{median} ({p25} ; {p75})", "{min} ; {max}", "{sum}")
  if(any(!stat_var_quanti %in% vec_stat)){
    stop(glue::glue("stat_var_quanti names must be `{vec_stat[1]}` for mean (SD); `{vec_stat[2]}` for median (Q1, Q3);  `{vec_stat[3]}` for range or `{vec_stat[4]}` for Sum"))
  }

  if(by_group){
    col_1 <- rlang::ensym(var_group)
  } else {
    col_1 <- NULL
  }

  ##### missing data parameters
  missing_stat <- ifelse(show_missing_data,
                         yes = "{N_nonmiss} ({N_miss})",
                         no = "{N_nonmiss}")

  ##### base gtsummary table
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
      missing = "always",
      missing_stat = missing_stat,
      ## Toujours calculer les NA
      statistic = list(
        gtsummary::all_continuous2() ~ stat_var_quanti,
        ## Stat à afficher pour les VAR (quantitatives)
        gtsummary::all_categorical() ~ "{n} ({p}%)" ## Stat à afficher pour les VAR (categorielles)
      ),
      sort = if(freq_relevel) list(gtsummary::all_categorical() ~ "frequency") else NULL,
      digits = list(all_continuous() ~ digits$r_quanti |> as.integer(),
                    all_categorical() ~ digits$r_quali |> as.integer()) ## le nbre de décimale pour les variables.
    ) %>%
    gtsummary::bold_labels()  ## Variables en gras.

  return(base_table)
}
