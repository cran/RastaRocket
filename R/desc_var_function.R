
#' Generate Descriptive Tables for Variables
#'
#' This function creates descriptive tables for variables in a dataset. It can handle qualitative and quantitative variables, grouped or ungrouped, and supports multiple configurations for handling missing data (DM).
#'
#' @param data1 A data frame containing the dataset to be analyzed.
#' @param table_title A character string specifying the title of the table.
#' @param quali A vector of qualitative variables to be described. Defaults to `NULL`.
#' @param quanti A vector of quantitative variables to be described. Defaults to `NULL`.
#' @param add_total A boolean (default is TRUE) to add total column or not when var_group is specified.
#' @param var_title A character string for the title of the variable column in the table. Defaults to `"Variable"`.
#' @param by_group A boolean (default is FALSE) to analyse by group.
#' @param var_group A variable used for grouping (if applicable). Defaults to `NULL`.
#' @param group_title A character string specifying the title for the grouping variable. Default is `NULL` and get the label or the variable.
#' @param digits A list, the number of decimal places to round categorical and
#'        continuous variable. Default is list(mean_sd = 1,
#'        median_q1_q3_min_max = 1, pct = 1).
#' @param drop_levels Boolean (default = TRUE). Drop unused levels.
#' @param freq_relevel Boolean (default = FALSE). Reorder factors by frequency.
#' @param tests A value in order to add p value. Default to `FALSE` OPTION :
#'   - `FALSE`: No p-value add
#'   - `TRUE`: Add p-value made by default by gtsummary. See gtsummary add_p() options.
#'   - `list()`: To force tests. See gtsummary add_p() options.
#' @param show_n_per_group Default to `FALSE`. Should the 'N' appears in the column header of the groups. Can be either :
#'   - `FALSE`: No 'N' is shown
#'   - `TRUE`: 'N' is shown
#' @param show_missing_data Default to `NULL`. Should the missing data be displayed. Can be either :
#'   - `FALSE`: No missing data displayed
#'   - `TRUE`: Missing data displayed
#'   - `NULL` (default): will be switch to `anyNA(data1)` value.
#' @param var_tot A string specifying the name of total column. Default is `NULL` and will guess from `theme_gtsummary_language()`.
#' @param var_characteristic A string specifying the name of characteristic column. Default is `NULL` and will guess from `theme_gtsummary_language()`.
#'
#' @details
#' The function processes the dataset according to the specified parameters and generates descriptive tables.
#' - It first uses the `ajouter_label_ndm()` function to append missing data statistics to variable labels.
#' - Depending on the `group` and `DM` arguments, it adjusts the dataset and creates tables using helper functions like `desc_group`, `desc_degroup`, and `desc_degroup_group`.
#' - Qualitative variables are reordered, and unused levels are dropped when necessary.
#'
#' @return A gtsummary table object containing the descriptive statistics.
#'
#' @examples
#' # Example usage:
#' library(dplyr)
#'
#' # Sample dataset
#' data1 <- data.frame(
#'   group = c("A", "B", "A", "C"),
#'   var1 = c(1, 2, 3, NA),
#'   var2 = c("X", "Y", "X", NA)
#' )
#'
#' # Generate descriptive table
#' table <- desc_var(
#'   data1 = data1,
#'   table_title = "Descriptive Table"
#' )
#'
#' @importFrom dplyr mutate across where
#' @importFrom forcats fct_drop
#' @importFrom tidyr drop_na
#' @importFrom gtsummary tbl_summary
#' @import cardx
#' @export

# data1 = iris
# table_title = ""
# quali = NULL
# quanti = NULL
# group = FALSE
# var_title = "Variable"
# var_group = "Species" ## Variable de groupe (dégroupée les tables)
# group_title = NULL
# digits = list(mean_sd = 1,
#               median_q1_q3_min_max = 1,
#               pct = 1)
# drop_levels = TRUE
# freq_relevel = FALSE
# tests = FALSE
# show_missing_data = FALSE
# show_n_per_group = TRUE
# add_total = TRUE
# var_tot = "Total"

desc_var <- ## Les arguments de la fonction
  function(data1,
           ##  Jeux de données numéro 1.
           table_title = "",
           ## Titre de la table
           quali = NULL,
           ## Vecteur des variables quali ~tatives mal decrites
           quanti = NULL,
           ## Vecteur des variables quantitatives mal decrites
           add_total = TRUE,
           var_title = "Variable",
           by_group = FALSE, ## booléen pour préciser s'il faut degroupé ou pas les tables.
           var_group = NULL, ## Variable de groupe (dégroupée les tables)
           group_title = NULL,
           digits = list(mean_sd = 1,
                         median_q1_q3_min_max = 1,
                         pct = 1),
           drop_levels = TRUE,
           freq_relevel = FALSE,
           tests = FALSE,
           show_n_per_group = FALSE,
           show_missing_data = NULL,
           var_tot = NULL,
           var_characteristic = NULL) {
    
    # Check consistency between by_group and var_group
    if(by_group && is.null(var_group)){
      stop("If 'by_group = TRUE', you must provide a 'var_group'.")
    }
    if(!by_group && !is.null(var_group)){
      message("Note: 'var_group' is provided but 'by_group = FALSE'. The grouping variable will be ignored.")
    }
    
    # Check that statistical tests are not requested in ungrouped mode
    if(!by_group && isTRUE(tests)){
      stop("Cannot perform statistical tests when 'by_group = FALSE'. Please set 'by_group = TRUE' to enable tests.")
    }
    
    ### get missing data
    bool_missing_data <- anyNA(data1)
    
    if(is.null(show_missing_data)){
      show_missing_data <- bool_missing_data
    }
    
    if(!show_missing_data & bool_missing_data){
      warning("show_missing_data is set to FALSE but there are missing data in the dataset.") 
    }
    
    ### Prepare table
    data2 <- prepare_table(data1 = data1,
                           by_group = by_group,
                           var_group = var_group,
                           drop_levels = drop_levels,
                           freq_relevel = freq_relevel,
                           show_missing_data = show_missing_data)
    
    ### Basic gtsummary
    base_table <- base_table(data1 = data2,
                             by_group = by_group,
                             var_group = var_group,
                             quali = quali,
                             quanti = quanti,
                             digits = digits)
    
    ### Customize output
    res <- customize_table(base_table = base_table,
                           by_group = by_group,
                           var_group = var_group,
                           add_total = add_total,
                           show_missing_data = show_missing_data,
                           show_n_per_group = show_n_per_group,
                           group_title = group_title,
                           table_title = table_title,
                           var_title = var_title,
                           var_tot = var_tot,
                           var_characteristic = var_characteristic)
    
    # Add p-values
    res <- add_pvalues(res = res, tests = tests)
    
    return(res)
    
  }


  
