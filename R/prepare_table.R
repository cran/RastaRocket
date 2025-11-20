#' Prepare a Data Frame for Summarization with Custom Missing Data Handling and Factor Ordering
#'
#' This function prepares a data frame for summarization by handling missing data 
#' based on the `show_missing_data` argument and applying the specified data manipulation 
#' (DM) option to factor variables. It provides flexibility for data cleaning and ordering 
#' before summarizing with functions like `gtsummary`.
#'
#' @param data1 A data frame containing the data to be prepared.
#' @param drop_levels Boolean (default = TRUE). Drop unused levels.
#' @param freq_relevel Boolean  (default = FALSE). Reorder factors by frequency.
#' @param by_group A boolean (default is FALSE) to analyse by group.
#' @param var_group The group variable (used to correctly update the label if needed).
#' @return A data frame that has been prepared based on the `show_missing_data` and `DM` arguments. 
#'         The function modifies the input data frame by applying labels, ordering factor variables, 
#'         and potentially dropping unused levels.
#' @param show_missing_data Should the missing data be displayed. Can be either :
#'   - `FALSE`: No missing data displayed
#'   - `TRUE`(default): Missing data displayed
#'   
#' @details
#' - The `DM` option defines the data manipulation to be applied to factor variables:
#'   - `"tout"`: Both order factor levels and drop unused levels.
#'   - `"tri"`: Only order factor levels.
#'   - `"remove"`: Drop unused factor levels without ordering.
#'
#' @examples
#' # Example usage with the iris dataset
#' prepare_table(iris)
#'
#' @import dplyr
#' @import RastaRocket
#' @import forcats
#' @export
prepare_table <- function(data1,
                          by_group = FALSE,
                          var_group = NULL,
                          drop_levels = TRUE,
                          freq_relevel = FALSE,
                          show_missing_data = TRUE){
  
  
  ### Remove grouping variable if not used
  if (!by_group && !is.null(var_group) && var_group %in% names(data1)) {
    data1 <- data1 %>% dplyr::select(-all_of(var_group))
  }
  
  ### Deal with missing data
  if(show_missing_data){
    if(by_group){
      data1 <- data1 %>% RastaRocket::ajouter_label_ndm(col_to_skip = var_group)
    } else {
      data1 <- data1 %>% RastaRocket::ajouter_label_ndm()
    }
  } else {
    if(anyNA(data1)){
      warning("You ask not to show missing data but some are present in data1, be careful")
    }
  }
  
  ### Apply DM option
  if(freq_relevel){
    data1 <- ordonner_variables_qualitatives(data1)
  }
  
  if(drop_levels){
    data1 <- data1 %>%
      dplyr::mutate(across(where(is.factor) & !any_of(var_group),
                           ~ forcats::fct_drop(.)))
  }
  
  return(data1)
}
