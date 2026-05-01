#' Prepare a Data Frame for Summarization with Custom Missing Data Handling and Factor Ordering
#'
#' This function prepares a data frame for summarization by handling missing data
#' based on the `show_missing_data` argument and applying the specified data manipulation
#' (DM) option to factor variables. It provides flexibility for data cleaning and ordering
#' before summarizing with functions like `gtsummary`.
#'
#' @inheritParams desc_var
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
#' @importFrom forcats fct_drop
#' @export
prepare_table <- function(data1,
                          by_group = FALSE,
                          var_group = NULL,
                          drop_levels = TRUE,
                          show_missing_data = TRUE,
                          include_all_na_cat = TRUE){


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

  ### Deal with factors with missing levels
  bool_all_na <- data1 |> dplyr::summarise(across(everything(), ~ all(is.na(.x)))) |> any()
  na_col_names <- data1 |> dplyr::summarise(across(everything(), ~ all(is.na(.x)))) |> dplyr::select(where(~isTRUE(.x))) |> names() |> dput()

  if(include_all_na_cat & bool_all_na){

    data1 <- data1 |> dplyr::mutate(
      dplyr::across(
        where(~ is.factor(.x) && length(levels(.x)) == 0),
        ~ forcats::fct_explicit_na(.x, na_level = "(d.m.)"))
    )

    rlang::warn(glue::glue("The {na_col_names} is factor with missing level (all values are NA) and is displayed. "))
  }


  if(drop_levels){
    data1 <- data1 %>%
      dplyr::mutate(across(where(is.factor) & !any_of(var_group),
                           ~ forcats::fct_drop(.x)))
  }

  return(data1)
}
