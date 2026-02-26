
#' desc_ei_per_grade
#'
#' A function to describe adverse events (AE) by grade.
#'
#' @param df_pat_grp A dataframe with two columns: USUBJID (Patient id) and RDGRPNAME (the RCT arm).
#' @param df_pat_grade A dataframe with four columns: USUBJID (Patient id), EINUM (the AE id), EIGRDM (the AE grade) and EIGRAV (the AE severity which must be "Grave" and "Non grave").
#' @param id_col  Patient id column (default: "USUBJID").
#' @param group_col group column, the rct arm (default: "RDGRPNAME").
#' @param ei_num_col AE id column (default: "EINUM").
#' @param ei_grdm_col AE grade column (default: "EIGRDM").
#' @param ei_grav_col AE severity column (default: "EIGRAV").
#' @param severity A boolean to show severe adverse event line or not (default: TRUE).
#' @param digits Number of digits for percentages
#'
#' @return A gt table summarizing the AE by grade.
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples
#' df_pat_grp <- data.frame(USUBJID = paste0("ID_", 1:10),
#'                          RDGRPNAME = c(rep("A", 3), rep("B", 3), rep("C", 4)))
#'
#' df_pat_grade <- data.frame(USUBJID = c("ID_1", "ID_1",
#'                                       "ID_2",
#'                                       "ID_8",
#'                                       "ID_9"),
#'                            EINUM = c(1, 2,
#'                                       1,
#'                                       1,
#'                                       1),
#'                            EIGRDM = c(1, 3,
#'                                      4,
#'                                      2,
#'                                      4),
#'                            EIGRAV = c("Grave", "Non grave",
#'                                       "Non grave",
#'                                       "Non grave",
#'                                       "Grave"))
#'
#' desc_ei_per_grade(df_pat_grp = df_pat_grp,
#'                   df_pat_grade = df_pat_grade)
#'
#'


desc_ei_per_grade <- function(df_pat_grp,
                              df_pat_grade,
                              id_col = "USUBJID",
                              group_col = "RDGRPNAME",
                              ei_num_col = "EINUM",
                              ei_grdm_col = "EIGRDM",
                              ei_grav_col = "EIGRAV",
                              severity = TRUE,
                              digits = 1){


  id_col <- rlang::ensym(id_col)
  group_col <- rlang::ensym(group_col)
  ei_num_col <- rlang::ensym(ei_num_col)
  ei_grdm_col <- rlang::ensym(ei_grdm_col)
  ei_grav_col <- rlang::ensym(ei_grav_col)

  ##### Check column names and remove duplicates

  if(any(!c(rlang::as_string(id_col), rlang::as_string(group_col)) %in% colnames(df_pat_grp))){
    stop(glue::glue("df_pat_grp should contain '{rlang::as_string(id_col)}' = the patient id and '{rlang::as_string(group_col)}' = the randomization group"))
  }

  if(any(!c(rlang::as_string(id_col), rlang::as_string(ei_grdm_col), rlang::as_string(ei_num_col)) %in% colnames(df_pat_grade))){
    stop(glue::glue("df_pat_grade should contain '{rlang::as_string(id_col)}' = the patient id, '{rlang::as_string(ei_grdm_col)}' = the AE grade, and '{rlang::as_string(ei_num_col)}' = the AE id"))
  }

  ##### Check severity encoding

  if(severity){
    if(any(!rlang::as_string(ei_grav_col) %in% colnames(df_pat_grade))){
      stop(glue::glue("Because you used severity = TRUE, df_pat_grade should contain '{rlang::as_string(ei_grav_col)}' = the AE severity coded as 'Grave' or 'Non grave'"))
    }

    vec_severity <- unique(na.omit(dplyr::pull(df_pat_grade, !!ei_grav_col)))
    #vec_severity <- unique(na.omit(df_pat_grade |> pull(!!ei_grav_col)))
    if(!all(vec_severity %in% c("Grave", "Non grave"))){
      stop(glue::glue("'{rlang::as_string(ei_grav_col)}' should contain only 'Grave' or 'Non grave' but it contains: {paste(vec_severity, collapse = '; ')}"))
    }
  }


  ##### clean type and df, remove duplicate rows

  df_pat_grp <- df_pat_grp |>
    dplyr::mutate(id_pat = as.character(!!id_col),
                  grp = as.character(!!group_col)) |>
    dplyr::distinct(id_pat, grp)

  df_pat_grade <- df_pat_grade |>
    dplyr::distinct(!!id_col, !!ei_grdm_col, !!ei_num_col, !!ei_grav_col) |>
    dplyr::mutate(id_pat = as.character(!!id_col),
                  grade = as.character(!!ei_grdm_col),
                  severity = as.character(!!ei_grav_col)) |>
    dplyr::select(id_pat, grade, severity)



  ##### check for stupid missing data

  if(anyNA(df_pat_grp)){
    warning("Missing data removed from df_pat_grp please be careful !")
    df_pat_grp <- na.omit(df_pat_grp)
  }


  if(anyNA(df_pat_grade)){
    warning("Missing data removed from df_pat_grade please be careful ! If grade is unknown, replace missing data by 'Unknown'.")
    df_pat_grade <- df_pat_grade |>
      dplyr::mutate(grade = dplyr::if_else(is.na(grade), "Unknown", grade)) |>
      na.omit()
  }


  ##### Build augmented df, Any grade is a whole new group

  nb_grp <- length(unique(df_pat_grp$grp))

  ### set severe as an independent grade
  if(severity){
    df_pat_grade_sae <- df_pat_grade |>
      dplyr::filter(severity == "Grave") |>
      dplyr::mutate(grade = "SAE") |>
      dplyr::bind_rows(df_pat_grade) |>
      dplyr::select(id_pat, grade)
  } else {
    df_pat_grade_sae <- df_pat_grade |>
      dplyr::select(id_pat, grade)
  }

  if(nb_grp > 1){
    augmented_df_pat_grp <- dplyr::bind_rows(df_pat_grp,
                                             df_pat_grp |>
                                               dplyr::mutate(grp = "Total"))
  } else {
    augmented_df_pat_grp <- df_pat_grp |> dplyr::mutate(grp = "Total")
  }

  vec_grp <- unique(augmented_df_pat_grp$grp)

  augmented_df_pat_grade_grp <- df_pat_grade_sae |>
    dplyr::left_join(augmented_df_pat_grp,
                     by = "id_pat",
                     relationship = "many-to-many")


  ##### Prepare df_wide

  df_wide <- desc_ei_per_grade_prepare_df(augmented_df_pat_grp = augmented_df_pat_grp,
                                          augmented_df_pat_grade_grp = augmented_df_pat_grade_grp,
                                          digits = digits)

  ##### gt part

  res <- desc_ei_per_grade_df_to_gt(df_wide = df_wide,
                                    vec_grp = vec_grp)

  return(res)
}


# desc_ei_per_grade_prepare_df --------------------------------------------


#' desc_ei_per_grade_prepare_df
#'
#' Prepares a wide-format dataframe summarizing AE by grade and group.
#'
#' @param augmented_df_pat_grp A dataframe with patient IDs and groups, including a "Any grade" group.
#' @param augmented_df_pat_grade_grp A dataframe with patient IDs, grades, and groups.
#' @param digits Number of digits for percentages
#'
#' @return A dataframe in wide format with AE counts and percentages by grade and group.
#' @keywords internal
desc_ei_per_grade_prepare_df <- function(augmented_df_pat_grp,
                                         augmented_df_pat_grade_grp,
                                         digits = 1){
  ##### Build wide dataframe

  df_nb_pat_per_grp <- augmented_df_pat_grp |>
    dplyr::group_by(grp) |>
    dplyr::summarise(nb_pat_per_grp = n())

  df_ei_total <- augmented_df_pat_grade_grp |>
    dplyr::filter(grade != "SAE") |>
    dplyr::group_by(grp) |>
    dplyr::summarise(nb_ei = n(),
                     pct_ei = 100,
                     nb_pat = length(unique(id_pat)),
                     .groups = "drop") |>
    dplyr::full_join(df_nb_pat_per_grp, by = "grp") |>
    dplyr::mutate(across(.cols = c("nb_ei", "pct_ei", "nb_pat"),
                         .fns = function(x) if_else(is.na(x), 0, x))) |>
    dplyr::mutate(pct_pat = nb_pat/nb_pat_per_grp*100,
                  grade = "Any grade") |>
    dplyr::select(grade, grp, nb_ei, pct_ei, nb_pat, pct_pat)

  ##### compute summary statisticsby SOC and PT
  df_wide <- augmented_df_pat_grade_grp |>
    dplyr::group_by(grp, grade) |>
    dplyr::summarise(nb_ei = n(),
                     nb_pat = length(unique(id_pat)),
                     .groups = "drop") |>
    dplyr::left_join(df_ei_total |>
                       dplyr::select(grp, nb_ei_denom = nb_ei),
                     by = "grp") |>
    dplyr::left_join(df_nb_pat_per_grp,
                     by = "grp") |>
    dplyr::mutate(pct_pat = nb_pat/nb_pat_per_grp*100,
                  pct_ei = nb_ei/nb_ei_denom*100) |>
    dplyr::select(grade, grp, nb_ei, pct_ei, nb_pat, pct_pat) |>
    ### add total ei dataset
    dplyr::bind_rows(df_ei_total) |>
    ### Arrange dataframe for visualization
    dplyr::mutate(grade = as.factor(grade),
                  grade = forcats::fct_relevel(grade, "Any grade"),
                  grp = as.factor(grp),
                  grp = forcats::fct_relevel(grp, "Total")) |>
    ### merge n and pct
    dplyr::mutate(EI = paste0(nb_ei, " (", custom_round(pct_ei, digits = digits), ")"),
                  PAT = paste0(nb_pat, " (", custom_round(pct_pat, digits = digits), ")"),
                  .keep = "unused") |>
    dplyr::arrange(grade, grp) |>
    ### Go to wide format with correct names
    dplyr::group_by(grp) |>
    dplyr::group_split() |>
    lapply(function(df_i){
      grp_i <- df_i |> dplyr::pull(grp) |> unique()
      df_i |>
        dplyr::select(-grp) |>
        dplyr::rename_with(.cols = c("EI", "PAT"),
                           .fn = function(x) paste0(grp_i, "_", x))
    }) |>
    purrr::reduce(full_join, by = c("grade"))

  return(df_wide)

}

# desc_ei_per_grade_df_to_gt ----------------------------------------------

#' desc_ei_per_grade_df_to_gt
#'
#' Converts the processed AE grade dataframe into a gt table for visualization.
#'
#' @param df_wide A wide-format dataframe summarizing AE counts and percentages by grade and group.
#' @param vec_grp A vector of unique group names.
#'
#' @return A formatted gt table.
#' @keywords internal
desc_ei_per_grade_df_to_gt <- function(df_wide,
                                       vec_grp){

  ### Create the gt table and labels

  gt_temp <- df_wide |>
    gt::gt() |>
    gt::cols_label(
      grade = gt::md("**Grade**"),
      dplyr::ends_with("EI") ~ gt::md("**AE <br> N (%)**"),
      dplyr::ends_with("PAT") ~ gt::md("**Patient <br> N (%)**")
    )

  gt_temp2 <- gt_temp
  for (grp in vec_grp) {
    gt_temp2 <- gt_temp2 |>
      gt::tab_spanner(label = gt::md(glue::glue("**{grp}**")),
                      id = glue::glue("over_{grp}"),
                      columns = c(glue::glue("{grp}_EI"),
                                  glue::glue("{grp}_PAT")))
  }

  ### color table

  res <- gt_temp2 |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_align(align = "left",
                   columns = dplyr::everything()) |>
    gt::tab_style(
      locations = gt::cells_body(rows = grade %in% c("Any grade", "SAE")),
      style = gt::cell_text(weight = "bold",
                            style = "italic")
    )

  return(res)
}

