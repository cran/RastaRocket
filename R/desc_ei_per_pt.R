
#' desc_ei_per_pt
#'
#' A function to describe AE by soc and pt
#'
#' @param df_pat_grp A dataframe with two columns: id_pat and grp (the rct arm)
#' @param df_pat_llt A dataframe with two columns: id_pat (patient id), num_ae (AE id), llt (AE LLT), pt (AE PT), soc (AE)
#' @param language 'fr' default or 'en'
#' @param order_by_freq Logical. Should PT and SOC be ordered by frequency? Defaults to TRUE. If FALSE, PT and SOC are ordered alphabetically.
#' @param digits Number of digits for percentages
#' @param id_col Patient id column (default: "USUBJID").
#' @param group_col group column, the rct arm (default: "RDGRPNAME").
#' @param ei_num_col AE id column (default: "EINUM").
#' @param ei_llt_col AE LLT column (default: "EILLTN").
#' @param ei_soc_col AE SOC column (default: "EISOCPN").
#' @param ei_pt_col AE PT column (default: "EIPTN")
#'
#' @return A gt table
#' @export
#'
#' @examples
#' df_pat_grp <- data.frame(USUBJID = paste0("ID_", 1:10),
#'                          RDGRPNAME = c(rep("A", 3), rep("B", 3), rep("C", 4)))
#'
#' df_pat_llt <- data.frame(USUBJID = c("ID_1", "ID_1",
#'                                     "ID_2",
#'                                     "ID_4",
#'                                     "ID_9"),
#'                          EINUM = c(1, 2, 1, 1, 1),
#'                          EILLTN = c("llt1", "llt1",
#'                                  "llt4", "llt3",
#'                                  "llt1"),
#'                          EIPTN = c("Arrhythmia", "Myocardial Infarction",
#'                                  "Arrhythmia", "Pneumonia",
#'                                  "Pneumonia"),
#'                          EISOCPN = c("Cardiac Disorders", "Cardiac Disorders",
#'                                  "Cardiac Disorders", "Infections",
#'                                  "Infections"))
#'
#' desc_ei_per_pt(df_pat_grp = df_pat_grp,
#'                df_pat_llt = df_pat_llt)
#'
desc_ei_per_pt <- function(df_pat_grp,
                           df_pat_llt,
                           id_col = "USUBJID",
                           group_col = "RDGRPNAME",
                           ei_num_col = "EINUM",
                           ei_llt_col = "EILLTN",
                           ei_soc_col = "EISOCPN",
                           ei_pt_col = "EIPTN",
                           language = "fr",
                           order_by_freq = TRUE,
                           digits = 1){


  id_col <- rlang::ensym(id_col)
  group_col <- rlang::ensym(group_col)
  ei_num_col <- rlang::ensym(ei_num_col)
  ei_llt_col <- rlang::ensym(ei_llt_col)
  ei_soc_col <- rlang::ensym(ei_soc_col)
  ei_pt_col <- rlang::ensym(ei_pt_col)

  unknown_ei <- " Unknown"

  ##### Check column names and remove duplicates

  if(any(!c(rlang::as_string(id_col), rlang::as_string(group_col)) %in% colnames(df_pat_grp))){
    stop(glue::glue("df_pat_grp should contain '{rlang::as_string(id_col)}' = the patient id and '{rlang::as_string(group_col)}' = the randomization group"))
  }

  if(any(!c(rlang::as_string(id_col), rlang::as_string(ei_llt_col), rlang::as_string(ei_soc_col), rlang::as_string(ei_pt_col), rlang::as_string(ei_num_col)) %in% colnames(df_pat_llt))){
    stop(glue::glue("df_pat_llt should contain '{rlang::as_string(id_col)}' = the patient id, '{rlang::as_string(ei_num_col)}' = the AE event number, '{rlang::as_string(ei_llt_col)}' = the AE LLT, '{rlang::as_string(ei_soc_col)}' = the AE SOC, and '{rlang::as_string(ei_pt_col)}' = the AE PT"))
  }


  ##### clean type and df

  df_pat_grp <- df_pat_grp |>
    dplyr::mutate(id_pat = as.character(!!id_col),
                  grp = as.character(!!group_col)) |>
    dplyr::distinct(id_pat, grp)

  df_pat_llt <- df_pat_llt |>
    dplyr::distinct(!!id_col, !!ei_llt_col, !!ei_soc_col, !!ei_pt_col, !!ei_num_col) |>
    dplyr::select(id_pat = !!id_col,
                  soc = !!ei_soc_col,
                  pt = !!ei_pt_col) |>
    dplyr::mutate(id_pat = as.character(id_pat))

  ##### check for stupid missing data
  if(anyNA(df_pat_grp)){
    warning("Missing data removed from df_pat_grp please be careful !")
    df_pat_grp <- na.omit(df_pat_grp)
  }

  if(anyNA(df_pat_llt)){
    warning("Missing data removed from df_pat_llt please be careful !")
    df_pat_llt <- df_pat_llt |>
      dplyr::mutate_at(.vars = c("soc", "pt"),
                       .funs = function(x) if_else(is.na(x), unknown_ei, x)) |>
      na.omit()
  }


  ##### Build augmented df, Total is a whole new group

  nb_grp <- length(unique(df_pat_grp$grp))

  if(nb_grp > 1){
    augmented_df_pat_grp <- dplyr::bind_rows(df_pat_grp,
                                             df_pat_grp |>
                                               dplyr::mutate(grp = "Total"))
  } else {
    augmented_df_pat_grp <- df_pat_grp |> dplyr::mutate(grp = "Total")
  }

  vec_grp <- unique(augmented_df_pat_grp$grp)

  augmented_df_pat_pt_grp <- df_pat_llt |>
    dplyr::left_join(augmented_df_pat_grp,
                     by = "id_pat",
                     relationship = "many-to-many")

  ##### Build wide dataframe

  df_wide <- desc_ei_per_pt_prepare_df(augmented_df_pat_grp = augmented_df_pat_grp,
                                       augmented_df_pat_pt_grp = augmented_df_pat_pt_grp,
                                       unknown_ei = unknown_ei,
                                       digits = digits)

  ##### gt part

  res <- desc_ei_per_pt_df_to_gt(df_wide = df_wide,
                                 vec_grp = vec_grp,
                                 language = language)

  return(res)
}


# desc_ei_per_pt_prepare_df ----------------------------------------------

#' Prepare Data for AE Description by SOC and PT
#'
#' This function processes patient and adverse event data to generate a wide-format summary dataframe,
#' including total counts and percentages of events and patients per SOC (System Organ Class) and PT (Preferred Term).
#'
#' @param augmented_df_pat_grp A dataframe containing patient IDs and group assignments, including a "Total" group.
#' @param augmented_df_pat_pt_grp A dataframe linking patient IDs to SOC and PT, with group assignments.
#' @param order_by_freq Logical. Should PT and SOC be ordered by frequency? Defaults to TRUE. If FALSE, PT and SOC are ordered alphabetically.
#' @param unknown_ei How the unknown adverse event is labelled.
#' @param digits Number of digits for percentages
#'
#' @return A wide-format dataframe summarizing adverse event occurrences and patient counts across groups.
#' @importFrom purrr reduce
#' @keywords internal
#'
desc_ei_per_pt_prepare_df <- function(augmented_df_pat_grp,
                                      augmented_df_pat_pt_grp,
                                      order_by_freq = TRUE,
                                      unknown_ei = " Unknown",
                                      digits = 1){

  ##### compute summary statistics per group

  df_nb_pat_per_grp <- augmented_df_pat_grp |>
    dplyr::group_by(grp) |>
    dplyr::summarise(nb_pat_per_grp = n())

  df_ei_total <- augmented_df_pat_pt_grp |>
    dplyr::group_by(grp) |>
    dplyr::summarise(nb_ei = n(),
                     pct_ei = 100,
                     nb_pat = length(unique(id_pat)),
                     .groups = "drop") |>
    dplyr::full_join(df_nb_pat_per_grp, by = "grp") |>
    dplyr::mutate(across(.cols = c("nb_ei", "pct_ei", "nb_pat"),
                         .fns = function(x) if_else(is.na(x), 0, x))) |>
    dplyr::mutate(pct_pat = nb_pat/nb_pat_per_grp*100,
                  pt = "Total",
                  soc = "Total") |>
    dplyr::select(pt, soc, grp, nb_ei, pct_ei, nb_pat, pct_pat)

  ##### compute summary statistics by SOC and PT
  df_wide_temp <- list(pt = c("grp", "soc", "pt"),
                  soc = c("grp", "soc")) |>
    lapply(function(vec_grp_by){
      temp <- augmented_df_pat_pt_grp |>
        dplyr::group_by(across(all_of(vec_grp_by))) |>
        dplyr::summarise(nb_ei = n(),
                         nb_pat = length(unique(id_pat)),
                         .groups = "drop") |>
        dplyr::left_join(df_ei_total |>
                           dplyr::select(grp, nb_ei_denom = nb_ei),
                         by = "grp") |>
        dplyr::left_join(df_nb_pat_per_grp,
                         by = "grp") |>
        dplyr::mutate(pct_pat = nb_pat/nb_pat_per_grp*100,
                      pct_ei = nb_ei/nb_ei_denom*100)

      if(!"pt" %in% colnames(temp)){
        temp$pt <- "Total"
      }

      res <- temp |>
        dplyr::select(soc, pt, grp, nb_ei, pct_ei, nb_pat, pct_pat)

      return(res)
    }) |>
    bind_rows() |>
    ### add total ei dataset
    bind_rows(df_ei_total)

  if(order_by_freq){
    # Calculate frequency-based levels for pt and soc
    pt_levels <- df_wide_temp %>%
      group_by(pt) %>%
      summarise(total_nb_pat = sum(nb_pat, na.rm = TRUE)) %>%
      arrange(desc(total_nb_pat)) %>%
      pull(pt)

    soc_levels <- df_wide_temp %>%
      group_by(soc) %>%
      summarise(total_nb_pat = sum(nb_pat, na.rm = TRUE)) %>%
      arrange(desc(total_nb_pat)) %>%
      pull(soc)

    # Apply factor levels to the dataframe
    df_wide_temp_ordered <- df_wide_temp %>%
      mutate(
        pt = factor(pt, levels = pt_levels),
        soc = factor(soc, levels = soc_levels),
        grp = as.factor(grp),
        across(c("pt", "soc", "grp"), function(x) forcats::fct_relevel(x, "Total"))
      )
  } else {
    df_wide_temp_ordered <- df_wide_temp |>
      ### Arrange dataframe for visualization
      mutate(across(c("pt", "soc", "grp"), as.factor),
             across(c("pt", "soc", "grp"), function(x) forcats::fct_relevel(x, "Total")))
  }

  df_wide <- df_wide_temp_ordered |>
    mutate(EI = paste0(nb_ei, " (", custom_round(pct_ei, digits = digits),")"),
           PAT = paste0(nb_pat, " (", custom_round(pct_pat, digits = digits),")"),
           .keep = "unused") |>
    dplyr::arrange(soc, pt, grp) |>
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
    purrr::reduce(full_join, by = c("pt", "soc")) |>
    ## remove duplicate total and unknown for unknow ae
    filter(!(soc == unknown_ei & pt == unknown_ei))

  return(df_wide)

}


# desc_ei_per_pt_df_to_gt ------------------------------------------------

#' Convert AE Summary Data to GT Table
#'
#' This function takes a prepared wide-format dataframe summarizing adverse events and patients
#' and converts it into a formatted `gt` table for easy visualization.
#'
#' @param df_wide A wide-format dataframe containing summarized AE data.
#' @param vec_grp A character vector of group names for which AE data is presented.
#' @param language 'fr' default or 'en'
#'
#' @return A `gt` table formatted with appropriate labels, spans, and styling.
#' @keywords internal
#'
desc_ei_per_pt_df_to_gt <- function(df_wide,
                                    vec_grp,
                                    language = "fr"){
  ### language
  if (language == 'fr'){
    label_pt <- "**Ev\u00e9nements ind\u00e9sirables**"
    ###### MS
    lable_ae <- "EI"
  } else if (language == 'en'){
    label_pt <- "**Adverse events**"
    lable_ae <- "AE"
  } else {
    label_pt <- language
  }

  ### Create the gt table and labels

  gt_temp <- df_wide |>
    dplyr::group_by(soc) |>
    gt::gt() |>
    gt::cols_label(
      pt = gt::md(label_pt),
      soc = "soc",
      #dplyr::ends_with("EI") ~ gt::md("**AE <br> N (%)**"),
      dplyr::ends_with("EI") ~ gt::md("**label_ae <br> N (%)**"),
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
      locations = gt::cells_body(rows = pt == "Total"),
      style = gt::cell_text(weight = "bold",
                            style = "italic")
    ) |>
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_fill(color = "#F5F3F4")
    ) |>
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = gt::cell_text(weight = "bold")
    )

  return(res)
}

