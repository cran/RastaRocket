#' Prepare a dataframe for creating AE plots
#'
#' @param df_pat_llt A data frame with USUBJID (subject ID), EINUM (AE ID),
#' EILLTN (LLT identifier), EIPTN (PT identifier), EISOCPN (soc identifier) and
#' EIGRDM (severity grade)
#' @param df_pat_grp A data frame of patient groups. Must contain columns `USUBJID ` (patient ID)
#' and `RDGRPNAME` (group assignment).
#' @param ref_grp (Optional) A reference group for comparisons. Defaults to the first group in `df_pat_grp`.
#'
#' @return A dataframe with all the info to build AE plots
#'
#' @export
df_builder_ae <- function(df_pat_grp,
                          df_pat_llt,
                          ref_grp = NULL){



  ################ check for missing data ##################
  if(anyNA(df_pat_llt)){
    warning("Missing data removed from df_pat_llt please be careful/Some AE are missing !")
    df_pat_llt <- na.omit(df_pat_llt)

  }

  if(anyNA(df_pat_grp)){
    warning("Missing data removed from df_pat_grp please be careful/Some the RCT arm are missing !")
    df_pat_grp <- na.omit(df_pat_grp)
  }

  vec_pt <- unique(df_pat_llt$EIPTN)
  vec_grp <- unique(df_pat_grp$RDGRPNAME)

  if(is.null(ref_grp)){
    ref_grp <- vec_grp[1]
  }

  ########## Data preparation
  df_SOC_PT <- df_pat_llt |>
    dplyr::distinct(EIPTN, EISOCPN)

  df_SOC_PT_GRP <- lapply(vec_grp,
         function(grp_i) df_SOC_PT |>
           dplyr::mutate(RDGRPNAME = grp_i)) |>
    bind_rows()

  ##### Nb EI per arm

  df_nb_ei_per_arm <- df_pat_llt |>
    left_join(df_pat_grp, by = "USUBJID") |>
    group_by(RDGRPNAME, EIPTN) |>
    summarise(nb_ei = n_distinct(EINUM, USUBJID),
              .groups = "drop") |>
    full_join(df_SOC_PT_GRP, by = c("RDGRPNAME", "EIPTN")) |>
    mutate(nb_ei = if_else(is.na(nb_ei), 0, nb_ei))

  ##### Proportion of patient with EI per arm

  df_nb_pat_per_grp <- df_pat_grp |>
    group_by(RDGRPNAME) |>
    summarise(nb_pat_per_grp = n(),
              .groups = "drop")

  df_freq_pat <- df_pat_llt |>
    dplyr::distinct(USUBJID, EIPTN) |>
    left_join(df_pat_grp, by = "USUBJID") |>
    group_by(EIPTN, RDGRPNAME) |>
    summarise(nb_pat = n(),
              .groups = "drop") |>
    full_join(df_SOC_PT_GRP, by = c("RDGRPNAME", "EIPTN")) |>
    left_join(df_nb_pat_per_grp, by = "RDGRPNAME") |>
    dplyr::mutate(nb_pat = if_else(is.na(nb_pat), 0, nb_pat),
                  freq_pat = nb_pat/nb_pat_per_grp,
                  freq_pat_txt = paste0(nb_pat, "/", nb_pat_per_grp))

  ##### Confidence interval

  df_confidence_interval <- lapply(vec_pt,
                                   function(pt_i){
                                     df_i <-  df_freq_pat |> filter(EIPTN == pt_i)

                                     nb_disease_exposed <- df_i |> filter(RDGRPNAME == vec_grp[1]) |> pull(nb_pat)
                                     nb_disease_nonexposed <- df_i |> filter(RDGRPNAME == vec_grp[2]) |> pull(nb_pat)
                                     pop_at_risk_exposed <- df_i |> filter(RDGRPNAME == vec_grp[1]) |> pull(nb_pat_per_grp)
                                     pop_at_risk_nonexposed <- df_i |> filter(RDGRPNAME == vec_grp[2]) |> pull(nb_pat_per_grp)

                                     res <- riskdifference(a = nb_disease_exposed, N1 = pop_at_risk_exposed,
                                                           b = nb_disease_nonexposed, N0 = pop_at_risk_nonexposed)

                                     res <- data.frame(EIPTN = pt_i,
                                                       RD = res$estimate,
                                                       CIinf = res$conf.int[1],
                                                       CIsup = res$conf.int[2],
                                                       p_val = res$p.value)

                                     return(res)
                                   }) |>
    bind_rows() |>
    left_join(df_SOC_PT_GRP |>
                dplyr::select(-RDGRPNAME) |>
                distinct(),
              by = "EIPTN") |>
    mutate(significant_bool = p_val < 0.05,
           significant_label = factor(significant_bool, levels = c(T,F), labels = c("*", "")))

  ##### Combine df

  df_all <- list(
    df_nb_ei_per_arm |>
      select(EIPTN, EISOCPN, RDGRPNAME, nb_ei) |>
      mutate(facet = "Total nb of AE"),
    df_freq_pat |>
      select(EIPTN, EISOCPN, RDGRPNAME, freq_pat, nb_pat) |>
      mutate(facet = "Prop. of patients"),
    df_confidence_interval |>
      select(EIPTN, EISOCPN, RD, CIinf, CIsup, significant_bool, significant_label, p_val) |>
      mutate(facet = "Risk difference with 95% CI")
  ) |>
    bind_rows() |>
    mutate(RDGRPNAME = as.factor(RDGRPNAME),
           RDGRPNAME = forcats::fct_relevel(RDGRPNAME, ref_grp),
           grp_num = if_else(RDGRPNAME == ref_grp, 0, 1),
           EIPTN = forcats::fct_reorder(EIPTN, RD, .na_rm = TRUE))

  return(df_all)

}
