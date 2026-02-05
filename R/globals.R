# desc_ei_per_grade.R
utils::globalVariables(c(
  "USUBJID",
  "RDGRPNAME",
  "id_pat",
  "grp",
  "EIGRDM",
  "EINUM",
  "EIGRAV",
  "grade",
  "nb_pat",
  "nb_pat_per_grp",
  "nb_ei",
  "pct_ei",
  "pct_pat",
  "nb_ei_denom"
))


# # desc_ei_per_pt.R
utils::globalVariables(c("EILLTN",
                         "EISOCPN",
                         "EIPTN",
                         "EINUM",
                         "soc",
                         "pt",
                         "nb_pat",
                         "nb_pat_per_grp",
                         "nb_ei",
                         "pct_ei",
                         "pct_pat",
                         "p_val",
                         "significant_bool",
                         "freq_pat",
                         "RD",
                         "CIinf",
                         "CIsup",
                         "significant_label",
                         "total_nb_pat"))

# plot_dumbell.R
utils::globalVariables(c("facet", "grp_num"))
