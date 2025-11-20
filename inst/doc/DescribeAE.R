## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  results = 'asis'
)

## ----setup--------------------------------------------------------------------
library(RastaRocket)

## -----------------------------------------------------------------------------
df_pat_grp <- data.frame(
  USUBJID = paste0("ID_", 1:10),
  RDGRPNAME = c(rep("A", 3), rep("B", 3), rep("C", 4))
)

## -----------------------------------------------------------------------------
df_pat_llt <- data.frame(
  USUBJID = c("ID_1", "ID_1", "ID_2", "ID_8", "ID_9"),
  EINUM = c(1, 2, 1, 1, 1),
  EILLTN = c("llt1", "llt2", "llt1", "llt3", "llt4"),
  EIPTN = c("Arrhythmia", "Myocardial Infarction", "Myocardial Infarction", "Pneumonia", "Pneumonia"),
  EISOCPN = c("Cardiac Disorders", "Cardiac Disorders", "Cardiac Disorders", "Infections", "Infections"),
  EIGRDM = c(1, 3, 4, 2, 4),
  EIGRAV = c("Grave", "Non grave", "Non grave", "Non grave", "Grave") 
)

## -----------------------------------------------------------------------------
desc_ei_per_pt(df_pat_grp = df_pat_grp,
               df_pat_llt = df_pat_llt)

## -----------------------------------------------------------------------------
desc_ei_per_grade(df_pat_grp = df_pat_grp,
                  df_pat_grade = df_pat_llt)

## -----------------------------------------------------------------------------
desc_ei_per_pt(df_pat_grp = df_pat_grp |> dplyr::mutate(RDGRPNAME = "A"),
               df_pat_llt = df_pat_llt)

## -----------------------------------------------------------------------------
desc_ei_per_grade(df_pat_grp = df_pat_grp,
                  df_pat_grade = df_pat_llt |> dplyr::mutate(RDGRPNAME = "A"))

## -----------------------------------------------------------------------------
df_pat_grp <- df_pat_grp |> 
  dplyr::mutate(RDGRPNAME = c(rep("A", 5), rep("B", 5)))

## ----fig.height=5, fig.width=8------------------------------------------------
plot_dumbell(df_pat_llt = df_pat_llt,
             df_pat_grp = df_pat_grp)

## ----fig.height=6, fig.width=6------------------------------------------------
plot_volcano(df_pat_llt = df_pat_llt,
             df_pat_grp = df_pat_grp)

## ----fig.height=5, fig.width=6------------------------------------------------
plot_butterfly_stacked_barplot(df_pat_llt = df_pat_llt,
                               df_pat_grp = df_pat_grp)

