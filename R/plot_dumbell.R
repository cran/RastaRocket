utils::globalVariables(c("facet", "grp_num"))

#' Plot a Dumbbell Chart for Adverse Events Analysis
#' 
#' This function creates a dumbbell plot comparing the occurrence of adverse events 
#' across different patient groups. The plot includes the total number of adverse events, 
#' the proportion of patients affected, and the risk difference with confidence intervals.
#' 
#' @param df_pat_llt A data frame with USUBJID (subject ID), EINUM (AE ID),
#' EILLTN (LLT identifier), EIPTN (PT identifier), EISOCPN (soc identifier) and
#' EIGRDM (severity grade)
#' @param df_pat_grp A data frame of patient groups. Must contain columns `USUBJID ` (patient ID) 
#' and `RDGRPNAME` (group assignment).
#' @param ref_grp (Optional) A reference group for comparisons. Defaults to the first group in `df_pat_grp`.
#' @param colors_arm A vector of colors for the patient groups. Defaults to `c("#1b9e77", "#7570b3")`.
#' @param color_label A string specifying the legend label for the groups. Defaults to `"Arm"`.
#' 
#' @return A `ggplot` object displaying the dumbbell chart.
#' 
#' @examples
#' df_pat_grp <- data.frame(
#'  USUBJID = paste0("ID_", 1:10),
#'  RDGRPNAME = c(rep("A", 5), rep("B", 5))
#' )
#' 
#' df_pat_llt <- data.frame(
#'   USUBJID = c("ID_1", "ID_1", "ID_2", "ID_4", "ID_9"),
#'   EINUM = c(1, 2, 1, 1, 1),
#'   EILLTN = c("llt1", "llt2", "llt1", "llt3", "llt4"),
#'   EIPTN = c("Arrhythmia", "Myocardial Infarction", "Arrhythmia", "Pneumonia", "Pneumonia"),
#'   EISOCPN = c("Cardiac Disorders", "Cardiac Disorders", "Cardiac Disorders",
#'   "Infections", "Infections"),
#'   EIGRDM = c(1, 3, 4, 2, 4)
#' )
#' 
#' plot_dumbell(df_pat_llt = df_pat_llt, df_pat_grp = df_pat_grp)
#' 
#' @import ggplot2 dplyr forcats ggh4x scales
#' @export
plot_dumbell <- function(df_pat_grp,
                         df_pat_llt,
                         ref_grp = NULL,
                         colors_arm = c("#1b9e77", "#7570b3"),
                         color_label = "Arm"){
  
  ########## Prepare dataframe
  
  df_all <- df_builder_ae(df_pat_grp = df_pat_grp,
                          df_pat_llt = df_pat_llt,
                          ref_grp = ref_grp)
  
  df_vline <- data.frame(xintercept = 0,
                         facet = "Risk difference with 95% CI")
  
  ########## Plot
  
  p <- ggplot(mapping = aes(y = EIPTN)) +
    geom_point(df_all |> filter(facet == "Prop. of patients"),
               mapping = aes(x = freq_pat, color = RDGRPNAME, shape = RDGRPNAME)) +
    geom_text(df_all |> filter(facet == "Total nb of AE"),
              mapping = aes(label = nb_ei, x = grp_num,
                            color = RDGRPNAME)) +
    geom_point(df_all |> filter(facet == "Risk difference with 95% CI"),
               mapping = aes(x = RD)) +
    geom_vline(data = df_vline, mapping = aes(xintercept = 0),
               color = "red", lty = 2) +
    geom_text(df_all |> filter(facet == "Risk difference with 95% CI"),
              mapping = aes(x = CIsup, label = significant_label),
              color = "red", nudge_x = 0.1) +
    geom_errorbarh(df_all |> filter(facet == "Risk difference with 95% CI"),
                   mapping = aes(xmin = CIinf, xmax = CIsup),
                   height = 0) +
    scale_color_manual(values = colors_arm) +
    facet_grid(EISOCPN ~ facet, scales = "free", space = "free", switch = "both") +
    theme_bw() +
    theme(legend.position="bottom",
          legend.text = element_text(size=10),
          axis.line.x = element_line(color = "black", linetype = 1),
          axis.line.y = element_line(color = "black", linetype = 1),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size=12),
          strip.text.y.left = element_text(angle = 0, size = 10, face = "bold"),
          strip.text.x.bottom = element_text(angle = 0, size = 10, face = "bold"),
          strip.placement = "outside",
          strip.background = element_rect(fill = "#F5F5F5", color = "white"),
          panel.spacing = unit(10,"pt"),
          panel.border = element_rect(color = "lightgrey", fill = NA)) +
    ggh4x::facetted_pos_scales(x = list(scale_x_continuous(labels = scales::label_percent()),
                                        scale_x_continuous(),
                                        scale_x_continuous(limits = c(-0.5, 1.5),
                                                           minor_breaks = NULL,
                                                           breaks = c(0, 1),
                                                           labels = levels(df_all$RDGRPNAME)))) +
    ggh4x::force_panelsizes(rows = c(1, 1),
                            cols = c(2, 2, 1)) +
    labs(x = "", y = "", color = color_label, shape = color_label)
  
  return(p)
  
}
