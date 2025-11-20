utils::globalVariables(c("nb_pat_per_group", "freq_ei"))

#' Butterfly Stacked Bar Plot for Adverse Event Grades
#'
#' Creates a butterfly stacked bar plot to visualize the frequency of adverse event (AE) grades 
#' across patient groups, with system organ class (SOC) and preferred terms (PT) as labels.
#'
#' @param df_pat_llt A data frame with USUBJID (subject ID), EINUM (AE ID),
#' EILLTN (LLT identifier), EIPTN (PT identifier), EISOCPN (soc identifier) and
#' EIGRDM (severity grade)
#' @param df_pat_grp A data frame of patient groups. Must contain columns `USUBJID ` (patient ID) 
#' and `RDGRPNAME` (group assignment).
#' @param ref_grp A character string specifying the reference group (used for alignment in the plot). 
#'   If NULL (default), the first level of `df_pat_grp$grp` is used.
#' @param max_text_width An integer specifying the maximum width (in characters) for SOC labels 
#'   before wrapping to the next line. Default is 9.
#' @param vec_fill_color A vector of colors used for filling the AE grade bars. Default is 
#'   `viridis::viridis(n = 4)`.
#'
#' @return A ggplot2 object representing the butterfly stacked bar plot.
#'
#' @details 
#' The function processes input data to calculate the frequency of adverse events per patient 
#' group and AE grade. It then generates a stacked bar plot where:
#' \itemize{
#'   \item The x-axis represents the percentage of patients experiencing an AE.
#'   \item The y-axis represents PTs (with SOCs as facets).
#'   \item Bars are stacked by AE grade.
#'   \item Labels for PTs are displayed in the center.
#'   \item The left and right panels correspond to different patient groups.
#' }
#'
#' The function utilizes the `ggh4x` package to adjust panel sizes and axes for a symmetrical 
#' butterfly plot.
#'
#' @examples
#' 
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
#' plot_butterfly_stacked_barplot(df_pat_grp, df_pat_llt)
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_manual theme_bw labs
#' @importFrom dplyr group_by summarise mutate filter distinct left_join bind_rows
#' @importFrom forcats fct_relevel
#' @importFrom ggh4x facetted_pos_scales force_panelsizes
#' @importFrom viridis viridis
#' @export
plot_butterfly_stacked_barplot <- function(df_pat_grp,
                                           df_pat_llt,
                                           ref_grp = NULL,
                                           max_text_width = 9,
                                           vec_fill_color = viridis::viridis(n = 4)) {
  
  if (is.null(ref_grp)) {
    ref_grp <- levels(df_pat_grp$RDGRPNAME)[1]
    if (is.null(ref_grp)) {
      ref_grp <- df_pat_grp$RDGRPNAME[1]
    }
  }
  
  df_nb_pat_per_group <- df_pat_grp |> 
    group_by(RDGRPNAME) |> 
    summarise(nb_pat_per_group = n())
  
  df_label_pt_pt <- df_pat_llt |>
    distinct(EISOCPN, EIPTN) |> 
    mutate(RDGRPNAME = "PT")
  
  df_plot <- df_pat_llt |> 
    left_join(df_pat_grp, by = "USUBJID") |> 
    group_by(RDGRPNAME, EIPTN, EISOCPN, EIGRDM) |> 
    summarise(nb_ei = n_distinct(USUBJID, EINUM), .groups = "drop") |> 
    left_join(df_nb_pat_per_group, by = "RDGRPNAME") |> 
    bind_rows(df_label_pt_pt) |> 
    mutate(freq_ei = nb_ei / nb_pat_per_group,
           EIGRDM = as.factor(EIGRDM),
           RDGRPNAME = as.factor(RDGRPNAME),
           RDGRPNAME = forcats::fct_relevel(RDGRPNAME, ref_grp, "PT"),
           EIPTN = purrr::map_chr(EIPTN, ~ paste(strwrap(.x, width = max_text_width), collapse = "\n")))
  
  p <- ggplot(data = df_plot |> filter(RDGRPNAME != 'PT'),
              mapping = aes(x = freq_ei, y = EIPTN, fill = EIGRDM)) +
    geom_bar(position = position_stack(), stat = "identity") +
    geom_text(data = df_plot |> filter(RDGRPNAME == 'PT'),
              mapping = aes(x = 0, y = EIPTN, label = EIPTN),
              hjust = "center",
              inherit.aes = FALSE,
              size = 3) +
    scale_fill_manual(values = vec_fill_color) +
    facet_grid(EISOCPN ~ RDGRPNAME, scales = "free", switch = "y", space = "free") +
    ggh4x::facetted_pos_scales(x = list(scale_x_continuous(labels = scales::label_percent(),
                                                           trans = "reverse",
                                                           limits = c(1, 0)),
                                        scale_x_continuous(labels = NULL,
                                                           breaks = NULL),
                                        scale_x_continuous(labels = scales::label_percent(),
                                                           limits = c(0, 1)))) +
    ggh4x::force_panelsizes(cols = c(2, 1, 2)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 10),
          axis.line.x = element_line(color = "black", linetype = 1),
          axis.line.y = element_line(color = "black", linetype = 1),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 12),
          strip.text.y.left = element_text(angle = 0, size = 10, face = "bold"),
          strip.text.x.bottom = element_text(angle = 0, size = 10, face = "bold"),
          strip.placement = "outside",
          strip.background = element_rect(fill = "#F5F5F5", color = "white"),
          panel.spacing = unit(10, "pt"),
          panel.border = element_rect(color = "lightgrey", fill = NA)) +
    labs(x = "Percent of patients", y = "", fill = "Grade")
  
  return(p)
}
