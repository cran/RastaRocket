utils::globalVariables(c("start", "end", "rando_date", "treatment_date"))

#' Plot a Patient Span Chart (Panchart)
#'
#' This function visualizes the timeline of adverse events (AEs), treatments, and randomization 
#' for a selected patient. The span chart helps track AE duration and treatment events relative 
#' to randomization.
#'
#' @param df_soc_pt A data frame mapping System Organ Class (SOC) to Preferred Terms (PT).
#' @param df_pat_grp_rando A data frame containing patient IDs, randomization groups, and 
#'   randomization dates.
#' @param df_pat_pt_grade_date A data frame with patient IDs, PT terms, AE grades, start 
#'   and end dates of AEs.
#' @param df_pat_treatment_date A data frame with patient IDs and treatment dates.
#' @param pat_id A character string specifying the patient ID to plot.
#' @param vec_fill_color A vector of colors for AE grades. Default is `viridis::viridis(n = 4)`.
#'
#' @return A ggplot object representing the patient span chart.
#' @import dplyr ggplot2 forcats glue
#' @importFrom viridis viridis
#' @export
#'
#' @examples
#' 
#' df_pat_grp_rando <- data.frame(
#'   id_pat = c("ID_1", "ID_2"),
#'   grp = c("A", "B"),
#'   rando_date = c("2020-12-01", "2021-01-03")
#' )
#' 
#' df_pat_pt_grade_date <- data.frame(
#'   id_pat = c("ID_1", "ID_1", "ID_1", "ID_1", "ID_2"),
#'   pt = c("Arrhythmia", "Myocardial Infarction", "Arrhythmia",
#'           "Pneumonia", "Pneumonia"),
#'   grade = c(4, 2, 1, 3, 4),
#'   start = c("2021-01-01", "2021-02-03", "2021-01-02", "2021-03-05", "2021-02-01"),
#'   end = c("2021-01-14", "2021-03-03", "2021-01-22", "2021-05-05", "2021-02-03")
#' )
#' 
#' df_pat_treatment_date <- data.frame(
#'   id_pat = c("ID_1", "ID_1", "ID_1"),
#'   treatment_date = c("2021-01-25", "2021-03-01", "2021-01-20")
#' )
#' df_soc_pt <- data.frame(
#'   pt = c("Arrhythmia", "Myocardial Infarction", "Pneumonia", "Sepsis"),
#'   soc = c("Cardiac Disorders", "Cardiac Disorders", "Infections", "Infections")
#' )
#' 
#' plot_patient_panchart(
#'   df_soc_pt = df_soc_pt,
#'   df_pat_grp_rando = df_pat_grp_rando,
#'   df_pat_pt_grade_date = df_pat_pt_grade_date,
#'   df_pat_treatment_date = df_pat_treatment_date,
#'   pat_id = "ID_1"
#' )
plot_patient_panchart <- function(df_soc_pt,
                                  df_pat_grp_rando,
                                  df_pat_pt_grade_date,
                                  df_pat_treatment_date,
                                  pat_id,
                                  vec_fill_color = viridis::viridis(n = 4, direction = -1, end = 0.95, option = "magma")){
  
  
  df_patient <- df_pat_pt_grade_date |> 
    left_join(df_soc_pt, by = "pt") |> 
    filter(id_pat == pat_id)
  
  df_treatment <- df_patient |> 
    distinct(id_pat, soc, pt) |> 
    full_join(df_pat_treatment_date, by = "id_pat",
              relationship = "many-to-many") |> 
    mutate(treatment_date = as.Date(treatment_date))
  
  df_plot <- df_patient |> 
    bind_rows(df_treatment) |> 
    bind_rows(df_treatment |> 
                mutate(pt = "Treatment",
                       soc = "Treatment") |> 
                distinct()) |> 
    left_join(df_pat_grp_rando, by = "id_pat") |> 
    mutate(across(c("start", "end", "rando_date", "treatment_date"),
                  as.Date),
           grade = as.factor(grade),
           soc = as.factor(soc),
           soc = forcats::fct_relevel(soc, "Treatment", after = 0))
  
  grp_patient <- unique(df_plot$grp)
  rando_date_patient <- unique(df_plot$rando_date)
  
  title_chart <- glue::glue("Span chart of patient {pat_id} (arm: {grp_patient})")
  
  p <- df_plot |> 
    ggplot(mapping = aes(color = grade, xmin = start, xmax = end, y = pt)) +
    geom_linerange(data = df_plot |> filter(!is.na(start)),
                   linewidth = 3, na.rm = TRUE) +
    geom_vline(mapping = aes(linetype = "Randomisation", xintercept = rando_date),
               linewidth = 1, na.rm = TRUE) +
    geom_vline(mapping = aes(linetype = "Treatment", xintercept = treatment_date),
               na.rm = TRUE) +
    geom_point(data = df_plot |> 
                 filter(soc == "Treatment"),
               color = "black",
               mapping = aes(x = treatment_date)) +
    scale_x_date(limits = c(rando_date_patient, max(df_patient$end))) +
    scale_linetype_manual(values = c(2, 3)) +
    scale_color_manual(values = vec_fill_color) +
    facet_grid(soc ~ ., scales = "free" , space = "free", switch = "y") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = 10),
          axis.line.x = element_line(color = "black", linetype = 1),
          axis.line.y = element_line(color = "black", linetype = 1),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          strip.text.y.left = element_text(angle = 0, size = 10, face = "bold"),
          strip.text.x.bottom = element_text(angle = 0, size = 10, face = "bold"),
          strip.placement = "outside",
          strip.background = element_rect(fill = "#F5F5F5", color = "white"),
          panel.spacing = unit(10,"pt"),
          panel.border = element_rect(color = "lightgrey", fill = NA)) +
    labs(x = "Date", y = "", linetype = "", color = "Grade",
         title = title_chart)
  
  return(p)
}
