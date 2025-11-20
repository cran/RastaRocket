#' Generate qmd, html and css files for reporting
#'
#' This function creates and writes a qmd file with css and html to report statistical analysis.
#'
#' @param output_folder The folder where the html will be recorded.
#' @param structure Character string indicating the organizational structure, either "USMR" or "EUCLID". Default is "USMR".
#' @param path_logo Character string specifying the path to the logo image. If NULL, a default logo is used.
#' @param confidential Logical value indicating whether the report should be marked as confidential. Default is FALSE.
#' @param report_type Character string specifying the type of report. Default is "Data review report".
#' @param study_id Character string representing the study identifier. Default is "CHUBXYYYY/NN".
#' @param study_name Character string specifying the name of the study. Default is "The Study Name".
#' @param study_abbreviation Character string providing the abbreviation of the study. Default is "TSN".
#' @param investigator Character string representing the investigator's name. Default is "Investigator name".
#' @param methodologist Character string specifying the methodologist's name. Default is "Jean Dupont".
#' @param biostatistician Character string specifying the biostatistician's name. Default is "George Frais".
#' @param datamanager Character string specifying the data manager's name. Default is "Peter Parker".
#' @param methodologist_mail Character string specifying the methodologist's email. If NULL, it is generated automatically.
#' @param biostatistician_mail Character string specifying the biostatistician's email. If NULL, it is generated automatically.
#' @param datamanager_mail Character string specifying the data manager's email. If NULL, it is generated automatically.
#' @param language Character string indicating the language of the report, either "fr" (French) or "en" (English). Default is "fr".
#' @param folder_path The folder where this should be created
#' @param name The name of the files
#'
#' @return None. The function writes an HTML report to the specified file path.
#'
#' @export
start_new_reporting <- function(folder_path,
                                output_folder,
                                name = "report",
                                structure = "USMR",
                                path_logo = NULL,
                                confidential = FALSE,
                                report_type = "Data review report",
                                study_id = "CHUBXYYYY/NN",
                                study_name = "The Study Name",
                                study_abbreviation = "TSN",
                                investigator = "Investigator name",
                                methodologist = "Jean Dupont",
                                biostatistician = "George Frais",
                                datamanager = "Peter Parker",
                                methodologist_mail = NULL,
                                biostatistician_mail = NULL,
                                datamanager_mail = NULL,
                                language = "fr"){
  # create folder
  dir.create(file.path(folder_path))
  
  css_path = paste0(folder_path, "/", name, "_custom.css")
  html_path = paste0(folder_path, "/", name, "_custom.html")
  qmd_path = paste0(folder_path, "/", name, ".qmd")
  yaml_path = paste0(folder_path, "/_quarto.yml")
  date_stamp_path = paste0(folder_path, "/datestamp_output_file.R")
  
  write_css(css_path)
  
  write_qmd(path = qmd_path,
            path_html = paste0(name, "_custom.html"),
            path_css = paste0(name, "_custom.css"))
  
  write_html_file(path = html_path,
                  structure = structure,
                  path_logo = path_logo,
                  confidential = confidential,
                  report_type = report_type,
                  study_id = study_id,
                  study_name = study_name,
                  study_abbreviation = study_abbreviation,
                  investigator = investigator,
                  methodologist = methodologist,
                  biostatistician = biostatistician,
                  datamanager = datamanager,
                  methodologist_mail = methodologist_mail,
                  biostatistician_mail = biostatistician_mail,
                  datamanager_mail = datamanager_mail,
                  language = language)
  
  write_quarto_yml(path = yaml_path)
  
  write_datestamp_output_file(output_folder = output_folder,
                              path = date_stamp_path,
                              from_file = paste0(name, ".html"))
}