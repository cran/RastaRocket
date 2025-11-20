#' Generate an HTML Report File
#'
#' This function creates and writes an HTML report file based on specified study and structure details.
#'
#' @param path Character string specifying the file path where the HTML file will be saved.
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
#'
#' @return None. The function writes an HTML report to the specified file path.
#' @importFrom glue glue
#' @export
write_html_file <- function(path,
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
  vec_structure <- c("",
                     "Universit\u00e9 de Bordeaux",
                     "146 rue L\u00e9o Saignat",
                     "CS61292 - Case 75",
                     "33076 Bordeaux cedex - France",
                     "T\u00e9l : 33 (0)5 57 57 11 29, Fax : 33 (0)5 57 57 15 78")
  if (structure == "USMR") {
    vec_structure[1] <- "Unit\u00e9 de Soutien M\u00e9thodologique \u00e0 la Recherche clinique et \u00e9pid\u00e9miologique du CHU de Bordeaux (USMR)"
    
    if (is.null(path_logo)) {
      path_logo <- system.file("LogoCHUBdx.jpg", package = "RastaRocket")
    }
  } else if (structure == "EUCLID") {
    vec_structure[1] <- "EUropean CLInical Trials Platform & Development (EUCLID)"
    
    if (is.null(path_logo)) {
      path_logo <- system.file("LogoEuclid.jpg", package = "RastaRocket")
    }
  }
  
  
  if (is.null(methodologist_mail)) methodologist_mail <- from_name_to_adress(methodologist)
  if (is.null(biostatistician_mail)) biostatistician_mail <- from_name_to_adress(biostatistician)
  if (is.null(datamanager_mail)) datamanager_mail <- from_name_to_adress(datamanager)
  
  if (language == "fr") {
    report_type1 = "Rapport d\u0027analyses statistiques"
    label_investigator = "Investigateur coordinateur"
    label_confidential = "Confidentiel"
    ctu_label = "Centre de M\u00e9thodologie et de Gestion : "
    data_label = "Data manager :"
    biostat_label = "Biostatisticien :"
    methodo_label = "M\u00e9thodologiste :"
    doc_label = "DOCUMENT D\u0027ENREGISTREMENT"
  } else if (language == "en") {
    report_type1 = "Statistical analysis report"
    label_investigator = "Coordinating investigator"
    label_confidential = "Confidential"
    ctu_label = "Clinical Trial Unit: "
    data_label = "Data manager:"
    biostat_label = "Biostatistician:"
    methodo_label = "M\U00e9thodologist:"
    doc_label = "RECORDING DOCUMENT"
  } else {
    stop("Language must be 'en' or 'fr'.")
  }
  
  if (confidential) {
    confidential_html <- glue::glue('<p style="text-align: center;text-transform: uppercase; font-weight: bolder;font-size:26px;">{label_confidential}</p><br />')
  } else {
    confidential_html <- ''
  }
  
  html_string <- glue::glue(
  '<!DOCTYPE html> 
<div class="custom-table">
<table style="border-collapse: collapse; width: 100%; height: 88px;" border="1">
  <colgroup>
    <col style="width: 20%;"> 
    <col style="width: 60%;"> 
    <col style="width: 20%;"> 
  </colgroup>
  <tbody>
    <tr style="height: 44px;">
      <td style="padding: 0; text-align: center;" rowspan="2"><img style="max-width: 90%; height: auto; display: inline-block;" src={path_logo} /></td>
      <td style="text-align: center;">Entit\u00e9 d\u0027application : USMR<br />Emetteur : USMR</td>
  <td style="text-align: center;">EN-USM-600</td>
  </tr>
  <tr style="height: 22px;">
  <td style="text-align: center;">{doc_label}</td>
      <td style="text-align: center;">Ind : 03</td>
    </tr>
    <tr style="height: 22px;">
      <td style="text-align: center; text-transform: uppercase; font-weight: bolder;" colspan="3">{report_type1}</td>
  </tr>
  </tbody>
  </table>
  </div>  


  <br />
  
  <p style="text-align: center;text-transform: uppercase; font-weight: bolder;font-size:20px;">{report_type}</p>
  {confidential_html}
  
  <HR COLOR="black" ALIGN=CENTER WIDTH="100%">
  
  <p style="text-align: center;text-transform: uppercase; font-weight: bolder; font-size:20px;">{study_id}</p>
  <p style="text-align: center; font-size:20px;">{study_name}</p>
  <p style="text-align: center;text-transform: uppercase; font-weight: bolder; font-size:20px;">{study_abbreviation}</p>
  
  <HR COLOR="black" ALIGN=CENTER WIDTH="100%" >
    
  <br/>
    
    <p style="padding-left: 1%;">
      <u style="text-decoration: underline">{label_investigator}:</u> &nbsp
      <u style="text-decoration: none"> {investigator}</u><br/>
  
    </p><br/>
   
    <p style="padding-left: 1%;">
      <u style="text-decoration: underline">{ctu_label}</u><br/>
      <u style="text-decoration: none">{vec_structure[1]}</u><br/>
      <u style="text-decoration: none">{vec_structure[2]}</u><br/>
      <u style="text-decoration: none">{vec_structure[3]}</u><br/>
      <u style="text-decoration: none">{vec_structure[4]}</u><br/>
      <u style="text-decoration: none">{vec_structure[5]}</u><br/>
      <u style="text-decoration: none">{vec_structure[6]}</u><br/>
    </p><br/>
  <p>
    
  </p> 
    <p style="float: left;padding-left: 1%;margin: 0;">
      <u style="text-decoration: none">{methodo_label} {methodologist} </u><br/>
      <u style="text-decoration: none">{biostat_label} {biostatistician} </u><br/>
      <u style="text-decoration: none">{data_label} {datamanager} </u><br/>
      <br/>
    </p>
    
    <p style="margin: 0;margin-left: 300px;padding-left: 4%;">
      <u style="text-decoration: none"> Email: {methodologist_mail}</u><br/>
      <u style="text-decoration: none"> Email: {biostatistician_mail} </u><br/>
      <u style="text-decoration: none"> Email: {datamanager_mail}</u><br/>
      <br/>
    </p> 
  <br />  
  
  </html>'
  )
            
  html_styled <- paste(as.character(html_string), collapse = "\n")
  
  write.table(html_styled, 
              file = path, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  return()
}