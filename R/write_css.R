#' Generate a CSS File
#'
#' This function creates and writes a CSS file with predefined styling for tables and text formatting.
#'
#' @param path Character string specifying the file path where the CSS file will be saved.
#'
#' @return None. The function writes a CSS file to the specified file path.
#'
#' @export
#' @importFrom utils write.table
write_css <- function(path){
  css_string <- ".custom-table table {
    width: 100% !important;
    border-collapse: collapse !important;
}

.custom-table td, .custom-table th {
    border: 1px solid black !important;
    text-align: center !important;
    padding: 5px !important;
}


h1 .header-section-number{
  color: rgb(51, 90, 137) !important;  
  font-weight: bold !important;
}

h2 .header-section-number{
  color: rgb(51, 90, 137) !important; 
  font-weight: bold !important;
}

h3 .header-section-number, h4 .header-section-number, h5 .header-section-number, h6 .header-section-number {
  color: rgb(51, 90, 137) !important;  
  font-weight: bold !important;
}

h1 {
  color: rgb(51, 90, 137) !important;  
  font-weight: bold !important;
  border-bottom: 3px solid rgb(51, 90, 137) !important;  
  padding-bottom: 5px;
}

h2 {
  color: rgb(51, 90, 137) !important; 
  font-weight: bold !important;
  border-bottom: 3px solid rgb(51, 90, 137) !important; 
  padding-bottom: 5px;
  margin-left: 40px;
}

h3, h4, h5, h6 {
  color: rgb(51, 90, 137) !important;  
  font-weight: bold !important;
  margin-left: 80px;
}


table caption {
  text-align: left !important; 
  font-weight: bold !important;
  }


caption p {
    display: inline;  
    margin-left: 5px; 
}



figure figcaption {
  text-align: left !important; 
  font-weight: bold !important;
  color: black !important;
  }


table {
  font-size: 13px !important;
}"
  
  write.table(css_string, 
              file = path, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
}