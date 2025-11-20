#' write_quarto_yml
#'
#' Write quarto extension
#'
#' @param path The path toward quarto yaml file
#'
#' @return nothing
#' @export
write_quarto_yml <- function(path){
  yml_string <- 'project:
  post-render: datestamp_output_file.R'
  
  write.table(yml_string, 
              file = path, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  return()
}