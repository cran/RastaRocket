#' write_datestamp_output_file
#'
#' A function to write a R file and add datestamp
#'
#' @param output_folder The output folder
#' @param path The path of the R script
#' @param from_file The initial html file to be renamed
#'
#' @return Nothing
#' 
#' @importFrom glue glue
write_datestamp_output_file <- function(output_folder,
                                        path,
                                        from_file){
  r_string <- glue::glue(
  'string_time <- format(Sys.time(), "%Y%m%d_%H%M")
  new_name <- paste0({output_folder}, "report_", string_time, ".html")
  file.copy(
    from = "{from_file}",
    to = new_name)
  browseURL(new_name)'
  )
  
  write.table(r_string, 
              file = path, 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  return()
}