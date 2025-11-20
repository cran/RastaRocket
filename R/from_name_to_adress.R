utils::globalVariables(c("."))

#' Convert a Name to an Email Address
#'
#' This function transforms a given name into an email address following the format `firstname.lastname@chu-bordeaux.fr`.
#' 
#' @param name A character string representing a full name. Default is "Peter Parker".
#' 
#' @return A character string containing the generated email address.
#' 
#' @examples
#' from_name_to_adress("John Doe")
#' from_name_to_adress()
#' 
#' @export
from_name_to_adress <- function(name = "Peter Parker"){
  name %>%
    tolower() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    gsub(x = ., pattern = " ", replacement = ".") %>%
    paste0(., "@chu-bordeaux.fr") %>%
    return(.)
}
