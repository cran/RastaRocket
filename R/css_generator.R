#' css_generator
#'
#' @description
#' Generate css to be included in quarto.
#' 
#'
#' @param path_logo The path to logo, will automatically be guessed on the package.
#'
#' @return A css string
#' @export
#' 
css_generator <- function(path_logo = NULL){
  if(is.null(path_logo)){
    path_logo <- system.file("LogoCHUBdx.jpg", package = "RastaRocket")
  }
  
  css_style <- paste0('<style>
#title-block-header::before {
  content: "Entit\U00E9 d\U0027application/Emetteur : USMR ---- EN-USM-508 ---- Ind : 09";
  display: block; /* Makes the pseudo-element a block-level element */
  width: 100vw;   /* Makes the width 100% of the viewport width */
  height: 40px;  /* Adjust the height as needed */
  background-color: #158cba; /* Optional, to visualize the full width */
  text-align: center;  /* Centers the text */
  font-size: 14px;    /* Customize the font size */
  color: #fff;
  padding: 10px 0;    /* Adjust padding for space around the text */
}
#TOC {
  background: url(', path_logo, ');
  background-size: 50%;
  padding-top: 80px !important;
  background-repeat: no-repeat;
}
</style>')

  return(css_style)
}