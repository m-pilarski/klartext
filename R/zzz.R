#' @export
..klartext_num2words <<- NULL

.onLoad <- function(...){

  install_num2words()
  ..klartext_num2words <<- reticulate::import("num2words", delay_load=TRUE)

}
