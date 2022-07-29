#' @export
..klartext_num2words <<- NULL

.onLoad <- function(...){

  ..klartext_num2words <<- reticulate::import("num2words", delay_load=TRUE)

}
