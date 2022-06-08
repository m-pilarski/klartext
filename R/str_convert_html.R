#' Filter Rows That Contain Markers
#'
#' \code{str_convert_html} - Convert all Emojis to some ...
#'
#' @param .str ...
#' @return \code{str_convert_html} - returns a ...
#' @rdname str_convert_html
#' @export
#' @examples
#' ## str_convert_html EXAMPLE:
#'
#' str_convert_html("&gt; &amp; &lt;")
str_convert_html <- function(.str){
  xml2::xml_text(xml2::read_html(stringi::stri_c("<x>", .str, "</x>")))
}
