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
  stringi::stri_replace_all_fixed(
    str=.str,
    pattern=klartext::table_char_html$pattern_fixed,
    replacement=klartext::table_char_html$replacement,
    vectorize_all=FALSE
  )
}
