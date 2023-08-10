#' Convert HTML character references to Unicode characters
#'
#' \code{str_convert_html} - Convert HTML character references to Unicode
#' characters
#'
#' @param .str Character vector to be converted
#' @return \code{str_convert_html} - Returns the converted character vector
#' @references https://www.w3.org/TR/REC-html40/sgml/entities.html
#' @rdname str_convert_html
#' @export
#' @examples
#' ## str_convert_html EXAMPLE:
#'
#' str_convert_html("&gt; &amp; &lt;")
str_convert_html <- function(.str){
  .str |> 
    stringi::stri_replace_all_fixed(
      pattern=klartext::table_char_html$pattern_fixed,
      replacement=klartext::table_char_html$replacement,
      vectorize_all=FALSE
    ) |> 
    rlang::set_names(names(.str))
}
