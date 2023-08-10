#' Filter Rows That Contain Markers
#'
#' \code{str_blur_url} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @param .str ...
#' @param .pattern_reg_url ...
#' @param .replacement_url ...
#' @return \code{str_blur_url} - returns a ...
#' @rdname str_blur_url
#' @export
#' @examples
#' ## str_blur_url EXAMPLE:
#'
#' str_blur_url(c(
#'   "The 2020 United States presidential election was the 59th quadrennial ",
#'   "presidential election, held on Tuesday, November 3, 2020"
#' ))
str_blur_url <- function(
  .str, .pattern_reg_url=pattern_reg_url, .replacement_url="<URL>"
){
  .str |> 
    stringi::stri_replace_all_regex(
      pattern=.pattern_reg_url,
      replacement=.replacement_url
    ) |> 
    rlang::set_names(names(.str))
}
