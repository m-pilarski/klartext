#' Filter Rows That Contain Markers
#'
#' \code{str_unify_punctuation} - Convert all Emojis to some ...
#'
#' @param .str ...
#' @return \code{str_unify_punctuation} - returns a ...
#' @rdname str_unify_punctuation
#' @export
#' @examples
#' ## str_unify_punctuation EXAMPLE:
#'
#' str_unify_punctuation(
#'   "fdsfsd sd  f\n fdsfsd, <TAG> test!"
#' )
str_unify_punctuation <- function(.str){

  .punctuation_regex_dict <- c(
    "(\\u0022|\\u0027|\\u0060|\\u00B4|\\u2018|\\u2019|\\u201C|\\u201D)"=
      "\'",
    "(\\u2013|\\u2014|\\u2212|\\u002D)"=
      "-"
  )

  stringi::stri_replace_all_regex(
    str=.str,
    pattern=names(.punctuation_regex_dict),
    replacement=unname(.punctuation_regex_dict),
    vectorize_all=FALSE
  )

}
