#' Replace latin letters with extensions with their respective base forms
#'
#' \code{str_to_ascii} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @param .str ...
#' @param .repl_non_ascii ..
#' @return \code{str_to_ascii} - returns a ...
#' @rdname str_to_ascii
#' @export
#' @examples
#' ## str_to_ascii EXAMPLE:
#'
#' str_to_ascii(.str="Ŧêśť")
str_to_ascii <- function(.str, .repl_non_ascii=""){

  .str <-
    .str %>%
    stringi::stri_replace_all_charclass(
      "[\u0022\u0027\u0060\u00B4\u2018\u2019\u201C\u201D]", "'"
    ) %>%
    stringi::stri_replace_all_charclass(
      "[\u2013\u2014\u2212\u002D]", "-"
    ) %>%
    stringi::stri_trans_general(id="Any-Latin; Latin-ASCII")

  if(!rlang::is_null(.repl_non_ascii)){
    .str <- iconv(.str, to="ASCII", sub=.repl_non_ascii)
  }

  return(.str)

}

# globalVariables(c("code_hex", "description", "pattern", "replacement"))
