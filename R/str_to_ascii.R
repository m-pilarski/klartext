#' Unicode to ASCII transliteration
#'
#' \code{str_to_ascii} - Converts Unicode characters to their best ASCII representation using ICU Transliterators and some basic transformations.
#'
#' @param .str Character vector to be transliterated
#' @param .repl_non_ascii Value to replace non-transliterable characters with. Set to \code{NULL} to keep them.
#' @return \code{str_to_ascii} - Returns the transliterated character vector
#' @rdname str_to_ascii
#' @export
#' @examples
#' ## str_to_ascii EXAMPLE:
#'
#' str_to_ascii(.str="¿Ŧêśť &amp; 法?")
str_to_ascii <- function(.str, .parse_html_chars=TRUE, .repl_no_trans=""){

  .str <-
    .str %>%
    purrr::modify_if(
      rep_len(.parse_html_chars, length(.str)), str_convert_html
    ) %>%
    stringi::stri_replace_all_charclass(
      pattern="[\u0022\u0027\u0060\u00B4\u2018\u2019\u201C\u201D]",
      replacement="'"
    ) %>%
    stringi::stri_replace_all_charclass(
      pattern="[\u2013\u2014\u2212\u002D]", replacement="-"
    ) %>%
    stringi::stri_trans_general(
      id="Fullwidth-Halfwidth; Any-Latin; Latin-ASCII"
    ) %>%
    purrr::modify_if(
      rep_len(!rlang::is_null(.repl_no_trans), length(.str)),
      stringi::stri_replace_all_charclass,
      pattern="[^[:ascii:]]", replacement=.repl_no_trans
    ) |> 
    rlang::set_names(names(.str))

  return(.str)

}

# globalVariables(c("code_hex", "description", "pattern", "replacement"))
