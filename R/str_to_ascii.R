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

#' Replace latin letters with extensions with their respective base forms
#'
#' \code{get_table_char_unicode} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @return \code{get_table_char_unicode} - returns a ...
#' @rdname get_table_char_unicode
get_table_char_unicode <- function(){

  "https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt" %>%
    readr::read_lines(progress=FALSE) %>%
    stringi::stri_subset_regex("^[:xdigit:]{4};[^;]+") %>%
    paste(collapse="\n") %>%
    readr::read_delim(
      delim=";", trim_ws=TRUE,
      col_names=c("code_hex", "description", "general_category"),
      col_types=readr::cols_only(
        code_hex=readr::col_character(),
        description=readr::col_character()
      )
    )

}

#' Replace latin letters with extensions with their respective base forms
#'
#' \code{get_table_char_latin} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @return \code{get_table_char_latin} - returns a ...
#' @rdname get_table_char_latin
get_table_char_latin <- function(){

  .latin_letter_regex <-
    "(?i)^latin ([a-z]+) letter " %s+%
    "(?:african|dotless|long|open|reversed|sharp )?" %s+%
    "([a-z])(?: with .+)?$"

  get_table_char_unicode() %>%
    dplyr::filter(
      stringi::stri_detect_regex(description, .latin_letter_regex)
    ) %>%
    dplyr::mutate(
      pattern = stringi::stri_unescape_unicode(paste0("\\u", code_hex)),
      description = stringi::stri_trans_tolower(description),
      case = stringi::stri_replace_all_regex(
        description, .latin_letter_regex, "$1"
      ),
      letter = stringi::stri_replace_all_regex(
        description, .latin_letter_regex, "$2"
      ),
      replacement = dplyr::case_when(
        case == "capital" ~ stringi::stri_trans_totitle(letter),
        case == "small" ~ stringi::stri_trans_tolower(letter),
        TRUE ~ NA_character_
      )
    ) %>%
    tidyr::drop_na(pattern, replacement) %>%
    dplyr::group_by(pattern, replacement) %>%
    dplyr::summarize(
      description = paste(description, collapse=" OR "),
      .groups="drop"
    )

}
# table_char_latin <- get_table_char_latin()
# usethis::use_data(table_char_latin, overwrite=TRUE)

#' Dictionary for replacing latin letters with extensions with their respective base forms
#'
#' Data from ...
#'
#' @docType data
#' @usage data(table_char_latin)
#' @format Named character vector where the names are regular expressions and values are regular latin characters without extensions
#' @keywords datasets
#' @rdname table_char_latin
"table_char_latin"


globalVariables(c("code_hex", "description", "pattern", "replacement"))
