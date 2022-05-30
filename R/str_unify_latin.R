#' Replace latin letters with extensions with their respective base forms
#'
#' \code{str_unify_char_latin} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @param .str ...
#' @param .table_char ..
#' @param .col_pattern ..
#' @param .col_replacement ..
#' @return \code{str_unify_char_latin} - returns a ...
#' @rdname str_unify_char_latin
#' @export
#' @examples
#' ## str_unify_char_latin EXAMPLE:
#'
#' str_unify_char_latin(.str="Ŧêśť")
str_unify_char_latin <- function(
  .str, .table_char=klartext::table_char_latin, .col_pattern=pattern,
  .col_replacement=replacement
){

  stringi::stri_replace_all_fixed(
    str=.str,
    pattern=dplyr::pull(.table_char, {{.col_pattern}}),
    replacement=dplyr::pull(.table_char, {{.col_replacement}}),
    vectorize_all=FALSE
  )

}

#' Replace latin letters with extensions with their respective base forms
#'
#' \code{get_table_char_unicode} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @return \code{get_table_char_unicode} - returns a ...
#' @rdname get_table_char_unicode
get_table_char_unicode <- function(){

  "https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt" %>%
    readr::read_lines() %>%
    stringi::stri_subset_regex("^[:xdigit:]{4};[^;]+") %>%
    paste(collapse="\n") %>%
    readr::read_delim(
      delim=";", trim_ws=TRUE,
      col_names=c("code_hex", "description"),
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

  get_table_char_unicode() %>%
    dplyr::filter(
      stringi::stri_detect_regex(description, "(?i)^latin \\w+ letter \\w")
    ) %>%
    dplyr::mutate(
      pattern = stringi::stri_unescape_unicode(paste0("\\u", code_hex)),
      description = stringi::stri_trans_tolower(description),
      case = stringi::stri_replace_all_regex(
        description, "^latin (\\w+) letter \\w.*", "$1"
      ),
      letter = stringi::stri_replace_all_regex(
        description, "^latin \\w+ letter (\\w).*", "$1"
      ),
      replacement = dplyr::case_when(
        case == "capital" ~ stringi::stri_trans_toupper(letter),
        case == "small" ~ stringi::stri_trans_tolower(letter),
        TRUE ~ NA_character_
      )
    ) %>%
    tidyr::drop_na(pattern, replacement) %>%
    dplyr::select(pattern, replacement, description)

}

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
