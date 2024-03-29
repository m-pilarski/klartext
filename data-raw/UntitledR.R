#' Replace latin letters with extensions with their respective base forms
#'
#' \code{get_table_char_unicode} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @return \code{get_table_char_unicode} - returns a ...
#' @rdname get_table_char_unicode
get_table_char_unicode <- function(){

  "https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt" |>
    readr::read_lines(progress=FALSE) |>
    stringi::stri_subset_regex("^[:xdigit:]{4};[^;]+") |>
    paste(collapse="\n") |>
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

  get_table_char_unicode() |>
    dplyr::filter(
      stringi::stri_detect_regex(description, .latin_letter_regex)
    ) |>
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
    ) |>
    tidyr::drop_na(pattern, replacement) |>
    dplyr::group_by(pattern, replacement) |>
    dplyr::summarize(
      description = paste(description, collapse=" OR "),
      .groups="drop"
    )

}
# table_char_latin <- get_table_char_latin()
# usethis::use_data(table_char_latin, overwrite=TRUE)
