#' Filter Rows That Contain Markers
#'
#' \code{str_blur_numbers} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @param .str ...
#' @param .rep_cardinal ...
#' @param .rep_ordinal ...
#' @param .lang ...
#' @param .excl_substr ...
#' @param .excl_hash_at ...
#' @param .relax_space ...
#' @return \code{str_blur_numbers} - returns a ...
#' @rdname str_blur_numbers
#' @export
#' @examples
#' ## str_blur_numbers EXAMPLE:
#'
#' str_blur_numbers(c(
#'   "The 2020 United States presidential election",
#'   "was the 59th quadrennial presidential election",
#'   "held on Tuesday, November 3, 2020"
#' ))
str_blur_numbers <- function(
  .str, .rep_cardinal="<NUM_CARDI>", .rep_ordinal="<NUM_ORDI>", .lang="en",
  .excl_substr=TRUE, .excl_hash_at=TRUE, .relax_space=TRUE
){

  stopifnot(any(is.character(.rep_cardinal), is.character(.rep_ordinal)))
  stopifnot(is.character(.rep_cardinal) | is.null(.rep_cardinal))
  stopifnot(is.character(.rep_ordinal) | is.null(.rep_ordinal))

  if(is.character(.rep_ordinal)){

    .ord_regex <- make_ordinal_regex(.lang)

    if(isTRUE(.relax_space)){
      .ord_regex <- make_regex_relax_space(.ord_regex)
    }

    if(isTRUE(.excl_substr)){
      .ord_regex <- make_regex_excl_substr(.ord_regex)
    }else if(isTRUE(.excl_hash_at)){
      .ord_regex <- make_regex_excl_hash_at(.ord_regex)
    }

    .str <- stringi::stri_replace_all_regex(.str, .ord_regex, .rep_ordinal)

  }

  if(is.character(.rep_cardinal)){

    .card_regex <- make_cardinal_regex(.lang)

    if(isTRUE(.relax_space)){
      .card_regex <- make_regex_relax_space(.card_regex)
    }

    if(isTRUE(.excl_substr)){
      .card_regex <- make_regex_excl_substr(.card_regex)
    }else if(isTRUE(.excl_hash_at)){
      .card_regex <- make_regex_excl_hash_at(.card_regex)
    }

    # if_else(
      # stringi::stri_detect_charclass(.str, "[^[:space:][:cntrl:]]"),
      .str <- stringr::str_replace_all(.str, .card_regex, .rep_cardinal)#,
      # .str
    # )

  }

  return(.str)

}

#' @noRd
make_cardinal_regex <- function(.lang="en"){

  stopifnot(.lang=="en")

  .c_1_9_reg <- "(?:f(?:ive|our)|s(?:even|ix)|t(?:hree|wo)|(?:ni|o)ne|eight)"

  .c_10_19_reg <- paste0(
    "(?:(?:(?:s(?:even|ix)|f(?:our|if)|nine)te|e(?:ighte|lev))en|",
    "t(?:(?:hirte)?en|welve))"
  )

  .c_2_9x10_reg <- "(?:(?:s(?:even|ix)|t(?:hir|wen)|f(?:if|or)|eigh|nine)ty)"

  .c_1_99_reg <- paste0(
    "(?:", .c_2_9x10_reg, " ", .c_1_9_reg, "?|", .c_10_19_reg, "|",
    .c_1_9_reg, ")"
  )

  .c_1_999_reg <- paste0(
    "(?:", .c_1_9_reg, " hundred(?: (?:and )?(?:", .c_1_99_reg, "))?|",
    .c_1_99_reg, ")"
  )

  .c_1_9999_reg <- paste0(
    "(?:", .c_1_9_reg, " thousand(?: (?:and )?(?:", .c_1_999_reg, "))?|",
    .c_1_999_reg, ")"
  )

  .c_big_l_reg <- "(?:hundred|thousand|(?:m|b|tr)illion)"

  .c_big_s_reg <- paste0(
    "(?:(?:k|thsnd|m(?:ill?|ln)?|b(?:ill?|l?n)?|t(?:r(?:ill?|l?n))?)\\.?)"
  )

  .c_start_reg <- paste0(
    "(?:zero|", .c_1_9999_reg, "|", .c_big_l_reg, "s?)"
  )

  .c_rest_reg <- paste0("(?:", .c_start_reg, "|", .c_big_s_reg, ")")

  .c_full_reg <- paste0("(", .c_start_reg, "|", "[[:digit:]][[:digit:] .,]*)(?:", .c_rest_reg, ")*s?")

  return(.c_full_reg)

}

#' @noRd
make_ordinal_regex <- function(.lang="en"){

  stopifnot(.lang=="en")

  .ordi_base <- c(
    "first", "second", "third", "fourth", "fifth", "sixth", "seventh",
    "eighth", "ninth"
  )

  .ordi_20_90_pre <- c(
    "twent", "thirt", "fort", "fift", "sixt", "sevent", "eight", "ninet"
  )

  .ordi_full <- c(
    .ordi_base, "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth",
    "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth",
    paste0(rep(.ordi_20_90_pre, each=10), c("ieth", paste0("y ", .ordi_base))),
    "hundredth"
  )

  .reg_ordi <- paste0(
    "(?:", paste(.ordi_full, collapse='|'), ")|",
    "(?:[0-9]*(?:1st|2nd|3rd|[0456789]th))"
  )

  return(.reg_ordi)

}

#' @noRd
make_regex_excl_substr <- function(.pattern){
  paste0("(?<=(^| ))", .pattern, "(?=( |[[:punct:]]|$))")
}

#' @noRd
make_regex_excl_hash_at <- function(.pattern){
  paste0("(?<!(?:^| )[@#][a-z0-9_]{0,279})", .pattern)
}

#' @noRd
make_regex_relax_space <- function(.pattern){
  stringi::stri_replace_all_fixed(.pattern, " ", "[ -]?")
}
