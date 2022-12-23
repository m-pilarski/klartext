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
  .relax_space=TRUE
){

  stopifnot(any(is.character(.rep_cardinal), is.character(.rep_ordinal)))
  stopifnot(any(is.character(.rep_cardinal), is.null(.rep_cardinal)))
  stopifnot(any(is.character(.rep_ordinal), is.null(.rep_ordinal)))

  if(isTRUE(is.character(.rep_ordinal))){
    .str <- stringi::stri_replace_all_regex(
      .str, make_pattern_reg_ordinal(.lang), .rep_ordinal
    )
  }

  if(isTRUE(is.character(.rep_cardinal))){
    .str <- stringi::stri_replace_all_regex(
      .str, make_pattern_reg_cardinal(.lang), .rep_cardinal
    )
  }

  return(.str)

}

#' @noRd
make_regex_excl_substr <- function(.pattern){
  str_c("(?<=(^| ))", .pattern, "(?=( |[[:punct:]]|$))")
}

#' @noRd
make_regex_excl_hash_at <- function(.pattern){
  str_c("(?<!(?:^| )[@#][a-z0-9_]{0,279})", .pattern)
}

#' @noRd
make_regex_relax_space <- function(.pattern){
  stringi::stri_replace_all_fixed(.pattern, " ", "[ -]?")
}
