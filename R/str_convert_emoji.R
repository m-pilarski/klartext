#' Filter Rows That Contain Markers
#'
#' \code{str_convert_emoji} - Convert all Emojis to some ...
#'
#' @param .str ...
#' @param .resolution ...
#' @return \code{str_convert_emoji} - returns a ...
#' @rdname str_convert_emoji
#' @export
#' @examples
#' ## str_convert_emoji EXAMPLE:
#'
#' str_convert_emoji(
#'   "😀😃😄😁😆😅😂"
#' )
#' str_convert_emoji(
#'   "😀😃😄😁😆😅😂", .resolution="subgroup"
#' )
#' str_convert_emoji(
#'   "😀😃😄😁😆😅😂", .resolution="group"
#' )
str_convert_emoji <- function(
  .str, .resolution=c("name", "subgroup", "group")
){

  .resolution <- .resolution[1]

  .pattern <-
    klartext::table_char_emoji %>%
    purrr::chuck("pattern_fixed") %>%
    stringi::stri_escape_unicode()

  .replacement <-
    klartext::table_char_emoji %>%
    purrr::chuck(stringi::stri_c("replacement_", .resolution)) %>%
    klartext:::format_tag(.str_prepend="emo")

  .str %>%
    stringi::stri_escape_unicode() %>%
    stringi::stri_replace_all_fixed(
      pattern=.pattern, replacement=.replacement, vectorize_all=FALSE
    ) %>%
    stringi::stri_unescape_unicode()

}

# cat(str_convert_emoji(c(
#   "ow about the 200 a Month you promised 💜😍 but never followed through on for social security fixed low income people. Fight for equal pay for all not just who will possibly give more votes.",
#   "How are those strict gun laws working in Chicago ? Your a piece of wor",
#   "My brother in Christ you are the president"
# )))

globalVariables(c(
  "pattern_fixed", "replacement_name", "replacement_subgroup",
  "replacement_group"
))
