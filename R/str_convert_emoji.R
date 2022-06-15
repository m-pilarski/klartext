#' Filter Rows That Contain Markers
#'
#' \code{str_convert_emoji} - Convert all Emojis to some ...
#'
#' @param .str ...
#' @param .table_emoji ...
#' @param .col_emoji ...
#' @param .col_description ...
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
#'   "😀😃😄😁😆😅😂", .col_description=subgroup
#' )
#' str_convert_emoji(
#'   "😀😃😄😁😆😅😂", .col_description=group
#' )
str_convert_emoji <- function(
  .str, .table_emoji=emoji::emojis, .col_emoji=emoji, .col_description=name
){

  # .str <<- .str; .table_emoji <<- emoji::emojis; .col_emoji <<- .col_emoji; .col_description <<- .col_description

  stringi::stri_replace_all_fixed(
    str=stringi::stri_escape_unicode(.str),
    pattern=stringi::stri_escape_unicode(
      dplyr::pull(.table_emoji, {{.col_emoji}})
    ),
    replacement=format_tag(
      stringi::stri_c("emo_", dplyr::pull(.table_emoji, {{.col_description}}))
    ),
    vectorize_all=FALSE
  )

}

# cat(str_convert_emoji(c(
#   "ow about the 200 a Month you promised 💜😍 but never followed through on for social security fixed low income people. Fight for equal pay for all not just who will possibly give more votes.",
#   "How are those strict gun laws working in Chicago ? Your a piece of wor",
#   "My brother in Christ you are the president"
# )))

globalVariables(c("emoji", "name"))
