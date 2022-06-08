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

  stringi::stri_replace_all_fixed(
    str=stringi::stri_escape_unicode(
      .str
    ),
    pattern=stringi::stri_escape_unicode(
      dplyr::pull(.table_emoji, {{.col_emoji}})
    ),
    replacement=format_tag(
      dplyr::pull(.table_emoji, {{.col_description}})
    ),
    vectorize_all=FALSE
  )

}


globalVariables(c("emoji", "name"))
