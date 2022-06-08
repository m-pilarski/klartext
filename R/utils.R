#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom stringi %s+%
#' @export
stringi::`%s+%`

#' @noRd
format_tag <- function(.str){

  .str <- stringi::stri_trans_toupper(str=.str)
  .str <- stringi::stri_replace_all_charclass(
    str=.str, pattern="[^A-Z0-9]", replacement="_", merge=TRUE
  )
  .str <- stringi::stri_replace_all_regex(
    str=.str, pattern="^_+|_+$", replacement=""
  )
  .str <- stringi::stri_c("<", .str, ">")
  return(.str)

}
