#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom stringi %s+%
#' @export
stringi::`%s+%`

#' @noRd
format_tag <- function(.str){

  .str_format <-
    .str %>%
    stringi::stri_trans_toupper() %>%
    stringi::stri_replace_all_charclass(
      pattern="[^A-Z0-9]", replacement="_", merge=TRUE
    ) %>%
    stringi::stri_replace_all_regex(pattern="^_|_$", replacement="")

  .tag <- stringi::stri_c("<", .str_format, ">")

  return(.tag)

}
