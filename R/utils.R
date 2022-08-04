#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom stringi %s+%
#' @export
stringi::`%s+%`


#' @importFrom stringr str_c
#' @export
stringr::str_c


#' @noRd
format_klartag <- function(
  .str, .str_prepend="", .str_append=""
){

  stopifnot(is.character(.str_prepend) & is.character(.str_append))
  stopifnot(all(lengths(list(.str_prepend, .str_append)) == 1))

  .str_format <-
    stringi::stri_c(.str_prepend, .str, .str_append, sep="_") %>%
    stringi::stri_trans_toupper() %>%
    stringi::stri_replace_all_charclass(
      pattern="[^A-Z0-9]", replacement="_", merge=TRUE
    ) %>%
    stringi::stri_replace_all_regex(pattern="^_|_$", replacement="")

  .tag <- stringi::stri_c("<", .str_format, ">")

  return(.tag)

}
