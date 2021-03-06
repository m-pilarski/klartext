#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom stringi %s+%
#' @export
stringi::`%s+%`

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

#' @noRd
install_num2words <- function(.method="auto", .conda="auto"){

  # .py_env <- Sys.getenv("RETICULATE_PYTHON_ENV")
  # .py_env <- dplyr::if_else(.py_env == "", "r-reticulate", .py_env)
  # .py_pkg <- reticulate::py_list_packages(envname=.py_env, type=.method)$package
  #
  # if(!"num2words" %in% .py_pkg){
  #   reticulate::py_install(
  #     "num2words", envname=.py_env, method=.method, conda=.conda
  #   )
  # }

  reticulate::py_install("num2words", method=.method, conda=.conda)

}
