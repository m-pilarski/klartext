#' \code{str_describe_numbers} - Replace all cardinal or ordinal numbers in a character vector with generic tags.
#'
#' @param .str ...
#' @param .lang ...
#' @param .ordinal ...
#' @return \code{str_describe_numbers} - returns a ...
#' @rdname str_describe_numbers
#' @export
#' @examples
#' ## str_describe_numbers EXAMPLE:
#'
#' \dontrun{
#' str_describe_numbers(c(
#'   "The 2020 United States presidential election",
#'   "was the 59th quadrennial presidential election",
#'   "held on Tuesday, November 3, 2020"
#' ))
#' }
# str_describe_numbers <- function(
#   .str, .lang="en", .ordinal=TRUE
# ){
#
#   check_num2words()
#   stopifnot(is.character(.str))
#   stopifnot(.lang %in% c("en"))
#
#   .str |>
#     stringi::stri_split_boundaries(type="word") |>
#     tibble::as_tibble_col("tok") |>
#     tibble::rowid_to_column("doc_id") |>
#     tidyr::unnest_longer(tok) |>
#     dplyr::mutate(dplyr::across(tok, function(..tok){
#
#       ..tok_is_number <- stringi::stri_detect_regex(
#         ..tok, "^(?:([0-9]*[.,])*[0-9]+|(?:[0-9]*(?:1st|2nd|3rd|[0456789]th)))$"
#       )
#       ..tok_is_ordinal <- dplyr::if_else(
#         ..tok_is_number, stringi::stri_detect_regex(..tok, "(?:st|nd|rd|th)$"),
#         FALSE
#       )
#       ..tok_is_cardinal <- ..tok_is_number & !..tok_is_ordinal
#
#       ..tok_mod <-
#         ..tok |>
#         purrr::modify_if(..tok_is_ordinal, function(..tok){
#           ..klartext_num2words$num2words(
#             stringi::stri_replace_all_regex(..tok, "[^0-9]+", ""),
#             ordinal=TRUE, lang=.lang
#           )
#         }) |>
#         purrr::modify_if(..tok_is_cardinal, function(..tok){
#           ..klartext_num2words$num2words(
#             stringi::stri_replace_all_regex(..tok, ",", ""),
#             ordinal=FALSE, lang=.lang
#           )
#         })
#
#       return(..tok_mod)
#
#     })) |>
#     dplyr::group_by(doc_id) |>
#     dplyr::summarize(
#       str = stringi::stri_c(tok, collapse=""), .groups="drop"
#     ) |>
#     tidyr::complete(doc_id = seq_along(.str), fill=list(str="")) |>
#     dplyr::pull(str) |>
#     rlang::set_names(names(.str))
#
# }



#' check_num2words
#'
#' @param .do_install ...
#'
#' @return invisible(NULL)
#'
#' @examples
#' if(FALSE){
#'   check_num2words(.do_install=TRUE)
#' }
check_num2words <- function(.do_install){

  .py_path <- reticulate::py_discover_config()$python

  .num2words_installed <-
    reticulate::py_list_packages(python=.py_path) |>
    dplyr::filter(package == "num2words")

  if(nrow(.num2words_installed) == 0){
    message(
      "The python-package \"num2words\" is not installed in the environment ",
      "currently used by reticulate."
    )
    if(missing(.do_install)){
      .do_install <- try(utils::askYesNo(
        msg="Dou you want to install it?", default=FALSE
      ))
    }
    if(isTRUE(.do_install)){
      reticulate::py_install("num2words==0.5.10", pip=TRUE)
    }else{
      stop("The package was not installed.")
    }
  }else if(.num2words_installed$version != "0.5.10"){
    message(
      "The python-package \"num2words\" is installed but the version does not ",
      "match the tested one (0.5.10). If you're running into problems, ",
      "consider reinstalling it by running:\n",
      "reticulate::py_install(\"num2words==0.5.10\", ignore_installed=TRUE)"
    )
  }
  
  return(invisible(NULL))

}


globalVariables(c("package", ".do_install"))
