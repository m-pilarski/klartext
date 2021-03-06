#' Filter Rows That Contain Markers
#'
#' \code{str_unify_spacing} - Convert all Emojis to some ...
#'
#' @param .str ...
#' @return \code{str_unify_spacing} - returns a ...
#' @rdname str_unify_spacing
#' @export
#' @examples
#' ## str_unify_spacing EXAMPLE:
#'
#' str_unify_spacing(c(
#'   "This    @test_at that\n #test_hash",
#'   "<test-no-tag> <TEST_TAG> test!?!?",
#'   "An URL www.example.com/test ."
#' ))
str_unify_spacing <- function(.str){

  .tok_lock_regex <-
    "\\<([A-Z0-9_]+?)\\>|" %s+%
    "(?<![[:alnum:]&])@[[:alnum:]_]+(?=[^[:alnum:]_]|$)|" %s+%
    "(?<![[:alnum:]&])#[[:alpha:]][[:alnum:]_]*(?=[^[:alnum:]_]|$)|" %s+%
    "\\b((http|ftp)s?://|mailto:|www\\.)[^\\s/$.?#][[[:alnum:]]-._~:/?#" %s+%
    "\\[\\]@!$&'()*+%,;=]+[^\\.]\\b"

  .str %>%
    stringi::stri_replace_all_regex(.tok_lock_regex, " $0 ") %>%
    stringi::stri_replace_all_regex("<[A-Z0-9_]+?>", " $0 ") %>%
    stringi::stri_split_charclass("\\p{WHITE_SPACE}", omit_empty=TRUE) %>%
    tibble::as_tibble_col("tok") %>%
    tibble::rowid_to_column("doc_id") %>%
    tidyr::unnest_longer(tok) %>%
    dplyr::mutate(tok = dplyr::if_else(
      condition=stringi::stri_detect_regex(tok, .tok_lock_regex, negate=TRUE),
      true=stringi::stri_split_boundaries(tok, type="word"),
      false=as.list(tok)
    )) %>%
    tidyr::unnest_longer(tok) %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarize(
      str = stringi::stri_c(tok, collapse=" "), .groups="drop"
    ) %>%
    tidyr::complete(doc_id = seq_along(.str), fill=list(str="")) %>%
    dplyr::pull(str) %>%
    rlang::set_names(names(.str))

}

globalVariables(c("doc_id", "str", "tok"))

# str_unify_spacing <- function(.str){
#
#   .excl_regex_dict <-
#     "\\<([A-Z0-9_]+?)\\>|" %s+%
#     "(?<![[:alnum:]&])@[[:alnum:]_]+(?=[^[:alnum:]_]|$)|" %s+%
#     "(?<![[:alnum:]&])#[[:alpha:]][[:alnum:]_]*(?=[^[:alnum:]_]|$)|" %s+%
#     "\\b((http|ftp)s?://|mailto:|www\\.)[^\\s/$.?#][[[:alnum:]]-._~:/?#" %s+%
#     "\\[\\]@!$&'()*+%,;=]+[^\\.]\\b"
#
#   .excl_loc <- stringi::stri_locate_all_regex(
#     str=.str, pattern=.excl_regex_dict
#   )
#
#   purrr::map2_chr(.str, .excl_loc, function(..str, ..excl_loc){
#
#     ..excl_loc <-
#       ..excl_loc %>%
#       tibble::as_tibble() %>%
#       tibble::add_column(incl = FALSE) %>%
#       tidyr::drop_na()
#
#     ..incl_loc <- tibble::tibble(
#       start = c(1, ..excl_loc[["end"]] + 1),
#       end = c(..excl_loc[["start"]] - 1, nchar(..str)),
#       incl = TRUE
#     )
#
#     ..all_loc <-
#       dplyr::bind_rows(..excl_loc, ..incl_loc) %>%
#       dplyr::filter(start < end & start <= nchar(..str)) %>%
#       dplyr::arrange(start) %>%
#       dplyr::mutate(str_sub = stringi::stri_sub(
#         str=..str, from=cbind(start, end), use_matrix=TRUE
#       )) %>%
#       dplyr::mutate(str_sub = dplyr::case_when(
#         incl ~
#           str_sub %>%
#           stringi::stri_split_boundaries(type="word") %>%
#           purrr::map_chr(function(...str_sub_split){
#             ...str_sub_split %>%
#               stringi::stri_subset_charclass("[^[:space:][:cntrl:]]") %>%
#               paste(collapse=" ")
#           }),
#         TRUE ~
#           str_sub
#       )) %>%
#       dplyr::pull(str_sub) %>%
#       paste(collapse=" ")
#
#   })
#
# }
# globalVariables(c("start", "end", "incl", "str_sub"))
