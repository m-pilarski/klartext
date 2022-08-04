#' Whitespace normalization
#'
#' \code{str_unify_spacing} - Normalizes whitespace by replacing everything
#' between words and punctuation characters with single space characters. The
#' identification of boundaries is performed using ICU Breakiterators with added
#' exceptions for #hashtags, @screen_names, URLs and <KLARTAGS> (as created by
#' other functions of this package)
#'
#' @param .str Character vector to be normalized
#' @return \code{str_unify_spacing} - Returns the normalized character vector
#' @references https://www.unicode.org/reports/tr29/#Word_Boundaries
#' @rdname str_unify_spacing
#' @export
#' @examples
#' ## str_unify_spacing EXAMPLE:
#'
#' str_unify_spacing(c(
#'   "This  @screen_name that\n #hash_tag, #123",
#'   "<not-A_KLARTAG> <A_KLARTAG>!?!? An URL",
#'   "www.example.com/test ..."
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
    stringi::stri_split_charclass("\\p{WHITE_SPACE}", omit_empty=TRUE) %>%
    tibble::as_tibble_col("tok") %>%
    tibble::rowid_to_column("doc_id") %>%
    tidyr::unnest_longer(tok) %>%
    dplyr::mutate(tok = dplyr::if_else(
      condition=stringi::stri_detect_regex(tok, .tok_lock_regex),
      true=as.list(tok),
      false=stringi::stri_split_boundaries(tok, type="word")
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
