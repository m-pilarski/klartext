#' Remove all \code{@mentions} from the beginning of strings.
#'
#' \code{str_remove_reply_mentions} - Remove all \code{@mentions} from the beginning of strings. This can be useful for cleaning tweets because the texts of replies start with the \code{screen_names} of all users further down the reply tree.
#'
#' @param .str ...
#' @return \code{str_remove_reply_mentions} - returns a ...
#' @rdname str_remove_reply_mentions
#' @export
#' @examples
#' ## str_remove_reply_mentions EXAMPLE:
#'
#' str_remove_reply_mentions(c(
#'   "@test test!",
#'   "@test @test test test!"
#' ))
str_remove_reply_mentions <- function(
  .str
){
  stringi::stri_replace_all_regex(
    str=.str,
    pattern=str_c("(?:", pattern_reg_screen_name, " )+"),
    replacement=""
  )
}
