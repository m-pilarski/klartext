#' Filter Rows That Contain Markers
#'
#' \code{str_describe_emoji} - Convert all Emojis to some ...
#'
#' @param .str ...
#' @param .resolution ... \code{c("name", "subgroup", "group")}
#' @return \code{str_describe_emoji} - returns a ...
#' @rdname str_describe_emoji
#' @export
#' @examples
#' ## str_describe_emoji EXAMPLE:
#'
#' example_emoji <- "😀 😆 😡 💀"
#' str_describe_emoji(example_emoji, .resolution="name")
#' str_describe_emoji(example_emoji, .resolution="subgroup")
#' str_describe_emoji(example_emoji, .resolution="group")
str_describe_emoji <- function(
  .str, .resolution="name"
){
  
  .str_names <- names(.str)
  
  .resolution <- .resolution[1]

  .pattern <- klartext::table_char_emoji[["pattern_fixed"]]

  .replacement <-
    klartext::table_char_emoji |>
    purrr::chuck(stringi::stri_c("replacement_", .resolution)) |>
    format_klartag(.str_prepend="emo")

  .str <- stringi::stri_replace_all_fixed(
    .str, pattern=.pattern, replacement=.replacement, vectorize_all=FALSE
  )
  
  names(.str) <- .str_names
  
  return(.str)
  
}

globalVariables(c(
  "pattern_fixed", "replacement_name", "replacement_subgroup",
  "replacement_group"
))
