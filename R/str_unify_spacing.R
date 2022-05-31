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
#' str_unify_spacing(
#'   .str <- "fds  f\n @test_sn fdsfsd #test_ht, <<TAG> test! #dsa"
#' )
str_unify_spacing <- function(.str){

  .excl_regex_dict <-
    "\\<([A-Z0-9_]+?)\\>|" %s+%
    "(?<![[:alnum:]&])@[[:alnum:]_]+(?=[^[:alnum:]_]|$)|" %s+%
    "(?<![[:alnum:]&])#[[:alpha:]][[:alnum:]_]*(?=[^[:alnum:]_]|$)"

  .excl_loc <- stringi::stri_locate_all_regex(
    str=.str, pattern=.excl_regex_dict
  )

  purrr::map2_chr(.str, .excl_loc, function(..str, ..excl_loc){

    ..excl_loc <- tibble::add_column(
      tibble::as_tibble(..excl_loc), incl = FALSE
    )

    ..incl_loc <- tibble::tibble(
      start = c(1, ..excl_loc[["end"]] + 1),
      end = c(..excl_loc[["start"]] - 1, -1),
      incl = TRUE
    )

    dplyr::bind_rows(..excl_loc, ..incl_loc) %>%
      dplyr::filter(start != end & start <= nchar(..str)) %>%
      dplyr::arrange(start) %>%
      dplyr::mutate(
        str_sub=
          stringi::stri_sub(..str, from=start, to=end) %>%
          purrr::map_if(incl, function(...str){
            ...str %>%
              stringi::stri_split_boundaries(type="word") %>%
              purrr::pluck(1) %>%
              stringi::stri_subset_charclass("[^[:space:][:cntrl:]]") %>%
              paste(collapse=" ")
          }, .else=identity) %>%
          purrr::flatten_chr()
      ) %>%
      dplyr::pull(str_sub) %>%
      paste(collapse=" ")

  })

}

globalVariables(c("start", "end", "incl", "str_sub"))

