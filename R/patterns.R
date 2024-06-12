#' @noRd
pattern_reg_url <- str_c(
  "\\b((http|ftp)s?://|mailto:|www\\.)",
  "[^[:space:]/$.?#][[:alnum:]-._~:/?#\\[\\]@!$&'()*+%,;=]+[^\\.]\\b"
)

#' @noRd
pattern_reg_screen_name <- "(?<![[:alnum:]&])@[[:alnum:]_]+(?=[^[:alnum:]_]|$)"


#' @noRd
pattern_reg_hashtag <- (
  "(?<![[:alnum:]&])#[[:alpha:]][[:alnum:]_]*(?=[^[:alnum:]_]|$)"
)


#' @noRd
pattern_reg_klartag <- "\\<([A-Z0-9_]+?)\\>"


#' @noRd
make_pattern_reg_cardinal <- function(.lang="en", .space="[ -]*"){

  stopifnot(.lang=="en")

  .c_1_9_reg <- "(?:f(?:ive|our)|s(?:even|ix)|t(?:hree|wo)|(?:ni|o)ne|eight)"

  .c_10_19_reg <- str_c(
    "(?:(?:(?:s(?:even|ix)|f(?:our|if)|nine)te|e(?:ighte|lev))en|",
    "t(?:(?:hirte)?en|welve))"
  )

  .c_2_9x10_reg <- "(?:(?:s(?:even|ix)|t(?:hir|wen)|f(?:if|or)|eigh|nine)ty)"

  .c_1_99_reg <- str_c(
    "(?:", .c_2_9x10_reg, .space, .c_1_9_reg, "?|", .c_10_19_reg, "|",
    .c_1_9_reg, ")"
  )

  .c_1_999_reg <- str_c(
    "(?:", .c_1_9_reg, .space, "hundred(?:", .space, "(?:and", .space, ")?(?:",
    .c_1_99_reg, "))?|", .c_1_99_reg, ")"
  )

  .c_1_9999_reg <- str_c(
    "(?:", .c_1_9_reg, .space, "thousand(?:", .space, "(?:and", .space, ")?(?:",
    .c_1_999_reg, "))?|", .c_1_999_reg, ")"
  )

  .c_big_l_reg <- "(?:hundred|thousand|(?:m|b|tr)illion)"

  .c_big_s_reg <- (
    "(?:(?:k|thsnd|m(?:ill?|ln)?|b(?:ill?|l?n)?|t(?:r(?:ill?|l?n))?)\\.?)"
  )

  .c_start_reg <- str_c(
    "(?:zero|", .c_1_9999_reg, "|", .c_big_l_reg, "s?)"
  )

  .c_rest_reg <- str_c("(?:", .c_start_reg, "|", .c_big_s_reg, ")")

  .c_full_reg <- str_c(
    "(?:", .c_start_reg, "|-? ?(?:[0-9][0-9 .,]*)?[0-9])(?:", .space,
    .c_rest_reg, ")*s?"
  )

  return(str_c("(?<=(^| ))(?:", .c_full_reg, ")(?=( |[[:punct:]]|$))"))

}


#' @noRd
make_pattern_reg_ordinal <- function(.lang="en", .space="[ -]*"){

  stopifnot(.lang=="en")

  .ordi_base <- c(
    "first", "second", "third", "fourth", "fifth", "sixth", "seventh",
    "eighth", "ninth"
  )

  .ordi_20_90_pre <- c(
    "twent", "thirt", "fort", "fift", "sixt", "sevent", "eight", "ninet"
  )

  .ordi_20_99 <-
    tidyr::expand_grid(
      start=.ordi_20_90_pre,
      end=c("ieth", str_c("y", .space, .ordi_base))
    ) |>
    dplyr::transmute(str_c(start, end)) |>
    dplyr::pull()

  .ordi_full <- c(
    .ordi_base, "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth",
    "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth",
    .ordi_20_99, "hundredth"
  )

  .reg_ordi <- str_c(
    "(?:", str_c(.ordi_full, collapse='|'), ")|",
    "(?:[0-9]*(?:1st|2nd|3rd|[0456789]th))"
  )

  return(str_c("(?<=(^| ))(?:", .reg_ordi, ")(?=( |[[:punct:]]|$))"))

}
