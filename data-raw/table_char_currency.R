`%>%` <- magrittr::`%>%`

table_char_currency <-
  "https://en.wikipedia.org/wiki/Template:List_of_currency_symbols" %>%
  rvest::read_html() %>%
  rvest::html_elements("table") %>%
  as.character() %>%
  stringr::str_remove("<tr>[[:any:]]*?</tr>") %>%
  rvest::read_html() %>%
  rvest::html_table(header=TRUE) %>%
  purrr::pluck(1) %>%
  janitor::clean_names() %>%
  dplyr::select(symbol, name, currency) %>%
  dplyr::mutate(dplyr::across(where(is.character), function(.str){
    .str %>%
      stringr::str_replace_all(" *\\[.+?\\] *", " ") %>%
      stringr::str_replace_all("\\/\\.", ".") %>%
      stringr::str_squish()
  })) %>%
  dplyr::mutate(dplyr::across(name, stringr::str_replace_all, c(
    "cent(, centavo, etc\\.| etc\\. variant)" = "cent"
  ))) %>%
  dplyr::mutate(dplyr::across(currency, stringr::str_replace_all, c(
    "\\(£L\\) the \"pound\" unit" = "(£L), the \"pound\" unit",
    "\\(Afl\\.\\) Netherlands" = "\\(Afl\\.\\), Netherlands",
    ", the \"pound\" unit .+ Saint Helena\\)" = "",
    "Kiribati, Liberian" = "Kiribati ( ), Liberian",
    "Mauritian, Nepalese" = "Mauritian ( ), Nepalese",
    "Pakistani and Sri Lankan" = "Pakistani ( ) and Sri Lankan",
    "Sri Lankan rupees" = "Sri Lankan rupee",
    "Syrian \\(£S\\) pounds." = "Syrian (£S)",
    "North Korean won," = "North Korean won ( ),",
    "Japanese yen \\(円 / 圓\\);" = "Japanese yen (円 / 圓)"
  ))) %>%
  dplyr::mutate(
    info = stringr::str_split(
      stringr::str_c(name, " (", symbol, "), ", currency),
      "(?<=\\))(,? and|,) *"
    )
  ) %>%
  tidyr::unnest_longer(info) %>%
  dplyr::mutate(
    symbol =
      stringr::str_extract(info, "(?<=\\().+(?=\\))"),
    currency =
      info %>%
      stringr::str_replace("^(.*?) *\\((.+)\\)", "\\1") %>%
      stringr::str_c(" ", name) %>%
      stringr::str_replace("(.+)s? +\\1 *$", "\\1"),
    symbol_is_symbol =
      stringr::str_detect(symbol, "\\p{Sc}")
  ) %>%
  dplyr::mutate(
    symbol = stringr::str_split(symbol, "( *[,/] *| +(and|or) +)")
  ) %>%
  tidyr::unnest_longer(symbol) %>%
  dplyr::mutate(dplyr::across(where(is.character), stringr::str_squish)) %>%
  dplyr::select(symbol, currency=name, currency_spec=currency) %>%
  dplyr::filter(symbol != "") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    symbol_is_ascii =
      symbol %>%
      stringr::str_detect("[^[:ascii:]]", negate=TRUE),
    symbol_covertible_ascii =
      symbol %>%
      klartext::str_to_ascii(.repl_non_ascii=NULL) %>%
      stringr::str_detect("[^[:ascii:]]", negate=TRUE)
  )

table_char_currency %>%
  dplyr::mutate(
    dplyr::across(where(is.logical), dplyr::if_else, "\u2705", "\u274C")
  ) %>%
  knitr::kable(format="pipe") %>%
  readr::write_lines("table_char_currency.md")

usethis::use_data(table_char_currency)
