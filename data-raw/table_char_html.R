table_char_html <-
  rvest::read_html(
    "https://dev.w3.org/html5/spec-LC/named-character-references.html"
  ) |> 
  rvest::html_element(xpath="//div[@id='named-character-references-table']") |> 
  rvest::html_table() |> 
  dplyr::mutate(
    pattern_fixed = 
      Name |> 
      stringi::stri_replace_first_regex("^(?!&)", "&") |> 
      stringi::stri_replace_first_regex("$(?<!;)", ";"), 
    replacement = Glyph,
    .keep="none"
  ) |> 
  tidyr::drop_na() |> 
  dplyr::distinct()

usethis::use_data(table_char_html, overwrite=TRUE)
