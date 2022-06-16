table_char_emoji <-
  emoji::emojis %>%
  tibble::as_tibble()

usethis::use_data(table_char_emoji)
