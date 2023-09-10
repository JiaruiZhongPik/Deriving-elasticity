purrr::walk(list.files(here::here("code/functions"), full.names = TRUE, pattern = "\\.R$"), source)

