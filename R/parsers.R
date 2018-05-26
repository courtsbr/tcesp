html_table2 <- function(x) {
  f <- purrr::possibly(rvest::html_table, NA, quiet = TRUE)
  res <- tibble::as_tibble(f(x))
  if (is.na(res)) tibble::enframe(xml2::xml_text(x)) else res
}

parse_titulo <- function(h) {
  h %>%
    xml2::xml_text() %>%
    stringr::str_split("\t+") %>%
    dplyr::first() %>%
    stringr::str_squish() %>%
    stringr::str_subset("[a-zA-Z]") %>%
    stringr::str_split(": ") %>%
    purrr::map_dfr(~tibble::tibble(key = .x[1], val = .x[2])) %>%
    tidyr::spread(key, val) %>%
    purrr::set_names(abjutils::rm_accent) %>%
    janitor::clean_names()
}
parse_infos <- function(h) {
  h %>%
    rvest::html_table() %>%
    tibble::as_tibble() %>%
    dplyr::group_by(X1) %>%
    dplyr::summarise(res = paste(X2, collapse = "\n")) %>%
    tidyr::spread(X1, res) %>%
    purrr::set_names(abjutils::rm_accent) %>%
    janitor::clean_names()
}
parse_movs <- function(h) {
  tb <- h %>%
    rvest::html_table() %>%
    tibble::as_tibble()
  dplyr::bind_rows(
    purrr::set_names(tb[,c("X1", "X2")], c("a", "b")),
    purrr::set_names(tb[,c("X3", "X4")], c("a", "b"))
  ) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = paste(b, collapse = "\n")) %>%
    tidyr::spread(a, b) %>%
    purrr::set_names(abjutils::rm_accent) %>%
    janitor::clean_names()
}

zoados <- function(.x) {
  txt <- xml2::xml_text(.x)
  stringr::str_detect(txt, "Despachos|Decisões") || txt == ""
}

parse_html <- function(html) {
  titulos <- html %>%
    xml2::xml_find_all("//table[@bgcolor='#ADCEEF']") %>%
    purrr::map_dfr(parse_titulo, .id = "id")
  infos <- html %>%
    xml2::xml_find_all("//table[@bgcolor='#ADCEEF']") %>%
    xml2::xml_find_first("./following-sibling::table") %>%
    purrr::map_dfr(parse_infos, .id = "id")
  movs <- html %>%
    xml2::xml_find_all("//table[@bgcolor='#F1F1F1']") %>%
    xml2::xml_find_first("./following-sibling::table") %>%
    # ESTA RUIM
    purrr::discard(zoados) %>%
    purrr::map_dfr(parse_movs, .id = "id")
  list(titulos, infos, movs) %>%
    purrr::map(~tidyr::nest(dplyr::group_by(.x, id))) %>%
    purrr::reduce(dplyr::inner_join, by = "id") %>%
    purrr::set_names(c("id", "titulo", "infos", "movs"))
}

parse_d_ <- function(file) {
  h <- xml2::read_html(file)
  a <- xml2::xml_find_all(h, "//a[@target='_blank']")
  txt <- xml2::xml_text(a)
  link <- xml2::xml_attr(a, "href")
  tibble::tibble(txt = txt, link = link)
}
parse_d <- function(file) {
  file %>%
    purrr::set_names(file) %>%
    purrr::map_dfr(parse_d_, .id = "file")
}
