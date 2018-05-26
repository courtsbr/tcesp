tce_download_link <- function(i, path) {
  pdf_file <- paste0(path, "/", i, ".pdf")
  html_file <- paste0(path, "/", i, ".html")
  if (!file.exists(pdf_file)) {
    link <- paste0("http://www2.tce.sp.gov.br/arqs_juri/pdf/", i, ".pdf")
    r <- httr::GET(link, httr::write_disk(pdf_file, overwrite = TRUE))
    ct <- r$all_headers[[1]]$headers[["content-type"]]
    if (ct != "application/pdf") {
      file.rename(pdf_file, html_file)
    }
  }
}

tce_materias <- function(txt) {
  link <- "https://www2.tce.sp.gov.br/protocolo-drupal/pesquisa-materia.asp"
  body <- list('XPROCESSO' = '',
               'XCDMATERIA' = '',
               'xNUMERO' = '',
               'xREGIONAL' = '',
               'xANO' = '',
               'XEXERCICIO' = '',
               'xAUTUACAO_DE' = '',
               'xAUTUACAO_ATE' = '',
               'XMATERIA' = '',
               'XDSPARTE1' = '',
               'XDSPARTE2' = '',
               'XOrdenadorDespesa' = '',
               'XCDRELATOR' = '',
               'XCDAUDITOR' = '',
               'XOBJETO' = txt)
  r <- httr::POST(link, body = body, encode = "form")
  materias <- r %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("onclick") %>%
    stringr::str_subset("Pesquisar") %>%
    stringr::str_extract("[0-9]+")
  materias
}

tce_paginas <- function(materia, txt) {
  # para cada categoria resultante
  link <- "https://www2.tce.sp.gov.br/protocolo-drupal/Tc01sql1.asp"
  body <- list('XPROCESSO' = '',
               'XEXERCICIO' = '',
               'xAUTUACAO_DE' = '',
               'xAUTUACAO_ATE' = '',
               'XCDMATERIA' = materia,
               'xCDRELATOR' = '',
               'xCDAUDITOR' = '',
               'xDSPARTE' = '',
               'xDSPARTE1' = '',
               'xDSPARTE2' = '',
               'XOBJETO' = txt)
  r <- httr::POST(link, body = body, encode = "form")
  pags_results <- r %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//b") %>%
    xml2::xml_text() %>%
    stringr::str_squish() %>%
    stringr::str_extract_all("(?<=de )[0-9]+") %>%
    dplyr::first() %>%
    as.numeric()
  pags_results
}

tce_palavra_chave <- function(txt, path) {
  materias <- tce_materias(txt)
  paginas <- purrr::map(materias, tce_paginas, txt = txt)
  pags <- purrr::transpose(paginas)[[1]] %>%
    purrr::flatten_dbl() %>%
    purrr::map(seq_len)
  purrr::map2(pags, materias, download_pagina, txt = txt, path = path)
}

download_pagina <- function(paginas, materia, txt, path) {
  message(paste("materia", materia, "============"))
  purrr::map(seq_along(paginas), download_pagina_,
             materia = materia, txt = txt, path = path)
}

download_pagina_ <- function(pag, materia, txt, path) {
  message(paste("pagina", pag, "------------"))
  file_pag <- sprintf("%s/materia_%s_pag_%03d/pag_%03d.html", path,
                      materia, pag, pag)
  path_dir <- dirname(file_pag)
  fs::dir_create(path_dir)
  link <- "https://www2.tce.sp.gov.br/protocolo-drupal/Tc01sql1.asp"
  body <- list('direcao' = '+',
               'total' = '0',
               'pagina' = pag - 2,
               'x' = '26',
               'y' = '17',
               'xIdProtocolo' = '',
               'xEXERCICIO' = 'Null',
               'xAUTUACAO_DE' = 'Null',
               'xAUTUACAO_ATE' = 'Null',
               'XCDMATERIA' = materia,
               'xDSPARTE' = 'Null',
               'xDSPARTE1' = 'Null',
               'xDSPARTE2' = 'Null',
               'xOrdenadorDespesa' = 'Null',
               'xCDRELATOR' = 'Null',
               'xCDAUDITOR' = 'Null',
               'xVIGENCIAINICIAL' = 'Null',
               'xAUTORIDADERESPONSAVEL' = 'Null',
               'xMODALIDADE' = 'Null',
               'xOBJETO' = txt,
               'totpag' = '0')
  r <- httr::POST(link, body = body, encode = "form",
                  httr::write_disk(file_pag, overwrite = TRUE))
  ids <- xml2::read_html(r) %>%
    xml2::xml_find_all("//table[@bgcolor='#ADCEEF']") %>%
    purrr::map_dfr(parse_titulo, .id = "id") %>%
    dplyr::pull(processo_no)
  message("Despachos")
  purrr::walk(ids, download_despachos, path = path_dir)
  message("Decisoes")
  purrr::walk(ids, download_decisoes, path = path_dir)
  path_dir
}

#-------------------------------------------------------------------------------

download_despachos <- function(.x, path) {
  id <- stringr::str_remove_all(.x, "[^0-9]")
  file <- sprintf("%s/despachos_%s.html", path, id)
  link <- "https://www2.tce.sp.gov.br/protocolo-drupal/tc01sql1-despachos.asp"
  body <- list(xCDPROCESSO1 = .x, xCDPROCESSO = .x)
  r <- httr::POST(link, body = body, encode = "form",
                  httr::write_disk(file, overwrite = TRUE))
  file
}

download_decisoes <- function(.x, path) {
  id <- stringr::str_remove_all(.x, "[^0-9]")
  file <- sprintf("%s/decisoes_%s.html", path, id)
  link <- "https://www2.tce.sp.gov.br/protocolo-drupal/tc01sql1-decisoes.asp"
  body <- list(xCDPROCESSO1 = .x, xCDPROCESSO = .x)
  r <- httr::POST(link, body = body, encode = "form",
                  httr::write_disk(file, overwrite = TRUE))
  file
}
