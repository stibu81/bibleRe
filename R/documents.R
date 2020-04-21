#' List Borrowed Documents
#'
#' @param session a logged in session to the library
#'  web interface
#'
#' @return
#' a tible containing a table of borrowed documents
#'
#' @export

bib_list_documents <- function(session) {

  page <- rvest::jump_to(session,
                         bib_urls$documents)

  # extract the node with the document table
  tab_node <- page %>%
    rvest::html_node(xpath = "//table[@class='table wo-grid-table']")

  if (length(tab_node) > 0) {
    table <- extract_document_table(tab_node) %>%
              dplyr::mutate(link = extract_document_links(tab_node),
                            chk_id = extract_checkbox_ids(tab_node))
  } else {
    table <- dplyr::tibble(
      id = integer(0),
      author = character(0),
      title = character(0),
      due_date = as.Date(character(0)),
      n_renewal = integer(0),
      renewal_date = as.Date(character(0)),
      link = character(0),
      chk_id = character(0)
    )
  }

  table
}

# replace HTML line breaks by "\n"
replace_br <- function(node) {
  # source: https://stackoverflow.com/a/46755666/4303162
  # user: hrbrmstr
  xml2::xml_find_all(node, ".//br") %>%
    xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(node, ".//br") %>%
    xml2::xml_remove()
}

extract_document_table <- function(tab_node) {

  replace_br(tab_node)
  table <- rvest::html_table(tab_node)
  table[, names(table) != ""]  %>%
    dplyr::as_tibble() %>%
    dplyr::rename(id = "Exemplarnr.",
                  author_title = "Autor / Titel",
                  due_date = "F\u00e4lligkeitsdatum",
                  n_renewal = "Verl\u00e4ngerungen",
                  renewal_date = "Datum der Verl\u00e4ngerung") %>%
    tidyr::separate("author_title", c("author", "title"),
                    "\n", fill = "left") %>%
    dplyr::mutate(
      due_date = lubridate::dmy(.data$due_date),
      renewal_date = lubridate::dmy(.data$renewal_date),
      author = stringr::str_remove(.data$author, "\\d+-\\d*") %>%
                stringr::str_trim(),
      title = stringr::str_trim(.data$title)
    ) %>%
    tidyr::replace_na(list(author = "---", title = "---"))

}

extract_document_links <- function(tab_node) {

  xml2::xml_find_first(tab_node, "//tbody") %>%
    xml2::xml_children() %>%
    xml2::xml_find_first(".//a") %>%
    xml2::xml_attr("href") %>%
    paste0(bib_urls$domain, .)

}


extract_checkbox_ids <- function(tab_node) {
  xml2::xml_find_first(tab_node, "//tbody") %>%
    xml2::xml_children() %>%
    xml2::xml_find_first(".//input[@type='checkbox']") %>%
    xml2::xml_attr("value")
}
