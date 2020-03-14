#' List Borrowed Documents
#'
#' @param session session object corresponding to a
#'  login to the library web inerface.
#'
#' @return
#' a tible containing a table of borrowed documents
#'
#' @export

bib_list_documents <- function(session) {

  url2 <- "http://katalog.iz-region-bern.ch/WebOPAC-Koeniz/account/circulations?sort=Circulations.DueDate&page=1&pageSize=200"
  page <- rvest::jump_to(session, url2)

  # extract the table
  tab_node <- page %>%
    rvest::html_node(xpath = "//table[@class='table wo-grid-table']")

  if (length(tab_node) > 0) {
    # reformat the table
    replace_br(tab_node)
    table <- rvest::html_table(tab_node)
    table <- table[, names(table) != ""]  %>%
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
        author = stringr::str_remove(.data$author, "\\d+-\\d*")
      ) %>%
      tidyr::replace_na(list(author = "---", title = "---"))
  } else {
    table <- dplyr::tibble()
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
