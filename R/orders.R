#' List Orders
#'
#' @inheritParams bib_list_documents
#'
#' @return
#' a tible containing a table of orders
#'
#' @export

bib_list_orders <- function(session) {

  page <- rvest::session_jump_to(session, bib_urls$orders)

  # extract the node with the document table
  tab_node <- page %>%
    rvest::html_element(xpath = "//table[@class='table wo-grid-table']")

  if (length(tab_node) > 0) {
    table <- extract_orders_table(tab_node) %>%
      dplyr::mutate(link = extract_document_links(tab_node),
                    author_search = author_search_link(.data$author))
  } else {
    table <- dplyr::tibble(
      id = character(0),
      author = character(0),
      title = character(0),
      type = character(0),
      order_date = as.Date(character(0)),
      link = character(0),
      author_search = character(0)
    )
  }

  table
}



extract_orders_table <- function(tab_node) {

  replace_br(tab_node)
  table <- rvest::html_table(tab_node)
  table[, names(table) != ""]  %>%
    dplyr::as_tibble() %>%
    dplyr::rename(id = "Exemplarnr.",
                  author_title = "Autor / Titel",
                  type = "Abholart",
                  order_date = "Erzeugungsdatum") %>%
    tidyr::separate("author_title", c("author", "title"),
                    "\n", fill = "left") %>%
    dplyr::mutate(
      # id is not always numeric => convert to character to avoid crash
      # when tables are combined.
      id = as.character(.data$id),
      order_date = lubridate::dmy(.data$order_date),
      author = stringr::str_remove(.data$author, "\\d+-\\d*") %>%
                stringr::str_trim(),
      title = stringr::str_trim(.data$title)
    ) %>%
    tidyr::replace_na(list(author = "---", title = "---"))

}
