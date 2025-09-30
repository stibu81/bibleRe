#' List Fees
#'
#' @inheritParams bib_list_documents
#'
#' @return
#' a tibble containing a table of fees
#'
#' @family functions to download data
#'
#' @export

bib_list_fees <- function(session) {

  logger::log_debug("jump to fees page")
  jump_to(session, bib_urls$fees)

  # extract the node with the fees table
  logger::log_debug("extract the fees table")
  tab_node <- session %>%
    rvest::html_element(css = "table.wo-grid-table")

  if (length(tab_node) > 0) {
    table <- extract_fees_table(tab_node)
  } else {
    table <- dplyr::tibble(
      type = character(0),
      date = as.Date(character(0)),
      amount = numeric(0),
      document_id = character(0),
      note = character(0)
    )
  }

  logger::log_debug("fees table completed")
  table
}



extract_fees_table <- function(tab_node) {

  tab_node  %>%
    flex_html_table() %>%
    dplyr::rename(type = "Art der Geb\u00fchr",
                  date = "Erzeugungsdatum",
                  amount = "Betrag",
                  document_id = "Exemplarnr.",
                  note = "Kurznotiz") %>%
    dplyr::mutate(
      date = lubridate::dmy(.data$date),
      amount = as.numeric(stringr::str_replace(.data$amount, ",", ".")),
      document_id = as.character(.data$document_id),
      note = as.character(.data$note)
    )

}
