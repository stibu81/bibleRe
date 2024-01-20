#' Download Watchlist
#'
#' Download a table of items on a users watchlist.
#'
#' @inheritParams bib_list_documents
#'
#' @return
#' a tibble containing a table items on the watchlist
#'
#' @family functions to download data
#'
#' @export

bib_list_watchlist <- function(session) {

  page <- rvest::session_jump_to(session, bib_urls$watchlist)

  # extract the node with the document table
  tab_node <- page %>%
    rvest::html_element(xpath = "//table[@class='table wo-grid-table']")

  if (length(tab_node) > 0) {
    table <- extract_watchlist_table(tab_node) %>%
      # the link to the document does not work in the watchlist because it actually
      # points to the item in the watchlist, which only works with login
      # => only extract the link for author search
      dplyr::mutate(author_search = author_search_link(.data$author))
  } else {
    table <- dplyr::tibble(
      author = character(0),
      title = character(0),
      year = integer(0),
      kind = character(0),
      age = character(0),
      library = character(0),
      author_search = character(0)
    )
  }

  table
}


extract_watchlist_table <- function(tab_node) {

  table <- tab_node  %>%
    flex_html_table() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(author_title = "Autor / Titel",
                  year = "Jahr",
                  kind_age = "Medienart / Alter",
                  library = "Bibliothek")

  # every other row contains no meaningful information
  if (nrow(table) > 0) {
    table <- table[seq(1, nrow(table), by = 2), ]
  }

  table %>%
    tidyr::separate("author_title", c("author", "title"),
                    "\n", fill = "left") %>%
    tidyr::separate("kind_age", c("kind", "age"),
                    "\n", fill = "left") %>%
    dplyr::mutate(
      author = stringr::str_remove(.data$author, "\\d+-\\d*") %>%
                stringr::str_trim(),
      title = stringr::str_trim(.data$title),
      year = as.integer(.data$year),
      kind = stringr::str_trim(.data$kind),
      age = stringr::str_trim(.data$age)
    ) %>%
    tidyr::replace_na(list(author = "---", title = "---"))

}
