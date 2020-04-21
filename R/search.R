#' Search the Library Database
#'
#' @param search a character giving the search string
#'
#' @details
#' \code{bib_search()} returns the link to be used for the search
#' as character string.
#'
#' \code{bib_run_search()} uses javascript to open the search link
#' in a new tab.
#'
#' @export

bib_search <- function(search) {

  # prepare the search terms
  search <- search[1] %>%
    stringr::str_trim() %>%
    xml2::url_escape()

  paste0(bib_urls$search, search)
}
