#' Renew Documents
#'
#' @param session a logged in session to the library
#'  web interface
#' @param chk_ids character giving the checkbox ids of the
#'  documents to be extended. Note that those documents
#'  should belong to the user that is currently logged in.
#'
#' @export

bib_renew <- function(session, chk_ids) {

  # create the query. The squre brackets [] must be escaped.
  query <- paste0("selectedItems%5B", seq_along(chk_ids) - 1, "%5D=", chk_ids,
                  collapse = "&")
  url <- paste0(bib_urls$renew, query)

  page <- rvest::jump_to(session, url)

}
