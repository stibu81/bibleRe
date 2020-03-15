#' Access Web Interface of the Public Library in Koeniz
#'
#' @docType package
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


# List of relevant URLs

bib_urls <- dplyr::tibble(
  base_url = "http://katalog.iz-region-bern.ch/WebOPAC-Koeniz/account",
  login = paste0(base_url, "/login"),
  documents = paste0(base_url,
                     "/circulations?sort=Circulations.DueDate&page=1&pageSize=200")
)
