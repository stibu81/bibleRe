#' Access Web Interface of the Public Library in Koeniz
#'
#' @docType package
#'
#' @importFrom magrittr %>% %<>%
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
  domain = "https://katalog.iz-region-bern.ch/",
  base_url = paste0(domain, "WebOPAC-Koeniz/"),
  login = paste0(base_url, "account/login"),
  documents = paste0(base_url,
                     "account/circulations?sort=Circulations.DueDate&page=1&pageSize=500"),
  notice = paste0(base_url, "search/notice?noticeNr="),
  search = paste0(base_url,
                  "search/shortview?searchField=W&searchType=Simple&searchTerm="),
  person_search = paste0(base_url,
                         "search/shortview?searchType=Extended&searchField=Person&searchTerm="),
  renew = paste0(base_url,
                 "account/renew?"),
  fees = paste0(base_url, "account/fees"),
  orders = paste0(base_url, "account/orders"),
  watchlist = paste0(base_url, "watchlist?sort=Notices.Author%2CNotices.Title&page=1&pageSize=500"),
  timestamp = paste0(base_url, "handler/timestamp")
)

globalVariables(".")
