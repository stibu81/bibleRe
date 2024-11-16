#' Download the Favicon from the Library's Website
#'
#' @param file path where the icon will be stored. If the name does
#'  not end with ".ico", the ending is appended.
#'
#' @export

bib_get_icon <- function(file = "~/biblere.ico") {

  file <- normalizePath(file, mustWork = FALSE)
  if (file.exists(file)) {
    stop("The file ", file, " already exists.")
  }
  if (!stringr::str_detect(file, "\\.ico$")) {
    file <- paste0(file, ".ico")
  }

  favicon <- get_favicon_url()
  utils::download.file(favicon, file)

  invisible(file.exists(file))
}


# Helper function to determine the url of the favicon from the library's
# webpage.

get_favicon_url <- function() {

  base_url <- "https://koenizerbibliotheken.ch/de"
  rvest::read_html(base_url) %>%
    rvest::html_elements("link[rel='icon']") %>%
    rvest::html_attr("href") %>%
    paste(base_url, ., sep = "/")

}
