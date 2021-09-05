#' Download the Favicon from the Library's Website
#'
#' @param file path where the icon will be stored. If the name does
#'  not end with ".ico", the ending is appended.
#'
#' @details
#' This requires the package `faviconPlease` to be installed. Install it
#' with `install.packages('faviconPlease')`.
#'
#' @export

bib_get_icon <- function(file = "~/biblere.ico") {

  if (!rlang::is_installed("faviconPlease")) {
    stop("The package faviconPlease must be installed to download the favicon. ",
         "Run install.packages('faviconPlease') to install it.")
  }

  file <- normalizePath(file, mustWork = FALSE)
  if (file.exists(file)) {
    stop("The file ", file, " already exists.")
  }
  if (!stringr::str_detect(file, "\\.ico$")) {
    file <- paste0(file, ".ico")
  }

  favicon <- faviconPlease::faviconDuckDuckGo("koenizerbibliotheken.ch")
  message("Downloading ", favicon, " ...")
  utils::download.file(favicon, file)

  invisible(file.exists(file))
}
