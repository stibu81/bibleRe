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

  rlang::check_installed("faviconPlease")

  file <- normalizePath(file, mustWork = FALSE)
  if (file.exists(file)) {
    stop("The file ", file, " already exists.")
  }
  if (!stringr::str_detect(file, "\\.ico$")) {
    file <- paste0(file, ".ico")
  }

  favicon <- faviconPlease::faviconDuckDuckGo("koenizerbibliotheken.ch")
  utils::download.file(favicon, file)

  invisible(file.exists(file))
}
