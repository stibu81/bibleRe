#' Run the application
#'
#' @param login_data_file character giving the full path to
#'  a file containing the login data.
#' @param n_due_days integer giving the default for the selection
#'  of the date used to filter due dates. By default, the app will
#'  show documents that are due at most `n_due_days` days form today.
#' @param launch.browser logical, if \code{TRUE}, the application
#'  is opened in the system's default browser, if \code{FALSE},
#'  no browser is started. If the argument is omitted, the value
#'  according to the option \code{shiny.launch.browser} is used,
#'  which in RStudio opens the internal shiny viewer.
#'
#' @export

run_biblere <- function(login_data_file = "~/.biblere_passwords",
                        n_due_days = 7,
                        launch.browser = NULL) {

    if (!file.exists(login_data_file)) {
      stop("an existing file must be provided.")
    }

    options(biblere_login_data_file = login_data_file)
    appDir <- system.file("shinyApp", package = "bibleRe")
    if (appDir == "") {
      stop("Could not find shiny app. Try re-installing `bibleRe`.",
           call. = FALSE)
    }

    options(biblere_n_due_days = n_due_days)

    if (is.null(launch.browser)) {
        launch.browser <- getOption("shiny.launch.browser", interactive())
    }

    shiny::runApp(appDir, display.mode = "normal",
                  launch.browser = launch.browser)
}


# Helper function to filter the documents table
filter_document_table <- function(table,
                                  due_date,
                                  show_renewable,
                                  show_nonrenewable,
                                  account,
                                  link_id = TRUE) {

  table %<>% dplyr::filter(.data$due_date <= !!due_date)
  if (!show_renewable) {
    table %<>% dplyr::filter(.data$n_renewal == 2)
  }
  if (!show_nonrenewable) {
    table %<>% dplyr::filter(.data$n_renewal != 2)
  }
  if (account != "alle") {
    table %<>% dplyr::filter(.data$account == !!account)
  }
  if (link_id) {
    table %<>% dplyr::mutate(id = as_link(.data$id, .data$link))
  }

  dplyr::select(table, -.data$link)
}


as_link <- function(text, link) {
  paste0("<a href=\"", link, "\" target=\"_blank\">",
         text, "</a>")
}
