#' Get All Data for a List of Users
#'
#' Get all the available data (documents, orders, fees) for
#' a list of users.
#'
#' @param users a list of users with username and password.
#'  Such a list can be created from a JSON file using
#'  \code{\link{bib_read_login_data}}.
#' @param with_progress logical indicating whether a progress indicator
#'  should be shown, if the function is called from a shiny app.
#'
#' @export

bib_get_all_data <- function(users, with_progress = FALSE) {

  check_chrome()

  logger::log_debug("downloading all data for users {.val {names(users)}}")

  # if running from a shiny app and requested,
  # show progress indicator
  all_data <-
    if (with_progress && shiny::isRunning()) {
      shiny::withProgress({
        lapply(seq_along(users), function(i) {
          base_msg <- glue("{names(users)[i]} ({i}/{length(users)})")
          shiny::incProgress(0.1, detail = glue("{base_msg}: Einloggen ..."))
          session <- bib_login(users[[i]], displayname = names(users)[i])
          shiny::incProgress(0.5, detail = glue("{base_msg}: Herunterladen ..."))
          userdata <- get_all_data(session)
          shiny::incProgress(0.4, detail = glue("{base_msg}: Fertig!"))
          userdata
        })
      },
      message = "Daten herunterladen",
      max = length(users),
      value = 0)
    } else {
      lapply(users, \(user) get_all_data(bib_login(user)))
    }
  names(all_data) <- names(users)

  logger::log_debug("download of data for users {.val {names(users)}} completed.")

  login_successfull <- !vapply(all_data, is.null, logical(1))
  if (any(login_successfull)) {
    logger::log_debug(
      "login successfull for users {.val {names(users)[login_successfull]}}"
    )
  }
  if (any(!login_successfull)) {
    logger::log_debug(
      "login failed for users {.val {names(users)[!login_successfull]}}"
    )
  }

  list(documents = bind_bib_data(all_data, "documents"),
       orders = bind_bib_data(all_data, "orders"),
       fees = bind_bib_data(all_data, "fees"),
       watchlist = bind_bib_data(all_data, "watchlist"),
       login = login_successfull)
}


# helper function to get all the data for a single user
get_all_data <- function(session) {
  if (is.null(session)) return(NULL)
  out <- list(documents = bib_list_documents(session),
              orders = bib_list_orders(session),
              fees = bib_list_fees(session),
              watchlist = bib_list_watchlist(session))
  session$session$close()
  out
}


# helper function to extract and combine bib data
bind_bib_data <- function(data, type) {
  accounts <- names(data)
  lapply(data, getElement, type) %>%
    dplyr::bind_rows(.id = "account")
}
