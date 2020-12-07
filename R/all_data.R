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

  # if running from a shiny app and requested,
  # show progress indicator
  all_data <-
    if (with_progress && shiny::isRunning()) {
      shiny::withProgress({
        lapply(seq_along(users), function(i) {
          name <- names(users)[i]
          msg <- paste0(name, " (", i, "/", length(users), ")")
          shiny::incProgress(0.1, detail = msg)
          userdata <- get_user_data(users[[i]])
          shiny::incProgress(0.9, detail = "OK")
          userdata
        })
      },
      message = "Userdaten herunterladen",
      max = length(users))
    } else {
      lapply(users, get_user_data)
    } %>%
    magrittr::set_names(names(users))

  list(documents = bind_bib_data(all_data, "documents"),
       orders = bind_bib_data(all_data, "orders"),
       fees = bind_bib_data(all_data, "fees"))
}


# helper function to get all the data for a single user
get_user_data <- function(user) {

  session <- bib_login(user$username, user$password)

  list(documents = bib_list_documents(session),
       orders = bib_list_orders(session),
       fees = bib_list_fees(session))
}


# helper function to extract and combine bib data
bind_bib_data <- function(data, type) {

  accounts <- names(data)
  lapply(data, getElement, type) %>%
    dplyr::bind_rows(.id = "account")

}
