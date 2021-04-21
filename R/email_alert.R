#' Send Email Alert if Rental Runs Out For Some Documents
#'
#' Send an email alert if the rental of some documents runs out
#' within a given number of days.
#'
#' @inheritParams bib_get_all_data
#' @param n_days numeric. Rentals that run out within `n_days` days from
#'  today will cause an alert message to be sent.
#' @param recipients e-mail addresses of one or several recipients.
#' @param host DNS name or IP address of the SMTP server.
#' @param username,password username and password for the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param verbose should verbose output be produced from the interaction
#'  with the SMTP server?
#'
#' @return
#' `TRUE` if the function was successful, i.e., if either an alert was sent
#' successfully or no alert was needed because no rentals are running out within
#' the given time frame. If any error occured (e.g., during login or when
#' sending the email), `FALSE` is returned.
#'
#' @export

bib_email_alert <- function(users, n_days, recipients,
                            host, username, password,
                            port = 465,
                            verbose = FALSE) {

  rlang::check_installed("emayili")

  docs <- bib_get_all_data(users)$documents

  # if no documents could be retrieved, abort and return FALSe
  if (nrow(docs) == 0) {
    warning("Could not retrieve any data.")
    return(FALSE)
  }

  # do any documents need to be returned during the next n days?
  max_date <- lubridate::today() + lubridate::days(floor(n_days))

  relevant_docs <- docs %>%
    dplyr::filter(.data$due_date <= max_date) %>%
      dplyr::arrange(.data$due_date) %>%
      dplyr::mutate(author = stringr::str_trunc(.data$author, 18),
                    title = stringr::str_trunc(.data$title, 32)) %>%
      dplyr::select(Konto = "account",
                    Autor = "author",
                    Titel = "title",
                    "F\u00e4lligkeit" = "due_date",
                    "Verl." = "n_renewal")

  message(nrow(relevant_docs), " document(s) must be returned in the next ",
                  n_days, " days")

  if (nrow(relevant_docs) > 0) {

    message("Sending email ...")

    subj <- paste("bibleRe-Alert: ",
                  nrow(relevant_docs),
                  "Dokument(e), m\u00fcssen in den n\u00e4chsten",
                  n_days, "Tagen retourniert werden")

    body <- paste0(
      "Nicht verl\u00e4ngerbare Dokumente ====================\n",
      if (sum(relevant_docs$Verl. == 2) == 0) {
        "keine\n\n"
      } else {
        dplyr::filter(relevant_docs, .data$Verl. >= 2) %>%
          tibble_output()
    },
    "Verl\u00e4ngerbare Dokumente ========================\n",
    if (sum(relevant_docs$Verl. < 2) == 0) {
      "keine\n\n"
    } else {
      dplyr::filter(relevant_docs, .data$Verl. < 2) %>%
        tibble_output()
    })

    # prepare message
    email <- emayili::envelope(
      to = recipients,
      subject = subj,
      text = body
    )

    # send message
    smtp <- emayili::server(
      host = host,
      port = port,
      username = username,
      password = password
    )
    success <- try(smtp(email, verbose = verbose))

    if (inherits(success, "try-error")) {
      warning("Sending email failed.")
      return(FALSE)
    }
  } else {
    message("No rentals are running out => no email was sent")
  }

  return(TRUE)
}

# convert a tibble to a character suitable for output
tibble_output <- function(x) {
  as.data.frame(x) %>%
    print() %>%
    utils::capture.output() %>%
    paste(collapse = "\n") %>%
    paste0("\n\n")
}
