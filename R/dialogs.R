# Show a dialog for confirmation of renewal
# renew is the table of documents to be renewed
renewal_dialog <- function(renew, n_selected) {

  n <- nrow(renew)

  # dialog if there are documents
  if (n > 0) {

    # if n_selected is given and some documents are not renewable,
    # add a message.
    text1 <- if (!is.null(n_selected) && n_selected != n) {
      shiny::tags$p(
        paste(n_selected - n,
              "der ausgew\u00e4hlten Dokumente k\u00f6nnen",
              "nicht verl\u00e4ngert werden.")
      )
    }
    text2 <- if (n == 1) {
      "Soll folgendes Dokument verl\u00e4ngert werden?"
    } else {
      paste("Sollen folgende", nrow(renew), "Dokumente verl\u00e4ngert werden?")
    }

    dialog <- shiny::modalDialog(
      text1, text2, DT::renderDT(create_renewal_dt(renew)),
      title = "Verl\u00e4ngerung",
      footer = shiny::tagList(
        shiny::actionButton("confirmRenew", "OK",
                            class = "btn btn-primary"),
        shiny::modalButton("Abbrechen") %>%
          shiny::tagAppendAttributes(class = "btn btn-primary")),
      size = "l",
      easyClose = TRUE
    )

  # dialog if there are now documents to renew
  } else {
    dialog <- shiny::modalDialog(
      paste("Es wurden keine verl\u00e4ngerbaren Dokumente ausgew\u00e4hlt."),
      title = "Verl\u00e4ngerung",
      footer = shiny::modalButton("OK") %>%
        shiny::tagAppendAttributes(class = "btn btn-primary"),
      easyClose = TRUE
    )
  }

  shiny::showModal(dialog)
}


# dialog to indicate that renewal failed
warn_failed_renewal <- function(documents, renew) {

  # renewal failed, if renewal date is NA (i.e., the document has
  # never been renewed) or any other date than today
  failed <- dplyr::filter(documents,
                          .data$chk_id %in% renew$chk_id,
                          .data$renewal_date != lubridate::today() |
                            is.na(.data$renewal_date))

  if (nrow(failed) > 0) {

    dialog <- shiny::modalDialog(
      "Folgende Dokumente konnten nicht verl\u00e4ngert werden:",
      DT::renderDT(create_renewal_dt(failed)),
      title = "Verl\u00e4ngerung",
      footer = shiny::modalButton("OK") %>%
          shiny::tagAppendAttributes(class = "btn btn-primary"),
      size = "l",
      easyClose = TRUE
    )
    shiny::showModal(dialog)
  }

}


# show information about the software
show_about <- function() {

  # get copyright info from LICENSE file
  license <- system.file("LICENSE", package = "bibleRe") %>%
    read.dcf()
  copyright <- paste("\u00a9", license[1, "YEAR"], license[1, "COPYRIGHT HOLDER"])

  excel_method <- bib_excel_method()

  shiny::showModal(
    shiny::modalDialog(
      "Einfacher Zugriff auf das Web Interface der",
      shiny::HTML(
        paste0(
          as_link("K\u00f6nizer Bibliotheken", "https://koenizerbibliotheken.ch"),
          ".")
      ),
      shiny::br(), shiny::br(),

      if (excel_method != "none") {
        paste("Excel-Export ist auf diesem System m\u00f6glich.",
              "Verwendetes Package: ", excel_method)
      } else {
        shiny::HTML(
          "Excel-Export ist auf diesem System nicht m\u00f6glich.<br>",
          "Verwende <tt>bib_setup_excel_export()</tt>, um die n\u00f6tigen",
          "R packages zu installieren",
          paste0(
            "(",
            as_link("writexl", "https://cran.r-project.org/web/packages/writexl/"),
            " und ",
            as_link("WriteXLS", "https://cran.r-project.org/web/packages/WriteXLS/"),
            ").<br>"
          ),
          "WriteXLS ben\u00f6tigt eine funktionierende Installation von",
          paste0(as_link("perl", "https://www.perl.org/"), ".")
        )
      },

      shiny::br(), shiny::br(),

      as.character(utils::packageVersion("bibleRe")), shiny::br(),
      shiny::HTML(
        as_link("github.com/stibu81/bibleRe", "https://github.com/stibu81/bibleRe")),
      shiny::br(), copyright,
      title = "\u00dcber bibleRe",
      footer = shiny::modalButton("OK") %>%
        shiny::tagAppendAttributes(class = "btn btn-primary"),
      easyClose = TRUE
    )
  )
}


# message if reading login data failed
show_login_file_missing <- function(file, users) {

  # create message from attribute of users
  error_type <- attr(users, "error_type")
  msg <-
    if (error_type == "file-not-exist") {
      "Die Datei existiert nicht."
    } else if (error_type == "file-not-valid") {
      "Die Datei ist keine g\u00fcltige JSON-Datei."
    } else {
      "Unbekannter Fehler."
    }

  shiny::showModal(
    shiny::modalDialog(
      "Das Einlesen der Datei", file, "ist fehlgeschlagen.",
      shiny::br(), shiny::br(), msg,
      title = "Lesen der Login-Daten fehlgeschlagen",
      footer = shiny::modalButton("OK") %>%
        shiny::tagAppendAttributes(class = "btn btn-primary"),
      easyClose = TRUE
    )
  )
}


# message if no connection to the server
show_no_connection <- function() {

  shiny::showModal(
    shiny::modalDialog(
      "Keine Verbindung zum Server. Bitte \u00fcberpr\u00fcfe die Internetverbindung.",
      title = "Keine Verbindung",
      footer = shiny::modalButton("OK") %>%
        shiny::tagAppendAttributes(class = "btn btn-primary"),
      easyClose = TRUE
    )
  )
}


# message if login failed for some users
show_failed_logins <- function(failed_logins) {

  shiny::showModal(
    shiny::modalDialog(
      "Der Login ist f\u00fcr folgende Benutzer fehlgeschlagen:",
      shiny::br(), shiny::br(),
      paste(failed_logins, collapse = ", "),
      title = "Login fehlgeschlagen",
      footer = shiny::modalButton("OK") %>%
        shiny::tagAppendAttributes(class = "btn btn-primary"),
      easyClose = TRUE
    )
  )
}
