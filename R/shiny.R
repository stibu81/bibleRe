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
      warning("file ", login_data_file, " does not exist.")
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


# Helper function to extract a table from the complete data
prepare_table <- function(data,
                          type = c("documents", "orders", "fees"),
                          due_date = as.Date("2100-01-01"),
                          account = "alle",
                          only_non_renwable = FALSE) {

  type <- match.arg(type)

  # extract the table from the data
  table <- data[[type]]

  # only table of documents is ordered by and filtered for due_date
  # and non-renewable documents
  if (type == "documents") {
    table %<>% dplyr::filter(.data$due_date <= !!due_date) %>%
      dplyr::arrange(.data$due_date) %>%
      dplyr::mutate(due = .data$due_date <= lubridate::today())
    if (only_non_renwable) {
      table %<>% dplyr::filter(.data$n_renewal == 2)
    }
  }

  if (account != "alle") {
    table %<>% dplyr::filter(.data$account == !!account)
  }

  # convert document id to link
  if (type %in%  c("documents", "orders")) {
    table %<>% dplyr::mutate(id = as_link(.data$id, .data$link)) %>%
      dplyr::select(-.data$link)
  }

  table

}


as_link <- function(text, link) {
  paste0("<a href=\"", link, "\" target=\"_blank\">",
         text, "</a>")
}


# create datatable with DT
create_datatable <- function(table,
                             type = c("documents", "orders", "fees"),
                             select_style = c("multi", "os")) {

  type <- match.arg(type)
  select_style <- match.arg(select_style)

  hide_cols <- get_hidden_cols(type)
  col_names <- get_col_names(type)
  date_cols <- stringr::str_subset(names(table), "date")

  data_table <-
    DT::datatable(
      table,
      extensions = c("Select", "Buttons"),
      selection = "none",
      options = list(
        pageLength = 200,
        columnDefs = list(list(
          visible = FALSE,
          targets = which(names(table) %in% hide_cols) - 1)),
        dom = "Bfti",
        language  = list(
          search = "<b>Suche:</b>",
          emptyTable = "Es sind keine Daten verf\u00fcgbar.",
          info = "_TOTAL_ Eintr\u00e4ge",
          infoEmpty = "0 Eintr\u00e4ge",
          infoFiltered = "(gefiltert aus _MAX_ Eintr\u00e4gen)",
          zeroRecords = "keine passenden Eintr\u00e4ge gefunden",
          buttons = list(
            selectAll = "Alle ausw\u00e4hlen",
            selectNone = "Keine ausw\u00e4hlen"
          ),
          select = list(
            rows = list(
              "_" = "%d Eintr\u00e4ge ausgew\u00e4hlt",
              "1" = "1 Eintrag ausgew\u00e4hlt",
              "0" = "")
          )
        ),
        buttons = list(
          list(extend = "selectAll", className = "btn btn-primary"),
          list(extend = "selectNone", className = "btn btn-primary")
        ),
        select = list(style = select_style),
        # in order for the bootstrapping theme to work correctly for the
        # DT-buttons, the class dt-button must be removed.
        # see answer by Stéphane Laurent, https://stackoverflow.com/a/62904879/4303162
        # This line is licenced under
        # CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0/).
        initComplete = DT::JS(
          "function() {",
            "$('.dt-buttons button').removeClass('dt-button');",
            # Modified to also change class of search field.
            "$('div.dataTables_filter input').addClass('form-control');",
          "}")
      ),
      rownames = FALSE,
      colnames = col_names,
      escape = FALSE
    ) %>%
    # use Swiss format for dates
    DT::formatDate(date_cols,
                   method = "toLocaleDateString",
                   params = "de-CH")

  # only for fees: format column amount as currency
  if (type == "fees") {
    data_table %<>% DT::formatCurrency("amount",
                                       currency = " CHF",
                                       mark = "'",
                                       before = FALSE)
  }

  # only for documents: use red text if due date is passed
  if (type == "documents") {
    data_table %<>% DT::formatStyle(
      "due",
      target = "row",
      color = DT::styleEqual(c(FALSE, TRUE), c("black", "red"))
    )
  }

  data_table

}


# helper function to create table for export
create_export_table <- function(table,
                                type = c("documents", "orders", "fees")) {

  type <- match.arg(type)

  hide_cols <- get_hidden_cols(type)
  col_names <- get_col_names(type)
  date_cols <- stringr::str_subset(names(table), "date")

  # if there is a column id, remove the link
  if ("id" %in% names(table)) {
    table %<>% dplyr::mutate(
      id = stringr::str_remove_all(.data$id, "<[^>]*>")
    )
  }

  # convert dates, rename columns
  table %<>%
    dplyr::mutate_at(date_cols,
                     ~format(., format = "%d.%m.%Y")) %>%
    magrittr::set_names(col_names)

  # remove hidden columns
  table <- table[, !names(table) %in% hide_cols]

  table

}


# helper function to get hidden columns and german column names
# for each table type
get_hidden_cols <- function(type) {
  hidden_cols <- list(documents = c("renewal_date", "chk_id", "due"),
                      orders = c(),
                      fees = c())
  hidden_cols[[type]]
}


get_col_names <- function(type) {
  col_names <- list(documents = c("Konto", "Exemplar", "Autor", "Titel",
                                  "F\u00e4lligkeit", "Verl.",
                                  "renewal_date", "chk_id", "due"),
                    orders = c("Konto", "Exemplar", "Autor", "Titel",
                               "Abholart", "Bestelldatum"),
                    fees = c("Konto", "Art der Geb\u00fchr", "Datum",
                             "Betrag", "Exemplar", "Notiz"))
  col_names[[type]]
}

get_table_name <- function(type) {
  table_names <- list(documents = "Ausleihen",
                      orders = "Reservationen",
                      fees = "Geb\u00fchren")
  table_names[[type]]
}


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


create_renewal_dt <- function(data) {

  dplyr::select(data, .data$author, .data$title, .data$due_date) %>%
    DT::datatable(
      rownames = FALSE,
      colnames = c("Autor", "Titel", "F\u00e4lligkeit"),
      selection = "none",
      options = list(
        dom = "t",
        scrollY = "300px",
        scrollCollapse = TRUE,
        paging = FALSE
      )
    ) %>%
    # use Swiss format for dates
    DT::formatDate("due_date",
                   method = "toLocaleDateString",
                   params = "de-CH")

}


# show information about the software
show_about <- function() {

  # get copyright info from LICENSE file
  license <- system.file("LICENSE", package = "bibleRe") %>%
    read.dcf()
  copyright <- paste("\u00a9", license[1, "YEAR"], license[1, "COPYRIGHT HOLDER"])

  shiny::showModal(
    shiny::modalDialog(
      "Einfacher Zugriff auf das Webinterface der",
      shiny::HTML(
        as_link("K\u00f6nizer Bibliotheken", "https://koenizerbibliotheken.ch")),
      ".", shiny::tags$br(), shiny::tags$br(),

      if (can_write_excel()) {
        "Excel-Export ist auf diesem System m\u00f6glich."
      } else {
        shiny::HTML(
          "Excel-Export ist auf diesem System nicht m\u00f6glich. Bitte installiere",
          as_link("perl", "https://www.perl.org/"),
          "und",
          as_link("WriteXLS", "https://cran.r-project.org/web/packages/WriteXLS/"),
          "."
        )
      },

      shiny::tags$br(), shiny::tags$br(),

      as.character(utils::packageVersion("bibleRe")), shiny::tags$br(),
      shiny::HTML(
        as_link("github.com/stibu81/bibleRe", "https://github.com/stibu81/bibleRe")),
      shiny::tags$br(), copyright,
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
      shiny::tags$br(), shiny::tags$br(), msg,
      title = "Lesen der Login-Daten fehlgeschlagen",
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
      shiny::tags$br(), shiny::tags$br(),
      paste(failed_logins, collapse = ", "),
      title = "Login fehlgeschlagen",
      footer = shiny::modalButton("OK") %>%
        shiny::tagAppendAttributes(class = "btn btn-primary"),
      easyClose = TRUE
    )
  )
}


# check that WriteXLS and Perl are installed
can_write_excel <- function() {
  ok <- length(find.package("WriteXLS", quiet = TRUE)) > 0
  if (ok) ok <- WriteXLS::testPerl(verbose = TRUE)
  # there is a bug in testPerl() if verbose = FALSE.
  ok
}
