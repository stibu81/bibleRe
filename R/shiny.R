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
      table %<>% dplyr::filter(.data$n_renewal >= 2)
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


# date input: set maximum date and highlight all dates where loans run out
prepare_date_input <- function(data, session, account, set_max_date = FALSE) {
  due_dates <- prepare_table(
      data,
      type = "documents",
      account = account
    ) %>%
    dplyr::pull("due_date") %>%
    unique()
  # determine the maximum date always over ALL accounts
  max_date <- max(c(data$documents$due_date, Sys.Date()))
  shinyWidgets::updateAirDateInput(
    session,
    "due_date",
    value = if (set_max_date) max_date,
    options = list(highlightedDates = due_dates,
                   maxDate = max_date)
  )
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
