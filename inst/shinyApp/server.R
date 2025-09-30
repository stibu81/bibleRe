library(bibleRe)
library(dplyr)

server <- function(input, output, session) {

  state <- reactiveValues(get_data = 0,
                          renew = NULL)
  run <- TRUE

  # deactivate download button, if WriteXLS and/or Perl are not available
  if (bib_excel_method() == "none") {
    cli::cli_alert_info("Excel export is not possible on this system.")
    shinyjs::disable("download_documents")
  }

  # read the login data and fill menu for user selection
  users <- bib_read_login_data(getOption("biblere_login_data_file"))
  if (length(users) == 0) {
    bibleRe:::show_login_file_missing(getOption("biblere_login_data_file"), users)
    run <- FALSE
  }

  # check if chromium-browser ist installed
  if (run &&
      inherits(try(bibleRe:::check_chrome(), silent = TRUE), "try-error")) {
    bibleRe:::show_no_chrome()
    run <- FALSE
  }

  # check connection
  if (run && !bib_check()) {
    bibleRe:::show_no_connection()
    run <- FALSE
  }


  # get documents, if state$get_data is incremented
  all_data <- eventReactive(state$get_data, {
    if (run) {
      data <- tryCatch(
        bib_get_all_data(users, with_progress = TRUE),
        error = function(e) e
      )

      if (inherits(data, "error")) {
        bibleRe:::show_unknown_error(data)
        return(NULL)
      }

      # check success of login. Warn in case of failure and remove the users from
      # the list
      if (!all(data$login)) {
        failed_logins <- names(data$login)[!data$login]
        cli::cli_warn(c("x" = "Login failed for users {.val {failed_logins}}."))
        bibleRe:::show_failed_logins(failed_logins)
        users <<- users[data$login]
      }

      counts <- c(nrow(data$documents),
                  vapply(names(users),
                         function(u) sum(data$documents$account == u),
                         integer(1))
                  )
      choices <- c("alle", names(users))
      names(choices) <- paste0(choices, " (", counts, ")")
      updateSelectInput(session,
                        "select_account",
                        choices = choices,
                        selected = choices[1])

      # if this is a reload after a renewal, check that renewal
      # was successful by checking that all the renewed documents
      # have renewal date today.
      if (!is.null(state$renew)) {
        bibleRe:::warn_failed_renewal(data$documents, state$renew)
        state$renew <- NULL
      }

      # if ALL logins failed, data does not have the correct format, which
      # leads to problems in later code => return NULL in that situation
      if (all(!data$login)) {
        NULL
      } else {
        bibleRe:::prepare_date_input(data, session, input$select_account)
        data
      }
    }
  })

  show_table <- reactive({
    if (is.null(all_data())) {
      NULL
    } else {
      bibleRe:::prepare_table(
          all_data(),
          input$select_table,
          input$due_date,
          input$select_account,
          input$non_renewable)
    }
  })

  # update the highlighted dates if the account is changed
  observeEvent(input$select_account, {
    if (!is.null(all_data())) {
      bibleRe:::prepare_date_input(all_data(), session, input$select_account)
    }
  })

  # table output
  output$table <- DT::renderDT(
    if (is.null(show_table())) {
      NULL
    } else {
      bibleRe:::create_datatable(show_table(), input$select_table)
    }, server = FALSE)

  # download table
  output$download_documents <- downloadHandler(
    filename = function() {
      paste0("biblere_",
             tolower(bibleRe:::get_table_name(input$select_table)),
             ".xlsx")
    },
    content = function(file) {
      bib_write_excel(show_table(), file, input$select_table)
    }
  )

  # show all documents
  observeEvent(input$show_all_dates, {
    bibleRe:::prepare_date_input(all_data(), session,
                                 input$select_account,
                                 set_max_date = TRUE)
  })

  # create search button
  # this complicated way of doing this was the only solution
  # I found that will not result in the pop-up blocker preventing
  # the tab from opening.
  output$search_button <- renderUI({
    if (stringr::str_trim(input$search) == "") {
      script <- NULL
    } else {
      search_url <- bib_search(input$search)
      script <- paste0("window.open('", search_url,
                       "', '_blank', ",
                       "'noopener,noreferrer')")
    }
    actionButton("start_search",
                 "Suche",
                 icon = icon("magnifying-glass"),
                 class = "btn btn-primary",
                 onclick = script)
  })

  # renew selected documents
  observeEvent(input$renew, {
    if (input$select_table == "documents") {
      selected <- show_table()[input$table_rows_selected, ]
      # only renew documents that can be renewed (less than 2 renewals)
      # that have not already be renewed today
      state$renew <- filter(
        selected,
        n_renewal < 2,
        is.na(renewal_date) | renewal_date != lubridate::today())
      bibleRe:::renewal_dialog(state$renew,
                               n_selected = nrow(selected))
    }
  })

  # renew if OK button is pressed in dialog
  observeEvent(input$confirmRenew, {
    removeModal()
    cli::cli_alert_info("Renewing {.val {state$renew$title}}")
    renew_accounts <- unique(state$renew$account)
    lapply(
      renew_accounts,
      function(acc) {
        chk_ids <- filter(state$renew, account == acc) %>%
          pull("chk_id")
        bib_login(users[[acc]][["username"]],
                  users[[acc]][["password"]],
                  displayname = acc) %>%
          bib_renew(chk_ids)
      })
    # reload documents
    state$get_data <- state$get_data + 1
  })

  # reload button
  observeEvent(input$reload, {
    state$get_data <- state$get_data + 1
  })

  # about bibleRe
  observeEvent(input$about, bibleRe:::show_about())

  # stop app when session ends
  session$onSessionEnded(function() {
        stopApp()
    })

}
