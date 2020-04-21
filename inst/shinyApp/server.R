library(magrittr)
library(bibleRe)
library(dplyr)

server <- function(input, output, session) {

  state <- reactiveValues(get_data = 0)

  # read the login data and fill menu for user selection
  users <- bib_read_login_data(getOption("biblere_login_data_file"))
  choices <-
    if (length(users) == 0) {
      "Passwortdatei nicht gefunden!"
    } else {
      c("alle", names(users))
    }
  updateSelectInput(session,
                    "select_account",
                    choices = choices,
                    selected = choices[1])

  # get documents, if state$get_data is incremented
  all_data <- eventReactive(state$get_data, {
    if (length(choices) > 1) {
      message("Getting data for user(s) ",
              paste(names(users), collapse = ", "))
      bib_get_all_data(users)
    } else {
      NULL
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
          input$select_account)
    }
  })

  # table output
  output$table <- DT::renderDT(
    if (is.null(show_table())) {
      NULL
    } else {
      bibleRe:::create_datatable(show_table(), input$select_table)
    })

  # download table
  output$download_documents <- downloadHandler(
    filename = function() {
      paste0("biblere_",
             tolower(bibleRe:::get_table_name(input$select_table)),
             ".xlsx")
    },
    content = function(file) {
      bibleRe:::create_export_table(show_table(),
                                    input$select_table) %>%
      WriteXLS::WriteXLS(file,
                         SheetNames = bibleRe:::get_table_name(input$select_table),
                         AdjWidth = TRUE,
                         BoldHeaderRow = TRUE,
                         FreezeRow = 1)
    }
  )

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
                 icon = icon("search"),
                 onclick = script)
  })

  # renew selected documents
  observeEvent(input$renew, {
    if (input$select_table == "documents") {
      selected <- show_table()[input$table_rows_selected, ]
      # only renew documents that can be renewd (less than 2 renewals)
      # that have not already be renewed today
      renew <- filter(selected, n_renewal < 2, renewal_date != lubridate::today())
      if (nrow(renew) > 0) {
        message("Renewing ", paste(renew$title, collapse = "; "))
        renew_accounts <- unique(renew$account)
        lapply(
          renew_accounts,
          function(acc) {
            chk_ids <- filter(renew, account == acc) %>%
              pull("chk_id")
            bib_login(users[[acc]][["username"]],
                      users[[acc]][["password"]]) %>%
              bib_renew(chk_ids)
        })
        # reload documents
        state$get_data <- state$get_data + 1
      }
    }
  })

  # select all / none
  proxy <- DT::dataTableProxy("table")
  observeEvent(input$select_all, {
      DT::selectRows(proxy, input$table_rows_all)
  })
  observeEvent(input$select_none, {
    DT::selectRows(proxy, NULL)
  })

  # stop app when session ends
  session$onSessionEnded(function() {
        stopApp()
    })

}
