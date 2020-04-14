library(magrittr)
library(bibleRe)
library(dplyr)

server <- function(input, output, session) {

  state <- reactiveValues(get_docs = 0)

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

  # get documents, if state$get_docs is incremented
  full_table <- eventReactive(state$get_docs, {
    if (length(choices) > 1) {
      message("Getting documents for user(s) ",
              paste(names(users), collapse = ", "))
    lapply(names(users),
            function(user) {
              bib_login(users[[user]][["username"]],
                        users[[user]][["password"]]) %>%
                bib_list_documents()
            }) %>%
          set_names(names(users)) %>%
          bind_rows(.id = "account") %>%
          arrange(.data$due_date)
    } else {
      NULL
    }
  })

  filtered_table <- reactive({
    if (is.null(full_table())) {
      NULL
    } else {
      bibleRe:::filter_document_table(
          full_table(),
          input$due_date,
          input$show_renewable,
          input$show_nonrenewable,
          input$select_account) %>%
        mutate(due = due_date <= lubridate::today())
    }
  })

  # table output
  output$table <- DT::renderDT(
    if (is.null(filtered_table())) {
      NULL
    } else {
      hide_cols <- c("renewal_date", "chk_id", "due")
      DT::datatable(
        filtered_table(),
        options = list(
          lengthMenu = c(10, 20, 50, 100),
          pageLength = 100,
          columnDefs = list(list(
            visible = FALSE,
            targets = which(names(filtered_table()) %in% hide_cols) - 1))
        ),
        rownames = FALSE,
        colnames = c("Konto", "Exemplar", "Autor", "Titel",
                     "F\u00e4lligkeit", "Verl.",
                     "renewal_date", "chk_id", "due"),
        escape = FALSE
      ) %>%
      # use Swiss format for dates
      DT::formatDate("due_date",
                     method = "toLocaleDateString",
                     params = "de-CH") %>%
      # use red text if due date is passed
      DT::formatStyle(
        "due",
        target = "row",
        color = DT::styleEqual(c(FALSE, TRUE), c("black", "red"))
      )
    })

  # download table
  output$download_documents <- downloadHandler(
    filename = "biblere_ausleihen.xlsx",
    content = function(file) {
      table <- bibleRe:::filter_document_table(
          full_table(),
          input$due_date,
          input$show_renewable,
          input$show_nonrenewable,
          input$select_account,
          link_id = FALSE) %>%
        mutate(due_date = format(due_date, format = "%d.%m.%Y")) %>%
        set_names(c("Konto", "Exemplar", "Autor", "Titel",
                     "F\u00e4lligkeit", "Verl."))
      WriteXLS::WriteXLS(table, file,
                         SheetNames = "Ausleihen",
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
    selected <- filtered_table()[input$table_rows_selected, ]
    # only renew documents that can be renewd (less than 2 renewals)
    # that have not already be renewed today
    renew <- filter(selected, n_renewal < 2, renewal_date != lubridate::today())
    if (nrow(renew) > 0) {
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
      state$get_docs <- state$get_docs + 1
    }
  })

}
