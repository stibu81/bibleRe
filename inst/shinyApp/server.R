library(magrittr)
library(bibleRe)
library(dplyr)

server <- function(input, output, session) {

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

  # get documents if button is clicked
  if (length(choices) > 1) {
    message("Getting documents for user(s) ",
            paste(names(users), collapse = ", "))

    full_table <- lapply(
          names(users),
          function(user) {
            bib_login(users[[user]][["username"]],
                      users[[user]][["password"]]) %>%
              bib_list_documents()
          }) %>%
        set_names(names(users)) %>%
        bind_rows(.id = "account") %>%
        select(-"renewal_date") %>%
        arrange(.data$due_date)
  }

  # table output
  output$table <- DT::renderDT(
    if (is.null(full_table)) {
      NULL
    } else {
      table <- bibleRe:::filter_document_table(
          full_table,
          input$due_date,
          input$show_renewable,
          input$show_nonrenewable,
          input$select_account) %>%
        mutate(due = due_date <= lubridate::today())
      DT::datatable(
        table,
        options = list(
          lengthMenu = c(10, 20, 50, 100),
          pageLength = 100,
          columnDefs = list(list(visible = FALSE,
                                 targets = which(names(table) == "due") - 1))
        ),
        rownames = FALSE,
        colnames = c("Konto", "Exemplar", "Autor", "Titel",
                     "F\u00e4lligkeit", "Verl.", "due"),
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
          full_table,
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

}
