library(magrittr)
library(bibleRe)
library(dplyr)

server <- function(input, output, session) {

  state <- reactiveValues(table = NULL)

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

    state$table <- lapply(
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

  output$table <- DT::renderDT(
    if (is.null(state$table)) {
      NULL
    } else {
      table <- filter(state$table, .data$due_date <= input$due_date)
      if (!input$show_renewable) {
        table %<>% filter(.data$n_renewal == 2)
      }
      if (!input$show_nonrenewable) {
        table %<>% filter(.data$n_renewal != 2)
      }
      if (input$select_account != "alle") {
        table %<>% filter(.data$account == input$select_account)
      }
      table %<>% mutate(id = as_document_link(id))
      DT::datatable(
        table,
        options = list(lengthMenu = c(10, 20, 50, 100),
                   pageLength = 100),
        rownames = FALSE,
        colnames = c("Konto", "Exemplar", "Autor", "Titel",
                     "F\u00e4lligkeit", "Verl."),
        escape = FALSE
      ) %>%
      DT::formatDate("due_date",
                     method = "toLocaleDateString",
                     params = "de-CH")
    },

  )

}
