library(magrittr)
library(bibleRe)
library(DT)

server <- function(input, output, session) {

  state <- reactiveValues(table = NULL)

  # read the login data and fill menu for user selection
  users <- bib_read_login_data("~/.biblere_passwords")
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
  observeEvent(
    input$get_documents,
    {
      get_users <-
        if (input$select_account == "alle") {
          names(users)
        } else {
          input$select_account
        }
      message("Getting documents for user(s) ",
              paste(get_users, collapse = ", "))

      state$table <- lapply(
          get_users,
          function(user) {
            bib_login(users[[user]][["username"]],
                      users[[user]][["password"]]) %>%
              bib_list_documents()
          }) %>%
        set_names(get_users) %>%
        dplyr::bind_rows(.id = "account") %>%
        dplyr::select(-"renewal_date") %>%
        dplyr::arrange(.data$due_date) %>%
        dplyr::select(Konto = "account",
                      Autor = "author",
                      Titel = "title",
                      "F\u00e4lligkeit" = "due_date",
                      "Verl." = "n_renewal")
    }
  )

  output$table <- renderDT(
    state$table,
    options = list(lengthMenu = c(10, 20, 50, 100)),
    rownames = FALSE
  )

}
