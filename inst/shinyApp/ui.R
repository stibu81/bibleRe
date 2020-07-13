ui <- fluidPage(

  theme = shinythemes::shinytheme("lumen"),

  titlePanel("bibleRe: Zugang zum Webinterface der K\u00f6nizer Bibliotheken"),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      tags$p(tags$b("Konto auswählen:")),
      fluidRow(
        column(8,
          selectInput(
            "select_account",
            label = NULL,
            "laden ...",
            selectize = FALSE)
          ),
        column(4,
          actionButton("reload",
                       label = NULL,
                       icon = icon("redo"),
                       class = "btn btn-primary"))
      ),
      tags$p(tags$b("Rückgabe bis:")),
      fluidRow(
        column(8,
          dateInput(
            "due_date",
            NULL,
            value = Sys.Date() + getOption("biblere_n_due_days"),
            min = Sys.Date(),
            max = Sys.Date() + 60,
            format = "dd.mm.yyyy",
            language = "de",
            weekstart = 1)),
      column(4,
        actionButton("show_all_dates", "alle",
                     class = "btn btn-primary"))
      ),
      shinyWidgets::awesomeRadio(
        "select_table",
        tags$b("Tabelle auswählen"),
        choices = c(Ausleihen = "documents",
                    Reservationen = "orders",
                    Gebühren = "fees")),
      tags$p(
        actionButton("renew", "Verlängern",
                     icon = icon("redo"),
                     class = "btn btn-primary"),
        downloadButton("download_documents",
                       "Speichern",
                       class = "btn btn-primary")
      ),
      textInput("search",
                tags$b("Katalogsuche:")),
      uiOutput("search_button"),
      width = 3
    ),

    # main panel #####
    mainPanel(
      DT::DTOutput("table"),
      width = 9
    )
  )
)
