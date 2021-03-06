ui <- fluidPage(

  theme = shinythemes::shinytheme("lumen"),

  titlePanel("bibleRe: Zugang zum Web Interface der K\u00f6nizer Bibliotheken"),

  shinyjs::useShinyjs(),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      p(tags$b("Konto auswählen:")),
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
      p(tags$b("Rückgabe bis:")),
      fluidRow(
        column(8,
          shinyWidgets::airDatepickerInput(
            "due_date",
            NULL,
            value = Sys.Date() + getOption("biblere_n_due_days"),
            minDate = Sys.Date(),
            maxDate = Sys.Date() + 60,
            dateFormat = "dd.mm.yyyy",
            language = "de",
            addon = "none",
            autoClose = TRUE,
            firstDay = 1)),
      column(4,
        actionButton("show_all_dates", "alle",
                     class = "btn btn-primary"))
      ),
      shinyWidgets::awesomeCheckbox(
        "non_renewable",
        "nur nicht verlängerbare"
      ),
      shinyWidgets::awesomeRadio(
        "select_table",
        tags$b("Tabelle auswählen"),
        choices = c(Ausleihen = "documents",
                    Reservationen = "orders",
                    Gebühren = "fees")),
      p(
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
      br(),
      actionLink("about",
                 "Über bibleRe",
                 class = "btn text-info"),
      width = 3
    ),

    # main panel #####
    mainPanel(
      DT::DTOutput("table"),
      width = 9
    )
  )
)
