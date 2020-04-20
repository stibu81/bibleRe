ui <- fluidPage(

  titlePanel("bibleRe: Zugang zum Webinterface der K\u00f6nizer Bibliotheken"),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      selectInput(
        "select_account",
        "Konto auswählen:",
        "laden ..."),
      dateInput("due_date",
                "Rückgabe bis:",
                value = Sys.Date() + getOption("biblere_n_due_days"),
                min = Sys.Date(),
                max = Sys.Date() + 60,
                format = "dd.mm.yyyy",
                language = "de",
                weekstart = 1),
      radioButtons("select_table",
                   "Tabelle auswählen",
                   choices = c(Ausleihen = "documents",
                               Reservationen = "orders",
                               Gebühren = "fees")),
      downloadButton("download_documents",
                     "Tabelle speichern"),
      br(), br(),
      actionButton("renew", "Verlängern",
                   icon = icon("redo")),
      br(), br(),
      textInput("search",
                "Katalogsuche:"),
      uiOutput("search_button"),
      width = 3
    ),

    # main panel #####
    mainPanel(
      div(actionButton("select_all", "Alle auswählen"),
          actionButton("select_none", "Keine auswählen")),
      br(), br(),
      DT::DTOutput("table"),
      width = 9
    )
  )
)
