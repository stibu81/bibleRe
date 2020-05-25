ui <- fluidPage(

  titlePanel("bibleRe: Zugang zum Webinterface der K\u00f6nizer Bibliotheken"),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      selectInput(
        "select_account",
        "Konto auswählen:",
        "laden ..."),
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
        actionButton("show_all_dates", "alle"))
      ),
      radioButtons("select_table",
                   "Tabelle auswählen",
                   choices = c(Ausleihen = "documents",
                               Reservationen = "orders",
                               Gebühren = "fees")),
      tags$p(
        downloadButton("download_documents",
                       "Tabelle speichern")
      ),
      tags$p(
        actionButton("renew", "Verlängern",
                     icon = icon("redo"))
      ),
      textInput("search",
                "Katalogsuche:"),
      uiOutput("search_button"),
      width = 3
    ),

    # main panel #####
    mainPanel(
      tags$p(actionButton("select_all", "Alle auswählen"),
             actionButton("select_none", "Keine auswählen")),
      DT::DTOutput("table"),
      width = 9
    )
  )
)
