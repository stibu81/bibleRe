library(DT)

ui <- fluidPage(

  titlePanel("bibleRe: Zugang zum Webinterface der K\u00f6nizer Bibliotheken"),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      selectInput(
        "select_account",
        "Konto auswählen:",
        ""),
      actionButton("get_documents",
                   "Ausleihen anzeigen"),
      br(), br(),
      dateInput("due_date",
                "Rückgabe bis:",
                value = Sys.Date() + 7,
                min = Sys.Date(),
                max = Sys.Date() + 60,
                format = "dd.mm.yyyy",
                language = "de",
                weekstart = 1),
      checkboxInput("show_renewable",
                    "Verlängerbare anzeigen",
                    value = TRUE),
      checkboxInput("show_nonrenewable",
                    "Nicht-verlängerbare anzeigen",
                    value = TRUE),
      width = 2
    ),

    # main panel #####
    mainPanel(
      DTOutput("table"),
      width = 10
    )
  )
)
