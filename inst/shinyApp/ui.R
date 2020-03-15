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
      DT::DTOutput("table"),
      width = 10
    )
  )
)
