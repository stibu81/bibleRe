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
      width = 2
    ),

    # main panel #####
    mainPanel(
      DTOutput("table"),
      width = 10
    )
  )
)
