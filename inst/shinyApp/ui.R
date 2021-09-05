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
      # use switch or checkbox
      if (getOption("biblere_use_switches")) {
        p(
          shinyWidgets::switchInput(
            "non_renewable",
            onLabel = "",
            offLabel = "",
            onStatus = "primary",
            size = "mini",
            inline = TRUE,
            labelWidth = 18,
            handleWidth = 14
          ),
          HTML("&nbsp;&nbsp;nur nicht verlängerbare")
        )
      } else {
        shinyWidgets::awesomeCheckbox(
          "non_renewable",
          "nur nicht verlängerbare"
        )
      },
      if (getOption("biblere_use_switches")) {
        shinyWidgets::radioGroupButtons(
          "select_table",
          tags$b("Tabelle auswählen"),
          status = "primary",
          direction = "vertical",
          size = "sm",
          choices = c(
            "<i class='fa fa-book'></i> Ausleihen" = "documents",
            "<i class='fa fa-calendar-check'></i> Reservationen" = "orders",
            "<i class='fa fa-coins'></i> Gebühren" = "fees")
        )
      } else {
        shinyWidgets::awesomeRadio(
          "select_table",
          tags$b("Tabelle auswählen"),
          status = "primary",
          choices = c(Ausleihen = "documents",
                      Reservationen = "orders",
                      Gebühren = "fees")
        )
      },
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
