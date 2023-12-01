title <- "bibleRe: Zugang zum Web Interface der K\u00f6nizer Bibliotheken"

ui <- fluidPage(

  theme = bslib::bs_theme(version = 5, preset = "lumen", font_scale = 0.8),

  # bs version 5 adds no padding on top of title => add some padding here
  titlePanel(div(title,  style={'padding-top: 20px'}),
             windowTitle = title),

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
                       icon = icon("arrow-rotate-right"),
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
            dateFormat = "dd.MM.yyyy",
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
          individual = TRUE,
          choices = c(
            "<i class='fa fa-book'></i> &nbsp;Ausleihen" = "documents",
            "<i class='fa fa-calendar-check'></i> &nbsp;Reservationen" = "orders",
            "<i class='fa fa-coins'></i> &nbsp;Gebühren" = "fees",
            "<i class='fa fa-list'></i> &nbsp;Merkliste" = "watchlist")
        )
      } else {
        shinyWidgets::awesomeRadio(
          "select_table",
          tags$b("Tabelle auswählen"),
          status = "primary",
          choices = c(Ausleihen = "documents",
                      Reservationen = "orders",
                      Gebühren = "fees",
                      Merkliste = "watchlist")
        )
      },
      p(
        actionButton("renew", "Verlängern",
                     icon = icon("arrow-rotate-right"),
                     class = "btn btn-primary"),
        downloadButton("download_documents",
                       "Speichern",
                       class = "btn btn-primary")
      ),
      textInput("search",
                tags$b("Katalogsuche:")),
      uiOutput("search_button"),
      br(),
      fluidRow(
        column(10,
          actionLink("about",
                     "Über bibleRe",
                     class = "btn text-info")
        ),
        column(2, bslib::input_dark_mode(mode = getOption("biblere_colour_mode")))
      ),
      width = 3
    ),

    # main panel #####
    mainPanel(
      DT::DTOutput("table"),
      width = 9
    )
  )
)
