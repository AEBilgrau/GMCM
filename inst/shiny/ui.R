library(shiny)

# Define UI for application
shinyUI(

  navbarPage(
    title = "Gaussian Mixture Copula Models",
    theme = "bootstrap.css",

    # File input tab ----
    tabPanel(
      title = "File input",

      sidebarPanel(
        # File input ----
        fileInput("in_file", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        p("The input file should be a plain text file where columns",
          "correspond to features/variables/experiments and rows correspond to",
          "observations."),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        radioButtons("quote", "Quotes",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),

        # Input: Select rows to show ----
        numericInput("n_rows", "Number of rows to display",
                     value = 20)
      ),

      htmlOutput("file_description"),
      tableOutput("in_file")
    ),


    # General model tab ----
    tabPanel(
      title = "General GMCM",

      # Show a plot of the generated distribution
      titlePanel("General GMCM for unsupervised clustering"),
      hr(),
      sidebarPanel(
        sliderInput(inputId = "bins", label = "No bins", min = 0, max = 50, value = 10)
      ),
      mainPanel(
        plotOutput("distPlot")
      )
    ),

    # Special model tab ----
    tabPanel(
      title = "Special GMCM"
    ),

    navbarMenu(
      title = "",
      icon = icon("bars"),
      #title = "More",
      tabPanel("Links"),
      tabPanel("References")
    )



  )
)
