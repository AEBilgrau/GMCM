library(shiny)
library(DT)

# Define UI for application
shinyUI(

  navbarPage(
    title = "Gaussian Mixture Copula Models",
    theme = "bootstrap.css",

    # navbarMenu: File input tab ----
    tabPanel(
      title = "File input",

      sidebarLayout(
        sidebarPanel(
          # * Input: File ----
          fileInput("in_file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),

          HTML("<p>The input file should be a plain text file where columns",
               "correspond to features/<wbr>variables/<wbr>experiments and",
               "rows correspond to observations.</p>"),

          # * Input: header checkbox ----
          checkboxInput("header", "Header", TRUE),

          # * Input: select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),

          # * Input: select quotes ----
          radioButtons("quote", "Quotes",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"')

        ),

        mainPanel(
          htmlOutput("file_description"),
          DTOutput("in_file_table")
        )
      )
    ),



    # navbarMenu: General model ----
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



    # navbarMenu: Special model ----
    tabPanel(
      title = "Special GMCM"
    ),


    # navbarMenu: More ----
    navbarMenu(
      title = "",
      icon = icon("bars"),
      #title = "More",
      tabPanel("Links"),
      tabPanel("References")
    )



  )
)
