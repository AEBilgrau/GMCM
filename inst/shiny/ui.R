library(shiny)
library(DT)
library(GMCM)

# Define UI for application
shinyUI(

  navbarPage(
    title = strong("GMCM"),
    windowTitle = "GMCM",
    theme = "bootstrap.css",

    # navbar: File input tab ----
    tabPanel(
      title = "File input",

      sidebarLayout(
        sidebarPanel(
          width = 3,

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
                       choices = c("Comma" = ",",
                                   "Semicolon" = ";",
                                   "Tabs" = "\t"),
                       selected = ";"),

          # * Input: select quotes ----
          radioButtons("quote", "Quotes",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"')

        ),

        # * Output ----
        mainPanel(
          htmlOutput("file_description"),
          DTOutput("in_file_table"),
          column(9, align = "center",
            plotOutput("raw_data_plot")
          )
        )
      )
    ),



    # navbar: General model ----
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



    # navbar: Special model ----
    tabPanel(
      title = "Special GMCM",

      sidebarLayout(
        sidebarPanel(
          width = 3,

          tabsetPanel(
            type = "tabs",

            # * Input: tab Start parameters ----
            tabPanel(
              title = "Start parameters",
              tags$br(),
              sliderInput(inputId = "par1",
                          label = "Mixture proportion of spurious signals",
                          min = 0, max = 1, value = 0.7, step = 0.01),

              numericInput(inputId = "par2",
                           label = "Mean value",
                           min = 0, value = 1),

              numericInput(inputId = "par3",
                           label = "Standard deviation",
                           min = 0, value = 1),

              sliderInput(inputId = "par4",
                          label = "Correlation",
                          min = 0, max = 1, value = 0.5, step = 0.01)

            ),

            # * Input: tab Addtional parameters ----
            tabPanel(
              title = "Addtional fitting parameters",

              tags$br(),
              selectInput(inputId = "meta_method",
                          label = "Optimization method",
                          choices = eval(formals(fit.meta.GMCM)$method)),

              checkboxInput(inputId = "meta_positive_rho",
                            label = "Enforce positive correlation (rho)",
                            value = formals(fit.meta.GMCM)$positive.rho),

              numericInput(inputId = "meta_max_ite",
                           label = "Maximum iterations",
                           min = 1,
                           value = formals(fit.meta.GMCM)$max.ite)

            )
          ),

          # * Input: Large vals => reproducible
          tags$hr(),
          strong("Important!"),
          checkboxInput(inputId = "meta_large_vals",
                        label = "Larger values in data indicate stronger evidence",
                        value = TRUE),

          # * Input: Fit model ----
          tags$hr(),
          actionButton(inputId = "fit_meta_push",
                       label = "Fit model",
                       icon = icon("cog"))

        ),

        # * Output ----
        mainPanel(
          h1("Special GMCM for reproducibility analysis"),
          verbatimTextOutput("meta_str")

        )
      )
    ),

    # navbar: More ----
    navbarMenu(
      title = "",
      icon = icon("bars"),
      #title = "More",
      tabPanel("Links"),
      tabPanel("References")
    )



  )
)

