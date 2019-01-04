library(shiny)
library(shinydashboard)
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
      dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(

          # * Input: File ----
          fileInput("in_file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),

          div(HTML("The input file should be a plain text file where columns",
                   "correspond to features/<wbr/>variables/<wbr/>>experiments and",
                   "rows correspond to observations.")),

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
        dashboardBody(
          box(
            width = 12,
            collapsible = TRUE,
            htmlOutput("input_file_description"),
            DTOutput("in_file_table")
          ),
          box(
            plotOutput("raw_data_plot")
          )
        )
      )
    ),



    # navbar: General model ----
    tabPanel(
      title = "General GMCM",
      dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(),
        dashboardBody(
          titlePanel("General GMCM for unsupervised clustering"),
          hr(),
          sidebarPanel(
            sliderInput(inputId = "bins", label = "No bins", min = 0, max = 50, value = 10)
          ),
          mainPanel(
            plotOutput("distPlot")
          )
        )
      )
    ),



    # navbar: Special model ----
    tabPanel(
      title = "Special GMCM",
      dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(
          sidebarMenu(
            type = "tabs",

            # sidebarSearchForm(textId = "searchText",
            #                   buttonId = "searchButton",
            #                   label = "Search..."),

            # * Input: tab Start parameters ----
            menuItem(
              text = "Fit model",
              icon = icon("tachometer"),
              startExpanded = FALSE,

              menuItem(
                text = "Start parameters",
                icon = icon("chevron-circle-right"),
                startExpanded = FALSE,

                # Content
                sliderInput(inputId = "par1",
                            label = "Mixture proportion of spurious signals",
                            min = 0, max = 1, value = 0.7, step = 0.01),

                numericInput(inputId = "par2",
                             label = "Mean value in reproducible component",
                             min = 0, value = 1),

                numericInput(inputId = "par3",
                             label = "Standard deviation in reproducible component",
                             min = 0, value = 1),

                sliderInput(inputId = "par4",
                            label = "Correlation in reproducible component",
                            min = 0, max = 1, value = 0.5, step = 0.01)

              ),

              # * Input: tab Addtional parameters ----
              menuItem(
                text = "Additional fit parameters",
                icon = icon("chevron-circle-right"),

                # Content
                selectInput(inputId = "meta_method",
                            label = "Optimization method",
                            choices = eval(formals(fit.meta.GMCM)$method)),

                numericInput(inputId = "meta_max_ite",
                             label = "Maximum iterations",
                             min = 1,
                             value = formals(fit.meta.GMCM)$max.ite),

                checkboxInput(inputId = "meta_positive_rho",
                              label = "Force positive correlation",
                              value = formals(fit.meta.GMCM)$positive.rho)
              ),

              # * Input: Large vals => reproducible
              checkboxInput(inputId = "meta_large_vals",
                            label = HTML(paste(strong("Important:"),
                                               "Check if larger values in data indicate stronger evidence")),
                            value = TRUE),

              # * Input: Fit model ----
              actionButton(inputId = "meta_fit_push",
                           label = "Fit model",
                           icon = icon("cogs")),
              br()



            ),

            menuItem(
              text = "IDR classification",
              icon = icon("flag-checkered"),

              # Content
              sliderInput(inputId = "meta_IDR_thres",
                          label = "IDR threshold",
                          min = 0, max = 1, value = 0.05, step = 0.01)

            )
          )


        ),

        # * Output ----
        dashboardBody(
          h1("Special GMCM for reproducibility analysis"),
          br(),
          uiOutput("infoBoxes"),
          box(
            # Box args
            title = "Rank plot",
            footer = "Values in the top right corresponds to reproducible values.",
            status = "primary",
            collapsible = TRUE,

            # Content
            plotOutput("rank_plot")
          ),
          box(
            # Box args
            title = "Latent process plot",
            footer = "The ranked observations mapped back to the latent process given the estimated parameters",
            status = "primary",
            collapsible = TRUE,

            # Content
            plotOutput("latent_plot")
          ),
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

