library(shiny)
library(shinydashboard)
library(DT)
library(GMCM)

# General named list for method choices
method_choices <-
  c("Nelder-Mead" = "NM",
    "Simulated Annealing" = "SANN",
    "limited-memory quasi-Newton method" = "L-BFGS",
    "limited-memory quasi-Newton method with box constraints" = "L-BFGS-B",
    "Pseudo EM algorithm" = "PEM")

# Define UI for application
shinyUI(
  navbarPage(
    title = tagList(icon("box-open"), strong("GMCM")),
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
                   "correspond to features/<wbr/>variables/<wbr/>experiments and",
                   "rows correspond to observations.")),

          # * Input: header checkbox ----
          checkboxInput("header", "Header", TRUE),

          # * Input: select separator ----
          radioButtons(inputId = "sep",
                       label = "Separator",
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
            # box args
            width = 12,
            title = " ",
            status = "primary",
            collapsible = TRUE,
            solidHeader = FALSE,


            # Content
            htmlOutput("input_file_description"),
            DTOutput("in_file_table")
          ),
          uiOutput("model_cols_box"),
          uiOutput("raw_data_box")

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
                            choices = method_choices),
                            #choices = eval(formals(fit.meta.GMCM)$method)),

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
              radioButtons(inputId = "meta_IDR_thres_type",
                           label = "Thresholding by",
                           choices = c("Adjusted IDR" = "IDR",
                                       "Local idr" = "idr"),
                           selected = "IDR",
                           inline = TRUE),
              sliderInput(inputId = "meta_IDR_thres",
                          label = "meta_IDR_thres",
                          min = 0, max = 1, value = 0.05, step = 0.01)
            )
          )


        ),

        # * Output ----
        dashboardBody(
          h1("Special GMCM for reproducibility analysis"),
          br(),
          conditionalPanel("input.meta_fit_push > 0",
            uiOutput("infoBoxes"),
            uiOutput("classificationBoxes"),
            box(
              # Box args
              title = "Observed values plot",
              footer = "The raw, observed values classified by the special GMCM approach.",
              status = "primary",
              width = 4,
              collapsible = TRUE,
              collapsed = FALSE,
              solidHeader = TRUE,

              # Content
              plotOutput("obs_plot")
            ),
            box(
              # Box args
              title = "Rank GMCM plot",
              footer = "Values in the top right corresponds to reproducible values. This plots shows the realisation for the Gaussian Mixture Copula Model.",
              status = "primary",
              width = 4,
              collapsible = TRUE,
              collapsed = FALSE,
              solidHeader = TRUE,

              # Content
              plotOutput("rank_plot")
            ),
            box(
              # Box args
              title = "Latent GMM process plot",
              footer = "The ranked observations mapped back to the latent process given the estimated parameters. I.e. this plots shows the estimated latent Gaussian Mixture Model realisations.",
              status = "primary",
              width = 4,
              collapsible = TRUE,
              collapsed = FALSE,
              solidHeader = TRUE,

              # Content
              plotOutput("latent_plot")
            ),
            uiOutput("ui_selectize_model_cols_xy"),
            box(
              title = "Classified data",
              footer = downloadButton('downloadData', 'Download'),
              status = "info",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              solidHeader = TRUE,

              DTOutput("meta_out_file_table")
            )
          )
          ,verbatimTextOutput("meta_str")

        )
      )
    ),

    # navbar: More ----
    navbarMenu(
      title = "",
      icon = icon("bars"),
      #title = "More",
      tabPanel("About", icon = icon("question"),
         h1("About"),
         p("This shiny app. was developed by Anders Ellern Bilgrau.")
      ),
      tabPanel("Bug reports", icon = icon("bug"),
               tags$embed(src = "https://github.com/AEBilgrau/GMCM/issues"))
    )

  )
)

