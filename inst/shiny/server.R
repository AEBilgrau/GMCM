library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Reactive value containing the data.frame
  user_data <- reactiveVal()

  # Load input file
  output$in_file <- renderTable({

    # Set required dependency, input$in_file is NULL initially.
    req(input$in_file)

    # Load file
    tryCatch(
      {
        user_data(read.table(input$in_file$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote))
      },
      error = function(e) {
        # Return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(head(user_data(), n = input$n_rows))
  })

  output$file_description <- renderUI({
    # Set required dependency
    req(usr_df <- user_data())

    tagList(
      h1("Dataset loaded"),
      p(sprintf("%i rows and %i columns detected. Showing %i rows below:",
                nrow(usr_df), ncol(usr_df), input$n_rows)),
      hr()
    )
  })




  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
