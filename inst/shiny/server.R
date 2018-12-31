library(shiny)
library(DT)
library(GMCM)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Reactive value containing the data.frame ----
  user_data <- reactiveVal()

  # FILE INPUT ____________________________________________________________ ----

  # Load input file ----
  output$in_file_table <- renderDT({

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

    d_t <- datatable(
      user_data(),
      style = "bootstrap",
      class = "display cell-border compact",
      selection = list(target = 'row+column')
    ) %>%
      formatSignif(columns = numeric())

    return(d_t)
  })


  # Create input file text ----
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

  output$raw_data_plot <- renderPlot(
    width = 700, #px
    height = 700, #px
    expr = {

    req(user_data())

    # Get data, select
    d <- user_data()
    col_sel <- setdiff(input$in_file_table_columns_selected, 0) # Exclude index 0 (rownames)
    row_sel <- input$in_file_table_rows_selected

    if (is.null(col_sel) || length(col_sel) != 2) {
      i <- 1
      j <- 2
    } else {
      i <- col_sel[1]
      j <- col_sel[2]
    }

    # Colour selected rows
    cols <- rep("#00000050", nrow(d))
    cols[row_sel] <- "red"

    # Do plot
    plot(x = d[, i],
         y = d[, j],
         xlab = names(d)[i],
         ylab = names(d)[j],
         axes = FALSE,
         main = "Scatter",
         col = cols,
         asp = 1)
    axis(1)
    axis(2)

    # Add selected points on top
    points(d[row_sel, i], d[row_sel, j], col = "red", pch = 16)
  })




  # GENERAL GMCM __________________________________________________________ ----

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })




  # SPECIAL GMCM __________________________________________________________ ----

  # Initalise reative values ----
  user_data_pre <- reactiveVal()
  meta_fit <- reactiveVal()

  observeEvent(input$meta_large_vals, {
    user_data_pre(Uhat(ifelse(input$meta_large_vals, 1, -1) * user_data()))
  })

  # observe button push and fit model ----
  observeEvent(input$fit_meta_push, {
    u <- user_data_pre()

    init.par <- c(pie1  = input$par1,
                  mu    = input$par2,
                  sigma = input$par3,
                  rho   = input$par4)
    res <- fit.meta.GMCM(u = u,
                         init.par = init.par,
                         method = input$meta_method,
                         max.ite = input$meta_max_ite,
                         verbose = TRUE,
                         positive.rho = input$meta_positive_rho,
                         trace.theta = TRUE)
    meta_fit(res)
  })

  output$meta_str <- renderPrint({
    cat("\n\nstr(meta_fit())\n")
    str(meta_fit())
    cat("\n\nstr(input$in_file_table)\n")
    str(input$in_file_table)
    cat("\n\nprint(input$in_file_table_columns_selected)\n")
    print(input$in_file_table_columns_selected)
  })

})
