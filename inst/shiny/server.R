library(shiny)
library(shinydashboard)
library(DT)
library(GMCM)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)

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
         main = "Scatter plot of raw data",
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
  observeEvent(input$meta_fit_push, {

    # Require user (preprocessed) data
    req(u <- user_data_pre())


    # Set initial parameters
    init.par <- c(pie1  = input$par1,
                  mu    = input$par2,
                  sigma = input$par3,
                  rho   = input$par4)

    # Fit model
    res <- fit.meta.GMCM(u = u,
                         init.par = init.par,
                         method = input$meta_method,
                         max.ite = input$meta_max_ite,
                         verbose = TRUE,
                         positive.rho = input$meta_positive_rho,
                         trace.theta = TRUE)

    # Determine reproducibility
    idr <- get.IDR(x = u, par = res[[1]], threshold = input$meta_IDR_thres)

    # Append data and reproducibility results
    res <- c(res, list(u = u))

    # Save results
    meta_fit(res)
  })

  # Classify the observations based on fit
  meta_classification <- reactive({
    req(meta_fit())

    input$meta_IDR_thres
    fit <- meta_fit()

    # Classify
    idr <- get.IDR(x = fit$u,
                   par = fit[[1]],
                   threshold = input$meta_IDR_thres)
    return(idr)
  })


  output$infoBoxes <- renderUI({
    #req(meta_classification())
    req(meta_fit())

    fitted_vals <- signif(meta_fit()[[1]], 3)

    box(
      # Box args
      title = "Estimated parameters:",
      status = "primary",
      collapsible = TRUE,
      width = 12,

      # Content
      valueBox(subtitle = "Mixture proportion",
              value = fitted_vals["pie1"],
              width = 3, color = "aqua",
              icon = icon("percent")),
      valueBox(subtitle = "Mean value",
              value = fitted_vals["mu"],
              width = 3, color = "teal",
              icon = icon("minus")),
      valueBox(subtitle = "Standard deviation",
              value = fitted_vals["sigma"],
              width = 3, color = "fuchsia",
              icon = icon("resize-horizontal", lib = "glyphicon")),
      valueBox(subtitle = "Correlation",
              value = fitted_vals["rho"],
              width = 3, color = "light-blue",
              icon = icon("bar-chart"))
    )

  })

  # Rank plot ----
  output$rank_plot <- renderPlot({

    req(meta_fit())
    req(meta_classification())
    fit <- meta_fit()
    idr <- meta_classification()

    col_sel <- setdiff(input$in_file_table_columns_selected, 0) # Exclude index 0 (rownames)
    row_sel <- input$in_file_table_rows_selected

    if (is.null(col_sel) || length(col_sel) != 2) {
      i <- 1
      j <- 2
    } else {
      i <- col_sel[1]
      j <- col_sel[2]
    }

    # Get ranked data
    u <- fit$u

    # Colour selected rows and reproducible
    cols <- rep("#00000050", nrow(u))
    cols[row_sel] <- "red"

    # Color by classification
    cols[idr$Khat == 2] <- "steelblue"

    # Do plot
    plot(x = u[, i],
         y = u[, j],
         xlab = colnames(u)[i],
         ylab = colnames(u)[j],
         axes = FALSE,
         main = "",
         col = cols,
         asp = 1)
    axis(1)
    axis(2)


    # Add selected points on top
    points(u[row_sel, i], u[row_sel, j], col = "red", pch = 16)
  })

  output$meta_str <- renderPrint({
    cat("\n\nstr(meta_fit())\n")
    str(meta_fit())
    cat("\n\nstr(meta_classification())\n")
    str(meta_classification())
    cat("\n\nstr(input$in_file_table)\n")
    str(input$in_file_table)
    cat("\n\nprint(input$in_file_table_columns_selected)\n")
    print(input$in_file_table_columns_selected)
  })

})
