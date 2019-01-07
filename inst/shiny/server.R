library(shiny)
library(shinydashboard)
library(DT)
library(GMCM)

#options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #session$onSessionEnded(stopApp)

  # Reactive values concering the data.frame ----
  user_data <- reactiveVal()
  rv <- reactiveValues(d = 2)

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

    d_t <-
      datatable(user_data(),
                style = "bootstrap",
                class = "display cell-border compact",
                selection = list(target = 'row')) %>%
      formatSignif(columns = numeric())

    return(d_t)
  })


  # Create input file text ----
  output$input_file_description <- renderUI({
    # Set required dependency
    validate(need(user_data(), "Please upload some data."))

    tagList(
      h1("Dataset loaded"),
      p(sprintf("%i rows and %i columns detected. Showing %i rows below.",
                nrow(user_data()),
                ncol(user_data()),
                length(input$in_file_table_rows_current)),
        "Select rows to hightlight in plots.")
    )
  })

  # model_cols_box ----
  output$model_cols_box <- renderUI({
    req(user_data())

    box(
      title = "Model column selection",
      solidHeader = TRUE,
      status = "success",
      collapsible = TRUE,

      # Content
      selectizeInput(inputId = "model_cols",
                     label = "Select columns to pass to model",
                     choices = names(user_data()),
                     multiple = TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop'))),
      fluidRow(
        column(width = 7,
               withTags(p(i("Note: "),
                          "You can drag and drop to change the order of the selected variables."))),
        column(width = 5,
               checkboxInput(inputId = "do_matrix_plot",
                             label = "Create matrix plot if nessesary.",
                             value = TRUE))
      )
    )
  })

  output$raw_data_box <- renderUI({
    req(user_data())

    footer <- NULL
    if (!is.null(input$model_cols) && length(input$model_cols) >= 2) {
      status <- "success"
      height <- "800px"
      if (length(input$model_cols) > 2 && !input$do_matrix_plot) {
        footer <- "Note: More than 2 variables/Columns selected but showing
        only the first two here. All selected columns are kept and used in
        the estimation."
      }
    } else {
      height <- "50px"
      status <- "warning"
    }

    box(
      # Box args
      title = "Scatter plot of raw data",
      solidHeader = TRUE,
      status = status,
      collapsible = TRUE,
      width = 12,
      footer = footer,

      # Content
      plotOutput("raw_data_plot",
                 height = height)
    )
  })

  output$raw_data_plot <- renderPlot({
    req(user_data())
    validate(need(length(input$model_cols) >= 2,
                  "Please select at least two columns."))

    # Get data, select
    d <- user_data()

    col_sel <- match(input$model_cols, names(d))
    row_sel <- input$in_file_table_rows_selected

    if (is.null(col_sel) || length(col_sel) < 2) {
      i <- 1
      j <- 2
    } else {
      i <- col_sel[1]
      j <- col_sel[2]
    }

    # Colour selected rows
    cols <- rep("#00000050", nrow(d))
    cols[row_sel] <- "blue"


    if (input$do_matrix_plot && length(input$model_cols) > 2) {
      # Order to draw red on top
      o <- order(seq_along(cols) %in% row_sel)

      pairs(d[o, input$model_cols],
            lower.panel = NULL,
            col = cols,
            asp = 1,
            pch = 16,
            cex = 0.1)
    } else {
      plot(x = d[, i],
           y = d[, j],
           xlab = names(d)[i],
           ylab = names(d)[j],
           axes = FALSE,
           col = cols,
           asp = 1)
      axis(1)
      axis(2)

      # Add selected points on top
      points(d[row_sel, i], d[row_sel, j], col = "red", pch = 16)
    }



  })





  # GENERAL GMCM __________________________________________________________ ----

  # Initalise reative values ----
  full_start_theta <- reactiveVal()

  # Randomize start theta
  observeEvent(input$full_random_theta, {
    req(input$full_m)
    req(rv$d)

    # A bit of interface fun
    die <-
      sprintf("dice-%s",c("one","two","three","four","five","six")[sample(6,1)])
    updateActionButton(session, inputId = "full_random_theta", icon = icon(die))

    # Generate random theta and set
    full_start_theta(
      rtheta(m = input$full_m, d = rv$d, method = input$full_rtheta_method)
    )
  })

  # View theta ----
  output$full_start_theta_str <- renderPrint({
    cat("str(full_start_theta())\n")
    cat(str(full_start_theta()))
  })







  # SPECIAL GMCM __________________________________________________________ ----

  # Initalise reative values ----
  user_data_pre <- reactiveVal()  # Holds 'preprocessed' user data
  meta_fit <- reactiveVal() # Holds fitted values
  meta_output_dataset <- reactiveVal()
  meta_rv <- reactiveValues(fit_time = 0,
                            ite = 0)

  # Preprocess ----
  observeEvent(input$meta_large_vals, {
    user_data_pre(Uhat(ifelse(input$meta_large_vals, 1, -1) * user_data()))
  })

  observeEvent(input$model_cols, {
    user_data_pre(user_data()[, input$model_cols])
  })

  # observe button push and fit model ----
  observeEvent(input$meta_fit_push, {
    cat("Fit model clicked.\n")

    # Require user (preprocessed) data
    req(u <- user_data_pre())


    # Set initial parameters
    init.par <- c(pie1  = input$par1,
                  mu    = input$par2,
                  sigma = input$par3,
                  rho   = input$par4)

    # Fit model
    fit_time <- system.time(
      res <- fit.meta.GMCM(u = u,
                           init.par = init.par,
                           method = input$meta_method,
                           max.ite = input$meta_max_ite,
                           verbose = TRUE,
                           positive.rho = input$meta_positive_rho,
                           trace.theta = TRUE)
    )[3]

    # Append data and reproducibility results
    res <- c(res, list(u = u, x = user_data()))

    # Save results
    meta_fit(res)
    meta_rv$fit_time <- fit_time
    if (input$meta_method == "PEM") {
      meta_rv$ite <- ncol(res[[2]]$loglik_tr)
    } else {
      meta_rv$ite <- res[[2]]$counts[1]
    }



  })

  # Classify the observations based on fit ----
  meta_classification <- reactive({
    req(meta_fit())

    input$meta_IDR_thres # Declare dependency
    fit <- meta_fit()

    # Classify
    out <- get.IDR(x = fit$u, par = fit[[1]], threshold = input$meta_IDR_thres)
    below <- out[[input$meta_IDR_thres_type]] < input$meta_IDR_thres
    out$l <- sum(below)
    out$Khat <- ifelse(below, 2, 1)

    return(out)
  })

  # infoBoxes ----
  output$infoBoxes <- renderUI({
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
              width = 3, color = "light-blue",#"aqua",
              icon = icon("percent")),
      valueBox(subtitle = "Mean value",
              value = fitted_vals["mu"],
              width = 3, color = "light-blue", #"teal",
              icon = icon("minus")),
      valueBox(subtitle = "Standard deviation",
              value = fitted_vals["sigma"],
              width = 3, color = "light-blue", #"fuchsia",
              icon = icon("resize-horizontal", lib = "glyphicon")),
      valueBox(subtitle = "Correlation",
              value = fitted_vals["rho"],
              width = 3, color = "light-blue",
              icon = icon("bar-chart"))
    )

  })

  # X/Y selection ----
  output$ui_selectize_model_cols_xy <- renderUI({
    req(input$model_cols)

    box(
      selectizeInput(inputId = "model_cols_xy",
                     label = "Select variables for X and Y axis",
                     choices = input$model_cols,
                     selected = input$model_cols[1:2],
                     multiple = TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')))
    )
  })

  # Values plot ----
  output$obs_plot <- renderPlot({

    req(meta_fit())
    req(meta_classification())

    meta_plot(
      fit = meta_fit(), # A fitted object data
      idr = meta_classification(),
      plot_type = "obs",
      col_sel = input$model_cols_xy,
      row_sel = input$in_file_table_rows_selected
    )
  })

  # Rank plot ----
  output$rank_plot <- renderPlot({

    req(meta_fit())
    req(meta_classification())

    meta_plot(
      fit = meta_fit(), # A fitted object data
      idr = meta_classification(),
      plot_type = "rank",
      col_sel = input$model_cols_xy,
      row_sel = input$in_file_table_rows_selected
    )
  })

  # Latent plot ----
  output$latent_plot <- renderPlot({
    req(meta_fit())
    req(meta_classification())

    meta_plot(
      fit = meta_fit(), # A fitted object data
      idr = meta_classification(),
      plot_type = "gmm",
      col_sel = input$model_cols_xy,
      row_sel = input$in_file_table_rows_selected
    )
  })



  # Classification UI update ----
  observeEvent(input$meta_IDR_thres_type, {
    updateSliderInput(session,
                      inputId = "meta_IDR_thres",
                      label = ifelse(input$meta_IDR_thres_type == "idr",
                                     "Local idr threshold:",
                                     "Adjusted IDR threshold:"),
                      value = ifelse(input$meta_IDR_thres_type == "idr",
                                     0.5,
                                     0.05))
  })

  # Classification boxes ----
  output$classificationBoxes <- renderUI({
    req(idr <- meta_classification())

    tagList(
      valueBox(value =  with(idr, paste0(l, "/", length(Khat),
                                         " (", 100*round(l/length(Khat), 3), "%)")),
               subtitle = tags$div("features deemed", tags$b("reproducible")),
               width = 4,
               color = "green",
               icon = icon("thumbs-up", lib = "glyphicon")),
      valueBox(value =  with(idr, paste0(length(Khat) - l, "/", length(Khat),
                                         " (", 100*round(1 - l/length(Khat), 3), "%)")),
               subtitle = tags$div("features deemed", tags$b(HTML("<em>ir</em>reproducible"))),

               width = 4,
               color = "red",
               icon = icon("thumbs-down", lib = "glyphicon")),
      valueBox(value = meta_rv$ite,
              #title = "Procedure stopped after",
              subtitle = paste("iterations in",
                                       round(meta_rv$fit_time,0),
                                       "seconds"),
              width = 4,
              color = "light-blue",
              icon = icon("hashtag"))
    )
  })


  # Output file -------------
  output$meta_out_file_table <- renderDT({

    # Set required dependency
    req(tab <- user_data())
    req(cls <- meta_classification())

    tab$local_idr <- cls$idr
    tab$adj_IDR <- cls$IDR
    tab[[paste0("reprodicible_", input$meta_IDR_thres_type, "_above_",
                input$meta_IDR_thres)]] <- cls$Khat == 2

    # Save as output dataset
    meta_output_dataset(tab)

    # Make DT
    d_t <- datatable(tab,
                     style = "bootstrap",
                     class = "display cell-border compact",
                     selection = list(target = 'row')) %>%
      formatSignif(columns = numeric())

    return(d_t)
  })

  output$downloadData <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0(input$in_file$name, "_classified.csv")
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(meta_output_dataset(),
                  file = file,
                  col.names = input$header,
                  sep = input$sep,
                  quote = ifelse(input$quote == "", FALSE, TRUE),
                  row.names = TRUE)
    }
  )


  output$meta_str <- renderPrint({
    cat("\n\nstr(meta_fit())\n")
    str(meta_fit())
    cat("\n\nstr(meta_classification())\n")
    str(meta_classification())
    cat("\n\nstr(input$in_file_table)\n")
    str(input$in_file_table)
    cat("\n\nprint(input$in_file_table_rows_selected)\n")
    print(input$in_file_table_rows_selected)
    cat("\n\nprint(input$model_cols)\n")
    print(input$model_cols)
  })

})
