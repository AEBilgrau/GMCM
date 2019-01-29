library(shiny)
library(shinydashboard)
library(rhandsontable)
library(DT)
library(GMCM)

hot_renderer <- "
  function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    if (row == col) {
      td.style.background = 'lightgrey';
    } else if (col < row) {
      td.style.background = 'grey';
     td.style.color = 'grey';
    } else if (value < -1) {
     td.style.background = 'LightSkyBlue';
    } else if (value > 1) {
     td.style.background = 'GreenYellow';
    }
    td.style.align = 'center';
  }"


#options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #session$onSessionEnded(stopApp)

  # Reactive values concering the data.frame ----
  user_data <- reactiveVal()
  rv <- reactiveValues(d = NULL, m = NULL)

  # __FILE INPUT___________________________________________________________ ----

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





  # __GENERAL GMCM_________________________________________________________ ----

  # Initalise reactive values ----
  full_start_theta <- reactiveVal()
  in_pie <- reactiveVal()
  in_mu <- reactiveVal()
  full_fit_log <- reactiveVal()
  full_output_dataset <- reactiveVal()

  # Randomize start theta
  observeEvent(input$full_random_theta, {
    req(rv$m)
    req(rv$d)

    # A bit of interface fun
    die <-
      sprintf("dice-%s",c("one","two","three","four","five","six")[sample(6,1)])
    updateActionButton(session, inputId = "full_random_theta", icon = icon(die))

    # Generate random theta and set
    rt <- rtheta(m = rv$m, d = rv$d, method = input$full_rtheta_method)

    full_start_theta(rt)
    in_mu(do.call(cbind, rt$mu))
    in_pie(rt$pie)
  })

  # Observe d/m change ----
  observeEvent({
    input$full_m
    rv$d
  },{
    rv$m <- input$full_m # Write to reactive value
    req(rv$m)
    req(rv$d)

    # Update in_pie()
    in_pie(rep(1/rv$m , times = rv$m))

    # Update in mu()
    mu <- in_mu()
    if (!is.null(mu)) {

      # Create new cols if needed
      if (rv$m > ncol(mu)) {
        mu <- do.call("cbind", c(list(mu), replicate(rv$m - ncol(mu), NA_real_,
                                                     simplify = FALSE)))
        colnames(mu) <- paste0("comp", seq_len(rv$m))
      }
      # Remove cols if needed
      if (rv$m < ncol(mu)) {
        mu <- mu[, seq_len(rv$m)]
      }
      # Create new rows if needed
      if (rv$d > nrow(mu)) {
        mu <- do.call("rbind", c(list(mu), replicate(rv$d - nrow(mu), NA_real_,
                                                     simplify = FALSE)))
        rownames(mu) <- paste0("dim", seq_len(rv$d))
      }
      # Remove rows if needed
      if (rv$d < nrow(mu)) {
        mu <- mu[seq_len(rv$d), ]
      }

      # Update mu
      in_mu(mu)
    }
  })

  # pie input functionality ----
  # pie box updater
  observe({# Needed to access rv$m
    req(rv$m)
    # Observe all sliders and change the others
    lapply(seq_len(rv$m), function(k) {

      observeEvent(
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        eventExpr = input[[paste0("full_slider_pie", k)]],
        handlerExpr = {
          # All pie slider IDs
          pie_slider_ids <- paste0("full_slider_pie", seq_len(rv$m))

          # Require all pies
          req(sapply(pie_slider_ids, function(s) !is.null(input[[s]])))

          # All non k indices
          nk <- setdiff(seq_len(rv$m), k)

          # Get value in question and all others
          pie_k  <- input[[pie_slider_ids[k]]]
          pie_nk <- sapply(pie_slider_ids[nk], function(s) input[[s]])

          # Solving "sum(pie_nk * x) + pie_k = 1" for x yields
          x = (1 - pie_k)/sum(pie_nk)
          # I.e. all but k need to be change by a factor x

          # To avoid jumping around if too close
          if (abs(1 - (pie_k + sum(pie_nk))) < 0.001) {
            return()
          }

          # Update sliders
          for (i in nk) {
            updateSliderInput(
              session,
              inputId = pie_slider_ids[i],
              value = input[[pie_slider_ids[i]]]*x
            )
          }

          # Update in_pie() reactive value
          new_pie <- rep(NA, rv$m)
          new_pie[k] <- pie_k
          new_pie[nk] <- pie_nk*x
          in_pie(new_pie)  # Update call
        })
    })
  })

  # Make pie box ----
  output$full_pie_box <- renderUI({
    req(m <- rv$m)

    # Get pie if available, otherwise create it
    if (is.null(in_pie())) {
      in_pie(rep(1/m, times = m))
    }

    # Ensure step size from digits chosen
    digits <- 3
    step <- round(1/9, digits) - round(1/9, digits - 1)

    # Ensure the initial pies are compatible with the 'step' size
    tmp <- round(in_pie(), digits)
    tmp[1] <- 1 - sum(tmp[-1])
    in_pie(tmp)

    # Mixture props box and content
    box(
      # Args
      title = "Initial mixture proportions",
      status = "primary",
      collapsible = TRUE,
      width = 4,


      # Generate content on the form:
      #   sliderInput(inputId = "full_slider_pie1",
      #               label = "Component 1", ...),
      #   sliderInput(inputId = "full_slider_pie2",
      #               label = "Component 2", ...),
      #   ...
      lapply(seq_along(in_pie()), function(k)
        sliderInput(inputId = paste0("full_slider_pie", k),
                    label = paste("Component", k),
                    min = 0, max = 1, step = step,
                    ticks = FALSE,
                    value = in_pie()[k])
      )
    )
  })


  # Update full_start_theta() based on in_pie()
  observeEvent(in_pie(), {
    req(full_start_theta())
    theta <- full_start_theta()
    theta$pie <- in_pie()
    full_start_theta(theta)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)



  # mu input functionality -----
  # Create rhandson table ----
  output$rhandson_mu <- renderRHandsontable({
    req(rv$d)
    req(rv$m)

    if (is.null(in_mu())) {
      mu <- replicate(n = rv$m, rep(NA_real_, rv$d), simplify = TRUE)
    } else {
      mu <- in_mu()
    }

    # Add col/row names
    colnames(mu) <- paste0("comp", seq_len(ncol(mu)))
    rownames(mu) <- paste0("dim",  seq_len(nrow(mu)))

    rhandsontable(mu, contextMenu = FALSE)
  })

  # Update in_mu reactiveVal upon edit event
  observeEvent(input$rhandson_mu, {
    req(input$rhandson_mu)
    in_mu(hot_to_r(input$rhandson_mu))
  })

  # Make mu box ----
  output$full_mu_box <- renderUI({
    req(rv$m)
    req(rv$d)

    # Mixture props box and content
    box(
      # Args
      title = "Initial mean vectors",
      status = "primary",
      collapsible = TRUE,
      width = 4,

      rHandsontableOutput("rhandson_mu")
    )
  })

  # Update full_start_theta() based on in_mu()
  observeEvent(in_mu(), {
    req(full_start_theta())
    theta <- full_start_theta()
    theta$mu <- lapply(seq_len(rv$m), function(k) in_mu()[,k])
    full_start_theta(theta)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)




  # sigma input functionality -----
  # Update full_start_theta reactiveVal upon edit event
  observe({
    req(rv$m)
    req(rv$d)
    #req(full_start_theta())

    # Make sure components are deleted if m is reduced
    if (!is.null(full_start_theta())) {
      theta <- full_start_theta()
      theta$sigma <- theta$sigma[seq_len(rv$m)]
      full_start_theta(theta)
    }

    # Create all rhandsontables
    lapply(seq_len(rv$m), function(k) {
      output[[paste0("rhandson_sigma", k)]] <- renderRHandsontable({
        # req(full_start_theta())
        #https://github.com/jrowen/rhandsontable/tree/master/inst/examples/rhandsontable_corr

        if (k > length(full_start_theta()$sigma) ||
              is.null(full_start_theta()$sigma[[k]])) {
          mat <- diag(rv$d)
        } else {
          mat <- full_start_theta()$sigma[[k]]
        }

        # If d has changed:
        stopifnot(nrow(mat) == ncol(mat))
        if (ncol(mat) < rv$d) {
          tmp_mat <- diag(rv$d)
          tmp_mat[seq_len(nrow(mat)), seq_len(ncol(mat))] <- mat
          mat <- tmp_mat
        }
        if (ncol(mat) > rv$d) {
          mat <- mat[seq_len(rv$d), seq_len(rv$d)]
        }

        # Colnames are needed for hot_to_r to work
        if (is.null(rownames(mat))) {
          colnames(mat) <- paste0("dim", seq_len(nrow(mat)))
        }
        if (is.null(rownames(mat))) {
          rownames(mat) <- colnames(mat)
        }

        # Make table
        rhandsontable(mat, readOnly = FALSE, selectCallback = FALSE,
                      contextMenu = TRUE) %>%
          hot_cols(renderer = hot_renderer)
      })
    })


  })

  # Make observers for each sigma change (edit of table)
  observe({
    req(rv$m)
    req(rv$d)

    lapply(seq_len(rv$m), function(k) {

      observeEvent(input[[paste0("rhandson_sigma", k)]], {
        # Write change full_start_theta
        theta <- full_start_theta()
        theta$sigma[[k]] <- hot_to_r(input[[paste0("rhandson_sigma", k)]])
        full_start_theta(theta)
      })

    })
  })

  # Make sigma box ----
  output$full_sigma_box <- renderUI({
    req(rv$m)
    req(rv$d)

    # Mixture props box and content
    box(
      # Args
      title = "Initial variance-covariance matrices",
      status = "primary",
      collapsible = TRUE,
      width = 4,

      # Show starting sigmas
      lapply(seq_len(rv$m), function(k) {
        list(
          tags$b(paste("Component", k)),
          rHandsontableOutput(paste0("rhandson_sigma", k))
        )
      })
    )
  })




  # Fit general model ----
  full_fit <- eventReactive(input$full_fit_push, {
    # Requirements
    req(full_start_theta())
    req(length(input$model_cols) >= 2)
    req(user_data())
    req(rv$m)

    # Get and preprocess user data
    x <- user_data()[, input$model_cols]
    u <- Uhat(x)

    # Fit model
    fit_time <- system.time(
      fit_log <- capture.output({
        theta <- fit.full.GMCM(u = u,
                               m = rv$m,
                               theta = choose.theta(u, rv$m), #full_start_theta())
                               method = input$full_method,
                               max.ite = input$meta_max_ite,
                               verbose = TRUE)
      })
    )[3]

    # Save caputured output to reactive val
    full_fit_log(fit_log)

    # Append data and reproducibility results
    return(list(theta = theta, u = u, x = x, fit_time = fit_time))
  })


  output$full_plot_pie <- renderUI({
    req(fit <- full_fit())

    pie <- fit$theta$pie
    if (length(pie) >= 7) {
      names(pie) <- paste("Comp", seq_along(pie))
    } else {
      names(pie) <- paste("Component ", seq_along(pie))
    }
    mat <- cbind(pie)
    colnames(mat) <- ""
    mat <- mat[order(rownames(mat)), , drop = FALSE]

    box(
      # box args
      title = "Estimated mixture proportions",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,

      # Content
      selectInput("full_pie_plot_type", label = "",
                  choices = c("Bar plot", "Dot chart", "Pie chart"),
                  selected = input$full_pie_plot_type),
      renderPlot({
        if (!is.null(input$full_pie_plot_type)) {
          if (input$full_pie_plot_type == "Bar plot") {
            barplot(mat,
                    legend.text = rownames(mat),
                    args.legend = list(x = "top", bty = "n",
                                       horiz = TRUE, xpd = TRUE, inset = -0.1),
                    horiz = TRUE,
                    beside = input$full_pie_beside,
                    xlab = "Proportion",
                    ylab = "Components",
                    xlim = 0:1)
            if (input$full_pie_beside) {
              text(mat, seq_along(mat) + 0.5, labels = round(c(mat), 2), pos = 4)
              text(mat, seq_along(mat) + 0.5, labels = rownames(mat), pos = 2)
            }
          } else if (input$full_pie_plot_type == "Dot chart") {
            dotchart(pie, xlim = c(0, 1), pch = 16, cex = 1.2)
          } else if (input$full_pie_plot_type == "Pie chart") {
            pie(pie)
          }
        } else {
          NULL
        }
      }),
      if (!is.null(input$full_pie_plot_type)) {
        if (input$full_pie_plot_type == "Bar plot") {
          checkboxInput("full_pie_beside", label = "Split bars", value = FALSE)
        }
      }
    )

  })


  output$full_res_mu <- renderUI({
    req(fit <- full_fit())

    # Create table
    mu <- fit$theta$mu
    tab <- do.call(data.frame, mu)
    rownames(tab) <- paste("Dimension", seq_len(nrow(tab)))

    box(
      # box args
      title = "Estimated means",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,

      # Content
      renderTable(
        tab,
        striped = TRUE, hover = TRUE,
        rownames = TRUE
      )
    )
  })


  output$full_res_sigma <- renderUI({
    req(fit <- full_fit())

    # Get list of sigmas
    sigmas <- fit$theta$sigma


    box(
      # box args
      title = "Estimated covariances",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,

      # Content
      lapply(seq_along(sigmas), function(k) {
        tab <- sigmas[[k]]
        rownames(tab) <- colnames(tab) <- paste0("dim", seq_len(nrow(tab)))
        list(
          tags$b(paste("Component", k)),
          renderTable(
            tab,
            striped = TRUE, hover = TRUE,
            rownames = TRUE
          )
        )
      })

    )
  })

  output$full_res_theta_plot <- renderUI({
    req(fit <- full_fit())
    req(input$full_model_cols_xy)

    which_dims <- match(input$full_model_cols_xy, colnames(fit$x))[1:2]
    req(sum(!is.na(which_dims)) == 2)

    box(
      # Box args
      title = "Vizualisation of estimated theta",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,

      # Content
      renderPlot(
        plot(full_fit()$theta,
             which.dims = which_dims,
             add.means = TRUE,
             add.ellipses = TRUE)
      )
    )
  })


  output$full_fit_log <- renderUI({
    req(full_fit_log())
    req(input$full_method)

    box(
      collapsible = TRUE,
      collapsed = TRUE,

      # box args
      title = "Log of fitting procedure:",
      footer = if (input$full_method == "SANN") {
                "Note: Simulated Annealing always uses all iterations."
               } else {
                NULL
               },

      # Content
      renderPrint(cat(full_fit_log(), sep = "\n"))
    )
  })


  # Classification ----
  full_prob <- reactive({
    req(fit <- full_fit())
    req(input$full_class_type)

    # Get class probabilities
    return(get.prob(fit$u, theta = fit$theta))
  })

  full_classification <- reactive({
    req(fit <- full_fit())
    req(input$full_class_type)

    # Get class probabilities
    prob_mat <- full_prob()

    if (input$full_class_type == "max_prob") {
      comps <- apply(prob_mat, 1, which.max)
    } else if (input$full_class_type == "thres_prob") {
      req(input$full_thres_prob)
      comps <- apply(prob_mat, 1, which.max)
      ok_max <- apply(prob_mat, 1, max) > input$full_thres_prob
      comps[!ok_max] <- NA
    } else {
      stop("full_class_type not matched")
    }

    return(comps)

  })

  # X/Y selection ----
  output$full_ui_selectize_model_cols_xy <- renderUI({
    req(input$model_cols)

    box(
      selectizeInput(inputId = "full_model_cols_xy",
                     label = "Select variables for X and Y axis",
                     choices = input$model_cols,
                     selected = input$model_cols[1:2],
                     multiple = TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')))
    )
  })

  #Values plot ----
  output$full_obs_plot <- renderUI({
    req(full_fit())
    req(full_classification())

    box(
      # Box args
      title = "Observed values",
      status = "primary",
      collapsible = TRUE,
      solidHeader = TRUE,
      width = 4,

      # Content
      renderPlot({
        full_plot(
          fit = full_fit(),
          comp = full_classification(),
          plot_type = "obs",
          col_sel = input$full_model_cols_xy,
          row_sel = input$in_file_table_rows_selected
        )
      })
    )
  })

  # Rank plot ----
  output$full_rank_plot <- renderUI({
    req(full_fit())
    req(full_classification())

    box(
      # Box args
      title = "Ranked values",
      status = "primary",
      collapsible = TRUE,
      solidHeader = TRUE,
      width = 4,

      # Content
      renderPlot({
        full_plot(
          fit = full_fit(),
          comp = full_classification(),
          plot_type = "rank",
          col_sel = input$full_model_cols_xy,
          row_sel = input$in_file_table_rows_selected
        )
      })
    )
  })

  # Latent plot ----
  output$full_latent_plot <- renderUI({
    req(full_fit())
    req(full_classification())

    box(
      # Box args
      title = "Latent values",
      status = "primary",
      collapsible = TRUE,
      solidHeader = TRUE,
      width = 4,

      # Content
      renderPlot({
        full_plot(
          fit = full_fit(),
          comp =  full_classification(),
          plot_type = "gmm",
          col_sel = input$full_model_cols_xy,
          row_sel = input$in_file_table_rows_selected
        )
      })
    )
  })


  # Output file -------------
  output$full_out_file_table <- renderDT({

    # Set required dependency
    req(tab <- user_data())
    req(cls <- full_classification())
    req(mat <- full_prob())

    colnames(mat) <- paste0("comp", seq_len(ncol(mat)), "prob")
    tab <- cbind(tab, as.data.frame(mat), class = cls)

    # Save as output dataset
    full_output_dataset(tab)

    # Make DT
    d_t <- datatable(tab,
                     style = "bootstrap",
                     class = "display cell-border compact",
                     selection = list(target = 'row')) %>%
      formatSignif(columns = numeric())

    return(d_t)
  })

  output$full_downloadData <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0(input$in_file$name, "_classified.csv")
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(full_output_dataset(),
                  file = file,
                  col.names = input$header,
                  sep = input$sep,
                  quote = ifelse(input$quote == "", FALSE, TRUE),
                  row.names = TRUE)
    }
  )


  # DEBUG ----
  output$DEBUG <- renderPrint({
    req(rv$d)
    req(rv$m)

    cat("str(full_fit())\n")
    cat(str(full_fit()))

    # cat("\n\nstr(in_mu())\n")
    # cat(str(in_mu()))
    cat("\n\nprint(in_pie())\n")
    print(in_pie())


    cat("\n\nprint(hot_to_r(input$rhandson_mu)\n")
    print(hot_to_r(input$rhandson_mu))

    cat("\n\nprint(in_mu())\n")
    print(in_mu())


    cat("\n\nprint(rv$m)\n")
    print(rv$m)
    cat("\n\nprint(rv$d)\n")
    print(rv$d)
  })





  # __SPECIAL GMCM ________________________________________________________ ----

  # Initalise reactive values ----
  meta_fit <- reactiveVal() # Holds fitted values
  meta_output_dataset <- reactiveVal()
  meta_rv <- reactiveValues(fit_time = 0,
                            ite = 0)

  # Preprocess ----

  # Set d
  observeEvent(input$model_cols, {
    rv$d <- length(input$model_cols)
  })

  # observe button push and fit model ----
  observeEvent(input$meta_fit_push, {
    req(!is.null(input$meta_large_vals))
    req(length(input$model_cols) >= 2)
    req(user_data())

    # Get and preprocess user data
    x <- user_data()[, input$model_cols]
    u <- Uhat(ifelse(input$meta_large_vals, 1, -1) * x)

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
    res <- c(res, list(u = u, x = x))

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
  output$meta_ui_selectize_model_cols_xy <- renderUI({
    req(input$model_cols)

    box(
      selectizeInput(inputId = "meta_model_cols_xy",
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
      col_sel = input$meta_model_cols_xy,
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
      col_sel = input$meta_model_cols_xy,
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
      col_sel = input$meta_model_cols_xy,
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
