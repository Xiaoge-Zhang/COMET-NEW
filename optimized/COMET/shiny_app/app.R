library(shiny)
library(COMET)
library(shinyjs)

s4_config_path     <- normalizePath("../R/s4_config.R", winslash = "/", mustWork = TRUE)
s4_policy_path     <- normalizePath("../R/s4_policy.R", winslash = "/", mustWork = TRUE)
s4_result_path     <- normalizePath("../R/s4_result.R", winslash = "/", mustWork = TRUE)
s4_simulator_path  <- normalizePath("../R/s4_simulator.R", winslash = "/", mustWork = TRUE)
s4_experiment_path <- normalizePath("../R/s4_experiment.R", winslash = "/", mustWork = TRUE)

source(s4_config_path)
source(s4_policy_path)
source(s4_result_path)
source(s4_simulator_path)
source(s4_experiment_path)

options(
  comet.s4_config_path = s4_config_path,
  comet.s4_policy_path = s4_policy_path,
  comet.s4_result_path = s4_result_path,
  comet.s4_simulator_path = s4_simulator_path
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("COMET Simulator"),

  sidebarLayout(
    sidebarPanel(
      h4("Simulation Settings"),

      numericInput("days", "Simulation days", value = 30, min = 1),
      numericInput("seed", "Seed", value = 123),
      numericInput("can_start", "Initial candidate count", value = 1250, min = 0),
      selectInput("desired", "Desired strategy", choices = c("random", "mean"), selected = "random"),

      checkboxInput("include_matches", "Include matches", value = FALSE),
      checkboxInput("return_params", "Return parameters", value = FALSE),

      tags$hr(),
      h4("Experiment Settings"),
      numericInput("n_runs", "Number of runs", value = 1, min = 1, step = 1),
      checkboxInput("parallel", "Run in parallel", value = FALSE),
      numericInput("workers", "Workers (used only if parallel)", value = 2, min = 1, step = 1),
      helpText("If Number of runs = 1, the app behaves like a single simulation."),

      selectInput(
        "policy_type",
        "Policy type",
        choices = c("CAS23", "LAS15", "LAS21"),
        selected = "CAS23"
      ),

      tags$hr(),
      h4("Policy Parameters"),
      uiOutput("policy_params_ui"),

      actionButton("run_sim", "Run Simulation"),
      br(), br(),
      htmlOutput("run_status")
    ),

    mainPanel(
      conditionalPanel(
        condition = "input.n_runs > 1",
        tags$div(
          style = "margin-bottom: 15px;",
          selectInput("selected_run", "Select run to inspect", choices = NULL)
        )
      ),
      tabsetPanel(
        tabPanel("Experiment Summary",
                 verbatimTextOutput("experiment_summary_out"),
                 tableOutput("experiment_metrics_table")
        ),
        tabPanel("Summary",
                 verbatimTextOutput("summary_out")
        ),

        tabPanel("Wait Time",
                 plotOutput("wait_time_plot"),
                 verbatimTextOutput("wait_time_stats")
        ),

        tabPanel("Post-Transplant Survival",
                 plotOutput("survival_curve"),
                 plotOutput("survival_hist"),
                 verbatimTextOutput("survival_stats")
        ),

        tabPanel("Outcome Composition",
                 plotOutput("outcome_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    result = NULL,          # currently selected COMETResult for plots/summary
    exp_result = NULL,      # COMETExperimentResult if n_runs > 1
    status = "Ready.",
    running = FALSE
  )

  observe({
    shinyjs::disable("parallel")
    shinyjs::disable("workers")
  })

  output$policy_params_ui <- renderUI({

    if (input$policy_type == "CAS23") {
      tagList(
        selectInput("match_alg_cas", "Matching algorithm",
                    choices = c("match_cas"), selected = "match_cas"),

        selectInput("wl_model_cas", "Waitlist model",
                    choices = c("CAS23"), selected = "CAS23"),

        selectInput("post_tx_model_cas", "Post-transplant model",
                    choices = c("CAS23"), selected = "CAS23"),

        numericInput("wl_weight", "Waitlist weight", value = 0.25, min = 0, step = 0.01),
        numericInput("wl_cap", "Waitlist cap", value = 365, min = 0, step = 1),
        numericInput("post_tx_weight", "Post-transplant weight", value = 0.25, min = 0, step = 0.01),
        numericInput("post_tx_cap", "Post-transplant cap", value = 1826, min = 0, step = 1),

        numericInput("bio_weight", "Biology weight", value = 0.15, min = 0, step = 0.01),
        numericInput("peds_weight", "Pediatrics weight", value = 0.20, min = 0, step = 0.01),
        numericInput("pld_weight", "Placement efficiency weight", value = 0.05, min = 0, step = 0.01),
        numericInput("efficiency_weight", "Efficiency weight", value = 0.10, min = 0, step = 0.01),
        numericInput("abo_weight", "ABO weight", value = 0, min = 0, step = 0.01),
        numericInput("height_weight", "Height weight", value = 0, min = 0, step = 0.01),
        numericInput("cpra_weight", "cPRA weight", value = 0, min = 0, step = 0.01),
        numericInput("cost_weight", "Cost weight", value = 0, min = 0, step = 0.01),
        numericInput("distance_weight", "Distance weight", value = 0, min = 0, step = 0.01),

        checkboxInput("checks", "Enable checks", value = TRUE)
      )
    } else {
      tagList(
        selectInput("match_alg_las", "Matching algorithm",
                    choices = c("match_las"), selected = "match_las"),

        selectInput("wl_model_las", "Waitlist model",
                    choices = c("LAS15", "LAS21"), selected = input$policy_type),

        selectInput("post_tx_model_las", "Post-transplant model",
                    choices = c("LAS15", "LAS21"), selected = input$policy_type),

        numericInput("wl_weight", "Waitlist weight", value = 0.25, min = 0, step = 0.01),
        numericInput("wl_cap", "Waitlist cap", value = 365, min = 0, step = 1),
        numericInput("post_tx_weight", "Post-transplant weight", value = 0.25, min = 0, step = 0.01),
        numericInput("post_tx_cap", "Post-transplant cap", value = 1826, min = 0, step = 1),

        checkboxInput("checks", "Enable checks", value = TRUE)
      )
    }
  })

  build_policy <- reactive({

    if (input$policy_type == "CAS23") {
      CASPolicy(
        name = "CAS policy",
        params = list(
          match_alg = input$match_alg_cas,
          wl_model = input$wl_model_cas,
          post_tx_model = input$post_tx_model_cas,
          wl_weight = input$wl_weight,
          wl_cap = input$wl_cap,
          post_tx_weight = input$post_tx_weight,
          post_tx_cap = input$post_tx_cap,
          bio_weight = input$bio_weight,
          peds_weight = input$peds_weight,
          pld_weight = input$pld_weight,
          efficiency_weight = input$efficiency_weight,
          abo_weight = input$abo_weight,
          height_weight = input$height_weight,
          cpra_weight = input$cpra_weight,
          cost_weight = input$cost_weight,
          distance_weight = input$distance_weight,
          checks = input$checks
        )
      )
    } else {
      LASPolicy(
        name = "LAS policy",
        params = list(
          match_alg = input$match_alg_las,
          wl_model = input$wl_model_las,
          post_tx_model = input$post_tx_model_las,
          wl_weight = input$wl_weight,
          wl_cap = input$wl_cap,
          post_tx_weight = input$post_tx_weight,
          post_tx_cap = input$post_tx_cap,
          checks = input$checks
        )
      )
    }
  })

  observeEvent(input$run_sim, {
    req(!rv$running)

    rv$running <- TRUE
    rv$status <- "Running simulation..."
    rv$result <- NULL
    rv$exp_result <- NULL

    shinyjs::disable("run_sim")
    shinyjs::html("run_sim", "Running...")

    on.exit({
      rv$running <- FALSE
      shinyjs::enable("run_sim")
      shinyjs::html("run_sim", "Run Simulation")
    }, add = TRUE)

    tryCatch({
      cfg <- COMETConfig(
        days = input$days,
        seed = input$seed,
        desired = input$desired,
        can_start = input$can_start,
        include_matches = input$include_matches,
        return_params = input$return_params
      )

      pol <- build_policy()

      sim <- COMETSimulator(
        config = cfg,
        policy = pol
      )

      withProgress(message = "Running COMET simulation...", value = 0, {

        incProgress(0.15, detail = "Building simulator")
        Sys.sleep(0.1)

        if (input$n_runs <= 1) {

          incProgress(0.5, detail = "Executing single simulation")
          rv$result <- run(sim)
          rv$exp_result <- NULL

          incProgress(0.95, detail = "Finalizing output")
          Sys.sleep(0.1)

          rv$status <- "Single simulation completed successfully."

        } else {

          incProgress(0.3, detail = "Building experiment")

          exp <- COMETExperiment(
            simulator = sim,
            n_runs = input$n_runs,
            seeds = input$seed,
            parallel = FALSE,
            workers = NULL
          )

          incProgress(0.6, detail = "Executing experiment")
          rv$exp_result <- runExperiment(exp)

          # Default selected result for downstream plots/tabs
          res_tbl <- getResultTable(rv$exp_result)
          rv$result <- getResults(rv$exp_result)[[1]]

          updateSelectInput(
            session,
            "selected_run",
            choices = as.character(res_tbl$run_id),
            selected = "1"
          )

          incProgress(0.95, detail = "Finalizing output")
          Sys.sleep(0.1)

          rv$status <- paste("Experiment completed successfully with", input$n_runs, "runs.")
        }

        incProgress(1)
      })

    }, error = function(e) {
      rv$status <- paste("Error:", conditionMessage(e))
      print(e)
    })
  })

  observeEvent(input$selected_run, {
    req(rv$exp_result)
    req(input$n_runs > 1)

    idx <- suppressWarnings(as.integer(input$selected_run))
    req(!is.na(idx))

    res_list <- getResults(rv$exp_result)
    req(idx >= 1, idx <= length(res_list))

    rv$result <- res_list[[idx]]
  })

  # -----------------------------
  # Wait time data helper
  # -----------------------------
  wait_time_data <- reactive({
    req(rv$result)

    d <- getData(rv$result)
    rec <- d$recipient_database

    req(!is.null(rec), nrow(rec) > 0)
    req(all(c("listing_day", "transplant_day") %in% names(rec)))

    wt <- rec$transplant_day - rec$listing_day
    wt <- wt[!is.na(wt) & is.finite(wt) & wt >= 0]

    req(length(wt) > 0)

    wt
  })

  output$wait_time_plot <- renderPlot({
    wt <- wait_time_data()

    hist(
      wt,
      breaks = 30,
      probability = TRUE,
      col = "lightblue",
      border = "white",
      main = "Wait Time Distribution",
      xlab = "Days waiting for transplant"
    )

    lines(density(wt), col = "darkblue", lwd = 2)
    abline(v = mean(wt), col = "red", lwd = 2, lty = 2)
    abline(v = median(wt), col = "darkgreen", lwd = 2, lty = 2)

    legend(
      "topright",
      legend = c("Density", "Mean", "Median"),
      col = c("darkblue", "red", "darkgreen"),
      lwd = 2,
      lty = c(1, 2, 2),
      bty = "n"
    )
  })

  output$wait_time_stats <- renderPrint({
    wt <- wait_time_data()

    cat("Transplanted recipients analyzed:", length(wt), "\n")
    cat("Average wait time:", round(mean(wt), 2), "days\n")
    cat("Median wait time:", round(median(wt), 2), "days\n")
    cat("Standard deviation:", round(sd(wt), 2), "days\n")
    cat("Min wait time:", round(min(wt), 2), "days\n")
    cat("Max wait time:", round(max(wt), 2), "days\n")
    cat("90th percentile:", round(as.numeric(quantile(wt, 0.9)), 2), "days\n")
  })

  survival_days_data <- reactive({
    req(rv$result)

    d <- getData(rv$result)
    surv <- d$post_tx_death_database

    req(!is.null(surv), nrow(surv) > 0)
    req(all(c("death_day", "transplant_day") %in% names(surv)))

    survival_days <- surv$death_day - surv$transplant_day
    survival_days <- survival_days[!is.na(survival_days) & is.finite(survival_days) & survival_days >= 0]

    req(length(survival_days) > 0)

    survival_days
  })

  output$survival_curve <- renderPlot({
    sd <- survival_days_data()

    hist(
      sd,
      breaks = 30,
      probability = TRUE,
      col = "darkgreen",
      border = "white",
      main = "Post-Transplant Survival Distribution",
      xlab = "Days survived after transplant"
    )

    lines(density(sd), lwd = 2)
    abline(v = mean(sd), lty = 2, lwd = 2)
  })

  output$survival_hist <- renderPlot({
    sd <- survival_days_data()

    hist(
      sd,
      breaks = 30,
      col = "darkgreen",
      border = "white",
      main = "Observed Survival Days After Transplant",
      xlab = "Days after transplant"
    )
  })

  output$survival_stats <- renderPrint({
    sd <- survival_days_data()

    cat("Observed post-transplant deaths:", length(sd), "\n")
    cat("Average survival:", round(mean(sd), 2), "days\n")
    cat("Median survival:", round(median(sd), 2), "days\n")
    cat("Standard deviation:", round(sd(sd), 2), "days\n")
    cat("Max survival:", round(max(sd), 2), "days\n")
  })

  output$outcome_plot <- renderPlot({
    req(rv$result)

    m <- metrics(rv$result)

    vals <- c(
      Transplants = as.numeric(m[["total_transplants"]]),
      `Waitlist deaths` = as.numeric(m[["waitlist_deaths"]]),
      `Post-tx deaths` = as.numeric(m[["post_tx_deaths"]]),
      `Unused donors` = as.numeric(m[["non_used_donors"]])
    )

    pie(
      vals,
      col = c("forestgreen", "red", "orange", "gray"),
      main = "Outcome Composition"
    )
  })

  output$run_status <- renderUI({
    if (rv$running) {
      tags$div(
        style = "display:flex; align-items:center; gap:10px; color:#1f77b4; font-weight:bold;",
        tags$div(
          class = "spinner-border text-primary",
          role = "status",
          style = "width:1.5rem; height:1.5rem;",
          tags$span(class = "sr-only", "Loading...")
        ),
        tags$span("Simulation is running...")
      )
    } else {
      tags$div(
        style = "color: #2e7d32; font-weight: bold;",
        rv$status
      )
    }
  })

  output$summary_out <- renderPrint({
    req(rv$result)
    summary(rv$result)
  })

  output$experiment_summary_out <- renderPrint({
    req(rv$exp_result)
    summary(rv$exp_result)
  })

  output$experiment_metrics_table <- renderTable({
    req(rv$exp_result)
    experimentMetrics(rv$exp_result)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui, server)
