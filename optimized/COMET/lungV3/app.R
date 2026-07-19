library(shiny)
library(COMETopt)
library(cometdata)
library(shinyjs)

ui <- navbarPage(
  "COMET Simulator",
  
  header = tagList(
    useShinyjs()
  ),
  
  tabPanel(
    "Run Experiment",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          
          h3("Experiment Settings"),
          textInput("experiment_label", "Experiment label", value = "Experiment 1"),
          numericInput("n_runs", "Number of runs", value = 1, min = 1, step = 1),
          checkboxInput("parallel", "Run in parallel", value = FALSE),
          numericInput("workers", "Workers (used only if parallel)", value = 2, min = 1, step = 1),
          textOutput("available_cores_text"),
          helpText("If Number of runs = 1, the app behaves like a single simulation."),
          
          tags$hr(),
          
          h3("Simulation Settings"),
          numericInput("days", "Simulation days", value = 30, min = 1),
          numericInput("seed", "Seed", value = 123),
          numericInput("can_start", "Initial candidate count", value = 1250, min = 0),
          
          radioButtons(
            "candidate_mode",
            "Candidate population",
            choices = c(
              "General population" = "general",
              "Specific candidate type" = "specific"
            ),
            selected = "general"
          ),
          
          conditionalPanel(
            condition = "input.candidate_mode == 'specific'",
            selectInput(
              "diag_group",
              "Diagnosis group",
              choices = c("A", "B", "C", "D"),
              selected = "A"
            ),
            selectInput(
              "blood_type",
              "Blood type",
              choices = c("A", "B", "AB", "O"),
              selected = "A"
            ),
            selectInput(
              "height_cat",
              "Height category",
              choices = c("<=62.25", "62.26-64.99", "65.00-66.99", "67.00-70.00", ">70"),
              selected = "65.00-66.99"
            ),
            selectInput(
              "wlauc_cat",
              "Medical urgency (WLAUC) category",
              choices = c("<250", "250-299", "300-324", "325-365"),
              selected = "300-324"
            ),
            tags$p(
              tags$a("WLAUC calculator", href = "#", target = "_blank")
            )
          ),
          
          selectInput("desired", "Desired strategy", choices = c("random", "mean"), selected = "random"),
          checkboxInput("include_matches", "Include matches", value = FALSE),
          checkboxInput("return_params", "Return parameters", value = FALSE),
          
          tags$hr(),
          
          h3("Policy Settings"),
          selectInput(
            "policy_type",
            "Policy type",
            choices = c("CAS23", "LAS15", "LAS21"),
            selected = "CAS23"
          ),
          uiOutput("policy_params_ui"),
          
          tags$br(),
          actionButton("run_save_exp", "Run and Save Experiment"),
          tags$br(),
          tags$br(),
          htmlOutput("run_status")
        ),
        
        column(
          width = 8,
          h3("Run Instructions"),
          tags$p("Configure the experiment on the left, then click 'Run and Save Experiment'."),
          tags$p("After the run finishes, inspect outputs in the Results tab."),
          tags$p("Saved experiments can be managed in the Saved Experiments tab and compared in the Compare Experiments tab.")
        )
      )
    )
  ),
  
  tabPanel(
    "Results",
    fluidPage(
      
      conditionalPanel(
        condition = "!output.has_result",
        tags$div(
          style = "margin-top: 30px; font-size: 16px; color: #666;",
          "No experiment loaded yet. Please run or load an experiment first."
        )
      ),
      
      conditionalPanel(
        condition = "output.has_result",
        tagList(
          
          conditionalPanel(
            condition = "output.has_exp_result",
            tags$div(
              style = "margin-bottom: 15px;",
              selectInput("selected_run", "Select run to inspect", choices = NULL)
            )
          ),
          
          tags$div(
            style = "margin-bottom: 15px;",
            selectInput(
              "stratify_by",
              "Stratify summary by",
              choices = c(
                "None" = "none",
                "Diagnosis group" = "diag_group",
                "Height category" = "height_cat",
                "Blood type" = "blood_type",
                "Candidate age" = "age_group",
                "Sex" = "sex",
                "Race/Ethnicity" = "race_ethnicity",
                "WLAUC category" = "wlauc_cat",
                "U.S. Region" = "region"
              ),
              selected = "none"
            )
          ),
          
          tabsetPanel(
            tabPanel(
              "Experiment Summary",
              verbatimTextOutput("experiment_summary_out"),
              verbatimTextOutput("population_settings_out"),
              tableOutput("experiment_metrics_table"),
              tags$hr(),
              h4("Population Summary"),
              tableOutput("population_summary_table")
            ),
            tabPanel(
              "Summary",
              verbatimTextOutput("summary_out")
            ),
            tabPanel(
              "Wait Time",
              plotOutput("wait_time_plot"),
              verbatimTextOutput("wait_time_stats")
            ),
            tabPanel(
              "Post-Transplant Survival",
              plotOutput("survival_curve"),
              plotOutput("survival_hist"),
              verbatimTextOutput("survival_stats")
            ),
            tabPanel(
              "Outcome Composition",
              plotOutput("outcome_plot")
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Saved Experiments",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Load Saved Experiment"),
          selectInput("saved_experiment", "Load saved experiment", choices = c()),
          actionButton("load_saved_exp", "Load Selected Experiment")
        ),
        column(
          width = 8,
          h3("Saved Experiment Registry"),
          tableOutput("saved_experiments_table")
        )
      )
    )
  ),
  
  tabPanel(
    "Compare Experiments",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Comparison Settings"),
          selectInput("compare_exp_1", "Experiment 1", choices = c()),
          selectInput("compare_exp_2", "Experiment 2", choices = c()),
          selectInput(
            "compare_metric",
            "Metric to compare",
            choices = c(
              "Total transplants" = "total_transplants",
              "Waitlist deaths" = "waitlist_deaths",
              "Post-transplant deaths" = "post_tx_deaths",
              "Unused donors" = "non_used_donors"
            )
          )
        ),
        column(
          width = 8,
          h3("Comparison Results"),
          tableOutput("comparison_summary_table"),
          plotOutput("comparison_boxplot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    result = NULL,
    exp_result = NULL,
    status = "Ready.",
    running = FALSE,
    experiments = list(),
    active_experiment_id = NULL
  )
  
  output$has_result <- reactive({
    !is.null(rv$result)
  })
  outputOptions(output, "has_result", suspendWhenHidden = FALSE)
  
  output$has_exp_result <- reactive({
    !is.null(rv$exp_result)
  })
  outputOptions(output, "has_exp_result", suspendWhenHidden = FALSE)
  
  observe({
    if (isTRUE(input$n_runs > 1)) {
      shinyjs::enable("parallel")
      
      if (isTRUE(input$parallel)) {
        shinyjs::enable("workers")
      } else {
        shinyjs::disable("workers")
      }
    } else {
      updateCheckboxInput(session, "parallel", value = FALSE)
      shinyjs::disable("parallel")
      shinyjs::disable("workers")
    }
  })
  
  output$available_cores_text <- renderText({
    cores <- if (requireNamespace("future", quietly = TRUE)) {
      future::availableCores()
    } else {
      parallel::detectCores(logical = TRUE)
    }
    paste("Available cores on this machine:", cores)
  })
  
  output$policy_params_ui <- renderUI({
    
    if (input$policy_type == "CAS23") {
      tagList(
        selectInput("match_alg", "Matching algorithm",
                    choices = c("match_cas"), selected = "match_cas"),
        
        selectInput("wl_model", "Waitlist model",
                    choices = c("CAS23"), selected = "CAS23"),
        
        selectInput("post_tx_model", "Post-transplant model",
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
        selectInput("match_alg", "Matching algorithm",
                    choices = c("match_las"), selected = "match_las"),
        
        selectInput("wl_model", "Waitlist model",
                    choices = c("LAS15", "LAS21"), selected = input$policy_type),
        
        selectInput("post_tx_model", "Post-transplant model",
                    choices = c("LAS15", "LAS21"), selected = input$policy_type),
        
        numericInput("wl_weight", "Waitlist weight", value = 0.25, min = 0, step = 0.01),
        numericInput("wl_cap", "Waitlist cap", value = 365, min = 0, step = 1),
        numericInput("post_tx_weight", "Post-transplant weight", value = 0.25, min = 0, step = 0.01),
        numericInput("post_tx_cap", "Post-transplant cap", value = 1826, min = 0, step = 1),
        
        checkboxInput("checks", "Enable checks", value = TRUE)
      )
    }
  })
  
  make_policy <- function(policy_type, params, name = "Policy") {
    if (policy_type == "CAS23") {
      CASPolicy(
        name = name,
        params = list(
          match_alg = params$match_alg,
          wl_model = params$wl_model,
          post_tx_model = params$post_tx_model,
          wl_weight = params$wl_weight,
          wl_cap = params$wl_cap,
          post_tx_weight = params$post_tx_weight,
          post_tx_cap = params$post_tx_cap,
          bio_weight = params$bio_weight,
          peds_weight = params$peds_weight,
          pld_weight = params$pld_weight,
          efficiency_weight = params$efficiency_weight,
          abo_weight = params$abo_weight,
          height_weight = params$height_weight,
          cpra_weight = params$cpra_weight,
          cost_weight = params$cost_weight,
          distance_weight = params$distance_weight,
          checks = params$checks
        )
      )
    } else {
      LASPolicy(
        name = name,
        params = list(
          match_alg = params$match_alg,
          wl_model = params$wl_model,
          post_tx_model = params$post_tx_model,
          wl_weight = params$wl_weight,
          wl_cap = params$wl_cap,
          post_tx_weight = params$post_tx_weight,
          post_tx_cap = params$post_tx_cap,
          checks = params$checks
        )
      )
    }
  }
  
  refresh_experiment_selectors <- function(selected_saved = NULL,
                                           selected_compare_1 = NULL,
                                           selected_compare_2 = NULL) {
    
    exp_choices <- setNames(
      names(rv$experiments),
      vapply(rv$experiments, function(x) {
        paste0(x$label, " | ", x$policy_type, " | runs=", x$n_runs)
      }, character(1))
    )
    
    updateSelectInput(
      session,
      "saved_experiment",
      choices = exp_choices,
      selected = selected_saved
    )
    
    updateSelectInput(
      session,
      "compare_exp_1",
      choices = exp_choices,
      selected = selected_compare_1
    )
    
    updateSelectInput(
      session,
      "compare_exp_2",
      choices = exp_choices,
      selected = selected_compare_2
    )
  }
  
  current_policy_params <- reactive({
    if (input$policy_type == "CAS23") {
      list(
        match_alg = input$match_alg,
        wl_model = input$wl_model,
        post_tx_model = input$post_tx_model,
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
    } else {
      list(
        match_alg = input$match_alg,
        wl_model = input$wl_model,
        post_tx_model = input$post_tx_model,
        wl_weight = input$wl_weight,
        wl_cap = input$wl_cap,
        post_tx_weight = input$post_tx_weight,
        post_tx_cap = input$post_tx_cap,
        checks = input$checks
      )
    }
  })
  
  build_policy <- reactive({
    make_policy(
      policy_type = input$policy_type,
      params = current_policy_params(),
      name = paste(input$policy_type, "policy")
    )
  })
  
  make_experiment_record <- function(label, cfg, policy_type, policy_params, n_runs,
                                     population_settings = NULL,
                                     result = NULL, exp_result = NULL) {
    id <- paste0("exp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    list(
      id = id,
      label = label,
      created_at = Sys.time(),
      config = cfg,
      policy_type = policy_type,
      policy_params = policy_params,
      n_runs = n_runs,
      population_settings = population_settings,
      result = result,
      exp_result = exp_result
    )
  }
  
  get_experiment_metrics_df <- function(rec) {
    if (!is.null(rec$exp_result)) {
      df <- experimentMetrics(rec$exp_result)
      
      keep <- intersect(
        c("total_transplants", "waitlist_deaths", "post_tx_deaths", "non_used_donors"),
        names(df)
      )
      
      df[, keep, drop = FALSE]
      
    } else if (!is.null(rec$result)) {
      m <- metrics(rec$result)
      
      data.frame(
        total_transplants = as.numeric(m[["total_transplants"]]),
        waitlist_deaths = as.numeric(m[["waitlist_deaths"]]),
        post_tx_deaths = as.numeric(m[["post_tx_deaths"]]),
        non_used_donors = as.numeric(m[["non_used_donors"]]),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }
  
  get_recipient_data <- function(result_obj) {
    d <- getData(result_obj)
    rec <- d$recipient_database
    req(!is.null(rec), nrow(rec) > 0)
    rec
  }
  
  add_grouping_columns <- function(rec) {
    out <- rec
    
    if ("diag_group" %in% names(out)) {
      out$diag_group <- as.character(out$diag_group)
    }
    
    if ("blood_type" %in% names(out)) {
      out$blood_type <- as.character(out$blood_type)
    }
    
    if ("height" %in% names(out)) {
      out$height_cat <- cut(
        out$height,
        breaks = c(-Inf, 62.25, 64.99, 66.99, 70.00, Inf),
        labels = c("<=62.25", "62.26-64.99", "65.00-66.99", "67.00-70.00", ">70"),
        right = TRUE
      )
    }
    
    if ("age" %in% names(out)) {
      out$age_group <- cut(
        out$age,
        breaks = c(18, 35, 50, 65, Inf),
        labels = c("18-34", "35-49", "50-64", ">=65"),
        right = FALSE
      )
    }
    
    if ("sex" %in% names(out)) {
      out$sex <- as.character(out$sex)
    }
    
    if ("race_ethnicity" %in% names(out)) {
      out$race_ethnicity <- as.character(out$race_ethnicity)
    }
    
    if ("region" %in% names(out)) {
      out$region <- as.character(out$region)
    }
    
    if ("wlauc" %in% names(out)) {
      out$wlauc_cat <- cut(
        out$wlauc,
        breaks = c(-Inf, 250, 300, 325, 365),
        labels = c("<250", "250-299", "300-324", "325-365"),
        right = FALSE
      )
    }
    
    out
  }
  
  summary_by_group <- reactive({
    req(rv$result)
    
    rec <- get_recipient_data(rv$result)
    rec <- add_grouping_columns(rec)
    
    group_var <- input$stratify_by
    req(!is.null(group_var))
    
    # helper columns
    if (!"listing_day" %in% names(rec)) rec$listing_day <- NA_real_
    if (!"transplant_day" %in% names(rec)) rec$transplant_day <- NA_real_
    if (!"death_day" %in% names(rec)) rec$death_day <- NA_real_
    
    rec$was_transplanted <- !is.na(rec$transplant_day)
    rec$wait_time <- ifelse(rec$was_transplanted, rec$transplant_day - rec$listing_day, NA_real_)
    rec$waitlist_death <- !is.na(rec$death_day) & is.na(rec$transplant_day)
    
    make_summary <- function(df, label) {
      wt <- df$wait_time[!is.na(df$wait_time) & is.finite(df$wait_time) & df$wait_time >= 0]
      
      data.frame(
        group = label,
        starting_candidates = nrow(df),
        transplants = sum(df$was_transplanted, na.rm = TRUE),
        waitlist_deaths = sum(df$waitlist_death, na.rm = TRUE),
        median_wait_time = if (length(wt) > 0) median(wt) else NA_real_,
        iqr_wait_time = if (length(wt) > 0) IQR(wt) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
    
    if (group_var == "none") {
      return(make_summary(rec, "Total"))
    }
    
    req(group_var %in% names(rec))
    
    split_list <- split(rec, rec[[group_var]], drop = TRUE)
    
    do.call(
      rbind,
      lapply(names(split_list), function(g) {
        make_summary(split_list[[g]], g)
      })
    )
  })
  
  observeEvent(input$run_save_exp, {
    req(!rv$running)
    
    rv$running <- TRUE
    rv$status <- "Running simulation..."
    rv$result <- NULL
    rv$exp_result <- NULL
    
    shinyjs::disable("run_save_exp")
    shinyjs::html("run_save_exp", "Running...")
    
    on.exit({
      rv$running <- FALSE
      shinyjs::enable("run_save_exp")
      shinyjs::html("run_save_exp", "Run and Save Experiment")
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
      
      saved_result <- NULL
      saved_exp_result <- NULL
      
      withProgress(message = "Running COMET simulation...", value = 0, {
        
        incProgress(0.15, detail = "Building simulator")
        Sys.sleep(0.1)
        
        if (input$n_runs <= 1) {
          
          incProgress(0.5, detail = "Executing single simulation")
          saved_result <- run(sim)
          saved_exp_result <- NULL
          
          rv$result <- saved_result
          rv$exp_result <- NULL
          
          incProgress(0.95, detail = "Finalizing output")
          Sys.sleep(0.1)
          
          rv$status <- "Single simulation completed successfully."
          
        } else {
          
          incProgress(0.3, detail = "Building experiment")
          
          if (input$n_runs > 1 && isTRUE(input$parallel)) {
            validate(
              need(!is.null(input$workers) && input$workers >= 1, "Workers must be at least 1."),
              need(input$workers <= input$n_runs, "Workers should not exceed the number of runs.")
            )
          }
          
          workers_to_use <- NULL
          
          if (input$n_runs > 1 && isTRUE(input$parallel)) {
            workers_to_use <- min(as.integer(input$workers), as.integer(input$n_runs))
          }
          
          exp <- COMETExperiment(
            simulator = sim,
            n_runs = input$n_runs,
            seeds = input$seed,
            parallel = isTRUE(input$parallel),
            workers = workers_to_use
          )
          
          incProgress(0.6, detail = "Executing experiment")
          saved_exp_result <- runExperiment(exp)
          saved_result <- getResults(saved_exp_result)[[1]]
          
          rv$exp_result <- saved_exp_result
          rv$result <- saved_result
          
          res_tbl <- getResultTable(saved_exp_result)
          
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
      
      rec <- make_experiment_record(
        label = input$experiment_label,
        cfg = cfg,
        policy_type = input$policy_type,
        policy_params = current_policy_params(),
        n_runs = input$n_runs,
        population_settings = list(
          candidate_mode = input$candidate_mode,
          diag_group = if (!is.null(input$diag_group)) input$diag_group else NA_character_,
          blood_type = if (!is.null(input$blood_type)) input$blood_type else NA_character_,
          height_cat = if (!is.null(input$height_cat)) input$height_cat else NA_character_,
          wlauc_cat = if (!is.null(input$wlauc_cat)) input$wlauc_cat else NA_character_
        ),
        result = saved_result,
        exp_result = saved_exp_result
      )
      
      rv$experiments[[rec$id]] <- rec
      rv$active_experiment_id <- rec$id
      
      existing_ids <- names(rv$experiments)
      
      default_compare_1 <- rec$id
      
      default_compare_2 <- if (length(existing_ids) >= 2) {
        existing_ids[existing_ids != rec$id][1]
      } else {
        rec$id
      }
      
      refresh_experiment_selectors(
        selected_saved = rec$id,
        selected_compare_1 = default_compare_1,
        selected_compare_2 = default_compare_2
      )
      
    }, error = function(e) {
      rv$status <- paste("Error:", conditionMessage(e))
      print(e)
    })
  })
  
  observeEvent(input$load_saved_exp, {
    req(input$saved_experiment)
    req(input$saved_experiment %in% names(rv$experiments))
    
    rec <- rv$experiments[[input$saved_experiment]]
    
    rv$active_experiment_id <- rec$id
    rv$result <- rec$result
    rv$exp_result <- rec$exp_result
    
    if (!is.null(rec$exp_result)) {
      res_tbl <- getResultTable(rec$exp_result)
      
      updateSelectInput(
        session,
        "selected_run",
        choices = as.character(res_tbl$run_id),
        selected = "1"
      )
    } else {
      updateSelectInput(
        session,
        "selected_run",
        choices = character(0),
        selected = character(0)
      )
    }
    
    rv$status <- paste("Loaded saved experiment:", rec$label)
  })
  
  observeEvent(input$selected_run, {
    req(rv$exp_result)
    
    res_list <- getResults(rv$exp_result)
    req(length(res_list) > 0)
    
    idx <- suppressWarnings(as.integer(input$selected_run))
    req(!is.na(idx))
    req(idx >= 1, idx <= length(res_list))
    
    rv$result <- res_list[[idx]]
  })
  
  comparison_records <- reactive({
    req(input$compare_exp_1, input$compare_exp_2)
    req(input$compare_exp_1 %in% names(rv$experiments))
    req(input$compare_exp_2 %in% names(rv$experiments))
    
    list(
      exp1 = rv$experiments[[input$compare_exp_1]],
      exp2 = rv$experiments[[input$compare_exp_2]]
    )
  })
  
  comparison_summary_table <- reactive({
    comps <- comparison_records()
    
    rec1 <- comps$exp1
    rec2 <- comps$exp2
    
    df1 <- get_experiment_metrics_df(rec1)
    df2 <- get_experiment_metrics_df(rec2)
    
    req(!is.null(df1), !is.null(df2))
    
    metric_names <- intersect(names(df1), names(df2))
    metric_names <- metric_names[metric_names %in% c(
      "total_transplants",
      "waitlist_deaths",
      "post_tx_deaths",
      "non_used_donors"
    )]
    
    metric_label <- function(x) {
      switch(
        x,
        total_transplants = "Total transplants",
        waitlist_deaths = "Waitlist deaths",
        post_tx_deaths = "Post-transplant deaths",
        non_used_donors = "Unused donors",
        x
      )
    }
    
    mean_or_value <- function(df, col) {
      mean(df[[col]], na.rm = TRUE)
    }
    
    data.frame(
      metric = vapply(metric_names, metric_label, character(1)),
      experiment_1 = vapply(metric_names, function(m) mean_or_value(df1, m), numeric(1)),
      experiment_2 = vapply(metric_names, function(m) mean_or_value(df2, m), numeric(1)),
      difference = vapply(metric_names, function(m) {
        mean_or_value(df1, m) - mean_or_value(df2, m)
      }, numeric(1)),
      stringsAsFactors = FALSE
    )
  })
  
  comparison_plot_df <- reactive({
    comps <- comparison_records()
    
    rec1 <- comps$exp1
    rec2 <- comps$exp2
    
    df1 <- get_experiment_metrics_df(rec1)
    df2 <- get_experiment_metrics_df(rec2)
    
    req(!is.null(df1), !is.null(df2))
    req(input$compare_metric %in% names(df1))
    req(input$compare_metric %in% names(df2))
    
    data.frame(
      value = c(df1[[input$compare_metric]], df2[[input$compare_metric]]),
      experiment = c(
        rep(rec1$label, nrow(df1)),
        rep(rec2$label, nrow(df2))
      ),
      stringsAsFactors = FALSE
    )
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
    surv_days <- survival_days_data()
    
    hist(
      surv_days,
      breaks = 30,
      probability = TRUE,
      col = "darkgreen",
      border = "white",
      main = "Post-Transplant Survival Distribution",
      xlab = "Days survived after transplant"
    )
    
    lines(density(surv_days), lwd = 2)
    abline(v = mean(surv_days), lty = 2, lwd = 2)
  })
  
  output$survival_hist <- renderPlot({
    surv_days <- survival_days_data()
    
    hist(
      surv_days,
      breaks = 30,
      col = "darkgreen",
      border = "white",
      main = "Observed Survival Days After Transplant",
      xlab = "Days after transplant"
    )
  })
  
  output$survival_stats <- renderPrint({
    surv_days <- survival_days_data()
    
    cat("Observed post-transplant deaths:", length(surv_days), "\n")
    cat("Average survival:", round(mean(surv_days), 2), "days\n")
    cat("Median survival:", round(median(surv_days), 2), "days\n")
    cat("Standard deviation:", round(stats::sd(surv_days), 2), "days\n")
    cat("Max survival:", round(max(surv_days), 2), "days\n")
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
    if (!is.null(rv$exp_result)) {
      summary(rv$exp_result)
    } else if (!is.null(rv$result)) {
      summary(rv$result)
    }
  })
  
  output$experiment_metrics_table <- renderTable({
    req(rv$exp_result)
    experimentMetrics(rv$exp_result)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$saved_experiments_table <- renderTable({
    req(length(rv$experiments) > 0)
    
    data.frame(
      id = vapply(rv$experiments, function(x) x$id, character(1)),
      label = vapply(rv$experiments, function(x) x$label, character(1)),
      policy_type = vapply(rv$experiments, function(x) x$policy_type, character(1)),
      n_runs = vapply(rv$experiments, function(x) x$n_runs, numeric(1)),
      candidate_mode = vapply(
        rv$experiments,
        function(x) {
          if (is.null(x$population_settings)) return(NA_character_)
          x$population_settings$candidate_mode
        },
        character(1)
      ),
      diagnosis_group = vapply(
        rv$experiments,
        function(x) {
          if (is.null(x$population_settings)) return(NA_character_)
          x$population_settings$diag_group
        },
        character(1)
      ),
      blood_type = vapply(
        rv$experiments,
        function(x) {
          if (is.null(x$population_settings)) return(NA_character_)
          x$population_settings$blood_type
        },
        character(1)
      ),
      height_cat = vapply(
        rv$experiments,
        function(x) {
          if (is.null(x$population_settings)) return(NA_character_)
          x$population_settings$height_cat
        },
        character(1)
      ),
      wlauc_cat = vapply(
        rv$experiments,
        function(x) {
          if (is.null(x$population_settings)) return(NA_character_)
          x$population_settings$wlauc_cat
        },
        character(1)
      ),
      created_at = vapply(rv$experiments, function(x) as.character(x$created_at), character(1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$population_summary_table <- renderTable({
    summary_by_group()
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$population_settings_out <- renderPrint({
    req(rv$active_experiment_id)
    req(rv$active_experiment_id %in% names(rv$experiments))
    
    rec <- rv$experiments[[rv$active_experiment_id]]
    ps <- rec$population_settings
    
    if (is.null(ps)) {
      cat("No population settings stored.\n")
    } else {
      print(ps)
    }
  })
  
  output$comparison_summary_table <- renderTable({
    comparison_summary_table()
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$comparison_boxplot <- renderPlot({
    df <- comparison_plot_df()
    
    boxplot(
      value ~ experiment,
      data = df,
      main = paste("Comparison of", input$compare_metric),
      ylab = input$compare_metric
    )
  })
  
}

shinyApp(ui, server)
