library(shiny)

# -------------------------------------------------------------------------
# Saved CAS data
# -------------------------------------------------------------------------
app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

find_data_file <- function(candidates) {
  for (nm in candidates) {
    p <- file.path(app_dir, nm)
    if (file.exists(p)) return(p)
  }
  stop("Missing data file. Expected one of: ", paste(candidates, collapse = ", "))
}

params_file <- find_data_file(c("params0.csv", "params0(1).csv"))
skew_file   <- find_data_file(c("params0_skew.csv", "params0_skew(1).csv"))

params <- read.csv(params_file, stringsAsFactors = FALSE, check.names = FALSE)
skew   <- read.csv(skew_file, stringsAsFactors = FALSE, check.names = FALSE)

weight_cols <- c(
  "wl_weight", "post_tx_weight", "abo_weight", "height_weight",
  "efficiency_weight", "cpra_weight", "peds_weight", "pld_weight"
)

weight_labels <- c(
  wl_weight = "Waitlist weight",
  post_tx_weight = "Post-transplant weight",
  abo_weight = "ABO weight",
  height_weight = "Height weight",
  efficiency_weight = "Efficiency weight",
  cpra_weight = "cPRA weight",
  peds_weight = "Pediatric weight",
  pld_weight = "Prior living donor weight"
)

required_params <- c(weight_cols, "params_1")
required_skew <- c("xi", "omega", "alpha", "mods_id", "name", "value", "params_1")

if (!all(required_params %in% names(params))) {
  stop("params0.csv is missing required columns: ",
       paste(setdiff(required_params, names(params)), collapse = ", "))
}
if (!all(required_skew %in% names(skew))) {
  stop("params0_skew.csv is missing required columns: ",
       paste(setdiff(required_skew, names(skew)), collapse = ", "))
}

for (w in weight_cols) params[[w]] <- as.numeric(params[[w]])
params$params_1 <- as.character(params$params_1)
skew$params_1 <- as.character(skew$params_1)
skew$xi <- as.numeric(skew$xi)
skew$omega <- as.numeric(skew$omega)
skew$alpha <- as.numeric(skew$alpha)

params <- params[!duplicated(params$params_1), , drop = FALSE]
weight_min <- sapply(params[weight_cols], min, na.rm = TRUE)
weight_max <- sapply(params[weight_cols], max, na.rm = TRUE)
adjustable_weights <- weight_cols[abs(weight_max - weight_min) > 1e-8]
fixed_weights <- setdiff(weight_cols, adjustable_weights)
initial_row <- params[1, , drop = FALSE]

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------
round_step <- function(x, step = 0.05) round(x / step) * step

find_exact_parameter_row <- function(values, tolerance = 1e-8) {
  matches <- rep(TRUE, nrow(params))
  for (w in weight_cols) matches <- matches & abs(params[[w]] - values[[w]]) < tolerance
  out <- params[matches, , drop = FALSE]
  if (nrow(out) == 0) NULL else out[1, , drop = FALSE]
}

skew_pdf <- function(x, xi, omega, alpha) {
  if (!is.finite(omega) || omega <= 0) return(rep(NA_real_, length(x)))
  z <- (x - xi) / omega
  2 / omega * dnorm(z) * pnorm(alpha * z)
}

skew_mean <- function(xi, omega, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  xi + omega * delta * sqrt(2 / pi)
}

skew_sd <- function(omega, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  omega * sqrt(1 - 2 * delta^2 / pi)
}

safe_range <- function(dat) {
  lo <- min(dat$xi - 4 * dat$omega, na.rm = TRUE)
  hi <- max(dat$xi + 4 * dat$omega, na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) c(-1, 1) else c(lo, hi)
}

display_name <- function(x) {
  mapping <- c(
    ov = "Overall",
    dx_grp = "Diagnosis group",
    hgt_cat = "Height category",
    abo = "Blood type",
    age_cat = "Age category",
    male = "Sex",
    reg = "Region",
    wlauc_cat = "WLAUC category"
  )
  ifelse(x %in% names(mapping), unname(mapping[x]), gsub("_", " ", x))
}

display_mod <- function(x) {
  mapping <- c(
    can_count = "Candidate count",
    tx_count = "Transplant count",
    wait_death = "Waitlist deaths",
    wld_ppy = "Waitlist deaths per patient-year",
    tx_ppy = "Transplants per patient-year",
    med_wlt = "Median waitlist time",
    med_dist = "Median distance",
    post_tx_death = "Post-transplant deaths",
    ptd_ppy = "Post-transplant deaths per patient-year",
    med_offer = "Median offers"
  )
  ifelse(x %in% names(mapping), unname(mapping[x]), gsub("_", " ", x))
}

format_result_table <- function(d) {
  if (nrow(d) == 0) return(d)
  data.frame(
    Bracket = as.character(d$value),
    `Expected mean` = skew_mean(d$xi, d$omega, d$alpha),
    `Estimated SD` = skew_sd(d$omega, d$alpha),
    `Location parameter` = d$xi,
    `Scale parameter` = d$omega,
    `Shape / skewness` = d$alpha,
    check.names = FALSE
  )
}

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- navbarPage(
  id = "main_nav",
  title = "COMET CAS Saved Results",

  tabPanel(
    title = "Run Experiment", value = "run",
    fluidPage(
      tags$style(HTML("
        .weight-status {position: sticky; top: 15px;}
        .status-good {border-left: 5px solid #2e7d32;}
        .status-warn {border-left: 5px solid #c62828;}
        .result-card {background:#fff;border:1px solid #ddd;border-radius:6px;padding:15px;margin-bottom:15px;}
        .summary-card {background:#f7f9fb;border:1px solid #d9e1e8;border-radius:6px;padding:9px 11px;margin-bottom:8px;}
        .summary-card h4 {margin:2px 0 6px 0;font-size:18px;}
        .summary-card .table {margin-bottom:0;font-size:13px;}
        .summary-card .table > thead > tr > th,
        .summary-card .table > tbody > tr > td {padding:4px 7px;line-height:1.15;}
        .requirements-card {background:#fffdf4;border:1px solid #eadca6;border-radius:6px;padding:8px 11px;margin-top:8px;font-size:13px;}
        .requirements-card ul {margin:3px 0 0 0;padding-left:18px;}
        .requirements-card li {margin-bottom:2px;line-height:1.25;}
        .weight-status .well {margin-bottom:8px;padding:10px 14px;}
        .weight-status .well h4 {margin:0 0 5px 0;font-size:22px;}
        .weight-status .well p {margin:2px 0;}
      ")),
      h3("Run a saved CAS experiment"),
      fluidRow(
        column(
          width = 8,
          textInput("experiment_label", "Experiment name", value = "Experiment 1"),
          h4("CAS weights"),
          uiOutput("weight_sliders"),
          actionButton(
            "see_result",
            "See Result",
            class = "btn-primary",
            style = "width:100%;font-size:16px;"
          ),
          tags$br(), tags$br(),
          uiOutput("run_message")
        ),
        column(
          width = 4,
          tags$div(
            class = "weight-status",
            uiOutput("weight_status_box"),
            tags$div(
              class = "summary-card",
              h4("Current weight settings"),
              tableOutput("current_weights_table")
            ),
            tags$div(
              class = "requirements-card",
              tags$strong("Requirements"),
              tags$ul(
                tags$li("All weights must sum to exactly 1.00."),
                tags$li("ABO weight plus Height weight cannot exceed 0.30."),
                tags$li("A weight combination can only run if it exists in the precomputed results.")
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Result Detail", value = "results",
    fluidPage(
      conditionalPanel(
        condition = "!output.has_current_experiment",
        tags$div(style = "margin-top:30px;color:#666;font-size:16px;", "No experiment is loaded. Create or load an experiment first.")
      ),
      conditionalPanel(
        condition = "output.has_current_experiment",
        h3(textOutput("result_title", inline = TRUE)),
        uiOutput("result_weight_summary"),
        tags$hr(),
        fluidRow(
          column(
            width = 4,
            div(class = "result-card",
                selectInput("result_name", "Stratified by", choices = NULL),
                selectInput("result_mod", "Outcome", choices = NULL))
          ),
          column(
            width = 8,
            div(class = "result-card",
                h4("Distribution summary"),
                tableOutput("result_summary_table"))
          )
        ),
        div(class = "result-card", plotOutput("result_distribution_plot", height = "540px"))
      )
    )
  ),

  tabPanel(
    title = "Saved Experiments", value = "saved",
    fluidPage(
      fluidRow(
        column(
          width = 3,
          h3("Open saved experiment"),
          selectInput("saved_experiment", "Experiment", choices = character(0)),
          actionButton("load_saved_exp", "Open Result Detail", class = "btn-primary")
        ),
        column(
          width = 9,
          h3("Saved experiment registry"),
          tableOutput("saved_experiments_table"),
          tags$p(class = "text-muted", "Experiments are stored for the current Shiny session.")
        )
      )
    )
  ),

  tabPanel(
    title = "Experiment Comparison", value = "comparison",
    fluidPage(
      h3("Compare saved experiments"),
      fluidRow(
        column(
          width = 4,
          selectInput("comparison_experiments", "Experiments", choices = character(0), multiple = TRUE),
          selectInput("comparison_name", "Stratified by", choices = NULL),
          selectInput("comparison_mod", "Outcome", choices = NULL)
        ),
        column(
          width = 8,
          tags$div(class = "result-card",
                   h4("Expected outcome by bracket"),
                   tags$p(class = "text-muted", "Rows are population brackets; columns are selected experiments. Values are fitted skew-normal expected means."),
                   tableOutput("comparison_table"))
        )
      )
    )
  )
)

# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  saved_experiments <- reactiveVal(list())
  current_experiment_key <- reactiveVal(NULL)
  run_message_text <- reactiveVal(NULL)

  output$weight_sliders <- renderUI({
    tagList(lapply(adjustable_weights, function(w) {
      sliderInput(
        inputId = paste0("weight_", w),
        label = weight_labels[[w]],
        min = unname(weight_min[[w]]),
        max = unname(weight_max[[w]]),
        value = as.numeric(initial_row[[w]]),
        step = 0.05,
        ticks = TRUE
      )
    }))
  })

  current_slider_values <- function() {
    vals <- setNames(numeric(length(weight_cols)), weight_cols)
    for (w in weight_cols) {
      if (w %in% fixed_weights) {
        vals[[w]] <- as.numeric(weight_min[[w]])
      } else {
        v <- input[[paste0("weight_", w)]]
        vals[[w]] <- if (is.null(v) || !is.finite(v)) as.numeric(initial_row[[w]]) else round_step(as.numeric(v))
      }
    }
    vals
  }

  apply_row_to_sliders <- function(row) {
    for (w in adjustable_weights) updateSliderInput(session, paste0("weight_", w), value = as.numeric(row[[w]]))
  }

  output$weight_status_box <- renderUI({
    values <- current_slider_values()
    total <- sum(values, na.rm = TRUE)
    abo_height_total <- values[["abo_weight"]] + values[["height_weight"]]
    total_valid <- abs(total - 1) < 1e-8
    abo_height_valid <- abo_height_total <= 0.30 + 1e-8
    exact <- if (total_valid && abo_height_valid) find_exact_parameter_row(values) else NULL
    good <- total_valid && abo_height_valid && !is.null(exact)

    warnings <- list()
    if (!abo_height_valid) {
      warnings <- append(warnings, list(tags$div(
        style = "margin-top:4px;color:#b71c1c;",
        tags$strong("ABO + Height exceeds 0.30.")
      )))
    }
    if (total_valid && abo_height_valid && is.null(exact)) {
      warnings <- append(warnings, list(tags$div(
        style = "margin-top:4px;color:#b71c1c;",
        tags$strong("This combination is not available in the precomputed results.")
      )))
    }

    status_content <- if (good) {
      tags$div(style = "margin-top:4px;color:#2e7d32;", "Ready to view and save.")
    } else if (length(warnings) > 0) {
      tagList(warnings)
    } else {
      tags$div(style = "margin-top:4px;color:#666;", "Adjust the total to 1.00.")
    }

    tags$div(
      class = paste("well", if (good) "status-good" else "status-warn"),
      h4(sprintf("Total: %.2f", total)),
      tags$p(tags$strong("ABO + Height: "), sprintf("%.2f / 0.30", abo_height_total)),
      status_content
    )
  })

  output$current_weights_table <- renderTable({
    values <- current_slider_values()
    data.frame(Weight = unname(weight_labels[weight_cols]), Value = sprintf("%.2f", values[weight_cols]), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$run_message <- renderUI({
    msg <- run_message_text()
    if (is.null(msg)) return(NULL)
    tags$div(class = "alert alert-success", msg)
  })

  update_experiment_choices <- function(exps, selected = NULL) {
    choices <- setNames(names(exps), vapply(exps, function(x) x$label, character(1)))
    updateSelectInput(session, "saved_experiment", choices = choices, selected = selected)
    updateSelectInput(session, "comparison_experiments", choices = choices,
                      selected = intersect(isolate(input$comparison_experiments), names(exps)))
  }

  observeEvent(input$see_result, {
    label <- trimws(input$experiment_label)
    if (!nzchar(label)) label <- paste("Experiment", length(saved_experiments()) + 1)

    values <- current_slider_values()
    total <- sum(values, na.rm = TRUE)
    abo_height_total <- values[["abo_weight"]] + values[["height_weight"]]

    if (abo_height_total > 0.30 + 1e-8) {
      showNotification(
        paste0("ABO weight + Height weight is ", sprintf("%.2f", abo_height_total),
               ". Their combined value cannot exceed 0.30."),
        type = "error", duration = 7
      )
      return()
    }

    if (abs(total - 1) > 1e-8) {
      showNotification(paste0("The selected weights total ", sprintf("%.2f", total), ". They must total exactly 1.00."), type = "error", duration = 6)
      return()
    }

    row <- find_exact_parameter_row(values)
    if (is.null(row)) {
      showNotification("These weights total 1.00, but they do not match a precomputed combination.", type = "error", duration = 7)
      return()
    }

    key <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(99999, 1))
    exp <- list(
      key = key,
      label = label,
      params_1 = as.character(row$params_1[[1]]),
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      weights = values
    )
    exps <- saved_experiments()
    exps[[key]] <- exp
    saved_experiments(exps)
    current_experiment_key(key)
    update_experiment_choices(exps, selected = key)
    run_message_text(paste0("Saved ", label, "."))
    updateNavbarPage(session, "main_nav", selected = "results")
  })

  current_experiment <- reactive({
    key <- current_experiment_key()
    exps <- saved_experiments()
    if (is.null(key) || is.null(exps[[key]])) return(NULL)
    exps[[key]]
  })

  output$has_current_experiment <- reactive(!is.null(current_experiment()))
  outputOptions(output, "has_current_experiment", suspendWhenHidden = FALSE)

  output$result_title <- renderText({
    exp <- current_experiment(); req(exp)
    paste0(exp$label, " — CAS Saved Results")
  })

  output$result_weight_summary <- renderUI({
    exp <- current_experiment(); req(exp)
    tags$div(class = "well",
             tags$strong("Saved: "), exp$created,
             tags$br(),
             paste(paste0(unname(weight_labels[names(exp$weights)]), " = ", sprintf("%.2f", exp$weights)), collapse = "  |  ")
    )
  })

  experiment_skew <- reactive({
    exp <- current_experiment(); req(exp)
    out <- skew[skew$params_1 == exp$params_1, , drop = FALSE]
    validate(need(nrow(out) > 0, "No fitted distributions were found for this experiment."))
    out
  })

  observeEvent(experiment_skew(), {
    dat <- experiment_skew()
    names_available <- unique(as.character(dat$name))
    choices <- setNames(names_available, vapply(names_available, display_name, character(1)))
    selected <- if ("ov" %in% names_available) "ov" else names_available[1]
    updateSelectInput(session, "result_name", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(list(experiment_skew(), input$result_name), {
    req(input$result_name)
    dat <- experiment_skew()
    mods <- unique(as.character(dat$mods_id[dat$name == input$result_name]))
    choices <- setNames(mods, vapply(mods, display_mod, character(1)))
    selected <- if (!is.null(isolate(input$result_mod)) && isolate(input$result_mod) %in% mods) isolate(input$result_mod) else mods[1]
    updateSelectInput(session, "result_mod", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  selected_result_data <- reactive({
    req(input$result_name, input$result_mod)
    d <- experiment_skew()
    d[d$name == input$result_name & d$mods_id == input$result_mod, , drop = FALSE]
  })

  output$result_summary_table <- renderTable({
    d <- selected_result_data()
    validate(need(nrow(d) > 0, "No summary is available."))
    format_result_table(d)
  }, digits = 4, striped = TRUE, bordered = TRUE, spacing = "s")

  output$result_distribution_plot <- renderPlot({
    d <- selected_result_data()
    validate(need(nrow(d) > 0, "No distributions are available."))
    xr <- safe_range(d)
    x <- seq(xr[1], xr[2], length.out = 700)
    group_values <- unique(as.character(d$value))
    cols <- grDevices::hcl.colors(max(length(group_values), 3), "Dark 3")[seq_along(group_values)]
    ltys <- seq_along(group_values)
    curves <- lapply(group_values, function(g) {
      one <- d[d$value == g, , drop = FALSE]
      skew_pdf(x, one$xi[1], one$omega[1], one$alpha[1])
    })
    ymax <- max(unlist(curves), na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1

    plot(x, curves[[1]], type = "l", lwd = 2, col = cols[1], lty = ltys[1],
         ylim = c(0, ymax * 1.08), xlab = display_mod(input$result_mod), ylab = "Density",
         main = paste(display_name(input$result_name), "—", display_mod(input$result_mod)))
    if (length(curves) > 1) for (j in 2:length(curves)) lines(x, curves[[j]], lwd = 2, col = cols[j], lty = ltys[j])
    legend("topright", legend = group_values, col = cols, lty = ltys, lwd = 2,
           title = if (input$result_name == "ov") "Result" else "Bracket", bty = "n", cex = 0.9)
    grid()
  })

  output$saved_experiments_table <- renderTable({
    exps <- saved_experiments()
    if (length(exps) == 0) return(data.frame(Message = "No experiments saved in this session."))
    do.call(rbind, lapply(exps, function(exp) {
      row <- data.frame(Experiment = exp$label, Saved = exp$created, check.names = FALSE)
      for (w in weight_cols) row[[weight_labels[[w]]]] <- sprintf("%.2f", exp$weights[[w]])
      row
    }))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  observeEvent(input$load_saved_exp, {
    req(input$saved_experiment)
    exps <- saved_experiments(); req(exps[[input$saved_experiment]])
    current_experiment_key(input$saved_experiment)
    exp <- exps[[input$saved_experiment]]
    row <- params[params$params_1 == exp$params_1, , drop = FALSE]
    if (nrow(row) > 0) apply_row_to_sliders(row[1, , drop = FALSE])
    updateNavbarPage(session, "main_nav", selected = "results")
  })

  observe({
    exps <- saved_experiments()
    req(length(exps) > 0)
    selected_keys <- input$comparison_experiments
    if (is.null(selected_keys) || length(selected_keys) == 0) return()
    ids <- vapply(exps[selected_keys], function(x) x$params_1, character(1))
    dat <- skew[skew$params_1 %in% ids, , drop = FALSE]
    names_available <- unique(as.character(dat$name))
    choices <- setNames(names_available, vapply(names_available, display_name, character(1)))
    selected <- if (!is.null(input$comparison_name) && input$comparison_name %in% names_available) input$comparison_name else names_available[1]
    updateSelectInput(session, "comparison_name", choices = choices, selected = selected)
  })

  observeEvent(list(input$comparison_experiments, input$comparison_name), {
    exps <- saved_experiments()
    req(length(input$comparison_experiments) > 0, input$comparison_name)
    ids <- vapply(exps[input$comparison_experiments], function(x) x$params_1, character(1))
    dat <- skew[skew$params_1 %in% ids & skew$name == input$comparison_name, , drop = FALSE]
    mods <- unique(as.character(dat$mods_id))
    choices <- setNames(mods, vapply(mods, display_mod, character(1)))
    selected <- if (!is.null(input$comparison_mod) && input$comparison_mod %in% mods) input$comparison_mod else mods[1]
    updateSelectInput(session, "comparison_mod", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  output$comparison_table <- renderTable({
    exps <- saved_experiments()
    req(length(input$comparison_experiments) > 0, input$comparison_name, input$comparison_mod)

    selected_exps <- exps[input$comparison_experiments]
    all_values <- unique(as.character(skew$value[
      skew$params_1 %in% vapply(selected_exps, function(x) x$params_1, character(1)) &
        skew$name == input$comparison_name & skew$mods_id == input$comparison_mod
    ]))
    validate(need(length(all_values) > 0, "No comparison data are available."))

    out <- data.frame(Bracket = all_values, stringsAsFactors = FALSE, check.names = FALSE)
    for (exp in selected_exps) {
      d <- skew[skew$params_1 == exp$params_1 & skew$name == input$comparison_name &
                  skew$mods_id == input$comparison_mod, , drop = FALSE]
      means <- skew_mean(d$xi, d$omega, d$alpha)
      lookup <- setNames(means, as.character(d$value))
      out[[exp$label]] <- unname(lookup[out$Bracket])
    }
    out
  }, digits = 4, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui, server)
