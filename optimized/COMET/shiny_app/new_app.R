
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

# Remove accidental duplicate parameter rows while preserving the first.
params <- params[!duplicated(params$params_1), , drop = FALSE]

weight_min <- sapply(params[weight_cols], min, na.rm = TRUE)
weight_max <- sapply(params[weight_cols], max, na.rm = TRUE)

# Initial configuration: first row in params0.csv.
initial_row <- params[1, , drop = FALSE]

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------
round_step <- function(x, step = 0.05) round(x / step) * step

find_exact_parameter_row <- function(values, tolerance = 1e-8) {
  matches <- rep(TRUE, nrow(params))
  for (w in weight_cols) {
    matches <- matches & abs(params[[w]] - values[[w]]) < tolerance
  }
  out <- params[matches, , drop = FALSE]
  if (nrow(out) == 0) NULL else out[1, , drop = FALSE]
}
skew_pdf <- function(x, xi, omega, alpha) {
  if (!is.finite(omega) || omega <= 0) return(rep(NA_real_, length(x)))
  z <- (x - xi) / omega
  2 / omega * dnorm(z) * pnorm(alpha * z)
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

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- navbarPage(
  id = "main_nav",
  title = "COMET CAS Saved Results",

  tabPanel(
    title = "Run Experiment",
    value = "run",
    fluidPage(
      fluidRow(
        column(
          width = 5,
          h3("Experiment Settings"),
          textInput("experiment_label", "Experiment name", value = "Experiment 1"),
          tags$p(
            class = "text-muted",
            "CAS is the only available score model. Set the weights manually; a result can be saved only when the total is 1.00 and the combination exists in params0.csv."
          ),
          tags$hr(),
          h3("CAS Weights"),
          uiOutput("weight_sliders"),
          tags$div(
            style = "margin-top:12px;padding:10px;border-radius:4px;background:#f5f5f5;",
            strong("Weight total: "),
            textOutput("weight_sum", inline = TRUE),
            tags$br(),
            strong("Matching parameter ID: "),
            textOutput("matched_param_id", inline = TRUE)
          ),
          tags$br(),
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
          width = 7,
          h3("How this page works"),
          tags$p("Move any weight slider in increments of 0.05."),
          tags$p(
            "While the total is below or equal to 1.00, every slider moves independently. This makes it easy to lower all weights and rebuild the configuration."
          ),
          tags$p(
            "If a change pushes the total above 1.00, the app automatically reduces the other adjustable weights in 0.05 steps until the total returns to 1.00."
          ),
          tags$p(
            "Press See Result only after the total is exactly 1.00. The selected weights must also match a precomputed row in params0.csv."
          ),
          tags$hr(),
          h4("Current CAS configuration"),
          tableOutput("current_weights_table")
        )
      )
    )
  ),

  tabPanel(
    title = "Result Detail",
    value = "results",
    fluidPage(
      conditionalPanel(
        condition = "!output.has_current_experiment",
        tags$div(
          style = "margin-top:30px;color:#666;font-size:16px;",
          "No experiment is loaded. Create or load an experiment first."
        )
      ),
      conditionalPanel(
        condition = "output.has_current_experiment",
        fluidRow(
          column(
            width = 12,
            h3(textOutput("result_title", inline = TRUE)),
            uiOutput("result_parameter_summary"),
            tags$hr(),
            selectInput("result_name", "Population category (name)", choices = NULL),
            uiOutput("mods_tabs")
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Saved Experiments",
    value = "saved",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Open Saved Experiment"),
          selectInput("saved_experiment", "Experiment", choices = character(0)),
          actionButton("load_saved_exp", "Open Result Detail", class = "btn-primary")
        ),
        column(
          width = 8,
          h3("Saved Experiment Registry"),
          tableOutput("saved_experiments_table"),
          tags$p(
            class = "text-muted",
            "Experiments are stored for the current Shiny session."
          )
        )
      )
    )
  )
)

# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  updating_sliders <- reactiveVal(FALSE)
  saved_experiments <- reactiveVal(list())
  current_experiment_key <- reactiveVal(NULL)
  run_message_text <- reactiveVal(NULL)

  output$weight_sliders <- renderUI({
    tagList(lapply(weight_cols, function(w) {
      mn <- unname(weight_min[[w]])
      mx <- unname(weight_max[[w]])
      val <- as.numeric(initial_row[[w]])

      # sliderInput supports equal endpoints; CSS makes fixed sliders visibly inactive.
      wrapper_style <- if (abs(mx - mn) < 1e-8) "opacity:0.65;pointer-events:none;" else ""
      tags$div(
        style = wrapper_style,
        sliderInput(
          inputId = paste0("weight_", w),
          label = paste0(weight_labels[[w]], if (abs(mx - mn) < 1e-8) " (fixed)" else ""),
          min = mn,
          max = mx,
          value = val,
          step = 0.05,
          ticks = TRUE
        )
      )
    }))
  })

  current_slider_values <- function() {
    vals <- setNames(numeric(length(weight_cols)), weight_cols)
    for (w in weight_cols) {
      v <- input[[paste0("weight_", w)]]
      vals[[w]] <- if (is.null(v) || !is.finite(v)) {
        as.numeric(initial_row[[w]])
      } else {
        round_step(as.numeric(v))
      }
    }
    vals
  }

  apply_values_to_sliders <- function(values) {
    updating_sliders(TRUE)
    for (w in weight_cols) {
      updateSliderInput(session, paste0("weight_", w), value = as.numeric(values[[w]]))
    }
    session$onFlushed(function() updating_sliders(FALSE), once = TRUE)
  }

  apply_row_to_sliders <- function(row) {
    values <- setNames(as.numeric(row[1, weight_cols]), weight_cols)
    apply_values_to_sliders(values)
  }

  # Sliders remain independent while the total is <= 1.00.
  # If a change pushes the total above 1.00, preserve the changed slider and
  # reduce the other adjustable sliders in 0.05 steps, starting with the
  # currently largest reducible weight.
  lapply(weight_cols, function(w_local) {
    observeEvent(input[[paste0("weight_", w_local)]], {
      if (updating_sliders()) return()

      values <- current_slider_values()
      total <- sum(values, na.rm = TRUE)

      if (total > 1 + 1e-8) {
        excess_steps <- as.integer(round((total - 1) / 0.05))
        other_weights <- setdiff(weight_cols, w_local)

        while (excess_steps > 0) {
          reducible <- other_weights[
            vapply(other_weights, function(w) {
              values[[w]] - weight_min[[w]] >= 0.05 - 1e-8
            }, logical(1))
          ]

          if (length(reducible) == 0) break

          # Reduce the largest currently adjustable weight first.
          target <- reducible[which.max(values[reducible] - weight_min[reducible])]
          values[[target]] <- round_step(values[[target]] - 0.05)
          excess_steps <- excess_steps - 1L
        }

        # Fallback: if the other sliders cannot absorb all excess, reduce the
        # slider that was just changed.
        while (excess_steps > 0 &&
               values[[w_local]] - weight_min[[w_local]] >= 0.05 - 1e-8) {
          values[[w_local]] <- round_step(values[[w_local]] - 0.05)
          excess_steps <- excess_steps - 1L
        }

        apply_values_to_sliders(values)
      }

      run_message_text(NULL)
    }, ignoreInit = TRUE)
  })

  output$weight_sum <- renderText({
    sprintf("%.2f", sum(current_slider_values(), na.rm = TRUE))
  })

  output$matched_param_id <- renderText({
    values <- current_slider_values()
    total <- sum(values, na.rm = TRUE)

    if (abs(total - 1) > 1e-8) {
      return("Complete weights to total 1.00")
    }

    row <- find_exact_parameter_row(values)
    if (is.null(row)) "No precomputed match" else as.character(row$params_1[[1]])
  })

  output$current_weights_table <- renderTable({
    values <- current_slider_values()
    data.frame(
      Weight = unname(weight_labels[weight_cols]),
      Value = sprintf("%.2f", values[weight_cols]),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$run_message <- renderUI({
    msg <- run_message_text()
    if (is.null(msg)) return(NULL)
    tags$div(class = "alert alert-success", msg)
  })

  observeEvent(input$see_result, {
    label <- trimws(input$experiment_label)
    if (!nzchar(label)) label <- paste("Experiment", length(saved_experiments()) + 1)

    values <- current_slider_values()
    total <- sum(values, na.rm = TRUE)
    if (abs(total - 1) > 1e-8) {
      showNotification(
        paste0("The selected weights total ", sprintf("%.2f", total),
               ". They must total exactly 1.00 before the experiment can be saved."),
        type = "error",
        duration = 6
      )
      return()
    }

    row <- find_exact_parameter_row(values)
    if (is.null(row)) {
      showNotification(
        "These weights total 1.00, but they do not match a precomputed combination in params0.csv.",
        type = "error",
        duration = 7
      )
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
    names(exp$weights) <- weight_cols

    exps <- saved_experiments()
    exps[[key]] <- exp
    saved_experiments(exps)
    current_experiment_key(key)

    choices <- setNames(names(exps), vapply(exps, function(x) {
      paste0(x$label, " (ID ", x$params_1, ")")
    }, character(1)))
    updateSelectInput(session, "saved_experiment", choices = choices, selected = key)

    run_message_text(paste0("Saved ", label, " using parameter ID ", exp$params_1, "."))
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
    exp <- current_experiment()
    req(exp)
    paste0(exp$label, " — CAS Saved Results")
  })

  output$result_parameter_summary <- renderUI({
    exp <- current_experiment()
    req(exp)
    tags$div(
      class = "well",
      tags$strong("Parameter ID: "), exp$params_1,
      tags$span("  |  "),
      tags$strong("Saved: "), exp$created,
      tags$br(),
      paste(
        paste0(unname(weight_labels[names(exp$weights)]), " = ",
               sprintf("%.2f", exp$weights)),
        collapse = "  |  "
      )
    )
  })

  experiment_skew <- reactive({
    exp <- current_experiment()
    req(exp)
    out <- skew[skew$params_1 == exp$params_1, , drop = FALSE]
    validate(need(nrow(out) > 0, "No fitted distributions were found for this parameter ID."))
    out
  })

  observeEvent(experiment_skew(), {
    dat <- experiment_skew()
    names_available <- unique(as.character(dat$name))
    choices <- setNames(names_available, vapply(names_available, display_name, character(1)))
    current <- isolate(input$result_name)
    selected <- if (!is.null(current) && current %in% names_available) current else names_available[1]
    updateSelectInput(session, "result_name", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  output$mods_tabs <- renderUI({
    dat <- experiment_skew()
    req(input$result_name)
    dat <- dat[dat$name == input$result_name, , drop = FALSE]
    mods <- unique(as.character(dat$mods_id))

    do.call(tabsetPanel, c(
      list(id = "result_mod_tabs"),
      lapply(seq_along(mods), function(i) {
        mod <- mods[[i]]
        tabPanel(
          title = paste0(display_mod(mod), " (", mod, ")"),
          value = mod,
          tags$br(),
          plotOutput(paste0("plot_mod_", i), height = "520px"),
          tableOutput(paste0("table_mod_", i))
        )
      })
    ))
  })

  observe({
    dat_all <- experiment_skew()
    req(input$result_name)
    dat_name <- dat_all[dat_all$name == input$result_name, , drop = FALSE]
    mods <- unique(as.character(dat_name$mods_id))

    lapply(seq_along(mods), function(i) {
      local({
        idx <- i
        mod_local <- mods[[i]]
        output[[paste0("plot_mod_", idx)]] <- renderPlot({
          dat <- experiment_skew()
          req(input$result_name)
          d <- dat[dat$name == input$result_name & dat$mods_id == mod_local, , drop = FALSE]
          validate(need(nrow(d) > 0, "No distributions are available."))

          xr <- safe_range(d)
          x <- seq(xr[1], xr[2], length.out = 700)
          group_values <- unique(as.character(d$value))
          cols <- grDevices::hcl.colors(max(length(group_values), 3), "Dark 3")[seq_along(group_values)]
          ltys <- seq_along(group_values)

          curves <- lapply(group_values, function(g) {
            one <- d[d$value == g, , drop = FALSE]
            # Usually one row per group. If duplicates exist, plot the first fit.
            skew_pdf(x, one$xi[1], one$omega[1], one$alpha[1])
          })
          ymax <- max(unlist(curves), na.rm = TRUE)
          if (!is.finite(ymax) || ymax <= 0) ymax <- 1

          plot(
            x, curves[[1]], type = "l", lwd = 2, col = cols[1], lty = ltys[1],
            ylim = c(0, ymax * 1.08),
            xlab = display_mod(mod_local),
            ylab = "Density",
            main = paste(display_name(input$result_name), "—", display_mod(mod_local))
          )
          if (length(curves) > 1) {
            for (j in 2:length(curves)) {
              lines(x, curves[[j]], lwd = 2, col = cols[j], lty = ltys[j])
            }
          }
          legend(
            "topright",
            legend = group_values,
            col = cols,
            lty = ltys,
            lwd = 2,
            title = "Group value",
            bty = "n",
            cex = 0.9
          )
          grid()
        })

        output[[paste0("table_mod_", idx)]] <- renderTable({
          dat <- experiment_skew()
          req(input$result_name)
          d <- dat[dat$name == input$result_name & dat$mods_id == mod_local, , drop = FALSE]
          d[, c("value", "xi", "omega", "alpha"), drop = FALSE]
        }, digits = 4, striped = TRUE, bordered = TRUE, spacing = "s")
      })
    })
  })

  output$saved_experiments_table <- renderTable({
    exps <- saved_experiments()
    if (length(exps) == 0) {
      return(data.frame(Message = "No experiments saved in this session."))
    }
    do.call(rbind, lapply(exps, function(exp) {
      data.frame(
        Experiment = exp$label,
        `Parameter ID` = exp$params_1,
        `CAS weight sum` = sprintf("%.2f", sum(exp$weights)),
        Saved = exp$created,
        check.names = FALSE
      )
    }))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  observeEvent(input$load_saved_exp, {
    req(input$saved_experiment)
    exps <- saved_experiments()
    req(exps[[input$saved_experiment]])
    current_experiment_key(input$saved_experiment)
    exp <- exps[[input$saved_experiment]]

    row <- params[params$params_1 == exp$params_1, , drop = FALSE]
    if (nrow(row) > 0) apply_row_to_sliders(row[1, , drop = FALSE])

    updateNavbarPage(session, "main_nav", selected = "results")
  })
}

shinyApp(ui, server)
