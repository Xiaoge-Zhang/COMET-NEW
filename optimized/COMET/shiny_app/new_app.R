library(shiny)

# -------------------------------------------------------------------------
# Saved CAS policy data
# -------------------------------------------------------------------------
app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

find_data_file <- function(candidates) {
  for (nm in candidates) {
    p <- file.path(app_dir, nm)
    if (file.exists(p)) return(p)
  }
  stop("Missing data file. Expected one of: ", paste(candidates, collapse = ", "))
}

policy_specs <- list(
  cas = list(
    label = "CAS",
    params_candidates = c("cas_params0(3).csv", "cas_params0(2).csv", "cas_params0.csv"),
    skew_candidates = c("cas_params0_skew(3).csv", "cas_params0_skew(2).csv", "cas_params0_skew.csv")
  ),
  abocas = list(
    label = "ABO-CAS",
    params_candidates = c("abocas_params1(3).csv", "abocas_params1(2).csv", "abocas_params1.csv"),
    skew_candidates = c("abocas_params1_skew(3).csv", "abocas_params1_skew(2).csv", "abocas_params1_skew.csv")
  ),
  amendcas = list(
    label = "Amended-CAS",
    params_candidates = c("amendcas_params2(3).csv", "amendcas_params2(2).csv", "amendcas_params2.csv"),
    skew_candidates = c("amendcas_params2_skew(3).csv", "amendcas_params2_skew(2).csv", "amendcas_params2_skew.csv")
  ),
  supplycas = list(
    label = "Supply CAS",
    params_candidates = c("supplycas_params3(3).csv", "supplycas_params3(2).csv", "supplycas_params3.csv"),
    skew_candidates = c("supplycas_params3_skew(3).csv", "supplycas_params3_skew(2).csv", "supplycas_params3_skew.csv")
  )
)

weight_order <- c(
  "wl_weight", "post_tx_weight", "abo_weight", "height_weight",
  "efficiency_weight", "cpra_weight", "peds_weight", "pld_weight",
  "bio_weight"
)

weight_labels <- c(
  wl_weight = "Waitlist weight",
  post_tx_weight = "Post-transplant weight",
  abo_weight = "ABO weight",
  height_weight = "Height weight",
  efficiency_weight = "Efficiency weight",
  cpra_weight = "cPRA weight",
  peds_weight = "Pediatric weight",
  pld_weight = "Prior living donor weight",
  bio_weight = "Biological priority weight"
)

required_skew <- c("xi", "omega", "alpha", "mods_id", "name", "value", "params_1")

load_policy_data <- function(spec) {
  params_file <- find_data_file(spec$params_candidates)
  skew_file <- find_data_file(spec$skew_candidates)
  
  params <- read.csv(params_file, stringsAsFactors = FALSE, check.names = FALSE)
  skew <- read.csv(skew_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  weight_cols <- intersect(weight_order, names(params))
  if (length(weight_cols) == 0) {
    stop(params_file, " does not contain any recognized weight columns.")
  }
  if (!all(required_skew %in% names(skew))) {
    stop(skew_file, " is missing required columns: ",
         paste(setdiff(required_skew, names(skew)), collapse = ", "))
  }
  
  if (!("params_1" %in% names(params))) {
    params$params_1 <- seq_len(nrow(params))
  }
  
  for (w in weight_cols) params[[w]] <- as.numeric(params[[w]])
  params$params_1 <- as.character(params$params_1)
  skew$params_1 <- as.character(skew$params_1)
  skew$xi <- as.numeric(skew$xi)
  skew$omega <- as.numeric(skew$omega)
  skew$alpha <- as.numeric(skew$alpha)
  
  params <- params[!duplicated(params$params_1), , drop = FALSE]
  
  default_idx <- integer(0)
  if ("default" %in% names(params)) {
    default_idx <- which(tolower(as.character(params$default)) %in% c("true", "t", "1", "yes", "y"))
  }
  default_row <- if (length(default_idx) > 0) {
    params[default_idx[1], , drop = FALSE]
  } else {
    params[1, , drop = FALSE]
  }
  
  # Custom mode starts from the first non-default row.
  # In custom mode, each slider should only expose values that occur in
  # non-default rows. The default row is still available through the
  # Default weight button, which disables all sliders.
  custom_params <- params
  if ("default" %in% names(params)) {
    is_default_row <- tolower(as.character(params$default)) %in% c("true", "t", "1", "yes", "y")
    custom_params <- params[!is_default_row, , drop = FALSE]
  } else if (nrow(params) > 1) {
    default_param_id <- as.character(default_row$params_1[[1]])
    custom_params <- params[as.character(params$params_1) != default_param_id, , drop = FALSE]
  }
  if (nrow(custom_params) == 0) custom_params <- default_row
  custom_start_values <- setNames(
    round(as.numeric(default_row[1, weight_cols]), 12),
    weight_cols
  )
  
  if (identical(spec$label, "Amended-CAS")) {
    custom_start_values <- round(custom_start_values / 0.05) * 0.05
    custom_start_values <- round(custom_start_values, 12)
  }
  
  # Slider stops are the actual unique values in the non-default rows only.
  # The resulting selected combination still must exist in the full params
  # table before it can run.
  weight_values <- lapply(weight_cols, function(w) {
    vals <- sort(unique(round(as.numeric(custom_params[[w]]), 12)))
    vals[is.finite(vals)]
  })
  names(weight_values) <- weight_cols
  
  weight_min <- sapply(weight_values, min, na.rm = TRUE)
  weight_max <- sapply(weight_values, max, na.rm = TRUE)
  adjustable_weights <- weight_cols[vapply(weight_values, length, integer(1)) > 1]
  fixed_weights <- setdiff(weight_cols, adjustable_weights)
  
  default_slider_index <- setNames(integer(length(weight_cols)), weight_cols)
  custom_slider_index <- setNames(integer(length(weight_cols)), weight_cols)
  for (w in weight_cols) {
    vals <- weight_values[[w]]
    default_value <- round(as.numeric(default_row[[w]]), 12)
    default_slider_index[[w]] <- which.min(abs(vals - default_value))
    custom_slider_index[[w]] <- 1L
  }
  
  list(
    label = spec$label,
    params = params,
    custom_params = custom_params,
    skew = skew,
    weight_cols = weight_cols,
    weight_values = weight_values,
    weight_min = weight_min,
    weight_max = weight_max,
    adjustable_weights = adjustable_weights,
    fixed_weights = fixed_weights,
    default_row = default_row,
    custom_start_values = custom_start_values,
    default_slider_index = default_slider_index,
    custom_slider_index = custom_slider_index
  )
}

policy_data_list <- lapply(policy_specs, load_policy_data)
policy_choices <- setNames(names(policy_data_list), vapply(policy_data_list, function(x) x$label, character(1)))

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------
round_step <- function(x, step = 0.05) round(x / step) * step

format_sig <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "",
    formatC(as.numeric(x), format = "f", digits = digits)
  )
}

clean_label <- function(x, max_len = 60) {
  x <- trimws(ifelse(is.null(x), "", as.character(x)))
  substr(x, 1, max_len)
}

format_weight_value <- function(x) {
  out <- format(round(as.numeric(x), 4), scientific = FALSE, trim = TRUE)
  out <- sub("\\.0+$", "", out)
  out <- sub("(\\.[0-9]*?)0+$", "\\1", out)
  out
}

value_to_slider_index <- function(pd, w, value) {
  vals <- pd$weight_values[[w]]
  if (length(vals) == 0) return(1L)
  which.min(abs(vals - round(as.numeric(value), 12)))
}

slider_index_to_value <- function(pd, w, index) {
  vals <- pd$weight_values[[w]]
  if (length(vals) == 0) return(NA_real_)
  idx <- suppressWarnings(as.integer(round(as.numeric(index))))
  if (!is.finite(idx)) idx <- value_to_slider_index(pd, w, pd$custom_start_values[[w]])
  idx <- max(1, min(length(vals), idx))
  vals[[idx]]
}

find_exact_parameter_row <- function(pd, values, tolerance = 1e-6) {
  params <- pd$params
  matches <- rep(TRUE, nrow(params))
  for (w in pd$weight_cols) {
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

skew_mean <- function(xi, omega, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  xi + omega * delta * sqrt(2 / pi)
}

skew_sd <- function(omega, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  omega * sqrt(1 - 2 * delta^2 / pi)
}

# Numerically invert the fitted skew-normal distribution. This keeps the app
# self-contained and avoids requiring an additional R package solely for
# quartile calculations.
skew_quantiles <- function(xi, omega, alpha, probs = c(0.25, 0.50, 0.75)) {
  if (!all(is.finite(c(xi, omega, alpha))) || omega <= 0) {
    return(rep(NA_real_, length(probs)))
  }
  
  z <- seq(-10, 10, length.out = 10001)
  density <- 2 * dnorm(z) * pnorm(alpha * z)
  dz <- diff(z)
  cdf <- c(0, cumsum((density[-length(density)] + density[-1]) * dz / 2))
  total <- tail(cdf, 1)
  if (!is.finite(total) || total <= 0) return(rep(NA_real_, length(probs)))
  cdf <- cdf / total
  
  as.numeric(approx(
    x = cdf,
    y = xi + omega * z,
    xout = probs,
    ties = "ordered",
    rule = 2
  )$y)
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
    can_count = "Candidate Count",
    tx_count = "Transplant Count",
    wait_death = "Waitlist Deaths",
    wld_ppy = "Waitlist Deaths per 100 Patient-Years",
    tx_ppy = "Transplants per 100 Patient-Years",
    med_wlt = "Median Waitlist Time",
    med_dist = "Median Distance",
    post_tx_death = "Post-Transplant Deaths",
    ptd_ppy = "Post-Transplant Deaths per 100 Patient-Years",
    med_offer = "Median Offers"
  )
  ifelse(x %in% names(mapping), unname(mapping[x]), gsub("_", " ", x))
}

display_category <- function(name, value) {
  value <- as.character(value)
  
  if (identical(name, "ov")) {
    value[] <- "Overall"
    return(value)
  }
  
  if (identical(name, "male")) {
    key <- tolower(trimws(value))
    mapping <- c(
      "0" = "Female",
      "0.0" = "Female",
      "false" = "Female",
      "f" = "Female",
      "female" = "Female",
      "1" = "Male",
      "1.0" = "Male",
      "true" = "Male",
      "t" = "Male",
      "male" = "Male"
    )
    matched <- key %in% names(mapping)
    value[matched] <- unname(mapping[key[matched]])
  }
  
  value
}

format_result_table <- function(d) {
  if (nrow(d) == 0) return(d)
  
  quantiles <- t(vapply(seq_len(nrow(d)), function(i) {
    skew_quantiles(d$xi[i], d$omega[i], d$alpha[i])
  }, numeric(3)))
  
  one_decimal <- function(x) {
    ifelse(is.finite(x), formatC(x, format = "f", digits = 1), "")
  }
  
  data.frame(
    Category = display_category(as.character(d$name[1]), d$value),
    Median = one_decimal(quantiles[, 2]),
    IQR = paste0(one_decimal(quantiles[, 1]), "–", one_decimal(quantiles[, 3])),
    check.names = FALSE
  )
}

result_output_id <- function(prefix, mod_id) {
  paste0(prefix, gsub("[^A-Za-z0-9_]", "_", mod_id))
}

result_palette <- function(n) {
  grDevices::hcl.colors(max(n, 3), "Dark 3")[seq_len(n)]
}

draw_result_figure <- function(d, stratification, mod_id) {
  outcome_label <- display_mod(mod_id)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  if (identical(stratification, "ov")) {
    par(mar = c(4.5, 4.5, 4.5, 1.2))
    xr <- safe_range(d)
    x <- seq(xr[1], xr[2], length.out = 700)
    density <- skew_pdf(x, d$xi[1], d$omega[1], d$alpha[1])
    density[!is.finite(density)] <- 0
    ymax <- max(density, na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    color <- result_palette(1)[1]
    
    plot(
      x, density,
      type = "n",
      ylim = c(0, ymax * 1.08),
      xlab = outcome_label,
      ylab = "Probability",
      main = paste(strwrap(outcome_label, width = 46), collapse = "\n"),
      cex.main = 0.95
    )
    grid(col = "#e6e6e6")
    polygon(
      c(x, rev(x)),
      c(density, rep(0, length(density))),
      col = grDevices::adjustcolor(color, alpha.f = 0.45),
      border = NA
    )
    lines(x, density, lwd = 2, col = color)
    return(invisible(NULL))
  }
  
  group_values <- unique(as.character(d$value))
  group_labels <- display_category(stratification, group_values)
  colors <- result_palette(length(group_values))
  yr <- safe_range(d)
  y <- seq(yr[1], yr[2], length.out = 500)
  figure_title <- paste(outcome_label, "Stratified by", display_name(stratification))
  
  # Extra bottom space is reserved for angled category labels. Drawing the
  # labels ourselves prevents base R from suppressing labels that overlap.
  par(mar = c(7.2, 4.5, 4.8, 1.2))
  
  plot(
    NA,
    xlim = c(0.5, length(group_values) + 0.5),
    ylim = yr,
    xaxt = "n",
    xlab = "",
    ylab = outcome_label,
    main = paste(strwrap(figure_title, width = 46), collapse = "\n"),
    cex.main = 0.90
  )
  grid(col = "#e6e6e6")
  axis(1, at = seq_along(group_values), labels = FALSE)
  
  usr <- par("usr")
  label_y <- usr[3] - 0.055 * diff(usr[3:4])
  text(
    x = seq_along(group_values),
    y = label_y,
    labels = group_labels,
    srt = 35,
    adj = 1,
    xpd = NA,
    cex = 0.78
  )
  mtext("Category", side = 1, line = 5.2)
  
  for (i in seq_along(group_values)) {
    one <- d[as.character(d$value) == group_values[i], , drop = FALSE]
    density <- skew_pdf(y, one$xi[1], one$omega[1], one$alpha[1])
    density[!is.finite(density)] <- 0
    max_density <- max(density, na.rm = TRUE)
    width <- if (is.finite(max_density) && max_density > 0) density / max_density * 0.4 else rep(0, length(density))
    
    polygon(
      c(i - width, rev(i + width)),
      c(y, rev(y)),
      col = grDevices::adjustcolor(colors[i], alpha.f = 0.65),
      border = colors[i],
      lwd = 1.2
    )
    
    median_value <- skew_quantiles(one$xi[1], one$omega[1], one$alpha[1], 0.50)
    points(i, median_value, pch = 19, cex = 0.9, col = "black")
  }
}

experiment_display <- function(exp) {
  lab <- clean_label(exp$label)
  if (nzchar(lab)) paste0(exp$name, " — ", lab) else exp$name
}

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- navbarPage(
  id = "main_nav",
  title = NULL,
  
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
        .summary-card .table > tbody > tr > td {padding:3px 6px;line-height:1.05;}
        .run-left .form-group {margin-bottom:7px;}
        .run-left .control-label {font-size:13px;margin-bottom:1px;}
        .run-left h4 {margin-top:8px;margin-bottom:6px;}
        .slider-grid {display:grid;grid-template-columns:repeat(2, minmax(230px, 330px));column-gap:22px;row-gap:0;align-items:start;}
        .slider-box {max-width:330px;margin-bottom:2px;}
        .slider-box .irs {height:42px;}
        .slider-box .irs-line,
        .slider-box .irs-bar {top:24px;}
        .slider-box .irs-handle {top:16px;}
        .slider-box .irs-min,
        .slider-box .irs-max,
        .slider-box .irs-single {font-size:10px; max-width:60px; overflow:hidden; text-overflow:clip;}
        .slider-box .irs-grid {display:none;}
        .slider-box .irs-single, .slider-box .irs-min, .slider-box .irs-max {
          min-width:auto !important; max-width:64px; overflow:hidden; text-overflow:clip;
        }
        .run-controls-row {display:flex;gap:14px;align-items:flex-end;flex-wrap:wrap;margin-bottom:6px;}
        .run-controls-row .form-group {margin-bottom:0;}
        .policy-select-box {width:260px;max-width:100%;}
        .experiment-name-box {min-width:170px;margin-bottom:6px;}
        .label-box {width:330px;max-width:100%;}
        .requirements-card {background:#fffdf4;border:1px solid #eadca6;border-radius:6px;padding:7px 10px;margin-top:6px;font-size:12px;}
        .requirements-card ul {margin:3px 0 0 0;padding-left:18px;}
        .requirements-card li {margin-bottom:2px;line-height:1.25;}
        .weight-status .well {margin-bottom:7px;padding:8px 12px;}
        .weight-status .well h4 {margin:0 0 4px 0;font-size:20px;}
        .weight-status .well p {margin:2px 0;}
        .slider-disabled {opacity:0.55; pointer-events:none;}
        .mode-button-row {margin-bottom:6px;}
        .mode-button-row .btn {margin-right:6px; min-width:118px; padding:4px 10px;}
        .compact-action {width:330px; max-width:100%; font-size:15px; padding:6px 12px;}
        .projection-subtitle {font-size:16px;color:#555;margin:-2px 0 7px 0;}
        .results-controls {display:flex;gap:24px;align-items:flex-end;flex-wrap:wrap;margin-bottom:10px;}
        .results-controls .form-group {margin-bottom:0;}
        .results-stratification {width:260px;max-width:100%;}
        .results-view-toggle {min-width:220px;}
        .results-dashboard {display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:12px;align-items:start;}
        .result-dashboard-card {background:#fff;border:1px solid #ddd;border-radius:6px;padding:9px 11px;min-width:0;}
        .result-dashboard-card h4 {margin:2px 0 7px 0;font-size:16px;}
        .result-dashboard-card .table {margin-bottom:0;font-size:12px;}
        .result-dashboard-card .table > thead > tr > th,
        .result-dashboard-card .table > tbody > tr > td {padding:4px 6px;}
        .compact-page h3 {font-size:22px;margin-top:8px;margin-bottom:6px;}
        .compact-page h4 {font-size:16px;margin-top:2px;margin-bottom:5px;}
        .compact-result-card {background:#fff;border:1px solid #ddd;border-radius:6px;padding:7px 9px;margin-bottom:7px;}
        .compact-result-card .form-group {margin-bottom:6px;}
        .compact-result-card .control-label {font-size:12px;margin-bottom:1px;}
        .compact-result-card select {height:30px;padding:3px 8px;font-size:12px;}
        .compact-result-card .table {margin-bottom:0;font-size:12px;}
        .compact-result-card .table > thead > tr > th,
        .compact-result-card .table > tbody > tr > td {padding:3px 5px;line-height:1.05;}
        .compact-meta {font-size:12px;background:#f7f9fb;border:1px solid #d9e1e8;border-radius:6px;padding:6px 9px;margin-bottom:6px;line-height:1.25;}
        .saved-page h3 {font-size:22px;margin-top:8px;margin-bottom:8px;}
        .saved-page h4 {font-size:16px;margin-top:8px;margin-bottom:5px;}
        .saved-page .form-group {margin-bottom:6px;}
        .saved-page .table {font-size:12px;margin-bottom:6px;table-layout:fixed;width:100%;}
        .saved-page .table > thead > tr > th,
        .saved-page .table > tbody > tr > td {padding:3px 5px;line-height:1.05;word-break:break-word;}
        .saved-controls {background:#f7f9fb;border:1px solid #d9e1e8;border-radius:6px;padding:8px 10px;margin-top:8px;}
        .disabled-see-result {opacity:0.45; cursor:not-allowed;}
      ")),
      tags$script(HTML("
        function compactNumber(x) {
          var raw = (x || '').toString().replace(/,/g, '').trim();
          if (!/^[-+]?\\d*\\.?\\d+(e[-+]?\\d+)?$/i.test(raw)) return x;
          var num = Number(raw);
          if (!isFinite(num)) return x;
          var out = num.toFixed(4);
          out = out.replace(/\\.?0+$/, '');
          if (out === '-0') out = '0';
          return out;
        }

        function applyAllowedValueLabels() {
          $('.slider-box').each(function() {
            var box = $(this);
            var valsRaw = box.attr('data-values') || '';
            if (!valsRaw.length) return;
            var vals = valsRaw.split('|');
            function labelForIndex(txt) {
              var raw = (txt || '').toString().replace(/,/g, '').trim();

              // Ion.RangeSlider displays internal slider positions as integers
              // because the real slider values are 1, 2, 3, ...
              // Only those pure integer labels should be mapped to CSV weights.
              // If the label is already a mapped decimal like 0.30, do not
              // interpret it again as index 0.
              if (!/^\\d+$/.test(raw)) return compactNumber(raw);

              var idx = parseInt(raw, 10);
              if (!isFinite(idx)) return compactNumber(raw);
              idx = Math.max(1, Math.min(vals.length, idx));
              return compactNumber(vals[idx - 1]);
            }
            box.find('.irs-min, .irs-max, .irs-single, .irs-from, .irs-to').each(function() {
              $(this).text(labelForIndex($(this).text()));
            });
          });
        }

        $(document).on('shiny:connected', function() {
          $('#experiment_label').attr('maxlength', 60);
          $('#saved_label_edit').attr('maxlength', 60);
          applyAllowedValueLabels();
          setInterval(applyAllowedValueLabels, 120);
        });

        $(document).on('shiny:value shiny:inputchanged shown.bs.tab', function() {
          setTimeout(applyAllowedValueLabels, 10);
          setTimeout(applyAllowedValueLabels, 80);
          setTimeout(applyAllowedValueLabels, 180);
        });
      ")),
      h3("Run a saved experiment"),
      fluidRow(
        column(
          width = 8,
          class = "run-left",
          tags$div(
            class = "run-controls-row",
            tags$div(class = "policy-select-box", selectInput("policy_select", "Policy", choices = policy_choices, selected = policy_choices[[1]])),
            tags$div(
              class = "experiment-name-box",
              tags$label("Experiment name"),
              tags$div(textOutput("next_experiment_name", inline = TRUE))
            )
          ),
          tags$div(class = "label-box", textInput("experiment_label", "Experiment label (optional)", value = "", placeholder = "Optional label, up to 60 characters")),
          tags$div(
            class = "mode-button-row",
            actionButton("use_default_weight", "Default weight", class = "btn-default"),
            actionButton("use_custom_weight", "Custom weight", class = "btn-default")
          ),
          h4("Weights"),
          uiOutput("weight_sliders"),
          uiOutput("see_result_button"),
          tags$br(), tags$br(),
          uiOutput("run_message")
        ),
        column(
          width = 4,
          tags$div(
            class = "weight-status",
            tags$div(
              class = "requirements-card",
              tags$strong("Requirements"),
              tags$ul(
                tags$li("All weights must sum to exactly 1.00."),
                tags$li("Total Biological priority weight (ABO weight plus Height weight) cannot exceed 0.30.")
              )
            ),
            uiOutput("weight_status_box"),
            tags$div(
              class = "summary-card",
              h4("Current weight settings"),
              tableOutput("current_weights_table")
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    title = "Result Detail", value = "results",
    fluidPage(
      tags$div(
        class = "compact-page",
        conditionalPanel(
          condition = "!output.has_current_experiment",
          tags$div(style = "margin-top:30px;color:#666;font-size:16px;", "No experiment is loaded. Create or load an experiment first.")
        ),
        conditionalPanel(
          condition = "output.has_current_experiment",
          h3(textOutput("result_title", inline = TRUE)),
          tags$div(class = "projection-subtitle", "Two-year projections"),
          uiOutput("result_weight_summary"),
          tags$div(
            class = "results-controls",
            tags$div(
              class = "results-stratification",
              selectInput("result_name", "Stratified by", choices = NULL)
            ),
            tags$div(
              class = "results-view-toggle",
              radioButtons(
                "result_view",
                "Display results as",
                choices = c("Tables" = "tables", "Figures" = "figures"),
                selected = "tables",
                inline = TRUE
              )
            )
          ),
          uiOutput("result_dashboard")
        )
      )
    )
  ),
  
  tabPanel(
    title = "Saved Experiments", value = "saved",
    fluidPage(
      tags$div(
        class = "saved-page",
        h3("Saved experiment registry"),
        tableOutput("saved_experiments_table"),
        tags$div(
          class = "saved-controls",
          fluidRow(
            column(
              width = 4,
              selectInput("saved_experiment", "Experiment", choices = character(0))
            ),
            column(
              width = 3,
              tags$br(),
              actionButton("load_saved_exp", "Open Result Detail", class = "btn-primary")
            ),
            column(
              width = 4,
              textInput("saved_label_edit", "Edit label", value = "", placeholder = "Optional label, up to 60 characters")
            ),
            column(
              width = 1,
              tags$br(),
              actionButton("save_label_edit", "Save", class = "btn-default")
            )
          )
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
          tags$p(
            class = "text-muted",
            "Select multiple experiments. Remove individual experiments by backspacing."
          ),
          selectInput("comparison_name", "Stratified by", choices = NULL)
        ),
        column(
          width = 8,
          tags$div(class = "result-card",
                   h4(textOutput("comparison_title", inline = TRUE)),
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
  weight_mode <- reactiveVal("default")
  
  current_policy_key <- reactive({
    key <- input$policy_select
    if (is.null(key) || !(key %in% names(policy_data_list))) names(policy_data_list)[1] else key
  })
  
  current_policy_data <- reactive({
    policy_data_list[[current_policy_key()]]
  })
  
  next_experiment_name <- reactive({
    paste0("Experiment ", length(saved_experiments()) + 1)
  })
  
  output$next_experiment_name <- renderText({
    next_experiment_name()
  })
  
  observeEvent(input$policy_select, {
    weight_mode("default")
    run_message_text(NULL)
  }, ignoreInit = TRUE)
  
  output$weight_sliders <- renderUI({
    pd <- current_policy_data()
    mode <- weight_mode()
    tags$div(
      class = "slider-grid",
      lapply(pd$weight_cols, function(w) {
        if (identical(mode, "default")) {
          # In Amended-CAS, a default value may not be one of the permitted
          # custom stops. Include that exact value while default mode is shown
          # so the handle and label represent the true default weight.
          default_value <- round(as.numeric(pd$default_row[[w]]), 12)
          vals <- pd$weight_values[[w]]
          if (identical(pd$label, "Amended-CAS")) {
            vals <- sort(unique(c(vals, default_value)))
          }
          is_fixed <- length(vals) <= 1
          disabled <- TRUE
          slider_n <- max(1, length(vals))
          slider_value <- which.min(abs(vals - default_value))
          label <- weight_labels[[w]]
        } else {
          # In custom mode, slider stops come only from non-default rows.
          vals <- pd$weight_values[[w]]
          is_fixed <- length(vals) <= 1
          disabled <- is_fixed
          slider_n <- max(1, length(vals))
          # If the exact default is unavailable here, start at the nearest
          # permitted custom stop. A slight handle movement is expected.
          slider_value <- value_to_slider_index(pd, w, pd$custom_start_values[[w]])
          label <- weight_labels[[w]]
          if (is_fixed) label <- paste0(label, " (fixed)")
        }
        
        tags$div(
          class = paste("slider-box", if (disabled) "slider-disabled" else ""),
          `data-values` = paste(format_weight_value(vals), collapse = "|"),
          sliderInput(
            inputId = paste0("weight_", w),
            label = label,
            min = 1,
            max = slider_n,
            value = slider_value,
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        )
      })
    )
  })
  
  current_slider_values <- function() {
    pd <- current_policy_data()
    vals <- setNames(numeric(length(pd$weight_cols)), pd$weight_cols)
    
    if (identical(weight_mode(), "default")) {
      for (w in pd$weight_cols) vals[[w]] <- round(as.numeric(pd$default_row[[w]]), 12)
      return(vals)
    }
    
    for (w in pd$weight_cols) {
      v <- input[[paste0("weight_", w)]]
      if (is.null(v) || !is.finite(v)) {
        vals[[w]] <- round(as.numeric(pd$custom_start_values[[w]]), 12)
      } else {
        vals[[w]] <- slider_index_to_value(pd, w, v)
      }
    }
    vals
  }
  
  apply_values_to_sliders <- function(pd, values) {
    for (w in pd$weight_cols) {
      updateSliderInput(session, paste0("weight_", w), value = value_to_slider_index(pd, w, values[[w]]))
    }
  }
  
  observeEvent(input$use_default_weight, {
    # Default mode keeps the full scale and handle position, but disables input.
    weight_mode("default")
    run_message_text(NULL)
  })
  
  observeEvent(input$use_custom_weight, {
    # If custom mode is already active, keep the user's current slider
    # positions instead of resetting them to the custom starting values.
    if (identical(weight_mode(), "custom")) {
      run_message_text(NULL)
      return()
    }
    
    weight_mode("custom")
    
    run_message_text(NULL)
  })
  
  validation_status <- reactive({
    pd <- current_policy_data()
    values <- current_slider_values()
    total <- sum(values, na.rm = TRUE)
    total_valid <- abs(total - 1) <= 1e-6
    
    has_abo_height <- all(c("abo_weight", "height_weight") %in% names(values))
    abo_height_total <- if (has_abo_height) values[["abo_weight"]] + values[["height_weight"]] else NA_real_
    abo_height_valid <- !has_abo_height || abo_height_total <= 0.30 + 1e-6
    
    exact <- if (total_valid && abo_height_valid) find_exact_parameter_row(pd, values) else NULL
    available_valid <- !is.null(exact)
    good <- total_valid && abo_height_valid && available_valid
    
    list(
      values = values,
      total = total,
      total_valid = total_valid,
      has_abo_height = has_abo_height,
      abo_height_total = abo_height_total,
      abo_height_valid = abo_height_valid,
      exact = exact,
      available_valid = available_valid,
      good = good
    )
  })
  
  output$weight_status_box <- renderUI({
    st <- validation_status()
    
    warnings <- list()
    if (!st$abo_height_valid) {
      warnings <- append(warnings, list(tags$div(
        style = "margin-top:4px;color:#b71c1c;",
        tags$strong("ABO + Height exceeds 0.30.")
      )))
    }
    if (st$total_valid && st$abo_height_valid && !st$available_valid) {
      warnings <- append(warnings, list(tags$div(
        style = "margin-top:4px;color:#b71c1c;",
        tags$strong("This combination is not available in the precomputed results.")
      )))
    }
    
    status_content <- if (st$good) {
      tags$div(style = "margin-top:4px;color:#2e7d32;", "Ready to view and save.")
    } else if (length(warnings) > 0) {
      tagList(warnings)
    } else {
      tags$div(style = "margin-top:4px;color:#666;", "Adjust the total to 1.00.")
    }
    
    abo_height_line <- if (st$has_abo_height) {
      tags$p(tags$strong("ABO + Height: "), sprintf("%.2f / 0.30", st$abo_height_total))
    } else {
      tags$p(tags$strong("ABO + Height: "), "not applicable for this policy")
    }
    
    tags$div(
      class = paste("well", if (st$good) "status-good" else "status-warn"),
      h4(sprintf("Total: %.2f", if (abs(st$total - 1) <= 1e-6) 1 else st$total)),
      abo_height_line,
      status_content
    )
  })
  
  output$current_weights_table <- renderTable({
    pd <- current_policy_data()
    values <- current_slider_values()
    data.frame(
      Weight = unname(weight_labels[pd$weight_cols]),
      Value = format_weight_value(values[pd$weight_cols]),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$see_result_button <- renderUI({
    st <- validation_status()
    tags$button(
      id = "see_result",
      type = "button",
      class = paste("btn btn-primary action-button compact-action", if (!st$good) "disabled-see-result" else ""),
      disabled = if (!st$good) "disabled" else NULL,
      `aria-disabled` = if (!st$good) "true" else "false",
      "See Result"
    )
  })
  
  output$run_message <- renderUI({
    msg <- run_message_text()
    if (is.null(msg)) return(NULL)
    tags$div(class = "alert alert-success", msg)
  })
  
  update_experiment_choices <- function(exps, selected = NULL) {
    choices <- setNames(names(exps), vapply(exps, experiment_display, character(1)))
    updateSelectInput(session, "saved_experiment", choices = choices, selected = selected)
    updateSelectInput(
      session,
      "comparison_experiments",
      choices = choices,
      selected = intersect(isolate(input$comparison_experiments), names(exps))
    )
  }
  
  observeEvent(input$see_result, {
    st <- validation_status()
    if (!st$good) {
      showNotification("This experiment cannot be saved until all rules are satisfied and the combination is available.", type = "error", duration = 6)
      return()
    }
    
    exps <- saved_experiments()
    exp_name <- next_experiment_name()
    if (exp_name %in% vapply(exps, function(x) x$name, character(1))) {
      showNotification("Experiment names must be unique. Please try again.", type = "error", duration = 6)
      return()
    }
    
    key <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(99999, 1))
    label <- clean_label(input$experiment_label)
    row <- st$exact
    exp <- list(
      key = key,
      name = exp_name,
      label = label,
      policy_key = current_policy_key(),
      policy_label = current_policy_data()$label,
      params_1 = as.character(row$params_1[[1]]),
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      weights = st$values
    )
    
    exps[[key]] <- exp
    saved_experiments(exps)
    current_experiment_key(key)
    update_experiment_choices(exps, selected = key)
    updateTextInput(session, "experiment_label", value = "")
    run_message_text(paste0("Saved ", exp$name, if (nzchar(exp$label)) paste0(" — ", exp$label) else "", "."))
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
    paste0(experiment_display(exp), " — Saved Results")
  })
  
  output$result_weight_summary <- renderUI({
    exp <- current_experiment(); req(exp)
    weight_text <- paste(
      paste0(unname(weight_labels[names(exp$weights)]), " = ", format_sig(exp$weights, 2)),
      collapse = "  |  "
    )
    tags$div(
      class = "compact-meta",
      tags$strong("Policy: "), exp$policy_label,
      tags$span("  |  "),
      tags$strong("Saved: "), exp$created,
      tags$span("  |  "),
      tags$strong("Weights: "), weight_text
    )
  })
  
  experiment_skew <- reactive({
    exp <- current_experiment(); req(exp)
    pd <- policy_data_list[[exp$policy_key]]
    out <- pd$skew[pd$skew$params_1 == exp$params_1, , drop = FALSE]
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
  
  available_result_mods <- reactive({
    req(input$result_name)
    dat <- experiment_skew()
    unique(as.character(dat$mods_id[dat$name == input$result_name]))
  })
  
  all_result_mods <- sort(unique(unlist(lapply(policy_data_list, function(pd) {
    as.character(pd$skew$mods_id)
  }))))
  
  for (mod_id in all_result_mods) {
    local({
      current_mod <- mod_id
      table_id <- result_output_id("result_table_", current_mod)
      plot_id <- result_output_id("result_plot_", current_mod)
      
      output[[table_id]] <- renderTable({
        req(identical(input$result_view, "tables"), input$result_name)
        dat <- experiment_skew()
        d <- dat[dat$name == input$result_name & dat$mods_id == current_mod, , drop = FALSE]
        validate(need(nrow(d) > 0, "No summary is available."))
        format_result_table(d)
      }, striped = TRUE, bordered = TRUE, spacing = "s")
      
      output[[plot_id]] <- renderPlot({
        req(identical(input$result_view, "figures"), input$result_name)
        dat <- experiment_skew()
        d <- dat[dat$name == input$result_name & dat$mods_id == current_mod, , drop = FALSE]
        validate(need(nrow(d) > 0, "No figure is available."))
        draw_result_figure(d, input$result_name, current_mod)
      }, height = 380, res = 96)
    })
  }
  
  output$result_dashboard <- renderUI({
    req(input$result_name, input$result_view)
    mods <- available_result_mods()
    validate(need(length(mods) > 0, "No outcomes are available for this selection."))
    
    tags$div(
      class = "results-dashboard",
      lapply(mods, function(mod_id) {
        if (identical(input$result_view, "tables")) {
          tags$div(
            class = "result-dashboard-card",
            tags$h4(display_mod(mod_id)),
            tableOutput(result_output_id("result_table_", mod_id))
          )
        } else {
          tags$div(
            class = "result-dashboard-card",
            plotOutput(result_output_id("result_plot_", mod_id), height = "380px")
          )
        }
      })
    )
  })
  
  output$saved_experiments_table <- renderTable({
    exps <- saved_experiments()
    if (length(exps) == 0) return(data.frame(Message = "No experiments saved in this session."))
    do.call(rbind, lapply(exps, function(exp) {
      weight_text <- paste(
        paste0(unname(weight_labels[names(exp$weights)]), "=", format_sig(exp$weights, 2)),
        collapse = "; "
      )
      data.frame(
        Experiment = exp$name,
        Label = ifelse(nzchar(exp$label), exp$label, ""),
        Policy = exp$policy_label,
        Weights = weight_text,
        Saved = exp$created,
        check.names = FALSE
      )
    }))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  observeEvent(input$saved_experiment, {
    exps <- saved_experiments()
    if (!is.null(input$saved_experiment) && input$saved_experiment %in% names(exps)) {
      updateTextInput(session, "saved_label_edit", value = exps[[input$saved_experiment]]$label)
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$save_label_edit, {
    req(input$saved_experiment)
    exps <- saved_experiments()
    req(exps[[input$saved_experiment]])
    exps[[input$saved_experiment]]$label <- clean_label(input$saved_label_edit)
    saved_experiments(exps)
    update_experiment_choices(exps, selected = input$saved_experiment)
    showNotification("Label updated.", type = "message", duration = 3)
  })
  
  observeEvent(input$load_saved_exp, {
    req(input$saved_experiment)
    exps <- saved_experiments(); req(exps[[input$saved_experiment]])
    current_experiment_key(input$saved_experiment)
    exp <- exps[[input$saved_experiment]]
    updateSelectInput(session, "policy_select", selected = exp$policy_key)
    weight_mode("custom")
    session$onFlushed(function() {
      pd <- policy_data_list[[exp$policy_key]]
      apply_values_to_sliders(pd, exp$weights)
    }, once = TRUE)
    updateNavbarPage(session, "main_nav", selected = "results")
  })
  
  observe({
    exps <- saved_experiments()
    req(length(exps) > 0)
    selected_keys <- input$comparison_experiments
    if (is.null(selected_keys) || length(selected_keys) == 0) return()
    
    dat_list <- lapply(exps[selected_keys], function(exp) {
      pd <- policy_data_list[[exp$policy_key]]
      pd$skew[pd$skew$params_1 == exp$params_1, , drop = FALSE]
    })
    dat <- do.call(rbind, dat_list)
    names_available <- unique(as.character(dat$name))
    choices <- setNames(names_available, vapply(names_available, display_name, character(1)))
    selected <- if (!is.null(input$comparison_name) && input$comparison_name %in% names_available) input$comparison_name else names_available[1]
    updateSelectInput(session, "comparison_name", choices = choices, selected = selected)
  })
  
  output$comparison_title <- renderText({
    req(input$comparison_name)
    if (identical(input$comparison_name, "ov")) {
      "Median expected outcomes"
    } else {
      paste("Median expected outcomes by", display_name(input$comparison_name))
    }
  })
  
  output$comparison_table <- renderTable({
    exps <- saved_experiments()
    req(length(input$comparison_experiments) > 0, input$comparison_name)
    
    selected_exps <- exps[input$comparison_experiments]
    experiment_data <- lapply(selected_exps, function(exp) {
      pd <- policy_data_list[[exp$policy_key]]
      pd$skew[
        pd$skew$params_1 == exp$params_1 &
          pd$skew$name == input$comparison_name,
        ,
        drop = FALSE
      ]
    })
    
    pair_list <- lapply(experiment_data, function(d) {
      data.frame(
        mods_id = as.character(d$mods_id),
        value = as.character(d$value),
        stringsAsFactors = FALSE
      )
    })
    pair_list <- pair_list[vapply(pair_list, nrow, integer(1)) > 0]
    validate(need(length(pair_list) > 0, "No comparison data are available."))
    
    all_pairs <- unique(do.call(rbind, pair_list))
    validate(need(nrow(all_pairs) > 0, "No comparison data are available."))
    
    out <- data.frame(
      Outcome = vapply(all_pairs$mods_id, display_mod, character(1)),
      Category = display_category(input$comparison_name, all_pairs$value),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    row_keys <- paste(all_pairs$mods_id, all_pairs$value, sep = "\r")
    used_names <- character(0)
    
    for (i in seq_along(selected_exps)) {
      exp <- selected_exps[[i]]
      d <- experiment_data[[i]]
      means <- skew_mean(d$xi, d$omega, d$alpha)
      data_keys <- paste(as.character(d$mods_id), as.character(d$value), sep = "\r")
      keep <- !duplicated(data_keys)
      lookup <- setNames(format_sig(means[keep], 2), data_keys[keep])
      
      col_nm <- experiment_display(exp)
      if (col_nm %in% used_names) col_nm <- paste0(col_nm, " (", exp$policy_label, ")")
      used_names <- c(used_names, col_nm)
      out[[col_nm]] <- unname(lookup[row_keys])
    }
    
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui, server)
