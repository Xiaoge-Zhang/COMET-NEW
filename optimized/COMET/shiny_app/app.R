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
    label = "CAS-ABO Modification",
    params_candidates = c("abocas_params1(3).csv", "abocas_params1(2).csv", "abocas_params1.csv"),
    skew_candidates = c("abocas_params1_skew(3).csv", "abocas_params1_skew(2).csv", "abocas_params1_skew.csv")
  ),
  amendcas = list(
    label = "CAS-Efficiency Modification",
    params_candidates = c("amendcas_params2(3).csv", "amendcas_params2(2).csv", "amendcas_params2.csv"),
    skew_candidates = c("amendcas_params2_skew(3).csv", "amendcas_params2_skew(2).csv", "amendcas_params2_skew.csv")
  ),
  supplycas = list(
    label = "CAS-Supply adjusted",
    params_candidates = c("supplycas_params3(3).csv", "supplycas_params3(2).csv", "supplycas_params3.csv"),
    skew_candidates = c("supplycas_params3_skew(3).csv", "supplycas_params3_skew(2).csv", "supplycas_params3_skew.csv")
  )
)

policy_hover_info <- list(
  cas = list(
    title = "CAS",
    description = paste(
      "Composite Allocation Score (CAS) – The original implementation of continuous distribution",
      "in lung transplant, active from 3/9/2023 through 9/26/2023."
    ),
    url = "https://www.hrsa.gov/sites/default/files/hrsa/optn/policy-notice_lung_continuous-distribution.pdf"
  ),
  abocas = list(
    title = "CAS-ABO Modification",
    description = paste(
      "The revised Composite Allocation Score, active 9/27/2023–5/6/2026, which assigned",
      "an additional 2 biological disadvantage points to candidates with ABO Type B blood",
      "and an additional 5 points to candidates with Type O blood."
    ),
    url = "https://www.hrsa.gov/sites/default/files/hrsa/optn/lung_blood-type_special-pc-summer-2023.pdf"
  ),
  amendcas = list(
    title = "CAS-Efficiency Modification",
    description = paste(
      "The current implementation (since 5/7/2026) of continuous distribution in lung transplant,",
      "which revises the CAS ABO-modified policy by increasing placement efficiency weight from 10% to 15% of the",
      "overall score while reducing the weights of other CAS components proportionally to maintain",
      "a total of 100 CAS points. The amended CAS also revises the placement efficiency rating scale,",
      "which assigns placement efficiency points based on the nautical mile distance between the donor",
      "hospital and the transplant hospital."
    ),
    url = "https://www.hrsa.gov/optn/news-events/news/changes-lung-cas-now-in-effect"
  ),
  supplycas = list(
    title = "CAS-Supply adjusted",
    description = paste(
      "A hypothetical alternative CAS implementation that replaces each candidate’s height and ABO",
      "blood type biological disadvantage points with a single subscore reflecting the expected rate",
      "of eligible donors for a candidate given their height, blood type, and diagnosis group.",
      "For other components of the CAS, the current policy (CAS-Efficiency Modification) calculation guidelines are used."
    ),
    url = "https://pmc.ncbi.nlm.nih.gov/articles/PMC11840864/"
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

weight_hover_info <- c(
  wl_weight = "The weight assigned to candidates’ estimated life expectancy (according to the corresponding CAS formula) in the absence of a transplant.",
  post_tx_weight = "The weight assigned to candidates’ estimated post-transplant life expectancy (according to the corresponding CAS formula).",
  abo_weight = "The weight assigned based on candidates’ ABO blood type.",
  height_weight = "The weight assigned based on candidates’ height.",
  efficiency_weight = "The weight assigned based on the proximity between a candidate’s transplant center and a specific donor organ.",
  cpra_weight = "Calculated Panel Reactive Antibody (cPRA) represents the estimated percentage of donors whose organs a candidate’s immune system would reject due to prior immunologic sensitization. This parameter is fixed at its default value in the simulation due to a lack of sufficient data in the transplant registry needed to properly model its behavior.",
  peds_weight = "This weight is fixed given that COMET-Lung currently only simulates adult lung transplant candidates.",
  pld_weight = "This weight is fixed at its default value given the extreme rarity of prior organ donors on the lung transplant waiting list.",
  bio_weight = "This weight takes the place of ABO Weight and Height Weight in the CAS-Supply adjusted policy. The value is derived based on the projected supply of eligible donor organs as a function of candidate diagnosis group, height, and ABO blood type."
)

stratification_hover_info <- c(
  wlauc_cat = "WLAUC = Waitlist Area Under the Curve, defined as the predicted number of days a candidate will survive without a transplant (Maximum = 365).",
  wlauc_cat2 = "WLAUC = Waitlist Area Under the Curve, defined as the predicted number of days a candidate will survive without a transplant (Maximum = 365).",
  `wlauc cat2` = "WLAUC = Waitlist Area Under the Curve, defined as the predicted number of days a candidate will survive without a transplant (Maximum = 365).",
  `WLAUC Category` = "WLAUC = Waitlist Area Under the Curve, defined as the predicted number of days a candidate will survive without a transplant (Maximum = 365).",
  `WLAUC Category (Expanded)` = "WLAUC = Waitlist Area Under the Curve, defined as the predicted number of days a candidate will survive without a transplant (Maximum = 365).",
  dx_grp = paste(
    "Group A: Obstructive lung disease",
    "Group B: Pulmonary vascular disease",
    "Group C: Cystic fibrosis and immunodeficiency disorders",
    "Group D: Restrictive lung disease",
    sep = "\n"
  ),
  `Diagnosis Groups` = paste(
    "Group A: Obstructive lung disease",
    "Group B: Pulmonary vascular disease",
    "Group C: Cystic fibrosis and immunodeficiency disorders",
    "Group D: Restrictive lung disease",
    sep = "\n"
  )
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

  if (identical(spec$label, "CAS-Efficiency Modification")) {
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

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "download")
}

ascii_csv_text <- function(x) {
  x <- as.character(x)
  x <- gsub("\u2014|\u2013|\u2212", "-", x, perl = TRUE)
  x <- gsub("\u2018|\u2019", "'", x, perl = TRUE)
  x <- gsub("\u201c|\u201d", "'", x, perl = TRUE)
  x
}

comparison_download_table <- function(out) {
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- ascii_csv_text(names(out))
  for (nm in names(out)) {
    if (is.character(out[[nm]])) out[[nm]] <- ascii_csv_text(out[[nm]])
  }
  out
}

short_download_code <- function(n = 4) {
  paste0(sample(c(LETTERS, 0:9), n, replace = TRUE), collapse = "")
}

format_weight_value <- function(x) {
  # Slider labels and the current-weights table use up to four decimal
  # places, while retaining at least two (for example, 0.2500 -> 0.25).
  # The underlying calculations still use the unformatted numeric values.
  values <- as.numeric(x)
  out <- formatC(values, format = "f", digits = 4)
  out <- sub("(\\.[0-9]{2,}?)0+$", "\\1", out, perl = TRUE)
  out[is.na(values)] <- ""
  out
}

format_weight_label_value <- function(x) {
  # Keep compact experiment-label values at their original two decimals.
  ifelse(
    is.na(as.numeric(x)),
    "",
    formatC(as.numeric(x), format = "f", digits = 2)
  )
}

policy_label_for_experiment <- function(policy_label) {
  gsub("\\s+", "_", trimws(policy_label))
}

auto_experiment_label <- function(pd, values, mode) {
  policy_name <- policy_label_for_experiment(pd$label)

  if (identical(mode, "default")) {
    return(substr(paste0(policy_name, "_Default"), 1, 60))
  }

  val <- function(w) {
    if (w %in% names(values)) format_weight_label_value(values[[w]]) else "NA"
  }

  if ("bio_weight" %in% names(values) && !("abo_weight" %in% names(values)) && !("height_weight" %in% names(values))) {
    label <- paste0(
      policy_name,
      "_WL", val("wl_weight"),
      "_PT", val("post_tx_weight"),
      "_EF", val("efficiency_weight"),
      "_BP", val("bio_weight")
    )
  } else if (identical(pd$label, "CAS-Supply adjusted") && "bio_weight" %in% names(values)) {
    label <- paste0(
      policy_name,
      "_WL", val("wl_weight"),
      "_PT", val("post_tx_weight"),
      "_EF", val("efficiency_weight"),
      "_BP", val("bio_weight")
    )
  } else {
    label <- paste0(
      policy_name,
      "_WL", val("wl_weight"),
      "_PT", val("post_tx_weight"),
      "_EF", val("efficiency_weight"),
      "_AB", val("abo_weight"),
      "_HT", val("height_weight")
    )
  }

  substr(label, 1, 60)
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
    dx_grp = "Diagnosis Groups",
    hgt_cat = "Height category",
    abo = "Blood type",
    age_cat = "Age category",
    male = "Sex",
    reg = "Census Subregions",
    wlauc_cat = "WLAUC Category",
    wlauc_cat2 = "WLAUC Category (Expanded)",
    `wlauc cat2` = "WLAUC Category (Expanded)"
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

outcome_axis_label <- function(mod_id) {
  label <- display_mod(mod_id)
  units <- c(
    med_wlt = "Days",
    med_dist = "NM"
  )
  if (mod_id %in% names(units)) {
    paste0(label, " (", units[[mod_id]], ")")
  } else {
    label
  }
}

stratification_axis_label <- function(stratification) {
  label <- display_name(stratification)
  units <- c(
    hgt_cat = "Inches",
    wlauc_cat = "Days",
    wlauc_cat2 = "Days",
    `wlauc cat2` = "Days"
  )
  if (stratification %in% names(units)) {
    paste0(label, " (", units[[stratification]], ")")
  } else {
    label
  }
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

  # Age has a special lower bound requested by the team.
  # Keep this before the generic -Inf replacement so age labels become
  # (18,35), not (0,35).
  if (identical(name, "age_cat")) {
    key <- gsub("\\s+", "", value)
    value[key %in% c("(-Inf,35]", "[-Inf,35]", "(-Inf,35)", "[-Inf,35)")] <- "(18,35)"
    value[key %in% c("(65,Inf)", "(65,Inf]", "[65,Inf)", "[65,Inf]", "(65,+Inf)", "(65,+Inf]")] <- "(>65)"
  }

  # For all other displayed interval categories, replace any -Inf lower bound
  # with 0. This covers labels such as (-Inf,75), (-Inf,250], and any future
  # category with a different cutoff.
  if (!identical(name, "age_cat")) {
    value <- sub("^\\s*\\(-Inf\\s*,", "(0,", value)
    value <- sub("^\\s*\\[-Inf\\s*,", "[0,", value)
    value <- sub("^\\s*\\(-\\s*Inf\\s*,", "(0,", value)
    value <- sub("^\\s*\\[-\\s*Inf\\s*,", "[0,", value)
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
    IQR = paste0(one_decimal(quantiles[, 1]), "-", one_decimal(quantiles[, 3])),
    check.names = FALSE
  )
}

result_output_id <- function(prefix, mod_id) {
  paste0(prefix, gsub("[^A-Za-z0-9_]", "_", mod_id))
}

result_palette <- function(n) {
  grDevices::hcl.colors(max(n, 3), "Dark 3")[seq_len(n)]
}

add_figure_watermark <- function() {
  usr <- par("usr")
  text(
    x = usr[2] - 0.015 * diff(usr[1:2]),
    y = usr[3] + 0.035 * diff(usr[3:4]),
    labels = "COMET-Lung Online",
    adj = c(1, 0),
    xpd = FALSE,
    cex = 0.62,
    col = grDevices::gray(0.45)
  )
}

combined_result_table <- function(dat, stratification, mods) {
  out <- lapply(mods, function(mod_id) {
    d <- dat[dat$name == stratification & dat$mods_id == mod_id, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    one <- format_result_table(d)
    data.frame(
      Outcome = display_mod(mod_id),
      one,
      check.names = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) {
    return(data.frame(Message = "No results are available.", check.names = FALSE))
  }
  do.call(rbind, out)
}

csv_result_footnote_lines <- c(
  "These simulated results are derived from the Computational Open-source Model for Evaluating Transplantation (COMET) developed under National Institutes of Health (NIH) National Heart Lung & Blood Institute (NHBI) grants R01HL153175 and R01HL153175.",
  "For methodologic details of the models, please see:",
  "Rose J, Gunsalus PR, Lehr CJ, Swiler MF, Dalton JE, Valapour M. A modular simulation framework for organ allocation. J Heart Lung Transplant. 2024 Aug;43(8):1326-1335. doi: 10.1016/j.healun.2024.04.063. Epub 2024 May 4. PMID: 38705499; PMCID: PMC11261589.",
  "Gunsalus PR, Rose J, Lehr CJ, Valapour M, Dalton JE. Creating synthetic populations in transplantation: A Bayesian approach enabling simulation without registry re-sampling. PLoS One. 2024 Mar 21;19(3):e0296839. doi: 10.1371/journal.pone.0296839. PMID: 38512928; PMCID: PMC10956776.",
  "Rose J, Gunsalus PR, Lehr CJ, Swiler MF, Dalton JE, Valapour M. A supply-based scoring approach to account for biological disadvantages in accessing lung transplant. J Heart Lung Transplant. 2025 Feb;44(2):193-201. doi: 10.1016/j.healun.2024.09.022. Epub 2024 Oct 15. PMID: 39412460; PMCID: PMC11840864."
)

add_csv_result_footnote <- function(out) {
  if (!is.data.frame(out) || ncol(out) == 0) return(out)

  blank_row <- as.data.frame(as.list(rep("", ncol(out))), stringsAsFactors = FALSE)
  names(blank_row) <- names(out)

  footnote_rows <- lapply(csv_result_footnote_lines, function(line) {
    row <- as.data.frame(as.list(rep("", ncol(out))), stringsAsFactors = FALSE)
    names(row) <- names(out)
    row[[1]] <- line
    row
  })

  do.call(rbind, c(list(out, blank_row), footnote_rows))
}

pptx_result_footnote_text <- function() {
  paste(csv_result_footnote_lines, collapse = "\n\n")
}

draw_result_figure <- function(d, stratification, mod_id, watermark = FALSE) {
  outcome_label <- outcome_axis_label(mod_id)
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
      yaxt = "n",
      main = paste(strwrap(outcome_label, width = 46), collapse = "\n"),
      cex.main = 0.95
    )
    grid(nx = NA, ny = NULL, col = "#e6e6e6")
    polygon(
      c(x, rev(x)),
      c(density, rep(0, length(density))),
      col = grDevices::adjustcolor(color, alpha.f = 0.45),
      border = NA
    )
    lines(x, density, lwd = 2, col = color)
    if (isTRUE(watermark)) add_figure_watermark()
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
  bottom_margin <- if (identical(stratification, "reg")) 9.4 else 8.4
  axis_title_line <- if (identical(stratification, "reg")) 7.4 else 6.4
  label_offset <- if (identical(stratification, "reg")) 0.070 else 0.075
  par(mar = c(bottom_margin, 4.5, 4.8, 1.2))

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
  label_y <- usr[3] - label_offset * diff(usr[3:4])
  text(
    x = seq_along(group_values),
    y = label_y,
    labels = group_labels,
    srt = 35,
    adj = 1,
    xpd = NA,
    cex = 0.78
  )
  mtext(stratification_axis_label(stratification), side = 1, line = axis_title_line)

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
  if (isTRUE(watermark)) add_figure_watermark()
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
    title = "Run an Experiment", value = "run",
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
        #policy-hover-card {
          display:none; position:fixed; z-index:10000; width:410px; max-width:calc(100vw - 24px);
          max-height:55vh; overflow-y:auto; padding:12px 14px; background:#fff;
          border:1px solid #b8c2cc; border-radius:7px; box-shadow:0 5px 18px rgba(0,0,0,0.20);
          color:#222; font-size:13px; line-height:1.4; pointer-events:auto;
        }
        #policy-hover-card .policy-hover-title {font-size:15px;font-weight:600;margin-bottom:5px;}
        #policy-hover-card .policy-hover-description {margin-bottom:7px;}
        #policy-hover-card a {font-weight:600;}
        #weight-hover-card {
          display:none; position:fixed; z-index:10001; width:390px; max-width:calc(100vw - 24px);
          max-height:55vh; overflow-y:auto; padding:12px 14px; background:#fff;
          border:1px solid #b8c2cc; border-radius:7px; box-shadow:0 5px 18px rgba(0,0,0,0.20);
          color:#222; font-size:13px; line-height:1.4; pointer-events:auto;
        }
        #weight-hover-card .weight-hover-title {font-size:15px;font-weight:600;margin-bottom:5px;}
        #weight-hover-card .weight-hover-description {margin-bottom:0; white-space:pre-line;}
        .weight-label-help {border-bottom:1px dotted #777; cursor:help;}
        #stratification-hover-card {
          display:none; position:fixed; z-index:10002; width:390px; max-width:calc(100vw - 24px);
          max-height:55vh; overflow-y:auto; padding:12px 14px; background:#fff;
          border:1px solid #b8c2cc; border-radius:7px; box-shadow:0 5px 18px rgba(0,0,0,0.20);
          color:#222; font-size:13px; line-height:1.4; pointer-events:auto;
        }
        #stratification-hover-card .stratification-hover-title {font-size:15px;font-weight:600;margin-bottom:5px;}
        #stratification-hover-card .stratification-hover-description {margin-bottom:0; white-space:pre-line;}
        .experiment-name-box {min-width:170px;margin-bottom:6px;}
        .label-box {width:330px;max-width:100%;}
        .requirements-card {background:#fffdf4;border:1px solid #eadca6;border-radius:6px;padding:7px 10px;margin-top:6px;font-size:12px;}
        .requirements-card ul {margin:3px 0 0 0;padding-left:18px;}
        .requirements-card li {margin-bottom:2px;line-height:1.25;}
        .weight-status .well {margin-bottom:7px;padding:8px 12px;}
        .weight-status .well h4 {margin:0 0 4px 0;font-size:20px;}
        .weight-status .well p {margin:2px 0;}
        .slider-disabled {opacity:0.55;}
        .slider-disabled .irs {pointer-events:none;}
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
        .download-row {display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin:0 0 10px 0;}
        .single-figure-download {text-align:right;margin-bottom:4px;}
        .single-figure-download .btn {padding:2px 8px;font-size:15px;line-height:1.2;}
        .download-all-pptx {margin-left:auto;}
        .disabled-see-result {opacity:0.45; cursor:not-allowed;}
      ")),
      tags$script(HTML("
        function formatSliderWeightNumber(x) {
          var raw = (x || '').toString().replace(/,/g, '').trim();
          if (!/^[-+]?\\d*\\.?\\d+(e[-+]?\\d+)?$/i.test(raw)) return x;
          var num = Number(raw);
          if (!isFinite(num)) return x;
          return num.toFixed(4).replace(/(\\.\\d{2,}?)0+$/, '$1');
        }

        function labelForSliderIndex(vals, txt) {
          var raw = (txt || '').toString().replace(/,/g, '').trim();

          // The actual Shiny slider still uses index values 1, 2, 3, ...
          // Internally, data-values stores the real CSV weights in the same order.
          // Only integer labels in the valid index range are mapped.
          if (!/^\\d+$/.test(raw)) return formatSliderWeightNumber(raw);

          var idx = parseInt(raw, 10);
          if (!isFinite(idx) || idx < 1 || idx > vals.length) {
            return formatSliderWeightNumber(raw);
          }

          return formatSliderWeightNumber(vals[idx - 1]);
        }

        function applyAllowedValueLabels() {
          $('.slider-box').each(function() {
            var box = $(this);
            var valsRaw = box.attr('data-values') || '';
            if (!valsRaw.length) return;

            var vals = valsRaw.split('|');

            // Attach a native Ion.RangeSlider prettify callback when possible.
            // Use box.find('input') instead of a complex selector so this line
            // cannot break the entire JavaScript block.
            try {
              var sliderInput = box.find('input').first();
              var ionSlider = sliderInput.data('ionRangeSlider');
              var appliedVals = box.attr('data-prettify-values') || '';

              if (ionSlider && appliedVals !== valsRaw) {
                ionSlider.update({
                  prettify_enabled: true,
                  prettify: function(num) {
                    return labelForSliderIndex(vals, String(Math.round(Number(num))));
                  }
                });
                box.attr('data-prettify-values', valsRaw);
              }
            } catch (e) {
              // Fallback rewriting below still handles displayed labels.
            }

            // Fallback and refresh pass. This also fixes labels drawn before
            // the prettify callback was attached.
            box.find('.irs-min, .irs-max, .irs-single, .irs-from, .irs-to').each(function() {
              $(this).text(labelForSliderIndex(vals, $(this).text()));
            });
          });
        }

        var policyHoverHideTimer = null;

        function policyHoverInfo(policyKey) {
          var item = $('#policy-hover-data .policy-hover-data-item').filter(function() {
            return $(this).attr('data-policy-key') === policyKey;
          }).first();
          if (!item.length) return null;
          return {
            title: item.attr('data-title') || '',
            description: item.attr('data-description') || '',
            url: item.attr('data-url') || ''
          };
        }

        function cancelPolicyHoverHide() {
          if (policyHoverHideTimer) {
            clearTimeout(policyHoverHideTimer);
            policyHoverHideTimer = null;
          }
        }

        function hidePolicyHoverCard() {
          cancelPolicyHoverHide();
          $('#policy-hover-card').hide().attr('aria-hidden', 'true');
        }

        function schedulePolicyHoverHide() {
          cancelPolicyHoverHide();
          policyHoverHideTimer = setTimeout(hidePolicyHoverCard, 300);
        }

        function positionPolicyHoverCard(anchor) {
          var card = $('#policy-hover-card');
          if (!card.length || !anchor) return;
          var rect = anchor.getBoundingClientRect();
          card.css({display: 'block', visibility: 'hidden', left: '0px', top: '0px'});
          var cardWidth = card.outerWidth();
          var cardHeight = card.outerHeight();
          var gap = 10;
          var left = rect.right + gap;
          var top = rect.top;

          if (left + cardWidth > window.innerWidth - 12) {
            left = rect.left - cardWidth - gap;
          }
          if (left < 12) left = 12;
          if (top + cardHeight > window.innerHeight - 12) {
            top = window.innerHeight - cardHeight - 12;
          }
          if (top < 12) top = 12;

          card.css({left: left + 'px', top: top + 'px', visibility: 'visible'});
        }

        function showPolicyHoverCard(policyKey, anchor) {
          var info = policyHoverInfo(policyKey);
          if (!info) return;
          cancelPolicyHoverHide();
          $('#policy-hover-title').text(info.title);
          $('#policy-hover-description').text(info.description);
          $('#policy-hover-link').attr('href', info.url);
          $('#policy-hover-card').attr('aria-hidden', 'false');
          positionPolicyHoverCard(anchor);
        }

        $(document).on('mouseenter', '.policy-select-box .selectize-input', function() {
          showPolicyHoverCard($('#policy_select').val(), this);
        });
        $(document).on('mouseleave', '.policy-select-box .selectize-input', schedulePolicyHoverHide);

        $(document).on('mouseenter', '.policy-select-box .selectize-dropdown .option', function() {
          showPolicyHoverCard($(this).attr('data-value'), this);
        });
        $(document).on('mouseleave', '.policy-select-box .selectize-dropdown .option', schedulePolicyHoverHide);

        $(document).on('mouseenter', '#policy-hover-card', cancelPolicyHoverHide);
        $(document).on('mouseleave', '#policy-hover-card', schedulePolicyHoverHide);
        $(window).on('resize scroll', hidePolicyHoverCard);

        var weightHoverHideTimer = null;

        function weightHoverInfo(weightKey) {
          var item = $('#weight-hover-data .weight-hover-data-item').filter(function() {
            return $(this).attr('data-weight-key') === weightKey;
          }).first();
          if (!item.length) return null;
          return {
            title: item.attr('data-title') || '',
            description: item.attr('data-description') || ''
          };
        }

        function cancelWeightHoverHide() {
          if (weightHoverHideTimer) {
            clearTimeout(weightHoverHideTimer);
            weightHoverHideTimer = null;
          }
        }

        function ensureWeightHoverCard() {
          var card = $('#weight-hover-card-global');
          if (!card.length) {
            card = $('<div/>', {
              id: 'weight-hover-card-global',
              role: 'tooltip',
              'aria-hidden': 'true'
            }).css({
              display: 'none',
              position: 'fixed',
              zIndex: 20001,
              width: '390px',
              maxWidth: 'calc(100vw - 24px)',
              maxHeight: '55vh',
              overflowY: 'auto',
              padding: '12px 14px',
              background: '#fff',
              border: '1px solid #b8c2cc',
              borderRadius: '7px',
              boxShadow: '0 5px 18px rgba(0,0,0,0.20)',
              color: '#222',
              fontSize: '13px',
              lineHeight: '1.4',
              pointerEvents: 'auto'
            });

            $('<div/>', {
              id: 'weight-hover-title-global'
            }).css({
              fontSize: '15px',
              fontWeight: '600',
              marginBottom: '5px'
            }).appendTo(card);

            $('<div/>', {
              id: 'weight-hover-description-global'
            }).css({
              marginBottom: '0',
              whiteSpace: 'pre-line'
            }).appendTo(card);

            $('body').append(card);
            card.on('mouseenter', cancelWeightHoverHide);
            card.on('mouseleave', scheduleWeightHoverHide);
          }
          return card;
        }

        function hideWeightHoverCard() {
          cancelWeightHoverHide();
          $('#weight-hover-card-global').hide().attr('aria-hidden', 'true');
          $('#weight-hover-card').hide().attr('aria-hidden', 'true');
        }

        function scheduleWeightHoverHide() {
          cancelWeightHoverHide();
          weightHoverHideTimer = setTimeout(hideWeightHoverCard, 250);
        }

        function positionWeightHoverCard(anchor) {
          var card = ensureWeightHoverCard();
          if (!card.length || !anchor) return;
          var rect = anchor.getBoundingClientRect();
          card.css({display: 'block', visibility: 'hidden', left: '0px', top: '0px'});
          var cardWidth = card.outerWidth();
          var cardHeight = card.outerHeight();
          var gap = 10;
          var left = rect.right + gap;
          var top = rect.top;

          if (left + cardWidth > window.innerWidth - 12) {
            left = rect.left - cardWidth - gap;
          }
          if (left < 12) left = 12;
          if (top + cardHeight > window.innerHeight - 12) {
            top = window.innerHeight - cardHeight - 12;
          }
          if (top < 12) top = 12;

          card.css({left: left + 'px', top: top + 'px', visibility: 'visible'});
        }

        function showWeightHoverCard(weightKey, anchor) {
          var info = weightHoverInfo(weightKey);
          if (!info) return;
          cancelWeightHoverHide();

          var card = ensureWeightHoverCard();
          $('#weight-hover-title-global').text(info.title);
          $('#weight-hover-description-global').text(info.description);
          card.attr('aria-hidden', 'false');

          $('#weight-hover-title').text(info.title);
          $('#weight-hover-description').text(info.description);
          $('#weight-hover-card').attr('aria-hidden', 'false');

          positionWeightHoverCard(anchor);
        }

        $(document).on('mouseenter', '.weight-label-help', function() {
          showWeightHoverCard($(this).attr('data-weight-key'), this);
        });
        $(document).on('mouseleave', '.weight-label-help', scheduleWeightHoverHide);
        $(document).on('mouseenter', '#weight-hover-card', cancelWeightHoverHide);
        $(document).on('mouseleave', '#weight-hover-card', scheduleWeightHoverHide);
        $(window).on('resize scroll', hideWeightHoverCard);

        var stratificationHoverHideTimer = null;

        function ensureStratificationHoverCard() {
          var card = $('#stratification-hover-card-global');
          if (!card.length) {
            card = $('<div/>', {
              id: 'stratification-hover-card-global',
              role: 'tooltip',
              'aria-hidden': 'true'
            }).css({
              display: 'none',
              position: 'fixed',
              zIndex: 20000,
              width: '390px',
              maxWidth: 'calc(100vw - 24px)',
              maxHeight: '55vh',
              overflowY: 'auto',
              padding: '12px 14px',
              background: '#fff',
              border: '1px solid #b8c2cc',
              borderRadius: '7px',
              boxShadow: '0 5px 18px rgba(0,0,0,0.20)',
              color: '#222',
              fontSize: '13px',
              lineHeight: '1.4',
              pointerEvents: 'auto'
            });

            $('<div/>', {
              id: 'stratification-hover-title-global'
            }).css({
              fontSize: '15px',
              fontWeight: '600',
              marginBottom: '5px'
            }).appendTo(card);

            $('<div/>', {
              id: 'stratification-hover-description-global'
            }).css({
              marginBottom: '0',
              whiteSpace: 'pre-line'
            }).appendTo(card);

            $('body').append(card);
            card.on('mouseenter', cancelStratificationHoverHide);
            card.on('mouseleave', scheduleStratificationHoverHide);
          }
          return card;
        }

        function stratificationHoverInfo(keyOrLabel) {
          var txt = (keyOrLabel || '').toString().trim();
          if (!txt.length) return null;
          var normalized = txt.toLowerCase();

          if (
            normalized === 'wlauc_cat' ||
            normalized === 'wlauc_cat2' ||
            normalized === 'wlauc cat2' ||
            normalized === 'wlauc category' ||
            normalized === 'wlauc category (expanded)'
          ) {
            return {
              title: normalized === 'wlauc_cat2' || normalized === 'wlauc cat2' || normalized === 'wlauc category (expanded)' ? 'WLAUC Category (Expanded)' : 'WLAUC Category',
              description: 'WLAUC = Waitlist Area Under the Curve, defined as the predicted number of days a candidate will survive without a transplant (Maximum = 365).'
            };
          }

          if (
            normalized === 'dx_grp' ||
            normalized === 'diagnosis groups' ||
            normalized === 'diagnosis group'
          ) {
            return {
              title: 'Diagnosis Groups',
              description: 'Group A: Obstructive lung disease\\nGroup B: Pulmonary vascular disease\\nGroup C: Cystic fibrosis and immunodeficiency disorders\\nGroup D: Restrictive lung disease'
            };
          }

          var item = $('#stratification-hover-data .stratification-hover-data-item').filter(function() {
            var dataKey = ($(this).attr('data-stratification-key') || '').toString().trim();
            var dataTitle = ($(this).attr('data-title') || '').toString().trim();
            return dataKey === txt || dataTitle.toLowerCase() === normalized;
          }).first();

          if (!item.length) return null;
          return {
            title: item.attr('data-title') || '',
            description: item.attr('data-description') || ''
          };
        }

        function cancelStratificationHoverHide() {
          if (stratificationHoverHideTimer) {
            clearTimeout(stratificationHoverHideTimer);
            stratificationHoverHideTimer = null;
          }
        }

        function hideStratificationHoverCard() {
          cancelStratificationHoverHide();
          $('#stratification-hover-card-global').hide().attr('aria-hidden', 'true');
          $('#stratification-hover-card').hide().attr('aria-hidden', 'true');
        }

        function scheduleStratificationHoverHide() {
          cancelStratificationHoverHide();
          stratificationHoverHideTimer = setTimeout(hideStratificationHoverCard, 250);
        }

        function positionStratificationHoverCard(anchor) {
          var card = ensureStratificationHoverCard();
          if (!card.length || !anchor) return;

          var rect = anchor.getBoundingClientRect();
          card.css({display: 'block', visibility: 'hidden', left: '0px', top: '0px'});

          var cardWidth = card.outerWidth();
          var cardHeight = card.outerHeight();
          var gap = 10;
          var left = rect.right + gap;
          var top = rect.top;

          if (left + cardWidth > window.innerWidth - 12) {
            left = rect.left - cardWidth - gap;
          }
          if (left < 12) left = 12;

          if (top + cardHeight > window.innerHeight - 12) {
            top = window.innerHeight - cardHeight - 12;
          }
          if (top < 12) top = 12;

          card.css({left: left + 'px', top: top + 'px', visibility: 'visible'});
        }

        function showStratificationHoverCard(keyOrLabel, anchor) {
          var info = stratificationHoverInfo(keyOrLabel);
          if (!info) return false;

          cancelStratificationHoverHide();

          var card = ensureStratificationHoverCard();
          $('#stratification-hover-title-global').text(info.title);
          $('#stratification-hover-description-global').text(info.description);
          card.attr('aria-hidden', 'false');

          positionStratificationHoverCard(anchor);
          return true;
        }

        function stratificationOptionFromEvent(event) {
          var el = document.elementFromPoint(event.clientX, event.clientY);
          if (!el) return null;

          var option = $(el).closest('.selectize-dropdown .option, .selectize-dropdown [data-selectable]');
          if (!option.length) return null;

          return option;
        }

        function tryShowStratificationHoverForOption(option) {
          if (!option || !option.length) return false;

          var rawValue = (option.attr('data-value') || option.data('value') || '').toString().trim();
          var visibleLabel = (option.text() || '').toString().trim();

          if (showStratificationHoverCard(rawValue, option[0])) return true;
          if (showStratificationHoverCard(visibleLabel, option[0])) return true;

          return false;
        }

        $(document).on('mouseenter', '.results-stratification .selectize-input', function() {
          showStratificationHoverCard($('#result_name').val(), this);
        });
        $(document).on('mouseleave', '.results-stratification .selectize-input', scheduleStratificationHoverHide);

        $(document).on('mouseenter', '.comparison-stratification .selectize-input', function() {
          showStratificationHoverCard($('#comparison_name').val(), this);
        });
        $(document).on('mouseleave', '.comparison-stratification .selectize-input', scheduleStratificationHoverHide);

        $(document).on('mouseenter mouseover', '.selectize-dropdown .option, .selectize-dropdown [data-selectable]', function() {
          if (!tryShowStratificationHoverForOption($(this))) {
            scheduleStratificationHoverHide();
          }
        });

        $(document).on('mousemove', function(event) {
          var option = stratificationOptionFromEvent(event);
          if (option && option.length) {
            if (tryShowStratificationHoverForOption(option)) return;
          }

          var card = $('#stratification-hover-card-global');
          if (card.length && card.is(':visible')) {
            var hoverCard = $(event.target).closest('#stratification-hover-card-global').length > 0;
            var hoverInput = $(event.target).closest('.results-stratification .selectize-input, .comparison-stratification .selectize-input').length > 0;
            if (!hoverCard && !hoverInput && (!option || !option.length)) {
              scheduleStratificationHoverHide();
            }
          }
        });

        $(document).on('mouseleave', '.selectize-dropdown .option, .selectize-dropdown [data-selectable]', scheduleStratificationHoverHide);

        $(document).on('mouseenter', '#stratification-hover-card-global', cancelStratificationHoverHide);
        $(document).on('mouseleave', '#stratification-hover-card-global', scheduleStratificationHoverHide);
        $(window).on('resize scroll', hideStratificationHoverCard);

        function sendCometKeepAlivePing() {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('comet_keepalive_ping', new Date().getTime(), {priority: 'event'});
          }
        }

        var cometKeepAliveInterval = null;
        function startCometKeepAlive() {
          if (cometKeepAliveInterval !== null) return;
          sendCometKeepAlivePing();
          cometKeepAliveInterval = setInterval(sendCometKeepAlivePing, 5 * 60 * 1000);
        }

        $(document).on('shiny:connected', function() {
          $('#experiment_label').attr('maxlength', 60);
          $('#saved_label_edit').attr('maxlength', 60);
          applyAllowedValueLabels();
          setInterval(applyAllowedValueLabels, 120);
          startCometKeepAlive();
        });

        $(document).on('shiny:value shiny:inputchanged shown.bs.tab', function() {
          setTimeout(applyAllowedValueLabels, 10);
          setTimeout(applyAllowedValueLabels, 80);
          setTimeout(applyAllowedValueLabels, 180);
        });
      ")),
      tags$div(
        id = "policy-hover-data",
        style = "display:none;",
        lapply(names(policy_hover_info), function(policy_key) {
          info <- policy_hover_info[[policy_key]]
          tags$div(
            class = "policy-hover-data-item",
            `data-policy-key` = policy_key,
            `data-title` = info$title,
            `data-description` = info$description,
            `data-url` = info$url
          )
        })
      ),
      tags$div(
        id = "policy-hover-card",
        role = "tooltip",
        `aria-hidden` = "true",
        tags$div(id = "policy-hover-title", class = "policy-hover-title"),
        tags$div(id = "policy-hover-description", class = "policy-hover-description"),
        tags$a(
          id = "policy-hover-link",
          href = "#",
          target = "_blank",
          rel = "noopener noreferrer",
          "Further details"
        )
      ),
      tags$div(
        id = "weight-hover-data",
        style = "display:none;",
        lapply(names(weight_hover_info), function(weight_key) {
          tags$div(
            class = "weight-hover-data-item",
            `data-weight-key` = weight_key,
            `data-title` = weight_labels[[weight_key]],
            `data-description` = weight_hover_info[[weight_key]]
          )
        })
      ),
      tags$div(
        id = "weight-hover-card",
        role = "tooltip",
        `aria-hidden` = "true",
        tags$div(id = "weight-hover-title", class = "weight-hover-title"),
        tags$div(id = "weight-hover-description", class = "weight-hover-description")
      ),
      tags$div(
        id = "stratification-hover-data",
        style = "display:none;",
        lapply(names(stratification_hover_info), function(stratification_key) {
          tags$div(
            class = "stratification-hover-data-item",
            `data-stratification-key` = stratification_key,
            `data-title` = display_name(stratification_key),
            `data-description` = stratification_hover_info[[stratification_key]]
          )
        })
      ),
      tags$div(
        id = "stratification-hover-card",
        role = "tooltip",
        `aria-hidden` = "true",
        tags$div(id = "stratification-hover-title", class = "stratification-hover-title"),
        tags$div(id = "stratification-hover-description", class = "stratification-hover-description")
      ),
      h3("Run an Experiment"),
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
          tags$div(class = "label-box", textInput("experiment_label", "Experiment label (editable)", value = "", placeholder = "Editable auto-generated label, up to 60 characters")),
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
          tags$div(
            class = "comparison-stratification",
            selectInput("comparison_name", "Stratified by", choices = NULL)
          ),
          uiOutput("comparison_download_ui")
        ),
        column(
          width = 8,
          tags$div(class = "result-card",
                   h4(textOutput("comparison_title", inline = TRUE)),
                   tableOutput("comparison_table"))
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
  auto_label_text <- reactiveVal("")
  label_user_edited <- reactiveVal(FALSE)

  # Lightweight no-op heartbeat from the browser. This does not modify app
  # state or outputs; it simply receives the periodic timestamp sent by the
  # client-side JavaScript while the app page remains open.
  observeEvent(input$comet_keepalive_ping, {
    invisible(NULL)
  }, ignoreInit = TRUE)

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
    label_user_edited(FALSE)
    weight_mode("default")
    run_message_text(NULL)
  }, ignoreInit = TRUE)

  slider_label_with_hover <- function(w, fixed = FALSE) {
    label_text <- weight_labels[[w]]
    hover_text <- if (w %in% names(weight_hover_info)) weight_hover_info[[w]] else ""
    tagList(
      tags$span(
        class = "weight-label-help",
        `data-weight-key` = w,
        title = hover_text,
        label_text
      ),
      if (fixed) tags$span(" (fixed)")
    )
  }

  output$weight_sliders <- renderUI({
    pd <- current_policy_data()
    mode <- weight_mode()
    tags$div(
      class = "slider-grid",
      lapply(pd$weight_cols, function(w) {
        if (identical(mode, "default")) {
          # A policy default may not be one of the permitted custom stops.
          # Include the exact default while default mode is shown so the
          # disabled handle, its label, and the table all represent the same
          # underlying weight.
          default_value <- round(as.numeric(pd$default_row[[w]]), 12)
          vals <- sort(unique(c(pd$weight_values[[w]], default_value)))
          is_fixed <- length(vals) <= 1
          disabled <- TRUE
          slider_n <- max(1, length(vals))
          slider_value <- which.min(abs(vals - default_value))
          label <- slider_label_with_hover(w, fixed = FALSE)
        } else {
          # In custom mode, slider stops come only from non-default rows.
          vals <- pd$weight_values[[w]]
          is_fixed <- length(vals) <= 1
          disabled <- is_fixed
          slider_n <- max(1, length(vals))
          # If the exact default is unavailable here, start at the nearest
          # permitted custom stop. A slight handle movement is expected.
          slider_value <- value_to_slider_index(pd, w, pd$custom_start_values[[w]])
          label <- slider_label_with_hover(w, fixed = is_fixed)
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
    label_user_edited(FALSE)
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

    label_user_edited(FALSE)
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

  observeEvent(input$experiment_label, {
    current <- input$experiment_label
    if (is.null(current)) return()
    if (!identical(current, auto_label_text())) {
      label_user_edited(TRUE)
    }
  }, ignoreInit = TRUE)

  observe({
    pd <- current_policy_data()
    values <- current_slider_values()
    generated <- auto_experiment_label(pd, values, weight_mode())
    auto_label_text(generated)

    if (!isTRUE(label_user_edited())) {
      updateTextInput(session, "experiment_label", value = generated)
    }
  })

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
    label_user_edited(FALSE)
    updateTextInput(session, "experiment_label", value = auto_label_text())
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

      output[[result_output_id("download_plot_", current_mod)]] <- downloadHandler(
        filename = function() {
          exp <- current_experiment(); req(exp, input$result_name)
          paste0(
            safe_filename(paste(exp$name, exp$label, display_name(input$result_name), display_mod(current_mod), sep = "_")),
            ".png"
          )
        },
        content = function(file) {
          req(input$result_name)
          dat <- experiment_skew()
          d <- dat[dat$name == input$result_name & dat$mods_id == current_mod, , drop = FALSE]
          validate(need(nrow(d) > 0, "No figure is available."))
          grDevices::png(file, width = 1600, height = 1050, res = 150)
          on.exit(grDevices::dev.off(), add = TRUE)
          draw_result_figure(d, input$result_name, current_mod, watermark = TRUE)
        }
      )
    })
  }

  output$download_results_csv <- downloadHandler(
    filename = function() {
      exp <- current_experiment(); req(exp, input$result_name)
      paste0(
        safe_filename(paste(exp$name, exp$label, display_name(input$result_name), "results", sep = "_")),
        ".csv"
      )
    },
    content = function(file) {
      req(input$result_name)
      dat <- experiment_skew()
      mods <- available_result_mods()
      out <- combined_result_table(dat, input$result_name, mods)
      out <- add_csv_result_footnote(out)
      utils::write.csv(out, file, row.names = FALSE)
    }
  )

  output$download_all_figures_pptx <- downloadHandler(
    filename = function() {
      exp <- current_experiment(); req(exp, input$result_name)
      paste0(
        safe_filename(paste(exp$name, exp$label, display_name(input$result_name), "figures", sep = "_")),
        ".pptx"
      )
    },
    content = function(file) {
      req(input$result_name)
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("The officer package is required for PPTX downloads. Please install it with install.packages('officer').")
      }

      dat <- experiment_skew()
      mods <- available_result_mods()
      ppt <- officer::read_pptx()

      for (mod_id in mods) {
        d <- dat[dat$name == input$result_name & dat$mods_id == mod_id, , drop = FALSE]
        if (nrow(d) == 0) next

        img <- tempfile(fileext = ".png")
        grDevices::png(img, width = 1600, height = 1050, res = 150)
        draw_result_figure(d, input$result_name, mod_id, watermark = TRUE)
        grDevices::dev.off()

        ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
        ppt <- officer::ph_with(
          ppt,
          value = officer::external_img(img, width = 9.2, height = 6.0),
          location = officer::ph_location(left = 0.55, top = 0.45, width = 9.2, height = 6.0)
        )
      }

      ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
      ppt <- officer::ph_with(
        ppt,
        value = "Notes and References",
        location = officer::ph_location(left = 0.55, top = 0.35, width = 9.2, height = 0.45)
      )
      ppt <- officer::ph_with(
        ppt,
        value = pptx_result_footnote_text(),
        location = officer::ph_location(left = 0.55, top = 0.95, width = 9.2, height = 5.8)
      )

      print(ppt, target = file)
    }
  )

  output$result_dashboard <- renderUI({
    req(input$result_name, input$result_view)
    mods <- available_result_mods()
    validate(need(length(mods) > 0, "No outcomes are available for this selection."))

    if (identical(input$result_view, "tables")) {
      tagList(
        tags$div(
          class = "download-row",
          downloadButton("download_results_csv", "Download CSV")
        ),
        tags$div(
          class = "results-dashboard",
          lapply(mods, function(mod_id) {
            tags$div(
              class = "result-dashboard-card",
              tags$h4(display_mod(mod_id)),
              tableOutput(result_output_id("result_table_", mod_id))
            )
          })
        )
      )
    } else {
      tagList(
        tags$div(
          class = "download-row",
          tags$span(class = "text-muted", "Download all figures:"),
          downloadButton("download_all_figures_pptx", "Download All", class = "download-all-pptx")
        ),
        tags$div(
          class = "results-dashboard",
          lapply(mods, function(mod_id) {
            tags$div(
              class = "result-dashboard-card",
              tags$div(
                class = "single-figure-download",
                downloadButton(result_output_id("download_plot_", mod_id), label = HTML("&#x2B07;"), title = "Download image")
              ),
              plotOutput(result_output_id("result_plot_", mod_id), height = "380px")
            )
          })
        )
      )
    }
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

  output$comparison_download_ui <- renderUI({
    req(length(input$comparison_experiments) > 0, input$comparison_name)
    downloadButton(
      "download_comparison_csv",
      "Download comparison CSV",
      class = "btn-primary"
    )
  })

  comparison_table_data <- reactive({
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

    # Sort comparison rows by outcome first, then category within each outcome.
    # This keeps categories such as Female/Male adjacent for the same outcome,
    # instead of showing all outcomes for one category before the next category.
    comparison_mod_order <- c(
      "can_count", "tx_count", "wait_death", "wld_ppy", "tx_ppy",
      "med_wlt", "med_dist", "post_tx_death", "ptd_ppy", "med_offer"
    )
    outcome_rank <- match(all_pairs$mods_id, comparison_mod_order)
    outcome_rank[is.na(outcome_rank)] <- length(comparison_mod_order) + seq_len(sum(is.na(outcome_rank)))

    category_labels <- display_category(input$comparison_name, all_pairs$value)
    category_numeric <- suppressWarnings(as.numeric(all_pairs$value))
    category_sort <- ifelse(is.na(category_numeric), NA_real_, category_numeric)
    fallback_category_sort <- rank(as.character(category_labels), ties.method = "first")
    category_sort[is.na(category_sort)] <- fallback_category_sort[is.na(category_sort)]

    all_pairs <- all_pairs[order(outcome_rank, category_sort, as.character(category_labels)), , drop = FALSE]
    category_labels <- display_category(input$comparison_name, all_pairs$value)

    out <- data.frame(
      Outcome = vapply(all_pairs$mods_id, display_mod, character(1)),
      Category = category_labels,
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
  })

  output$comparison_table <- renderTable({
    comparison_table_data()
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_comparison_csv <- downloadHandler(
    filename = function() {
      req(input$comparison_name)
      paste0(
        "COMET_Lung_comparison_",
        safe_filename(display_name(input$comparison_name)),
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "_",
        short_download_code(),
        ".csv"
      )
    },
    content = function(file) {
      out <- comparison_download_table(comparison_table_data())
      out <- add_csv_result_footnote(out)
      write.csv(
        out,
        file,
        row.names = FALSE,
        na = "",
        fileEncoding = "UTF-8"
      )
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
