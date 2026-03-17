## =========================================================
## S4 Result object (COMETResult)
## File: R/s4_result.R
## =========================================================

setClass(
  "COMETResult",
  slots = c(
    policy = "COMETPolicy",
    config = "COMETConfig",
    data = "ANY",              # keep minimal-change: store the original COMET list/tibbles
    timing = "list",
    decision_log = "character",
    call = "ANY"
  ),
  prototype = list(
    data = list(),
    timing = list(),
    decision_log = character(),
    call = quote(NULL)
  )
)

## ---- generics ----
setGeneric("getData", function(result) standardGeneric("getData"))
setGeneric("getTiming", function(result) standardGeneric("getTiming"))
setGeneric("getDecisionLog", function(result) standardGeneric("getDecisionLog"))
setGeneric("metrics", function(result) standardGeneric("metrics"))

## ---- methods ----
setMethod("show", "COMETResult", function(object) {
  cat("<COMETResult>\n")
  cat("  policy: ", policyLabel(object@policy), "\n", sep = "")
  cat("  days: ", object@config@days, "\n", sep = "")
  cat("  include_matches: ", object@config@include_matches, "\n", sep = "")
  cat("  return_params: ", object@config@return_params, "\n", sep = "")
  cat("  has data: ", length(object@data) > 0, "\n", sep = "")
  if (length(object@timing) > 0 && !is.null(object@timing$total)) {
    cat("  total runtime (sec): ", round(object@timing$total, 2), "\n", sep = "")
  }
})

setMethod("getData", "COMETResult", function(result) result@data)
setMethod("getTiming", "COMETResult", function(result) result@timing)
setMethod("getDecisionLog", "COMETResult", function(result) result@decision_log)

## A small, stable summary of key counts (works even if data internals change)
setMethod("summary", "COMETResult", function(object, ...) {
  cat("Policy:", policyLabel(object@policy), "\n")
  cat("Days:", object@config@days, "\n")

  tmg <- object@timing
  if (!is.null(tmg$total)) cat("Total runtime (sec):", round(tmg$total, 2), "\n")

  ## Try to pull common tables by conventional names from your existing output list.
  d <- object@data

  safe_nrow <- function(x) if (is.data.frame(x)) nrow(x) else NA_integer_

  wl_dead  <- if (!is.null(d$waitlist_death_database)) safe_nrow(d$waitlist_death_database) else NA_integer_
  post_dead <- if (!is.null(d$post_tx_death_database)) safe_nrow(d$post_tx_death_database) else NA_integer_
  non_used <- if (!is.null(d$non_used_donors)) safe_nrow(d$non_used_donors) else NA_integer_

  cat("Waitlist deaths:", wl_dead, "\n")
  cat("Post-tx deaths:", post_dead, "\n")
  cat("Non-used donors:", non_used, "\n")

  ## if per-iteration summary exists
  if (!is.null(tmg$per_iteration_timing_summary)) {
    s <- tmg$per_iteration_timing_summary
    cat("Total matches:", as.integer(s$total_matches), "\n")
    cat("Total transplants:", as.integer(s$total_transplants), "\n")
  }

  invisible(object)
})

## metrics(): return a compact named list for comparisons
setMethod("metrics", "COMETResult", function(result) {
  tmg <- result@timing
  d <- result@data

  safe_nrow <- function(x) if (is.data.frame(x)) nrow(x) else NA_integer_

  out <- list(
    policy = policyLabel(result@policy),
    days = result@config@days,
    runtime_total_sec = if (!is.null(tmg$total)) as.numeric(tmg$total) else NA_real_,
    waitlist_deaths = if (!is.null(d$waitlist_death_database)) safe_nrow(d$waitlist_death_database) else NA_integer_,
    post_tx_deaths = if (!is.null(d$post_tx_death_database)) safe_nrow(d$post_tx_death_database) else NA_integer_,
    non_used_donors = if (!is.null(d$non_used_donors)) safe_nrow(d$non_used_donors) else NA_integer_
  )

  if (!is.null(tmg$per_iteration_timing_summary)) {
    s <- tmg$per_iteration_timing_summary
    out$total_matches <- as.integer(s$total_matches)
    out$total_transplants <- as.integer(s$total_transplants)
    out$avg_alive_candidates <- as.numeric(s$avg_alive_candidates)
    out$avg_donors <- as.numeric(s$avg_donors)
  } else {
    out$total_matches <- NA_integer_
    out$total_transplants <- NA_integer_
    out$avg_alive_candidates <- NA_real_
    out$avg_donors <- NA_real_
  }

  out
})

## ---- constructor ----
COMETResult <- function(policy, config, data, timing = list(), decision_log = character(), call = quote(NULL)) {
  if (!is(policy, "COMETPolicy")) stop("policy must be a COMETPolicy.", call. = FALSE)
  if (!is(config, "COMETConfig")) stop("config must be a COMETConfig.", call. = FALSE)

  new(
    "COMETResult",
    policy = policy,
    config = config,
    data = data,
    timing = timing,
    decision_log = decision_log,
    call = call
  )
}
