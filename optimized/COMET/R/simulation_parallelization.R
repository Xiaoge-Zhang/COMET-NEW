#' Run Multiple COMET Simulations (Optionally in Parallel)
#'
#' This function executes multiple independent runs of
#' \code{\link{run_simulation}} using distinct random seeds.
#' Runs may be executed sequentially or in parallel via
#' the \pkg{future} framework.
#'
#' @param n_runs Integer. Number of independent simulation runs.
#' @param seeds Optional integer vector of length \code{n_runs}.
#'   Each element specifies the random seed used for one run.
#'   If \code{NULL}, seeds are generated internally.
#' @param parallel Logical. If \code{TRUE}, simulations are executed
#'   in parallel using \pkg{future.apply}. If \code{FALSE}, runs are
#'   executed sequentially.
#' @param workers Integer. Number of parallel workers to use when
#'   \code{parallel = TRUE}. Defaults to the minimum of
#'   \code{n_runs} and the number of available CPU cores.
#' @param days Integer. Number of days to simulate.
#' @param can_start Integer. Initial number of candidates in the waitlist.
#' @param match_alg Function. Matching algorithm used by
#'   \code{\link{run_simulation}} (e.g., \code{match_cas}).
#' @param ... Additional arguments passed directly to
#'   \code{\link{run_simulation}}.
#' @param desired Character. Desired match strategy passed to
#'   \code{\link{run_simulation}}.
#' @param return_params Logical. If \code{TRUE}, model parameters
#'   are returned alongside simulation results.
#' @param include_matches Logical. If \code{TRUE}, detailed match
#'   records are included in the output.
#'
#' @return A \code{tibble} with one row per simulation run, containing:
#' \itemize{
#'   \item \code{run_id}: Integer run index.
#'   \item \code{seed}: Random seed used for the run.
#'   \item \code{result}: Output object returned by
#'     \code{\link{run_simulation}}.
#' }
#'
#' @seealso \code{\link{run_simulation}}
#'
#' @export
#'
simulation_parallelization <- function(
    n_runs,
    seeds        = NULL,
    parallel     = FALSE,
    workers      = NULL,
    days         = 730,
    can_start    = 1200,
    match_alg    = match_cas,   # function, not match_cas()
    ...,
    desired      = "random",
    return_params   = FALSE,
    include_matches = FALSE
) {
  tic_overall <- Sys.time()
  # basic checks
  if (!is.numeric(n_runs) || length(n_runs) != 1L || n_runs < 1) {
    stop("n_runs must be a positive scalar integer.")
  }
  n_runs <- as.integer(n_runs)

  # handle seeds
  if (is.null(seeds)) {
    # if the caller wants reproducible seeds, they can call set.seed() before
    seeds <- sample.int(1e8, n_runs)

  } else if (length(seeds) == 1L) {
    # single seed provided: generate a deterministic seed set for all runs
    if (!is.numeric(seeds) || is.na(seeds)) stop("'seeds' must be a non-NA numeric value.")
    master_seed <- as.integer(seeds)

    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }

    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)

    set.seed(master_seed)
    seeds <- sample.int(1e8, n_runs)

  } else {
    if (length(seeds) != n_runs) {
      stop("Length of 'seeds' (", length(seeds),
           ") must equal 'n_runs' (", n_runs, ").")
    }
  }

  # one-run helper
  run_one <- function(one_seed) {
    res <- run_simulation(
      days            = days,
      can_start       = can_start,
      match_alg       = match_alg,
      ...,
      seed            = one_seed,
      desired         = desired,
      return_params   = return_params,
      include_matches = include_matches
    )
    list(seed = one_seed, result = res)
  }

  # sequential or parallel
  if (!parallel) {

    res_list <- lapply(seeds, run_one)

  } else {

    if (!requireNamespace("future.apply", quietly = TRUE) ||
        !requireNamespace("future", quietly = TRUE)) {
      stop("parallel = TRUE requires the 'future' and 'future.apply' packages.")
    }

    if (is.null(workers)) {
      workers <- min(n_runs, future::availableCores())
    }

    # save current plan and restore it on exit
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(future::multisession, workers = workers)

    res_list <- future.apply::future_lapply(
      seeds,
      run_one,
      future.seed = TRUE
    )

  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required to build the result table.")
  }

  result_tbl <- tibble::tibble(
    run_id = seq_len(n_runs),
    seed   = vapply(res_list, `[[`, numeric(1L), "seed"),
    result = lapply(res_list, `[[`, "result")
  )
  toc_overall <- Sys.time()
  total_runtime <- as.numeric(difftime(toc_overall, tic_overall, units = "secs"))
  cat("parallelization total runtime: ", round(total_runtime, 2), " secs\n")

  return(result_tbl)
}
