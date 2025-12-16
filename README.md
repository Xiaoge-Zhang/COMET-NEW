# COMET-NEW

This repository contains two side-by-side versions of the COMET R package:

- **original/COMET**: an upstream snapshot of the original COMET implementation, preserved for baseline behavior and reproducibility.
- **optimized/COMET**: an optimized and extended version (package name: **COMETopt**) with performance improvements and additional functionality, while preserving the original API and outputs.

The purpose of this repository is to enable fair benchmarking (baseline vs optimized), reproducible experimentation, and ongoing development without losing a clean reference implementation.

---

## Repository structure

```
original/COMET/     # baseline snapshot (DO NOT MODIFY)
optimized/COMET/    # optimized fork (COMETopt)
```

### Important rule

**Never modify anything under `original/COMET/`.**  
This folder should be treated as a frozen reference baseline for comparison and reproducibility.

---

## Working with RStudio projects

Both folders are independent R package projects (each contains a `.Rproj` file).

To work with both versions simultaneously:

1. Open `original/COMET/COMET.Rproj` in one RStudio session (baseline).
2. Open `optimized/COMET/COMET.Rproj` in a separate RStudio session (optimized).

Running them in separate R sessions avoids namespace conflicts and ensures clean testing.

---

## Dependencies

### System requirements (Windows)

- R (tested with R 4.5.x)
- Rtools (required to compile C++ code in `src/`)

### R packages

All required dependencies are declared in each package’s `DESCRIPTION` file.

In addition, COMET requires the **cometdata** package for full functionality.

Optional parallel execution in COMETopt uses:

- `future`
- `future.apply`

---

## Installation

### Install cometdata (required for both versions)

```r
install.packages("remotes")
remotes::install_github("ClevelandClinicQHS/cometdata", upgrade = "never")
```

### Install baseline COMET

From an R session opened at `original/COMET`:

```r
install.packages("remotes")
remotes::install_local(".", upgrade = "never", force = TRUE)
library(COMET)
```

### Install optimized COMET (COMETopt)

From an R session opened at `optimized/COMET`:

```r
install.packages("remotes")
remotes::install_local(".", upgrade = "never", force = TRUE)
library(COMETopt)
```

> **Note (Windows):** If you modify compiled C++ code (`src/*.cpp`), restart the R session before reinstalling due to DLL locking.

---

## Example: run a short simulation

The following example works in both COMET and COMETopt (run it in the corresponding RStudio session):

```r
r2 <- run_simulation(
  days = 10,
  can_start = 50,
  match_alg = match_cas,
  wl_model = "CAS23",
  post_tx_model = "CAS23",
  wl_weight = 0.25,
  post_tx_weight = 0.25,
  wl_cap = 365,
  post_tx_cap = 1826,
  bio_weight = 0.15,
  pld_weight = 0.05,
  peds_weight = 0.2,
  efficiency_weight = 0.1,
  seed = 26638
)
```

---

## Example: run multiple simulations (COMETopt only)

COMETopt provides a helper function for running multiple independent simulations, optionally in parallel.

### Sequential execution

```r
out <- simulation_parallelization(
  n_runs = 2,
  days = 10,
  can_start = 50,
  match_alg = match_cas,
  wl_model = "CAS23",
  post_tx_model = "CAS23",
  wl_weight = 0.25,
  post_tx_weight = 0.25,
  wl_cap = 365,
  post_tx_cap = 1826,
  bio_weight = 0.15,
  pld_weight = 0.05,
  peds_weight = 0.2,
  efficiency_weight = 0.1
)
```

### Parallel execution

```r
install.packages(c("future", "future.apply"))

out2 <- simulation_parallelization(
  n_runs = 2,
  parallel = TRUE,
  workers = 2,
  days = 10,
  can_start = 50,
  match_alg = match_cas,
  wl_model = "CAS23",
  post_tx_model = "CAS23",
  wl_weight = 0.25,
  post_tx_weight = 0.25,
  wl_cap = 365,
  post_tx_cap = 1826,
  bio_weight = 0.15,
  pld_weight = 0.05,
  peds_weight = 0.2,
  efficiency_weight = 0.1
)
```

---

## Development workflow (important)

### Modifying R code only (`R/*.R`)

You do **not** need to reinstall the package each time. Use:

```r
install.packages("devtools")
devtools::load_all()
```

### Modifying C++ code or package metadata

If you change:

- `src/*.cpp`
- `DESCRIPTION`
- roxygen exports / `NAMESPACE`

You should:

1. Restart the R session
2. Reinstall the package

```r
remotes::install_local(".", upgrade = "never", force = TRUE)
```

### Adding new exported functions

After adding roxygen comments:

```r
install.packages("roxygen2")
roxygen2::roxygenise()
```

Then restart and reinstall.

---

## Adding new dependencies

- Add optional dependencies to `Suggests:` and guard them with `requireNamespace()`.
- Add required runtime dependencies to `Imports:`.

After updating `DESCRIPTION`, run `roxygen2::roxygenise()` and reinstall the package.

---

## Notes

- `original/COMET` is intentionally preserved as a clean, immutable baseline.
- `optimized/COMET` preserves the original public API wherever possible to ensure drop-in compatibility and fair benchmarking.
