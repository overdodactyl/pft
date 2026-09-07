# Benchmark for pft::pft_interpret() cited in the SoftwareX paper.
#
# Protocol (kept minimal so it does not add a runtime dependency):
#   1. fixed-seed cohort generator, physiologically constrained so
#      FEV1 <= FVC and TLC > FVC (matches the paper's Section 3.2
#      generator);
#   2. one warm-up run at n = 1,000 (results discarded);
#   3. `reps` measured repetitions at n = 100,000 in a fresh serial
#      R session, reporting median elapsed time and range;
#   4. `sessionInfo()` printed for provenance.
#
# Re-run with:
#   Rscript inst/benchmarks/pft_interpret_benchmark.R
#
# Do not add a benchmark package to Imports/Suggests just to time
# this. `system.time()` is sufficient.

suppressPackageStartupMessages(library(pft))

reps <- 5L

make_cohort <- function(n, seed = 1L) {
  set.seed(seed)
  fvc      <- runif(n, 2.0, 5.0)
  fev1fvc  <- runif(n, 0.45, 0.90)
  data.frame(
    sex              = sample(c("M", "F"), n, replace = TRUE),
    age              = runif(n, 20, 80),
    height           = runif(n, 150, 190),
    fvc_measured     = fvc,
    fev1fvc_measured = fev1fvc,
    fev1_measured    = fev1fvc * fvc,
    tlc_measured     = fvc + runif(n, 0.3, 2.5)
  )
}

bench_one <- function(n, seed = 1L) {
  cohort <- make_cohort(n, seed = seed)
  system.time(pft_interpret(cohort))[["elapsed"]]
}

# Warm-up (discarded) so JIT / one-time setup does not bias the first
# measured n = 100,000 run.
invisible(bench_one(1000L))

# Measured repetitions on the paper-cited n = 100,000 cohort. Reseed
# per-rep so successive runs use the same input.
elapsed <- vapply(seq_len(reps),
                  function(i) bench_one(100000L, seed = i),
                  numeric(1))

cat(sprintf("n = 100,000  reps = %d\n", reps))
cat(sprintf("elapsed per rep (s): %s\n",
            paste(sprintf("%.3f", elapsed), collapse = " ")))
cat(sprintf("median = %.3f s  min = %.3f s  max = %.3f s\n",
            median(elapsed), min(elapsed), max(elapsed)))
cat(sprintf("median rate = %.0f rows/s\n", 100000 / median(elapsed)))

cat("\n--- sessionInfo() ---\n")
print(sessionInfo())
