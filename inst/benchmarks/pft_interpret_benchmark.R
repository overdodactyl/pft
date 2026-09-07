# Benchmark for pft::pft_interpret() cited in the SoftwareX paper.
#
# Reported result (paper Sec. 4, "Impact"):
#   n = 100,000 synthetic records completed in ~4.2 s (~24k rows/s) on an
#   AMD EPYC 9554P Linux host, R 4.2.2, single serial R session.
#
# Re-run with:
#   Rscript inst/benchmarks/pft_interpret_benchmark.R
#
# Prints elapsed time for n = 10,000 and n = 100,000 and reports
# sessionInfo() so hardware/software can be documented alongside results.

suppressPackageStartupMessages(library(pft))

bench_one <- function(n, seed = 1L) {
  set.seed(seed)
  cohort <- data.frame(
    sex              = sample(c("M", "F"), n, replace = TRUE),
    age              = runif(n, 20, 80),
    height           = runif(n, 150, 190),
    fev1_measured    = runif(n, 1.5, 4.5),
    fvc_measured     = runif(n, 2.0, 5.5),
    fev1fvc_measured = runif(n, 0.5, 0.9),
    tlc_measured     = runif(n, 3.5, 7.5)
  )
  elapsed <- system.time(pft_interpret(cohort))["elapsed"]
  cat(sprintf("n = %8d  elapsed = %6.3f s  (%6.0f rows/s)\n",
              n, elapsed, n / elapsed))
  invisible(elapsed)
}

# Warm-up + timed runs
invisible(bench_one(1000L))
bench_one(10000L)
bench_one(100000L)

cat("\n--- sessionInfo() ---\n")
print(sessionInfo())
