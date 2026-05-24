library(dplyr)

## --- pft_quality (ATS/ERS 2019) ----------------------------------------

test_that("pft_quality returns A for 3 maneuvers within 0.150 L", {
  expect_equal(pft_quality(c(3.20, 3.15, 3.10)), "A")
})

test_that("pft_quality returns B for 2 maneuvers within 0.150 L", {
  expect_equal(pft_quality(c(3.20, 3.15)), "B")
})

test_that("pft_quality returns C for 2 maneuvers within 0.200 L", {
  expect_equal(pft_quality(c(3.20, 3.05)), "C")
})

test_that("pft_quality returns D for 2 maneuvers within 0.250 L", {
  expect_equal(pft_quality(c(3.20, 3.00)), "D")
})

test_that("pft_quality returns E for a single maneuver", {
  expect_equal(pft_quality(c(3.20)), "E")
})

test_that("pft_quality returns F for zero maneuvers", {
  expect_equal(pft_quality(numeric(0)), "F")
})

test_that("pft_quality applies tighter thresholds for children under 6", {
  # 3 maneuvers within 0.100 L -> A for both ages
  expect_equal(pft_quality(c(3.00, 2.95, 2.90), age = 5), "A")
  # 3 maneuvers with best-two diff 0.13 L -> A for adult (<= 0.150),
  # only C for child (> 0.100 child-A threshold, but <= 0.150 child-C)
  expect_equal(pft_quality(c(3.20, 3.07, 3.05), age = 30), "A")
  expect_equal(pft_quality(c(3.20, 3.07, 3.05), age = 5),  "C")
})

test_that("pft_quality handles NAs in input", {
  expect_equal(pft_quality(c(3.20, NA, 3.15)), "B")  # 2 non-NA, within 0.150
})

## --- pft_gold ----------------------------------------------------------

test_that("pft_gold grades each tier correctly", {
  expect_equal(pft_gold(c(85, 65, 40, 25)),
               c("GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4"))
})

test_that("pft_gold uses the right boundary conventions", {
  expect_equal(pft_gold(80),    "GOLD 1")  # exactly 80 -> GOLD 1
  expect_equal(pft_gold(79.99), "GOLD 2")
  expect_equal(pft_gold(50),    "GOLD 2")  # exactly 50 -> GOLD 2
  expect_equal(pft_gold(49.99), "GOLD 3")
  expect_equal(pft_gold(30),    "GOLD 3")  # exactly 30 -> GOLD 3
  expect_equal(pft_gold(29.99), "GOLD 4")
})

test_that("pft_gold propagates NA", {
  expect_equal(pft_gold(c(85, NA, 25)), c("GOLD 1", NA, "GOLD 4"))
})

## --- pft_cohort_summary ------------------------------------------------

test_that("pft_cohort_summary returns expected list structure", {
  d <- data.frame(
    sex = c("M","F"), age = c(45, 60), height = c(178, 165), race = "Caucasian",
    fev1_measured = c(2.5, 1.8), fvc_measured = c(3.8, 2.4),
    fev1fvc_measured = c(0.66, 0.75), tlc_measured = c(6.0, 4.5)
  )
  out <- pft_cohort_summary(pft_interpret(d))
  expect_named(out, c("zscores", "patterns", "prism"))
  expect_true("fev1" %in% out$zscores$measure)
  expect_true(all(c("mean_z", "median", "pct_below_lln") %in% colnames(out$zscores)))
})

test_that("pft_cohort_summary handles empty input gracefully", {
  out <- pft_cohort_summary(data.frame())
  expect_equal(nrow(out$zscores), 0)
  expect_equal(nrow(out$patterns), 0)
  expect_equal(nrow(out$prism), 0)
})

## --- pft_report --------------------------------------------------------

test_that("pft_report renders an HTML file", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
                  fev1_measured = 2.5, fvc_measured = 3.8)
  result <- pft_interpret(d)
  out <- pft_report(result)
  expect_true(file.exists(out))
  content <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Sanity-check that key content is present
  expect_true(grepl("Pulmonary Function Test Report", content))
  expect_true(grepl("FEV1", content))
})
