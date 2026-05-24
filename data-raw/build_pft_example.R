#' Reproducibly build the pft_example synthetic patient cohort.
#'
#' Generates 20 synthetic patient profiles spanning the clinical patterns
#' the package classifies (Normal, Obstructed, Restricted, Mixed,
#' Non-specific, PRISm) plus a few bronchodilator-response cases. All
#' demographics are random within plausible ranges; all measured values
#' were hand-picked to land each row in its target pattern after the
#' GLI reference equations are applied. No patient identifiers; no PHI.
#'
#' Output: data/pft_example.rda

library(tibble)

pft_example <- tibble::tribble(
  ~patient_id, ~sex, ~age, ~height, ~race,         ~fev1_measured, ~fvc_measured, ~fev1fvc_measured, ~tlc_measured, ~fev1_pre, ~fev1_post,
  # --- normals ---
  1,           "M",  35,   178,     "Caucasian",   4.20,           5.10,          0.823,             7.00,          NA,        NA,
  2,           "F",  28,   165,     "Caucasian",   3.20,           3.80,          0.842,             5.00,          NA,        NA,
  3,           "M",  55,   175,     "AfrAm",       3.80,           4.70,          0.808,             6.40,          NA,        NA,
  # --- obstructed (low FEV1/FVC, normal TLC) ---
  4,           "M",  62,   170,     "Caucasian",   1.80,           3.80,          0.474,             6.20,          1.80,      2.20,
  5,           "F",  58,   162,     "AfrAm",       1.20,           2.30,          0.522,             4.80,          1.20,      1.45,
  6,           "M",  70,   168,     "Caucasian",   1.10,           2.80,          0.393,             6.50,          NA,        NA,
  # --- restricted (low TLC, normal ratio) ---
  7,           "F",  50,   160,     "Caucasian",   1.70,           2.10,          0.810,             3.50,          NA,        NA,
  8,           "M",  65,   172,     "NEAsia",      2.40,           2.90,          0.828,             4.50,          NA,        NA,
  # --- mixed (low ratio AND low TLC) ---
  9,           "M",  68,   170,     "Caucasian",   0.95,           1.90,          0.500,             4.30,          NA,        NA,
  10,          "F",  72,   158,     "AfrAm",       0.70,           1.50,          0.467,             3.40,          NA,        NA,
  # --- non-specific (low FVC, normal ratio, normal TLC) ---
  11,          "M",  60,   175,     "Caucasian",   2.20,           2.70,          0.815,             6.00,          NA,        NA,
  12,          "F",  45,   160,     "SEAsia",      1.90,           2.30,          0.826,             4.60,          NA,        NA,
  # --- PRISm-like (low FEV1, preserved ratio, no TLC) ---
  13,          "M",  40,   172,     "Caucasian",   2.50,           3.30,          0.758,             NA,            NA,        NA,
  14,          "F",  55,   163,     "AfrAm",       1.80,           2.40,          0.750,             NA,            NA,        NA,
  # --- BDR positive (clear bronchodilator response) ---
  15,          "F",  35,   165,     "Caucasian",   2.00,           3.20,          0.625,             5.10,          2.00,      2.50,
  16,          "M",  48,   178,     "Caucasian",   2.30,           4.00,          0.575,             6.80,          2.30,      2.90,
  # --- pediatric (still in GLI 2012 range) ---
  17,          "F",  10,   140,     "Caucasian",   1.80,           2.10,          0.857,             3.20,          NA,        NA,
  18,          "M",  12,   150,     "NEAsia",      2.20,           2.60,          0.846,             4.00,          NA,        NA,
  # --- elderly ---
  19,          "F",  85,   155,     "Caucasian",   1.20,           1.80,          0.667,             4.30,          NA,        NA,
  20,          "M",  90,   168,     "Caucasian",   1.40,           2.40,          0.583,             5.00,          NA,        NA
)

usethis::use_data(pft_example, overwrite = TRUE)
cat("wrote data/pft_example.rda:", nrow(pft_example), "rows,",
    ncol(pft_example), "columns\n")
