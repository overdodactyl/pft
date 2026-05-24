# Clinical threshold constants used across the package.
#
# These are kept here, named, with sources, so that:
#   1. The values appear exactly once in the codebase.
#   2. Reviewers can trace any threshold back to its source paper / standard
#      without grepping for magic numbers.
#   3. Updates to the underlying standards (e.g. a future Stanojevic
#      revision) can be applied in one place.
#
# Not exported. Internal-only. The numeric values are also stated in the
# user-facing docstrings of the functions that use them.

# z-score corresponding to the 5th and 95th percentiles (LLN and ULN)
# under the LMS framework. From Cole TJ. Stat Med. 1988;7(3):305-12.
LLN_Z <- -1.645
ULN_Z <-  1.645

# Severity grade boundaries from Stanojevic et al. ERJ 2022, "Severity
# of lung function impairment" section. Each entry is the *upper bound*
# of the named grade (i.e. z < bound is the grade itself, and z >=
# bound moves to the next lighter grade).
SEVERITY_BOUNDARIES <- c(
  severe   = -4.0,
  moderate = -2.5,
  mild     = -1.645  # equal to LLN_Z
)

# Bronchodilator response threshold (percent of predicted change in
# FEV1 or FVC) from Stanojevic et al. ERJ 2022. Replaces the 2005
# 12%-and-200-mL-from-baseline criterion.
BDR_THRESHOLD_PCT_PRED <- 10

# GOLD COPD severity tier upper bounds (FEV1 % predicted). Each entry
# is the *upper bound* of the named tier.
GOLD_BOUNDARIES <- c(
  "GOLD 4" = 30,
  "GOLD 3" = 50,
  "GOLD 2" = 80
)
# (GOLD 1 is "no upper bound" -- everything >= 80% predicted.)

# ATS/ERS 2019 spirometry quality grade thresholds (best-two
# repeatability difference, litres). From Graham BL et al. AJRCCM
# 2019;200(8):e70-e88, Table 10.
# Adult ( >= 6 yr ) and child ( < 6 yr ) thresholds differ.
QUALITY_THRESHOLD_ADULT <- c(A = 0.150, C = 0.200, D = 0.250)
QUALITY_THRESHOLD_CHILD <- c(A = 0.100, C = 0.150, D = 0.200)

# Conditional change score significance threshold (Stanojevic 2022).
# |CCS| > this value corresponds to one-sided p < 0.05 under normality.
CCS_SIGNIFICANCE <- 1.645  # equal to ULN_Z

# Default within-subject z-score autocorrelation for the conditional
# change score, used when the caller does not supply one. Mid-range
# value from adult FEV1 longitudinal studies; the result is sensitive
# to this choice -- see pft_change() docs.
DEFAULT_AUTOCORRELATION <- 0.7
