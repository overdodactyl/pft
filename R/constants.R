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

# Diffusion-pattern classifier (Hughes & Pride 2012 framework adopted
# by ERS/ATS Stanojevic 2017) uses the same LLN/ULN thresholds as the
# rest of the package. Aliased here for explicit traceability from
# `classify_diffusion()` -- the classifier is purely qualitative
# (low / normal / elevated per measure z-score) and inherits the LMS
# 5th / 95th percentile convention.
DIFFUSION_LLN_Z <- LLN_Z
DIFFUSION_ULN_Z <- ULN_Z

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

# Predecessor 2005 BDR criterion: significant if BOTH
#   - relative change from baseline > 12%
#   - absolute change > 200 mL
# From Pellegrino R et al. ERJ 2005;26(5):948-968, p. 958 ("Values
# >12% and 200 mL compared with baseline ... suggest a 'significant'
# bronchodilatation") and p. 959 ("(>12% of control and >200 mL)"
# which clarifies both operators are strict). The "200 mL"
# threshold on p. 958 lacks an operator; p. 959 disambiguates it as
# strictly greater than 200 mL.
BDR_2005_PCT_PRE    <- 12
BDR_2005_ABS_LITRES <- 0.2

# Predecessor 2005 severity grading (FEV1 percent predicted bands).
# From Pellegrino R et al. ERJ 2005;26(5):948-968, Table 6 p. 957.
# The paper's Table 6 wording ("Mild >70") creates a degenerate gap
# at exactly 70%; clinical convention (and this implementation)
# treats the lower bound of each band as inclusive. Each entry below
# is the lower bound (inclusive) of the named grade.
SEVERITY_2005_BOUNDARIES <- c(
  mild              = 70,
  moderate          = 60,
  moderately_severe = 50,
  severe            = 35
)

# GOLD COPD severity tier upper bounds (FEV1 % predicted), from
# Figure 2.10 of the GOLD 2026 report ("GOLD Grades and Severity of
# Airflow Obstruction in COPD (based on post-bronchodilator FEV1)").
# Each entry is the *upper bound* of the named tier.
#
# GOLD 4: FEV1       < 30   (very severe)
# GOLD 3: 30 <= FEV1 < 50   (severe)
# GOLD 2: 50 <= FEV1 < 80   (moderate)
# GOLD 1: 80 <= FEV1        (mild)
#
# Figure 2.10 specifies the prerequisite "In patients with COPD
# (FEV1/FVC < 0.7)"; the package's pft_gold() applies this as an
# optional `fev1fvc` parameter (see R/clinical.R).
GOLD_BOUNDARIES <- c(
  "GOLD 4" = 30,
  "GOLD 3" = 50,
  "GOLD 2" = 80
)

# ATS/ERS 2019 spirometry quality grade thresholds (best-two
# repeatability difference, litres). From Graham BL et al. AJRCCM
# 2019;200(8):e70-e88, Table 10.
# Adult ( >= 6 yr ) and child ( < 6 yr ) thresholds differ.
QUALITY_THRESHOLD_ADULT <- c(A = 0.150, C = 0.200, D = 0.250)
QUALITY_THRESHOLD_CHILD <- c(A = 0.100, C = 0.150, D = 0.200)

# Conditional change score "normal limits" threshold from
# Stanojevic et al. ERJ 2022 Box 2 p. 12: "Changes within +/- 1.96
# change scores are considered within the normal limits." (Two-sided
# 95% normal-limits cutoff, NOT the one-sided 1.645 cutoff used for
# the LLN.)
CCS_SIGNIFICANCE <- 1.96

# Coefficients of the Stanojevic 2022 conditional-change-score
# autocorrelation formula (Box 2 p. 12):
#   r = 0.642 - 0.04 * time(years) + 0.020 * age(years) at t1
# Derived from a children/young-people cohort; the 2022 standard
# notes this has "yet to be validated, extended to adults" but
# permits its use as "a reasonable tool to facilitate
# interpretation".
CCS_R_INTERCEPT <-  0.642
CCS_R_TIME_COEF <- -0.04
CCS_R_AGE_COEF  <-  0.020

# FEV1Q denominators from Stanojevic et al. ERJ 2022 Box 3 p. 13:
# "FEV1Q is the observed forced expiratory volume in 1 s (FEV1) in
# litres divided by the sex-specific 1st percentile of the FEV1
# distribution found in adult subjects with lung disease; these
# percentiles are 0.5 L for males and 0.4 L for females."
FEV1Q_DENOM_MALE   <- 0.5
FEV1Q_DENOM_FEMALE <- 0.4
# Box 3 p. 13 (closing sentence): "FEV1Q is not appropriate for
# children and adolescents." Adult-only cutoff applied when an age
# vector is supplied to pft_fev1q().
FEV1Q_MIN_AGE      <- 18

# Hemoglobin correction of DLCO/TLCO per Stanojevic 2017 (p. 9, p. 11)
# and the underlying Cotes 1972 formula. Used by pft_dlco_hb_correct().
# Reference Hb values are sex- and age-specific (Stanojevic 2017 p. 11:
# "146 g/L for males aged >=15 years and 134 g/L for females and
# children"). The constants 1.7 and 0.7 are the Cotes formula
# parameters, dependent on the assumption that the ratio of membrane
# diffusing capacity to pulmonary capillary blood volume is
# 0.7 mL/min/mmHg/mL-blood and that alveolar oxygen partial pressure
# is 14.63 kPa (110 mmHg).
HB_REF_MALE_ADULT      <- 146   # g/L
HB_REF_FEMALE_CHILD    <- 134   # g/L
HB_REF_MALE_ADULT_AGE  <- 15    # cutoff: males <15 use female/child ref
COTES_HB_K_NUM         <- 1.7
COTES_HB_K_DENOM       <- 0.7
