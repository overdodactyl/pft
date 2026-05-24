# Internal helpers shared across the reference functions
# (pft_spirometry, pft_volumes, pft_diffusion).

# Append the per-measure LMS outputs (pred, lln, uln) and, when measured
# values are supplied in the input data frame as `<measure>_measured`
# columns, also append `<measure>_zscore` and `<measure>_pctpred` columns.
#
# Inputs:
#   data     -- original demographics data frame; may also contain
#               `<measure>_measured` columns for any subset of `measures`.
#   M, S, L  -- numeric matrices of LMS parameters (nrow = nrow(data),
#               ncol = length(measures)).
#   lower    -- numeric matrix of LLN values.
#   upper    -- numeric matrix of ULN values.
#   measures -- character vector of measure names ("fev1", "fvc", ...).
#   suffix   -- optional suffix appended to every output column name
#               (e.g. "_2022" for GLI Global outputs).
#
# z-score and percent predicted use the LMS formulas:
#   z         = ((measured / M)^L - 1) / (L * S)
#   pctpred   = (measured / M) * 100
# Either is NA wherever measured, M, S, or L is NA.
bind_lms_outputs <- function(data, M, S, L, lower, upper, measures,
                             suffix = "") {
  results <- data
  for (j in seq_along(measures)) {
    measure <- measures[j]
    m_vec <- M[, j]
    s_vec <- S[, j]
    l_vec <- L[, j]

    results[[paste0(measure, "_pred", suffix)]] <- m_vec
    results[[paste0(measure, "_lln",  suffix)]] <- lower[, j]
    results[[paste0(measure, "_uln",  suffix)]] <- upper[, j]

    measured_col <- paste0(measure, "_measured")
    if (measured_col %in% names(data)) {
      measured <- data[[measured_col]]
      results[[paste0(measure, "_zscore",  suffix)]] <-
        ((measured / m_vec)^l_vec - 1) / (l_vec * s_vec)
      results[[paste0(measure, "_pctpred", suffix)]] <-
        (measured / m_vec) * 100
    }
  }
  tibble::as_tibble(results)
}
