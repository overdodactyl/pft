#' @title FEV1Q: ratio of FEV1 to a sex-specific survivable lower limit
#'
#' @description
#' Computes the FEV1Q survival index proposed by Miller & Pedersen
#' (ERJ 2010) and discussed as an adult alternative to the conditional
#' change score in Box 3 (p. 13) of the Stanojevic et al. ERJ 2022
#' interpretation standard. FEV1Q expresses FEV1 in relation to a
#' "bottom line" required for survival, rather than how far an
#' individual's result is from their predicted value; the race-neutral
#' evidence base was consolidated by Balasubramanian et al. (ERJ 2024).
#'
#' @details
#' Formula (Box 3 verbatim):
#'   \deqn{FEV1Q = FEV1 / Q_{sex}}
#'
#' where \eqn{Q_{male} = 0.5 L} and \eqn{Q_{female} = 0.4 L} are the
#' sex-specific 1st percentiles of the FEV1 distribution in adult
#' lung-disease populations. The index approximates the number of
#' turnovers remaining of a lower survivable limit of FEV1; values
#' closer to 1 indicate greater risk of death.
#'
#' The 2022 standard cautions (running text on p. 13, immediately
#' preceding Box 3): "FEV1Q is not appropriate for children and
#' adolescents." When `age` is supplied,
#' rows with `age < 18` return `NA_real_`. When `age` is omitted, the
#' age guard is skipped and the caller is responsible for restricting
#' input to adults.
#'
#' For longitudinal interpretation in adults the 2022 standard
#' suggests FEV1Q as an alternative to the conditional change score
#' (see [pft_change()]): under normal circumstances 1 unit of FEV1Q is
#' lost approximately every 18 years (every ~10 years in smokers and
#' the elderly).
#'
#' @param fev1 Numeric vector of FEV1 measurements in litres.
#' @param sex Character vector of patient sex. Accepts the soft-
#'   correctable variants from [pft_spirometry()] (`"Male"`, `"female"`,
#'   etc.); unrecognized values yield `NA`.
#' @param age Optional numeric vector. When supplied, rows with `age <
#'   18` return `NA_real_` per the paper's "not appropriate for children
#'   and adolescents" caveat. Default `NA_real_` skips the guard.
#'
#' @return Numeric vector of FEV1Q ratios, same length as `fev1`. `NA`
#'   propagates from any input.
#'
#' @references
#' Miller MR, Pedersen OF. New concepts for expressing forced expiratory
#' volume in 1 s arising from survival analysis. Eur Respir J.
#' 2010;35(4):873-882. \doi{10.1183/09031936.00025809}. Original
#' proposal of the FEV1Q index and the sex-specific 1st-percentile
#' denominators (0.5 L male, 0.4 L female).
#'
#' Balasubramanian A, Wise RA, Stanojevic S, Miller MR, McCormack MC.
#' FEV1Q: a race-neutral approach to assessing lung function. Eur Respir
#' J. 2024;63(4):2301622. \doi{10.1183/13993003.01622-2023}. Race-neutral
#' validation of the FEV1Q index.
#'
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical
#' standard on interpretive strategies for routine lung function
#' tests. Eur Respir J. 2022;60(1):2101499.
#' \doi{10.1183/13993003.01499-2021}. Discusses FEV1Q in Box 3
#' (p. 13) as an adult alternative to the conditional change score.
#'
#' @seealso [pft_change()] for the conditional change score (the
#'   children / young-people sibling); [pft_severity()] for the
#'   z-score-based severity grading.
#'
#' @examples
#' # Stanojevic 2022 Box 3 worked example: a 70-year-old woman with
#' # FEV1 of 0.9 L has FEV1Q of 0.9 / 0.4 = 2.25.
#' pft_fev1q(0.9, "F", age = 70)
#'
#' # Vectorised across sex.
#' pft_fev1q(c(1.0, 1.0), c("M", "F"))
#'
#' # Adolescents return NA when age is supplied.
#' pft_fev1q(1.0, "F", age = 10)
#'
#' @export
pft_fev1q <- function(fev1, sex, age = NA_real_) {
  sex_norm <- normalize_sex_vec(sex)$values
  denom <- ifelse(sex_norm == "M", FEV1Q_DENOM_MALE,
           ifelse(sex_norm == "F", FEV1Q_DENOM_FEMALE, NA_real_))
  out <- fev1 / denom
  # Apply the adult-only guard if age is supplied (any non-NA value).
  if (!all(is.na(age))) {
    age <- rep_len(age, length(fev1))
    out[!is.na(age) & age < FEV1Q_MIN_AGE] <- NA_real_
  }
  out
}
