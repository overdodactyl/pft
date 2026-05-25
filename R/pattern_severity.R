#' Compose an ATS pattern and per-measure severity into a single label
#'
#' Stanojevic 2022 reports pattern (Normal / Non-specific / Obstructed
#' / Restricted / Mixed) and per-measure severity (normal / mild /
#' moderate / severe) on separate axes. Clinicians often want a single
#' composite label like `"Moderate Obstructed"` or `"Severe Mixed"`
#' for cohort reporting and clinic notes. This function composes the
#' two using the convention that the severity reflects the worst
#' measure that drives the pattern:
#'
#' * `Obstructed` -> FEV1 severity (FEV1/FVC < LLN is the trigger;
#'   FEV1 z-score drives the severity grade).
#' * `Restricted` -> FVC severity (FVC < LLN with TLC < LLN is the
#'   trigger; FVC z-score drives severity grade).
#' * `Mixed`      -> worse of FEV1 and FVC severity.
#' * `Non-specific` / `PRISm` -> FEV1 severity.
#' * `Normal`     -> `"Normal"` (no severity qualifier).
#' * Any NA inputs -> `NA`.
#'
#' Operates on the existing wide-form output of [pft_interpret()] (the
#' `ats_classification`, `fev1_severity`, `fvc_severity` columns it
#' already produces); no new input data is required.
#'
#' @param data A data frame with the columns `ats_classification`,
#'   `fev1_severity`, and `fvc_severity` (typically a
#'   [pft_interpret()] result). The function also accepts the 2022-
#'   suffixed columns (`fev1_severity_2022`, `fvc_severity_2022`)
#'   when the GLI Global 2022 standard is in use, and falls back to
#'   them if the unsuffixed columns are absent.
#'
#' @return The original data frame with a `pattern_severity` column
#'   appended.
#'
#' @references
#' Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical
#' standard on interpretive strategies for routine lung function
#' tests. \emph{Eur Respir J}. 2022;60:2101499.
#' \doi{10.1183/13993003.01499-2021}. The composition rule
#' implemented here -- severity from the driving measure -- follows
#' the practical reporting convention used in the standard's worked
#' examples (e.g., Section "Reporting and interpretation").
#'
#' @seealso [pft_classify()] for the pattern axis; [pft_severity()]
#'   for the per-measure z-score severity grader; [pft_interpret()]
#'   for the one-call workflow that produces both.
#'
#' @examples
#' d <- data.frame(
#'   ats_classification = c("Obstructed", "Restricted", "Mixed",
#'                            "Non-specific", "Normal", NA),
#'   fev1_severity      = c("moderate", "normal", "severe",
#'                            "mild", "normal", "moderate"),
#'   fvc_severity       = c("normal", "moderate", "moderate",
#'                            "normal", "normal", "mild")
#' )
#' pft_pattern_severity(d)
#'
#' @export
pft_pattern_severity <- function(data) {
  cols <- colnames(data)
  if (!"ats_classification" %in% cols) {
    stop("pft_pattern_severity() requires an `ats_classification` column. ",
         "Run pft_classify() or pft_interpret() first.",
         call. = FALSE)
  }

  fev1_sev_col <- if ("fev1_severity" %in% cols) "fev1_severity"
                  else if ("fev1_severity_2022" %in% cols) "fev1_severity_2022"
                  else NA_character_
  fvc_sev_col  <- if ("fvc_severity" %in% cols) "fvc_severity"
                  else if ("fvc_severity_2022" %in% cols) "fvc_severity_2022"
                  else NA_character_

  fev1_sev <- if (!is.na(fev1_sev_col)) data[[fev1_sev_col]]
              else rep(NA_character_, nrow(data))
  fvc_sev  <- if (!is.na(fvc_sev_col)) data[[fvc_sev_col]]
              else rep(NA_character_, nrow(data))

  data$pattern_severity <- compose_pattern_severity(
    data$ats_classification, fev1_sev, fvc_sev
  )
  data
}


# Internal: vectorized composer.
compose_pattern_severity <- function(pattern, fev1_sev, fvc_sev) {
  n <- length(pattern)
  fev1_sev <- rep_len(as.character(fev1_sev), n)
  fvc_sev  <- rep_len(as.character(fvc_sev),  n)
  out <- character(n)
  for (i in seq_len(n)) {
    pat <- pattern[i]
    if (is.na(pat)) {
      out[i] <- NA_character_
      next
    }
    if (pat == "Normal") {
      out[i] <- "Normal"
      next
    }
    sev <- switch(
      pat,
      "Obstructed"   = fev1_sev[i],
      "Restricted"   = fvc_sev[i],
      "Non-specific" = fev1_sev[i],
      "PRISm"        = fev1_sev[i],
      "Mixed"        = worst_severity(fev1_sev[i], fvc_sev[i]),
      NA_character_
    )
    if (is.na(sev) || sev == "normal") {
      out[i] <- pat
    } else {
      out[i] <- paste(tools::toTitleCase(sev), pat)
    }
  }
  out
}


# Internal: pick the worse of two severity strings on the ordered
# scale normal < mild < moderate < severe.
worst_severity <- function(a, b) {
  if (is.na(a) && is.na(b)) return(NA_character_)
  if (is.na(a)) return(b)
  if (is.na(b)) return(a)
  order <- c(normal = 0L, mild = 1L, moderate = 2L, severe = 3L)
  ra <- order[a]; rb <- order[b]
  if (is.na(ra) || is.na(rb)) return(NA_character_)
  if (ra >= rb) a else b
}
