# Internal input-normalization helpers shared by pft_spirometry(),
# pft_volumes(), and pft_diffusion().
#
# The reference functions previously trusted their inputs to be exactly
# in the canonical form ("M"/"F" for sex; specific GLI 2012 strings for
# race). Anything else silently degraded the output -- and in the case
# of sex, silently produced wrong predictions (anything != "M" was
# treated as female). These helpers normalise common variants
# (case, whitespace, synonyms) up-front and emit a single consolidated
# warning per call so callers find out *before* the cohort summary.
#
# Canonical accepted values:
#   sex:   "M", "F"
#   race:  "AfrAm", "NEAsia", "SEAsia", "Other/mixed", "Caucasian"
#
# Returned issue lists are character vectors; the calling function
# emits the rolled-up warning so users see a single message.

GLI_2012_RACE_LEVELS <- c("AfrAm", "NEAsia", "SEAsia", "Other/mixed", "Caucasian")

# Common race synonyms; mapping is case-insensitive. Values are the
# canonical GLI 2012 strings.
RACE_SYNONYMS <- c(
  "white"              = "Caucasian",
  "european"           = "Caucasian",
  "black"              = "AfrAm",
  "africanamerican"    = "AfrAm",
  "african american"   = "AfrAm",
  "asian"              = "NEAsia",  # ambiguous; closest single bucket
  "northeastasian"     = "NEAsia",
  "north east asian"   = "NEAsia",
  "southeastasian"     = "SEAsia",
  "south east asian"   = "SEAsia",
  "other"              = "Other/mixed",
  "mixed"              = "Other/mixed",
  "other/mixed"        = "Other/mixed"
)

# Normalise a sex vector. Returns a list:
#   $values:    canonical vector ("M" / "F" / NA_character_)
#   $corrected: character vector of original values that were normalised
#               (e.g. "male", "Female"); for the user warning
#   $dropped:   character vector of original values that didn't match
#               and got NA'd
normalize_sex_vec <- function(x) {
  out    <- rep(NA_character_, length(x))
  raw    <- as.character(x)
  clean  <- trimws(raw)
  upper  <- toupper(clean)
  not_na <- !is.na(x)

  is_canonical <- not_na & clean %in% c("M", "F")
  is_male_alt  <- not_na & !is_canonical &
                    upper %in% c("M", "MALE", "MAN", "BOY")
  is_female_alt <- not_na & !is_canonical &
                    upper %in% c("F", "FEMALE", "WOMAN", "GIRL")

  out[is_canonical]  <- clean[is_canonical]
  out[is_male_alt]   <- "M"
  out[is_female_alt] <- "F"

  is_dropped <- not_na & !is_canonical & !is_male_alt & !is_female_alt

  list(
    values    = out,
    corrected = raw[is_male_alt | is_female_alt],
    dropped   = raw[is_dropped]
  )
}

# Normalise a race vector to the canonical GLI 2012 levels. Case-
# insensitive, whitespace-tolerant, with synonym mapping. Same return
# shape as normalize_sex_vec().
normalize_race_vec <- function(x, levels = GLI_2012_RACE_LEVELS) {
  out    <- rep(NA_character_, length(x))
  raw    <- as.character(x)
  clean  <- trimws(raw)
  lower  <- tolower(clean)
  not_na <- !is.na(x)
  lower_levels <- tolower(levels)

  is_canonical  <- not_na & clean %in% levels
  is_case_match <- not_na & !is_canonical & lower %in% lower_levels
  is_synonym    <- not_na & !is_canonical & !is_case_match &
                     lower %in% names(RACE_SYNONYMS)

  out[is_canonical]  <- clean[is_canonical]
  if (any(is_case_match)) {
    out[is_case_match] <- levels[match(lower[is_case_match], lower_levels)]
  }
  if (any(is_synonym)) {
    out[is_synonym] <- unname(RACE_SYNONYMS[lower[is_synonym]])
  }

  is_dropped <- not_na & !is_canonical & !is_case_match & !is_synonym

  list(
    values    = out,
    corrected = raw[is_case_match | is_synonym],
    dropped   = raw[is_dropped]
  )
}

# Emit a single, consolidated warning summarising what was normalised
# and what was dropped to NA. No-op when the lists are all empty.
emit_normalization_warning <- function(sex_issues, race_issues = NULL) {
  msgs <- character(0)

  if (length(sex_issues$corrected) > 0) {
    counts <- sort(table(sex_issues$corrected), decreasing = TRUE)
    msgs <- c(msgs, sprintf("  - normalised %d sex value(s) (%s)",
                            length(sex_issues$corrected),
                            paste(sprintf("'%s'->'%s'",
                                          names(counts),
                                          ifelse(toupper(trimws(names(counts))) %in%
                                                   c("M","MALE","MAN","BOY"), "M", "F")),
                                  collapse = ", ")))
  }
  if (length(sex_issues$dropped) > 0) {
    counts <- sort(table(sex_issues$dropped), decreasing = TRUE)
    msgs <- c(msgs, sprintf("  - %d row(s) had unrecognised sex (%s) -> set NA",
                            length(sex_issues$dropped),
                            paste(sprintf("'%s' (n=%d)", names(counts), as.integer(counts)),
                                  collapse = ", ")))
  }
  if (!is.null(race_issues)) {
    if (length(race_issues$corrected) > 0) {
      counts <- sort(table(race_issues$corrected), decreasing = TRUE)
      msgs <- c(msgs, sprintf("  - normalised %d race value(s) (%s)",
                              length(race_issues$corrected),
                              paste(sprintf("'%s' (n=%d)",
                                            names(counts),
                                            as.integer(counts)),
                                    collapse = ", ")))
    }
    if (length(race_issues$dropped) > 0) {
      counts <- sort(table(race_issues$dropped), decreasing = TRUE)
      msgs <- c(msgs, sprintf("  - %d row(s) had unrecognised race (%s) -> set NA",
                              length(race_issues$dropped),
                              paste(sprintf("'%s' (n=%d)", names(counts), as.integer(counts)),
                                    collapse = ", ")))
    }
  }

  if (length(msgs) > 0) {
    warning("pft input normalization:\n", paste(msgs, collapse = "\n"),
            "\nSee ?pft_spirometry for accepted values.", call. = FALSE)
  }
  invisible(NULL)
}

# Resolve a column reference quosure to the corresponding column name
# (a single string) in `data`.
#
# Accepted shapes for the quosure expression:
#   - bare symbol (e.g. captured from `sex = Sex` or the default
#     `sex = sex`): the symbol is interpreted as a column name.
#   - string literal (e.g. captured from `sex = "Sex"`): used directly.
#   - any expression that evaluates to a length-1 character or symbol
#     (e.g. `!!my_var` where `my_var <- "Sex"`): evaluated and the
#     result is used as a column name.
#
# `default` is the canonical column name used purely for error messages.
resolve_column_name <- function(quosure, default) {
  expr <- rlang::quo_get_expr(quosure)

  if (rlang::is_symbol(expr)) return(rlang::as_name(expr))
  if (rlang::is_string(expr)) return(expr)

  resolved <- tryCatch(
    rlang::eval_tidy(quosure),
    error = function(e) NULL
  )
  if (rlang::is_string(resolved))  return(resolved)
  if (rlang::is_symbol(resolved))  return(rlang::as_name(resolved))

  stop(sprintf(
    "pft: could not resolve a column reference for '%s'. Pass a bare column name (e.g. %s = Sex), a string (e.g. %s = \"Sex\"), or inject from a variable (e.g. %s = !!my_var).",
    default, default, default, default
  ), call. = FALSE)
}

# Top-level normalization called by pft_spirometry, pft_volumes,
# pft_diffusion. Resolves the four input column references (each a
# quosure captured via rlang::enquo() at the caller's boundary),
# normalises sex and race in-place in the user's data frame, and
# returns a list:
#   $data : the data frame with user-supplied column names preserved;
#           sex and race columns now contain canonical values.
#   $cols : a named list mapping canonical names ("sex", "age",
#           "height", "race") to the corresponding user-supplied
#           column names. Inner LMS-fit functions read via
#           data[[cols$sex]] etc.
#
# Defaults: each quosure defaults to the canonical bare name (e.g.
# the function signature uses `sex = sex`), so callers who already
# have canonically-named data pass no extra arguments.
pft_normalize_inputs <- function(data,
                                  sex    = rlang::quo(sex),
                                  age    = rlang::quo(age),
                                  height = rlang::quo(height),
                                  race   = rlang::quo(race),
                                  requires_race = FALSE) {
  sex_name    <- resolve_column_name(sex,    "sex")
  age_name    <- resolve_column_name(age,    "age")
  height_name <- resolve_column_name(height, "height")
  race_name   <- if (requires_race) resolve_column_name(race, "race") else NULL

  required <- c(sex_name, age_name, height_name)
  if (requires_race) required <- c(required, race_name)
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "pft: required column(s) missing from input: %s.\n  Expected: %s\n  See ?pft_required_columns.",
      paste(sprintf("'%s'", missing_cols), collapse = ", "),
      paste(sprintf("'%s'", required), collapse = ", ")
    ), call. = FALSE)
  }

  sex_issues <- normalize_sex_vec(data[[sex_name]])
  data[[sex_name]] <- sex_issues$values

  race_issues <- NULL
  if (requires_race) {
    race_issues <- normalize_race_vec(data[[race_name]])
    data[[race_name]] <- race_issues$values
  }

  emit_normalization_warning(sex_issues, race_issues)

  list(
    data = data,
    cols = list(sex = sex_name, age = age_name,
                height = height_name, race = race_name)
  )
}


# ---- exported: unit normalisation ----------------------------------------
# Everything above is internal (sex / race string normalisation called by
# the reference engines). What follows is the exported user-facing
# pft_normalize_units() that catches common unit-of-measure mistakes
# (height in inches, volumes in mL).

#' Auto-detect and convert mis-unit'd PFT inputs
#'
#' Heuristically catches the two most common clinic / lab data-import
#' bugs that yield silently-wrong reference values: height in inches
#' (instead of centimetres) and measured volumes in millilitres
#' (instead of litres). When detected, the offending columns are
#' converted in place and a single consolidated warning is emitted so
#' the user can audit.
#'
#' Detection is conservative -- the heuristic only triggers when the
#' input is unambiguous (e.g., a column called `height` where every
#' value is below 100 cannot be in cm because no human is < 1 m tall).
#' If your cohort might include genuinely small values that look like
#' the wrong unit (e.g., infant heights < 100 cm), pass the offending
#' column to `skip` or `auto = FALSE` and convert by hand.
#'
#' @param data A data frame.
#' @param height The bare name (or string) of the height column.
#'   Default `"height"`. Pass `NULL` to skip height detection.
#' @param volume_cols Character vector of measured-volume column names
#'   to check. Default auto-detects `<measure>_measured` columns for
#'   spirometry / volumes / diffusion measures. Pass `character(0)`
#'   to skip volume detection.
#' @param height_inches_max Heuristic threshold for inches-->cm
#'   conversion: if `max(height) < height_inches_max`, the column is
#'   treated as inches. Default 100 (cm threshold corresponding to
#'   ~1 m).
#' @param volume_ml_min Heuristic threshold for mL-->L conversion: if
#'   `max(column) > volume_ml_min`, the column is treated as mL.
#'   Default 15 (litres; spirometry volumes rarely exceed ~10 L for
#'   the largest adults).
#'
#' @return The input data frame with possibly-converted columns. A
#'   warning is emitted when any conversion is performed,
#'   summarising which columns changed and the conversion factor.
#'
#' @examples
#' # Common mistake: height in inches.
#' d <- data.frame(sex = "M", age = 45, height = 70,
#'                 race = "Caucasian", fev1_measured = 2.5)
#' suppressWarnings(pft_normalize_units(d))
#' # -> height converted from inches to cm (70 -> 177.8)
#'
#' # Volume in millilitres.
#' d2 <- data.frame(sex = "M", age = 45, height = 178,
#'                  race = "Caucasian", fev1_measured = 2500)
#' suppressWarnings(pft_normalize_units(d2))
#' # -> fev1_measured converted from mL to L (2500 -> 2.5)
#'
#' @export
pft_normalize_units <- function(data,
                                  height = "height",
                                  volume_cols = NULL,
                                  height_inches_max = 100,
                                  volume_ml_min = 15) {
  issues <- character(0)

  # ---- height -----------------------------------------------------------
  if (!is.null(height) && height %in% colnames(data)) {
    h <- as.numeric(data[[height]])
    if (sum(!is.na(h)) > 0 && max(h, na.rm = TRUE) < height_inches_max) {
      data[[height]] <- h * 2.54
      issues <- c(issues, sprintf(
        "`%s` looked like inches (max %.1f); converted to cm (x 2.54).",
        height, max(h, na.rm = TRUE)
      ))
    }
  }

  # ---- volume columns ---------------------------------------------------
  if (is.null(volume_cols)) {
    # Auto-detect: any *_measured column for known volume measures.
    volume_measures <- c("fev1", "fvc", "fef2575", "fef75",
                         "frc", "tlc", "rv", "erv", "ic", "vc")
    candidates <- paste0(volume_measures, "_measured")
    volume_cols <- intersect(candidates, colnames(data))
  }
  for (col in volume_cols) {
    if (!col %in% colnames(data)) next
    v <- as.numeric(data[[col]])
    if (sum(!is.na(v)) == 0) next
    if (max(v, na.rm = TRUE) > volume_ml_min) {
      data[[col]] <- v / 1000
      issues <- c(issues, sprintf(
        "`%s` looked like mL (max %.0f); converted to L (/ 1000).",
        col, max(v, na.rm = TRUE)
      ))
    }
  }

  if (length(issues) > 0) {
    warning(paste(c("pft_normalize_units():", issues),
                  collapse = "\n  "),
            call. = FALSE)
  }
  data
}
