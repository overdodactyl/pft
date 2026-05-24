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
  corrected <- character(0)
  dropped   <- character(0)
  out <- rep(NA_character_, length(x))

  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    raw   <- as.character(x[i])
    clean <- trimws(raw)
    upper <- toupper(clean)

    if (clean %in% c("M", "F")) {
      out[i] <- clean
    } else if (upper %in% c("M", "MALE", "MAN", "BOY")) {
      out[i] <- "M"
      corrected <- c(corrected, raw)
    } else if (upper %in% c("F", "FEMALE", "WOMAN", "GIRL")) {
      out[i] <- "F"
      corrected <- c(corrected, raw)
    } else {
      out[i] <- NA_character_
      dropped <- c(dropped, raw)
    }
  }
  list(values = out, corrected = corrected, dropped = dropped)
}

# Normalise a race vector to the canonical GLI 2012 levels. Case-
# insensitive, whitespace-tolerant, with synonym mapping. Same return
# shape as normalize_sex_vec().
normalize_race_vec <- function(x, levels = GLI_2012_RACE_LEVELS) {
  corrected <- character(0)
  dropped   <- character(0)
  out <- rep(NA_character_, length(x))

  lower_levels <- tolower(levels)

  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    raw   <- as.character(x[i])
    clean <- trimws(raw)
    lower <- tolower(clean)

    if (clean %in% levels) {
      out[i] <- clean
    } else if (lower %in% lower_levels) {
      # Case-only mismatch -- soft-correct
      out[i] <- levels[match(lower, lower_levels)]
      corrected <- c(corrected, raw)
    } else if (lower %in% names(RACE_SYNONYMS)) {
      out[i] <- unname(RACE_SYNONYMS[lower])
      corrected <- c(corrected, raw)
    } else {
      out[i] <- NA_character_
      dropped <- c(dropped, raw)
    }
  }
  list(values = out, corrected = corrected, dropped = dropped)
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
