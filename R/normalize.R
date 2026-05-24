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

# Top-level normalization called by pft_spirometry, pft_volumes,
# pft_diffusion. Returns a data frame with `sex` (and `race` for GLI
# 2012) replaced by their canonical forms, with one consolidated
# warning emitted as a side-effect.
pft_normalize_inputs <- function(data, requires_race = FALSE) {
  # Required-column check (errors -- not recoverable)
  required <- c("sex", "age", "height")
  if (requires_race) required <- c(required, "race")
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("pft: required column(s) missing from input: %s",
                 paste(sprintf("'%s'", missing_cols), collapse = ", ")),
         call. = FALSE)
  }

  sex_issues  <- normalize_sex_vec(data$sex)
  data$sex    <- sex_issues$values

  race_issues <- NULL
  if (requires_race) {
    race_issues <- normalize_race_vec(data$race)
    data$race   <- race_issues$values
  }

  emit_normalization_warning(sex_issues, race_issues)
  data
}
