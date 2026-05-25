#' @title Clinical visualisations for PFT results
#'
#' @description
#' `pft_plot()` produces clinical-style visualisations of PFT results.
#' The default `type = "lollipop"` mode is the canonical single-patient
#' z-score figure (one row per measure, points at the patient's z-score,
#' shaded reference bands for Stanojevic 2022 severity). Four additional
#' modes cover the cohort, longitudinal, bronchodilator-response, and
#' equation-comparison visualisations that come up routinely in
#' clinic-style reporting:
#'
#' * `"lollipop"` -- single-patient z-score figure (default). Errors if
#'   `nrow(data) != 1`.
#' * `"histogram"` -- cohort z-score distribution by measure. Faceted
#'   histogram of z-scores for every patient in `data`, one panel per
#'   measure, with severity bands.
#' * `"trajectory"` -- longitudinal z-score over time, one line per
#'   measure. Requires a `time` column (numeric or Date).
#' * `"bdr"` -- pre/post bronchodilator response. Paired arrows from
#'   `<measure>_pre` to `<measure>_post` for each spirometry measure
#'   with a `_pre` / `_post` pair, with the Stanojevic 2022 +10 % of
#'   predicted significance threshold overlaid.
#' * `"compare"` -- equation reclassification. For each spirometry
#'   measure with both `<measure>_zscore` and `<measure>_zscore_2022`
#'   columns (the output shape of [pft_compare()]), draws a segment
#'   from the 2012 z-score to the 2022 z-score, coloured by whether
#'   the row crossed the LLN.
#' * `"flow_volume"` -- stylised single-patient flow-volume envelope.
#'   Requires `fvc_measured`, `fef2575_measured`, and `fef75_measured`
#'   columns; if `fvc_pred` / `fef2575_pred` / `fef75_pred` are also
#'   present, the predicted envelope is overlaid as a dashed line for
#'   reference. Errors if `nrow(data) != 1`. Note: this is a stylised
#'   envelope (4 anchor points), not the full continuous flow-volume
#'   loop -- the package's input contract supports only mid- and
#'   end-expiratory flows (FEF25-75 and FEF75), not PEF / FEF25 / FEF50
#'   individually.
#'
#' Requires the `ggplot2` package (a Suggested dependency).
#'
#' @param data A data frame produced by [pft_interpret()],
#'   [pft_compare()], or any of the reference functions with
#'   measured values supplied. Shape requirements depend on `type` --
#'   see the per-mode notes above.
#' @param type One of `"lollipop"` (default), `"histogram"`,
#'   `"trajectory"`, `"bdr"`, `"compare"`.
#' @param time For `type = "trajectory"`: the column name (bare or
#'   string) giving the time axis. Required for trajectory mode;
#'   ignored otherwise.
#' @param patient_id For `type = "trajectory"`: optional column name
#'   giving the patient identifier when `data` contains more than one
#'   patient. If omitted, all rows are assumed to be from the same
#'   patient.
#'
#' @return A `ggplot` object.
#'
#' @seealso [pft_interpret()] and [pft_compare()] for the input data
#'   shape; [pft_report()] to embed the plot into a clinical report.
#'
#' @examples
#' \dontrun{
#' # Single-patient lollipop (default).
#' p <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian",
#'                 fev1_measured = 2.5, fvc_measured = 3.8)
#' pft_plot(pft_interpret(p))
#'
#' # Cohort histogram (z-score distribution by measure).
#' cohort <- data.frame(
#'   sex = c("M","F","M","F","M"),
#'   age = c(45, 60, 30, 55, 70),
#'   height = c(178, 165, 175, 160, 170),
#'   race = "Caucasian",
#'   fev1_measured = c(2.5, 1.8, 4.0, 1.5, 2.2),
#'   fvc_measured  = c(3.8, 2.4, 5.2, 2.5, 3.5)
#' )
#' pft_plot(pft_interpret(cohort), type = "histogram")
#' }
#'
#' @export
pft_plot <- function(data,
                      type = c("lollipop", "histogram",
                                 "trajectory", "bdr", "compare",
                                 "flow_volume"),
                      time = NULL,
                      patient_id = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("pft_plot() requires the ggplot2 package. Install with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
  type <- match.arg(type)

  time_q       <- rlang::enquo(time)
  patient_id_q <- rlang::enquo(patient_id)

  switch(
    type,
    lollipop    = pft_plot_lollipop(data),
    histogram   = pft_plot_histogram(data),
    trajectory  = pft_plot_trajectory(data, time_q, patient_id_q),
    bdr         = pft_plot_bdr(data),
    compare     = pft_plot_compare(data),
    flow_volume = pft_plot_flow_volume(data)
  )
}


# Internal: severity-band data frame used by every mode that draws z-scores.
severity_bands <- function() {
  data.frame(
    ymin = c(-Inf, -4,   -2.5,   -1.645, 1.645),
    ymax = c(-4,   -2.5, -1.645,  1.645, Inf),
    sev  = c("severe", "moderate", "mild", "normal", "elevated"),
    fill = c("#d73027", "#fc8d59", "#fee090", "#e0e0e0", "#abd9e9"),
    stringsAsFactors = FALSE
  )
}


# Mode: single-patient lollipop (existing canonical figure). -----------------
pft_plot_lollipop <- function(data) {
  if (nrow(data) != 1) {
    stop("pft_plot(type = \"lollipop\") expects a single-patient data ",
         "frame (nrow == 1). For multi-patient summaries use ",
         "type = \"histogram\" or call pft_plot() per row.", call. = FALSE)
  }

  zcols <- grep("_zscore(?:_[0-9]+)?$", colnames(data),
                value = TRUE, perl = TRUE)
  # Prefer the unsuffixed columns when both are present (single-patient
  # lollipop should pick one standard, not mix).
  if (any(grepl("_zscore$", zcols)) && any(grepl("_zscore_[0-9]+$", zcols))) {
    zcols <- zcols[grepl("_zscore$", zcols)]
  }
  if (length(zcols) == 0) {
    stop("No z-score columns found in `data`. Did you forget to supply ",
         "`<measure>_measured` columns before calling pft_interpret()?",
         call. = FALSE)
  }

  plot_df <- data.frame(
    measure = sub("_zscore.*", "", zcols),
    zscore  = vapply(zcols, function(c) data[[c]][1], numeric(1)),
    stringsAsFactors = FALSE
  )
  plot_df <- plot_df[order(plot_df$zscore), ]
  plot_df$measure <- factor(plot_df$measure, levels = plot_df$measure)

  bands <- severity_bands()
  ggplot2::ggplot(plot_df, ggplot2::aes(x = measure, y = zscore)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = fill),
      xmin = -Inf, xmax = Inf, alpha = 0.4, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                         colour = "gray40") +
    ggplot2::geom_segment(
      ggplot2::aes(xend = measure, yend = 0), colour = "black"
    ) +
    ggplot2::geom_point(size = 4, colour = "black") +
    ggplot2::coord_flip(ylim = c(-6, 6)) +
    ggplot2::labs(
      x = NULL, y = "z-score",
      title = "Pulmonary function test result",
      subtitle = "Shaded bands: Stanojevic 2022 severity grades"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# Mode: cohort histogram of z-scores by measure. ----------------------------
pft_plot_histogram <- function(data) {
  zcols <- grep("_zscore(?:_[0-9]+)?$", colnames(data),
                value = TRUE, perl = TRUE)
  if (length(zcols) == 0) {
    stop("No z-score columns found in `data`. Did you forget to supply ",
         "`<measure>_measured` columns before calling pft_interpret()?",
         call. = FALSE)
  }

  # Stack the z-score columns into long form (one row per patient x measure).
  parts <- lapply(zcols, function(col) {
    data.frame(
      measure = sub("_zscore.*", "", col),
      zscore  = data[[col]],
      stringsAsFactors = FALSE
    )
  })
  plot_df <- do.call(rbind, parts)
  plot_df <- plot_df[!is.na(plot_df$zscore), ]

  bands <- severity_bands()
  ggplot2::ggplot(plot_df, ggplot2::aes(x = zscore)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(xmin = ymin, xmax = ymax, fill = fill),
      ymin = -Inf, ymax = Inf, alpha = 0.3, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_histogram(binwidth = 0.25, colour = "black",
                              fill = "gray30") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          colour = "gray40") +
    ggplot2::facet_wrap(~ measure, scales = "free_y") +
    ggplot2::coord_cartesian(xlim = c(-6, 6)) +
    ggplot2::labs(
      x = "z-score", y = "Patients",
      title = "Cohort z-score distribution",
      subtitle = "Shaded bands: Stanojevic 2022 severity grades"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# Mode: longitudinal z-score trajectory. ------------------------------------
pft_plot_trajectory <- function(data, time_q, patient_id_q) {
  if (rlang::quo_is_null(time_q)) {
    stop("pft_plot(type = \"trajectory\") requires a `time` column. ",
         "Pass it as `time = visit_date` or similar.", call. = FALSE)
  }
  time_name <- rlang::as_name(time_q)
  if (!(time_name %in% colnames(data))) {
    stop(sprintf("Column `%s` not found in `data`.", time_name),
         call. = FALSE)
  }

  has_pid <- !rlang::quo_is_null(patient_id_q)
  pid_name <- if (has_pid) rlang::as_name(patient_id_q) else NULL

  zcols <- grep("_zscore(?:_[0-9]+)?$", colnames(data),
                value = TRUE, perl = TRUE)
  if (length(zcols) == 0) {
    stop("No z-score columns found in `data`.", call. = FALSE)
  }

  parts <- lapply(zcols, function(col) {
    df <- data.frame(
      measure = sub("_zscore.*", "", col),
      time    = data[[time_name]],
      zscore  = data[[col]],
      stringsAsFactors = FALSE
    )
    if (has_pid) df$patient_id <- data[[pid_name]]
    df
  })
  plot_df <- do.call(rbind, parts)
  plot_df <- plot_df[!is.na(plot_df$zscore), ]

  bands <- severity_bands()
  p <- ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = time, y = zscore,
                                      colour = measure, group = measure)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = fill),
      xmin = -Inf, xmax = Inf, alpha = 0.25, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                         colour = "gray40") +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2) +
    ggplot2::coord_cartesian(ylim = c(-6, 6)) +
    ggplot2::labs(
      x = time_name, y = "z-score", colour = "Measure",
      title = "PFT trajectory",
      subtitle = "Shaded bands: Stanojevic 2022 severity grades"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # Use an explicit date / datetime scale when the time column is one
  # of those types. ggplot's auto-detection usually picks the right
  # scale, but being explicit guards against subclasses (e.g. hms) and
  # makes the axis-label format reliable.
  if (inherits(plot_df$time, "Date")) {
    p <- p + ggplot2::scale_x_date()
  } else if (inherits(plot_df$time, "POSIXct") ||
             inherits(plot_df$time, "POSIXlt")) {
    p <- p + ggplot2::scale_x_datetime()
  }

  if (has_pid) {
    p <- p + ggplot2::aes(group = interaction(measure, patient_id)) +
      ggplot2::facet_wrap(~ patient_id)
  }
  p
}


# Mode: pre/post BDR paired arrows. -----------------------------------------
pft_plot_bdr <- function(data) {
  measures <- c("fev1", "fvc", "fev1fvc")
  rows <- list()
  for (i in seq_len(nrow(data))) {
    for (m in measures) {
      pre  <- paste0(m, "_pre")
      post <- paste0(m, "_post")
      pred <- paste0(m, "_pred")
      pred22 <- paste0(m, "_pred_2022")
      if (pre %in% colnames(data) && post %in% colnames(data)) {
        pred_val <- if (pred %in% colnames(data)) data[[pred]][i]
                    else if (pred22 %in% colnames(data)) data[[pred22]][i]
                    else NA_real_
        rows[[length(rows) + 1]] <- data.frame(
          patient = i, measure = m,
          pre  = data[[pre]][i],
          post = data[[post]][i],
          pred = pred_val,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0) {
    stop("pft_plot(type = \"bdr\") requires at least one `<measure>_pre` ",
         "/ `_post` pair (fev1, fvc, or fev1fvc).", call. = FALSE)
  }
  plot_df <- do.call(rbind, rows)
  plot_df$pct_pred <- 100 * (plot_df$post - plot_df$pre) / plot_df$pred
  plot_df$significant <- !is.na(plot_df$pct_pred) &
    plot_df$pct_pred >= BDR_THRESHOLD_PCT_PRED

  sig_label <- sprintf("Significant (>= %g%% pred)", BDR_THRESHOLD_PCT_PRED)
  subtitle  <- sprintf(
    "Pre -> post; threshold +%g %% of predicted (Stanojevic 2022)",
    BDR_THRESHOLD_PCT_PRED
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = measure,
                                          y = pre, yend = post,
                                          group = patient,
                                          colour = significant)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = measure),
      arrow = grid::arrow(length = grid::unit(0.08, "inches"),
                          type = "closed"),
      linewidth = 0.7,
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "#1a9850", `FALSE` = "gray40"),
      labels = c(`TRUE` = sig_label, `FALSE` = "Not significant"),
      name = NULL
    ) +
    ggplot2::labs(
      x = NULL, y = "Measured (L or ratio)",
      title = "Bronchodilator response",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# Mode: equation-comparison (2012 vs 2022) reclassification arrows. ---------
pft_plot_compare <- function(data) {
  # Identify spirometry measures with both _zscore and _zscore_2022.
  z12 <- grep("_zscore$", colnames(data), value = TRUE)
  z22 <- grep("_zscore_[0-9]+$", colnames(data), value = TRUE)
  if (length(z22) == 0) {
    stop("pft_plot(type = \"compare\") requires _zscore_<year> companion ",
         "columns (the output shape of pft_compare()). None found.",
         call. = FALSE)
  }

  measures <- intersect(
    sub("_zscore$", "", z12),
    sub("_zscore_[0-9]+$", "", z22)
  )
  if (length(measures) == 0) {
    stop("pft_plot(type = \"compare\") requires matched _zscore and ",
         "_zscore_<year> columns. None matched.", call. = FALSE)
  }

  parts <- list()
  for (m in measures) {
    parts[[length(parts) + 1]] <- data.frame(
      patient   = seq_len(nrow(data)),
      measure   = m,
      zscore12  = data[[paste0(m, "_zscore")]],
      zscore22  = data[[grep(paste0("^", m, "_zscore_[0-9]+$"),
                              colnames(data), value = TRUE)[1]]],
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, parts)
  plot_df <- plot_df[!is.na(plot_df$zscore12) & !is.na(plot_df$zscore22), ]
  plot_df$lln_crossed <- (plot_df$zscore12 < -1.645) !=
                         (plot_df$zscore22 < -1.645)

  bands <- severity_bands()
  ggplot2::ggplot(plot_df, ggplot2::aes(x = measure,
                                          y = zscore12, yend = zscore22,
                                          colour = lln_crossed,
                                          group = patient)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = fill),
      xmin = -Inf, xmax = Inf, alpha = 0.25, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                         colour = "gray40") +
    ggplot2::geom_hline(yintercept = -1.645, linetype = "dotted",
                         colour = "gray30") +
    ggplot2::geom_segment(
      ggplot2::aes(xend = measure),
      arrow = grid::arrow(length = grid::unit(0.07, "inches"),
                          type = "closed"),
      linewidth = 0.5,
      position = ggplot2::position_jitter(width = 0.15, height = 0)
    ) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "#d73027", `FALSE` = "gray30"),
      labels = c(`TRUE` = "Crossed LLN", `FALSE` = "Same side of LLN"),
      name = NULL
    ) +
    ggplot2::coord_flip(ylim = c(-6, 6)) +
    ggplot2::labs(
      x = NULL, y = "z-score",
      title = "GLI 2012 -> GLI Global 2022 reclassification",
      subtitle = "Dotted line: LLN. Coloured arrows cross the LLN."
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


# Mode: single-patient flow-volume envelope. --------------------------------
# Stylised: only 4 anchor points (origin, FEF2575 at 50% FVC exhaled,
# FEF75 at 75% FVC exhaled, end of expiration). The package's input
# contract does not include PEF / FEF25 / FEF50, so a full continuous
# loop isn't producible without expanding the contract.
pft_plot_flow_volume <- function(data) {
  if (nrow(data) != 1) {
    stop("pft_plot(type = \"flow_volume\") expects a single-patient data ",
         "frame (nrow == 1).", call. = FALSE)
  }
  required <- c("fvc_measured", "fef2575_measured", "fef75_measured")
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "pft_plot(type = \"flow_volume\") requires columns: %s. Missing: %s.",
      paste(required, collapse = ", "),
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }

  fvc       <- data$fvc_measured[1]
  fef2575_m <- data$fef2575_measured[1]
  fef75_m   <- data$fef75_measured[1]

  measured_df <- data.frame(
    volume = c(0, 0.5 * fvc, 0.75 * fvc, fvc),
    flow   = c(0, fef2575_m, fef75_m, 0),
    label  = c("Start", "FEF25-75", "FEF75", "End"),
    stringsAsFactors = FALSE
  )
  # Drop the (0, 0) start point from the line trace -- it's an
  # anchor, not a real measurement.

  # Predicted envelope, if reference values are present.
  pred_df <- NULL
  if (all(c("fvc_pred", "fef2575_pred", "fef75_pred") %in% colnames(data))) {
    fvc_p   <- data$fvc_pred[1]
    f2575_p <- data$fef2575_pred[1]
    f75_p   <- data$fef75_pred[1]
    if (!is.na(fvc_p) && !is.na(f2575_p) && !is.na(f75_p)) {
      pred_df <- data.frame(
        volume = c(0, 0.5 * fvc_p, 0.75 * fvc_p, fvc_p),
        flow   = c(0, f2575_p, f75_p, 0)
      )
    }
  }

  p <- ggplot2::ggplot(measured_df, ggplot2::aes(x = volume, y = flow)) +
    ggplot2::geom_path(linewidth = 0.8, colour = "#1a1a1a") +
    ggplot2::geom_point(size = 3, colour = "#1a1a1a")

  if (!is.null(pred_df)) {
    p <- p +
      ggplot2::geom_path(data = pred_df,
                          ggplot2::aes(x = volume, y = flow),
                          linewidth = 0.6, linetype = "dashed",
                          colour = "gray50")
  }

  subtitle <- if (is.null(pred_df)) {
    "Stylised envelope from FEF25-75 and FEF75 (4 anchor points)."
  } else {
    "Solid: measured. Dashed: predicted envelope. Stylised, 4 anchor points each."
  }

  p +
    ggplot2::labs(
      x = "Volume exhaled (L)", y = "Flow (L/s)",
      title = "Flow-volume envelope",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
