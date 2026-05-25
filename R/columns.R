#' @title Canonical input-column names for the `pft` reference functions
#'
#' @description
#' `pft_required_columns()` returns the canonical column names that
#' [pft_spirometry()], [pft_volumes()], [pft_diffusion()], and
#' [pft_interpret()] consume. Use it to introspect the input contract
#' before calling the reference functions, or to wire pft into a
#' validation step inside a larger pipeline.
#'
#' Three groups of columns are reported per function:
#'
#' * `required` -- columns that **must** be present. Missing required
#'   columns raise an error.
#' * `optional_measured` -- columns whose presence unlocks z-score and
#'   percent-predicted outputs for the corresponding measure. Missing
#'   these is silent; the function simply emits fewer output columns.
#' * `optional_bdr` -- (`pft_interpret` only) `<measure>_pre` /
#'   `<measure>_post` columns that, when present, unlock bronchodilator
#'   response calculations.
#'
#' @param fun Name of the reference function to introspect. One of
#'   `"pft_spirometry"`, `"pft_volumes"`, `"pft_diffusion"`,
#'   `"pft_interpret"`.
#' @param year Spirometry equation year (`2012` or `2022`). Only
#'   relevant for `"pft_spirometry"` and `"pft_interpret"`: `race` is
#'   required for 2012 but ignored for 2022.
#' @param SI.units Whether [pft_diffusion()] is configured to emit SI
#'   units. Affects which `<measure>_measured` columns are recognised
#'   (`tlco_measured` / `kco_si_measured` for SI;
#'   `dlco_measured` / `kco_tr_measured` for traditional).
#'
#' @return A named list with character-vector elements `required`,
#'   `optional_measured`, and (for `pft_interpret` only) `optional_bdr`.
#'
#' @examples
#' pft_required_columns("pft_spirometry", year = 2012)
#' pft_required_columns("pft_spirometry", year = 2022)
#' pft_required_columns("pft_volumes")
#' pft_required_columns("pft_diffusion", SI.units = TRUE)
#' pft_required_columns("pft_interpret", year = 2012)
#'
#' @seealso [pft_spirometry()], [pft_volumes()], [pft_diffusion()],
#'   [pft_interpret()], [pft_validate()].
#'
#' @export
pft_required_columns <- function(fun = c("pft_spirometry", "pft_volumes",
                                          "pft_diffusion", "pft_interpret"),
                                  year = 2012, SI.units = FALSE) {
  fun <- match.arg(fun)
  base_demographics <- c("sex", "age", "height")

  spiro_measured <- c("fev1_measured", "fvc_measured", "fev1fvc_measured",
                      "fef2575_measured", "fef75_measured")
  vol_measured   <- c("frc_measured", "tlc_measured", "rv_measured",
                      "rv_tlc_measured", "erv_measured", "ic_measured",
                      "vc_measured")
  diff_measured  <- if (SI.units) {
    c("tlco_measured", "kco_si_measured", "va_measured")
  } else {
    c("dlco_measured", "kco_tr_measured", "va_measured")
  }

  switch(
    fun,
    "pft_spirometry" = list(
      required = if (year == 2012) c(base_demographics, "race") else base_demographics,
      optional_measured = spiro_measured
    ),
    "pft_volumes" = list(
      required = base_demographics,
      optional_measured = vol_measured
    ),
    "pft_diffusion" = list(
      required = base_demographics,
      optional_measured = diff_measured
    ),
    "pft_interpret" = list(
      required = if (year == 2012) c(base_demographics, "race") else base_demographics,
      optional_measured = c(spiro_measured, vol_measured, diff_measured),
      optional_bdr = c("fev1_pre", "fev1_post",
                       "fvc_pre", "fvc_post",
                       "fev1fvc_pre", "fev1fvc_post")
    )
  )
}
