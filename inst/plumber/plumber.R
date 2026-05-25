#* Plumber endpoint scaffolding for the pft package.
#*
#* This file is a starter -- it is NOT part of the package's exported
#* API and is not loaded automatically. To run it:
#*
#*   library(plumber)
#*   r <- plumb(system.file("plumber", "plumber.R", package = "pft"))
#*   r$run(port = 8080)
#*
#* Then POST a JSON body of patient demographics + measurements to
#* /interpret or /compare. The response is the corresponding
#* pft_interpret() or pft_compare() result, serialised as JSON.
#*
#* The endpoints accept JSON arrays (cohort) or objects (single
#* patient); both shapes round-trip via jsonlite. The result is
#* always returned as a JSON array of row objects.

library(pft)

#* @apiTitle pft Pulmonary Function Test API
#* @apiDescription Compute ATS/ERS-compliant reference values, lower /
#*   upper limits of normal, ATS pattern classification, severity, and
#*   reclassification deltas (GLI 2012 vs GLI Global 2022) for PFT
#*   data submitted as JSON.
#* @apiVersion 0.1.0


#* Compute reference values, z-scores, and interpretation for one or
#* more patients. Body is a JSON object (one patient) or array of
#* objects (cohort) carrying the standard pft input columns.
#* @param year Spirometry GLI year (2012 or 2022). Default 2012.
#* @param SI.units Whether to report diffusion in SI units. Default
#*   FALSE (traditional).
#* @param standard Interpretive standard ("2022" or "2005"). Default
#*   "2022".
#* @post /interpret
function(req, year = 2012, SI.units = FALSE, standard = "2022") {
  data <- parse_pft_body(req)
  out <- pft_interpret(
    data,
    year     = as.numeric(year),
    SI.units = as.logical(SI.units),
    standard = standard
  )
  as_json_rows(out)
}


#* Compare GLI 2012 vs GLI Global 2022 interpretation for the same
#* cohort, returning per-row reclassification deltas.
#* @param SI.units Whether to report diffusion in SI units. Default
#*   FALSE.
#* @param standard Interpretive standard for downstream rules.
#*   Default "2022".
#* @post /compare
function(req, SI.units = FALSE, standard = "2022") {
  data <- parse_pft_body(req)
  out <- pft_compare(
    data,
    SI.units = as.logical(SI.units),
    standard = standard
  )
  as_json_rows(out)
}


#* Echo the package's required input columns (the input contract).
#* Useful for clients building forms / validators around the API.
#* @get /schema
function() {
  list(
    required = pft_required_columns(),
    optional_measured = c(
      "fev1_measured", "fvc_measured", "fev1fvc_measured",
      "fef2575_measured", "fef75_measured",
      "frc_measured", "tlc_measured", "rv_measured",
      "erv_measured", "ic_measured", "vc_measured",
      "rv_tlc_measured", "frc_tlc_measured",
      "dlco_measured", "tlco_measured", "va_measured",
      "kco_tr_measured", "kco_si_measured"
    ),
    optional_pre_post = c(
      "fev1_pre",    "fev1_post",
      "fvc_pre",     "fvc_post",
      "fev1fvc_pre", "fev1fvc_post"
    )
  )
}


# --- helpers -------------------------------------------------------------

# Parse a plumber request body (already deserialised to a list or
# data.frame by plumber's default JSON parser) into a data.frame. The
# parser accepts:
#   * a single named list (one patient)  -> 1-row data.frame
#   * a list of named lists (cohort)     -> rbind to data.frame
#   * an already-a-data.frame             -> passed through
parse_pft_body <- function(req) {
  body <- req$body
  if (is.data.frame(body)) return(body)
  if (is.list(body)) {
    if (length(body) > 0 && is.list(body[[1]])) {
      # cohort: list of named lists
      rows <- lapply(body, function(x) as.data.frame(x, stringsAsFactors = FALSE))
      return(do.call(rbind, rows))
    }
    # single patient: named list
    return(as.data.frame(body, stringsAsFactors = FALSE))
  }
  stop("Could not parse request body. Expected a JSON object or array of objects.")
}

as_json_rows <- function(df) {
  # Drop the pft_result/pft_compare classes so jsonlite serialises the
  # underlying tibble; the row-object shape is friendlier for HTTP
  # clients than a column-major tibble.
  df <- as.data.frame(df)
  jsonlite::toJSON(df, dataframe = "rows", na = "null",
                    auto_unbox = TRUE)
}
