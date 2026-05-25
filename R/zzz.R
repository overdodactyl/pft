# Declare aes()-bound column names that R CMD check would otherwise flag
# as "no visible binding" in pft_plot(). They are not free variables but
# ggplot2 aesthetic references; this is the standard suppression pattern.
utils::globalVariables(c(
  "measure", "zscore", "ymin", "ymax", "fill",
  # bdr / compare / trajectory / flow_volume modes added later
  "patient", "patient_id", "significant", "time",
  "zscore12", "zscore22", "lln_crossed",
  "volume", "flow"
))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "pft ", utils::packageVersion("pft"), " | ",
    "Research and education use only. ",
    "Not validated for diagnostic decision-making; ",
    "all outputs require clinician interpretation. ",
    "See citation(\"pft\") for the source reference standards."
  )
}
