.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "pft ", utils::packageVersion("pft"), " | ",
    "Research and education use only. ",
    "Not validated for diagnostic decision-making; ",
    "all outputs require clinician interpretation. ",
    "See citation(\"pft\") for the source reference standards."
  )
}
