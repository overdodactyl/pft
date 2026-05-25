test_that("pft_plot(type = 'flow_volume') produces a ggplot from measured values", {
  skip_if_not_installed("ggplot2")
  patient <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured    = 3.0,
    fvc_measured     = 4.0,
    fef2575_measured = 3.5,
    fef75_measured   = 1.2
  )
  result <- pft_spirometry(patient)
  p <- pft_plot(result, type = "flow_volume")
  expect_s3_class(p, "ggplot")
})


test_that("flow_volume errors on multi-patient input", {
  skip_if_not_installed("ggplot2")
  cohort <- data.frame(
    fvc_measured     = c(4.0, 3.5),
    fef2575_measured = c(3.5, 3.0),
    fef75_measured   = c(1.2, 1.0)
  )
  expect_error(pft_plot(cohort, type = "flow_volume"),
               "single-patient")
})


test_that("flow_volume errors when required measured columns are missing", {
  skip_if_not_installed("ggplot2")
  patient <- data.frame(fvc_measured = 4.0)
  expect_error(pft_plot(patient, type = "flow_volume"),
               "fef2575_measured")
})


test_that("flow_volume overlays predicted envelope when *_pred columns present", {
  skip_if_not_installed("ggplot2")
  patient <- data.frame(
    fvc_measured     = 4.0,
    fef2575_measured = 3.5,
    fef75_measured   = 1.2,
    fvc_pred         = 4.5,
    fef2575_pred     = 4.0,
    fef75_pred       = 1.5
  )
  p <- pft_plot(patient, type = "flow_volume")
  expect_s3_class(p, "ggplot")
  # Two geom_path layers: one for measured, one for predicted overlay.
  path_layers <- vapply(p$layers, function(l) inherits(l$geom, "GeomPath"),
                         logical(1))
  expect_gte(sum(path_layers), 2L)
})
