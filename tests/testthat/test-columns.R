## --- pft_required_columns ----------------------------------------------

test_that("pft_spirometry year=2012 requires race; year=2022 does not", {
  c12 <- pft_required_columns("pft_spirometry", year = 2012)
  c22 <- pft_required_columns("pft_spirometry", year = 2022)
  expect_true("race" %in% c12$required)
  expect_false("race" %in% c22$required)
  expect_setequal(c12$required, c("sex", "age", "height", "race"))
  expect_setequal(c22$required, c("sex", "age", "height"))
})

test_that("pft_volumes / pft_diffusion only require sex/age/height", {
  cv <- pft_required_columns("pft_volumes")
  cd <- pft_required_columns("pft_diffusion")
  expect_setequal(cv$required, c("sex", "age", "height"))
  expect_setequal(cd$required, c("sex", "age", "height"))
})

test_that("pft_diffusion SI.units toggles measured-column names", {
  cd_si  <- pft_required_columns("pft_diffusion", SI.units = TRUE)
  cd_tr  <- pft_required_columns("pft_diffusion", SI.units = FALSE)
  expect_true("tlco_measured" %in% cd_si$optional_measured)
  expect_true("kco_si_measured" %in% cd_si$optional_measured)
  expect_true("dlco_measured" %in% cd_tr$optional_measured)
  expect_true("kco_tr_measured" %in% cd_tr$optional_measured)
})

test_that("pft_interpret advertises BDR pre/post columns", {
  ci <- pft_required_columns("pft_interpret")
  expect_true(all(c("fev1_pre", "fev1_post", "fvc_pre", "fvc_post",
                    "fev1fvc_pre", "fev1fvc_post") %in% ci$optional_bdr))
})

test_that("pft_interpret aggregates measured columns across all groups", {
  ci <- pft_required_columns("pft_interpret")
  expect_true("fev1_measured" %in% ci$optional_measured)   # spirometry
  expect_true("frc_measured"  %in% ci$optional_measured)   # volumes
  expect_true("dlco_measured" %in% ci$optional_measured)   # diffusion
})

test_that("pft_required_columns rejects unknown function names", {
  expect_error(pft_required_columns("not_a_function"))
})

## --- NSE column overrides: bare names, strings, injection --------------

test_that("pft_spirometry: bare-name override reproduces default-name output", {
  d_canon <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  d_named <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian")
  out_canon <- pft_spirometry(d_canon, year = 2012)
  out_named <- pft_spirometry(d_named, year = 2012,
                              sex = Sex, age = Age_y,
                              height = Ht_cm, race = Ancestry)
  expect_equal(out_canon$fev1_pred, out_named$fev1_pred)
  expect_equal(out_canon$fvc_lln,   out_named$fvc_lln)
})

test_that("pft_spirometry: string-form override also works", {
  d_named <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian")
  out <- pft_spirometry(d_named, year = 2012,
                         sex = "Sex", age = "Age_y",
                         height = "Ht_cm", race = "Ancestry")
  expect_false(is.na(out$fev1_pred))
})

test_that("pft_spirometry: !! injection from a variable works", {
  d_named <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian")
  s_col <- "Sex"; a_col <- "Age_y"; h_col <- "Ht_cm"; r_col <- "Ancestry"
  out <- pft_spirometry(d_named, year = 2012,
                         sex = !!s_col, age = !!a_col,
                         height = !!h_col, race = !!r_col)
  expect_false(is.na(out$fev1_pred))
})

test_that("pft_spirometry: user's original column names are preserved", {
  d_named <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian")
  out <- pft_spirometry(d_named, year = 2012,
                         sex = Sex, age = Age_y,
                         height = Ht_cm, race = Ancestry)
  expect_true(all(c("Sex","Age_y","Ht_cm","Ancestry") %in% colnames(out)))
  # No canonical aliases pollute the output:
  expect_false("age"    %in% colnames(out))
  expect_false("height" %in% colnames(out))
})

test_that("pft_spirometry: non-demographic columns flow through", {
  d <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian",
                  patient_id = 42L, visit = "baseline")
  out <- pft_spirometry(d, year = 2012,
                         sex = Sex, age = Age_y,
                         height = Ht_cm, race = Ancestry)
  expect_equal(out$patient_id, 42L)
  expect_equal(out$visit, "baseline")
})

test_that("pft_spirometry: normalised values are written back to user's column", {
  d <- data.frame(Sex = "male", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian")
  out <- suppressWarnings(pft_spirometry(d, year = 2012,
                         sex = Sex, age = Age_y,
                         height = Ht_cm, race = Ancestry))
  expect_equal(out$Sex, "M")    # normalised in place
})

test_that("pft_volumes: bare-name override works", {
  d_canon <- data.frame(sex = "M", age = 45, height = 178)
  d_named <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178)
  out_canon <- pft_volumes(d_canon)
  out_named <- pft_volumes(d_named,
                            sex = Sex, age = Age_y, height = Ht_cm)
  expect_equal(out_canon$tlc_pred, out_named$tlc_pred)
  expect_true("Sex" %in% colnames(out_named))
  expect_false("sex" %in% colnames(out_named))
})

test_that("pft_diffusion: bare-name override works", {
  d_canon <- data.frame(sex = "M", age = 45, height = 178)
  d_named <- data.frame(Sex = "M", Age_y = 45, Ht_cm = 178)
  out_canon <- pft_diffusion(d_canon)
  out_named <- pft_diffusion(d_named,
                              sex = Sex, age = Age_y, height = Ht_cm)
  expect_equal(out_canon$dlco_pred, out_named$dlco_pred)
})

test_that("missing override column errors with expected list in the message", {
  d <- data.frame(sex = "M", age = 45, height = 178, race = "Caucasian")
  expect_error(
    pft_spirometry(d, year = 2012, sex = NotAColumn),
    "Expected:.*'NotAColumn'"
  )
})

## --- pft_interpret end-to-end with NSE overrides -----------------------

test_that("pft_interpret: NSE column overrides flow through all reference fns", {
  patient_canon <- data.frame(
    sex = "M", age = 45, height = 178, race = "Caucasian",
    fev1_measured = 2.5, fvc_measured = 3.8,
    fev1fvc_measured = 2.5/3.8, tlc_measured = 6.0
  )
  patient_named <- data.frame(
    Sex = "M", Age_y = 45, Ht_cm = 178, Ancestry = "Caucasian",
    fev1_measured = 2.5, fvc_measured = 3.8,
    fev1fvc_measured = 2.5/3.8, tlc_measured = 6.0
  )
  out_canon <- pft_interpret(patient_canon)
  out_named <- pft_interpret(patient_named,
                              sex = Sex, age = Age_y,
                              height = Ht_cm, race = Ancestry)
  expect_equal(out_canon$fev1_pred,           out_named$fev1_pred)
  expect_equal(out_canon$ats_classification,  out_named$ats_classification)
})
