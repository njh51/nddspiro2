# P2 comes first in the document to check rows are sorted by patient.
# P1 trial 2 has FEV1 only (no predicted value), trial 3 has PEF only.
spiro_doc = function() {
  xml_doc(c(
    xml_patient("P2", xml_test(
      id = "20", date = "2023-06-01", dob = "1980-06-01",
      height = 160, weight = 55, gender = "Female", ethnicity = "Other",
      trials = c(
        xml_trial(1, 2, c(
          xml_param("FEV1", 2.0, 2.5, 2.0, -1.0),
          xml_param("FVC", 2.5, 3.0, 2.4, -1.2),
          xml_param("FEV1_FVC", 0.80, 0.82, 0.70, -0.3, unit = "")
        )),
        xml_trial(2, 1, c(
          xml_param("FEV1", 2.2, 2.5, 2.0, -0.5),
          xml_param("FVC", 2.6, 3.0, 2.4, -0.9),
          xml_param("FEV1_FVC", 0.85, 0.82, 0.70, 0.4, unit = "")
        ))
      )
    )),
    xml_patient("P1", xml_test(
      id = "10", date = "2020-01-01", dob = "1990-01-01",
      trials = c(
        xml_trial(1, 1, c(
          xml_param("FEV1", 3.0, 4.0, 3.2, -1.8),
          xml_param("FVC", 4.0, 5.0, 4.1, -1.7),
          xml_param("FEV1_FVC", 0.75, 0.80, 0.70, -0.9, unit = "")
        )),
        xml_trial(2, 3, xml_param("FEV1", 2.9)),
        xml_trial(3, 2, xml_param("PEF", 8.0, 9.0, 7.0, -0.8, unit = "L/s"))
      )
    ))
  ))
}

id_cols = c(
  "patientID", "height", "weight", "gender", "ethnicity", "dob", "age",
  "testDate", "testAutoID", "testType", "stageType", "trialNumber",
  "trialRank", "accepted"
)
value_cols = function(params, vars = c("value", "pred", "pctpred", "lln", "zscore", "unit")) {
  paste(rep(params, each = length(vars)), vars, sep = "_")
}

test_that("read_spiro validates its inputs", {
  doc = spiro_doc()
  expect_error(read_spiro("file.xml"), "xml2 object")
  expect_error(read_spiro(doc, trace = 0), "positive integer")
  expect_error(read_spiro(doc, trace = "1"), "positive integer")
  expect_error(read_spiro(doc, trace = c(1, 2)), "positive integer")
  expect_error(read_spiro(doc, parameters = "XYZ"), "No matching")
})

test_that("read_spiro returns one row per trial with the expected columns", {
  res = read_spiro(spiro_doc())
  expect_s3_class(res, "data.table")
  # P1 trial 3 has no default parameter and is dropped
  expect_identical(nrow(res), 4L)
  expect_identical(names(res), c(id_cols, value_cols(c("FEV1", "FVC", "FEV1_FVC"))))
  expect_type(res$height, "double")
  expect_type(res$trialNumber, "integer")
  expect_type(res$trialRank, "integer")
  expect_type(res$FEV1_value, "double")
  expect_type(res$FEV1_unit, "character")
})

test_that("read_spiro sorts rows and keeps test fields aligned with trials", {
  res = read_spiro(spiro_doc())
  expect_identical(res$patientID, c("P1", "P1", "P2", "P2"))
  expect_identical(res$trialNumber, c(1L, 2L, 1L, 2L))
  expect_identical(res$testAutoID, c("10", "10", "20", "20"))
  expect_identical(res$gender, c("Male", "Male", "Female", "Female"))
  expect_equal(res$height, c(170, 170, 160, 160))
  expect_equal(
    res$age,
    as.numeric(as.Date(c("2020-01-01", "2020-01-01", "2023-06-01", "2023-06-01")) -
      as.Date(c("1990-01-01", "1990-01-01", "1980-06-01", "1980-06-01")))/365.25
  )
})

test_that("read_spiro extracts parameter values and percent predicted", {
  res = read_spiro(spiro_doc())
  expect_equal(res$FEV1_value, c(3.0, 2.9, 2.0, 2.2))
  expect_equal(res$FVC_zscore, c(-1.7, NA, -1.2, -0.9))
  expect_equal(res$FEV1_pctpred, c(75, NA, 80, 88))
  expect_identical(res$FEV1_unit, c("L", "L", "L", "L"))
})

test_that("read_spiro gives NA for missing children and parameters", {
  res = read_spiro(spiro_doc())
  p1_t2 = res[patientID == "P1" & trialNumber == 2L]
  expect_equal(p1_t2$FEV1_value, 2.9)
  expect_true(is.na(p1_t2$FEV1_pred))
  expect_true(is.na(p1_t2$FEV1_lln))
  expect_true(all(is.na(unlist(p1_t2[, value_cols("FVC"), with = FALSE]))))
})

test_that("read_spiro omits percent predicted when pct_pred = FALSE", {
  res = read_spiro(spiro_doc(), pct_pred = FALSE)
  expect_identical(
    names(res),
    c(id_cols, value_cols(c("FEV1", "FVC", "FEV1_FVC"), c("value", "pred", "lln", "zscore", "unit")))
  )
})

test_that("read_spiro keeps trials with rank <= trace", {
  res = read_spiro(spiro_doc(), trace = 1)
  expect_identical(res$patientID, c("P1", "P2"))
  expect_identical(res$trialRank, c(1L, 1L))

  # P1 trial 3 (rank 2) has no default parameter, P1 trial 2 is rank 3
  res = read_spiro(spiro_doc(), trace = 2)
  expect_identical(res$patientID, c("P1", "P2", "P2"))
  expect_identical(res$trialNumber, c(1L, 1L, 2L))
  expect_identical(res$trialRank, c(1L, 2L, 1L))
})

test_that("read_spiro follows the order of `parameters` in the columns", {
  res = read_spiro(spiro_doc(), parameters = c("FEV1_FVC", "PEF", "FEV1"))
  expect_identical(names(res), c(id_cols, value_cols(c("FEV1_FVC", "PEF", "FEV1"))))
  # P1 trial 3 is kept because it has PEF
  expect_identical(nrow(res), 5L)
})

test_that("read_spiro silently skips parameters absent from the file", {
  res = read_spiro(spiro_doc(), parameters = c("FEV1", "FEF75"))
  expect_identical(names(res), c(id_cols, value_cols("FEV1")))
})
