# trial 1: flow longer than volume; trial 2: no volume channel;
# trial 3: empty sampling values
curve_doc = function() {
  xml_doc(xml_patient("P1", xml_test(
    id = "10", date = "2020-01-01",
    trials = c(
      xml_trial(1, 1, flow = "1 2 3", volume = "0.1 0.2"),
      xml_trial(2, 2, flow = "4 5"),
      xml_trial(3, 3, flow = "", volume = "")
    )
  )))
}

curve_cols = c(
  "patientID", "testAutoID", "testDate", "testType", "stageType",
  "trialNumber", "trialRank", "timeZeroOffsetFlow", "timeZeroOffsetVol",
  "samplingFreqFlow", "samplingFreqVol", "flow", "volume"
)

test_that("read_curve validates its inputs", {
  doc = curve_doc()
  expect_error(read_curve("file.xml"), "xml2 object")
  expect_error(read_curve(doc, trace = 0), "positive integer")
  expect_error(read_curve(doc, trace = "1"), "positive integer")
  expect_error(read_curve(doc, trace = c(1, 2)), "positive integer")
})

test_that("read_curve returns one row per sample with the expected columns", {
  res = read_curve(curve_doc())
  expect_s3_class(res, "data.table")
  expect_identical(names(res), curve_cols)
  # 3 samples from trial 1, 2 from trial 2, none from the empty trial 3
  expect_identical(res$trialNumber, c(1L, 1L, 1L, 2L, 2L))
  expect_identical(res$patientID, rep("P1", 5))
  expect_identical(res$testAutoID, rep("10", 5))
  expect_type(res$flow, "double")
  expect_type(res$volume, "double")
})

test_that("read_curve pads the shorter channel and missing channels with NA", {
  res = read_curve(curve_doc())
  expect_equal(res$flow, c(1, 2, 3, 4, 5))
  expect_equal(res$volume, c(0.1, 0.2, NA, NA, NA))
})

test_that("read_curve extracts channel metadata per trial", {
  res = read_curve(curve_doc())
  expect_equal(res$timeZeroOffsetFlow, rep(0.1, 5))
  expect_equal(res$timeZeroOffsetVol, c(0.2, 0.2, 0.2, NA, NA))
  expect_equal(res$samplingFreqFlow, rep(0.01, 5))
  expect_equal(res$samplingFreqVol, c(0.01, 0.01, 0.01, NA, NA))
})

test_that("read_curve keeps trials with rank <= trace", {
  expect_identical(read_curve(curve_doc(), trace = 1)$trialRank, c(1L, 1L, 1L))
  expect_identical(
    read_curve(curve_doc(), trace = 2)$trialRank, c(1L, 1L, 1L, 2L, 2L)
  )
})

test_that("read_curve does not merge trials with identical metadata", {
  doc = xml_doc(xml_patient("P1", xml_test(
    id = "10", date = "2020-01-01",
    trials = c(
      xml_trial(1, 1, flow = "1 2", volume = "3 4"),
      xml_trial(1, 1, flow = "5", volume = "6")
    )
  )))
  res = read_curve(doc)
  expect_equal(res$flow, c(1, 2, 5))
  expect_equal(res$volume, c(3, 4, 6))
})

test_that("read_curve returns an empty table when there are no trials", {
  res = read_curve(xml2::read_xml("<ndd/>"))
  expect_identical(nrow(res), 0L)
  expect_identical(names(res), curve_cols)
})
