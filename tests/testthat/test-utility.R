ndd_xml = xml2::read_xml(
  '<ndd>
    <Patients>
      <Patient ID="P001">
        <Test><ResultParameter ID="FEV1"/><ResultParameter ID="FVC"/></Test>
      </Patient>
      <Patient ID="P002">
        <Test><ResultParameter ID="FEV1"/><ResultParameter ID="FEV1FVC"/></Test>
      </Patient>
    </Patients>
  </ndd>'
)

# ls_pid / ls_params -----------------------------------------------------

test_that("ls_pid returns all patient IDs", {
  expect_identical(ls_pid(ndd_xml), c("P001", "P002"))
})

test_that("ls_params returns unique parameter IDs in order of appearance", {
  expect_identical(ls_params(ndd_xml), c("FEV1", "FVC", "FEV1FVC"))
})

test_that("ls_pid and ls_params return empty vectors when nodes are absent", {
  empty = xml2::read_xml("<ndd/>")
  expect_identical(ls_pid(empty), character(0))
  expect_identical(ls_params(empty), character(0))
})

test_that("ls_pid and ls_params reject non xml_document input", {
  expect_error(ls_pid("file.xml"), "xml2 object")
  expect_error(ls_params(list()), "xml2 object")
})

# recode_ethnicity_gli ---------------------------------------------------

test_that("recode_ethnicity_gli maps common labels to GLI-2012 codes", {
  x = c(
    "Caucasian", "white", " White British ", "White - Other",
    "African", "Black African", "African American", "african-american",
    "NE Asian", "North East Asian", "Northeast Asian", "north_east asian",
    "SE Asian", "South East Asian", "Southeast Asian",
    "Mixed", "Mixed White and Black", "Other", "Other Asian"
  )
  expect_identical(
    recode_ethnicity_gli(x),
    c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L,
      5L, 5L, 5L, 5L)
  )
})

test_that("recode_ethnicity_gli returns NA for unknown or missing labels", {
  expect_identical(
    recode_ethnicity_gli(c("Chinese", "Asian", "Mother", "", NA)),
    rep(NA_integer_, 5)
  )
})

test_that("recode_ethnicity_gli accepts factors and rejects other types", {
  expect_identical(recode_ethnicity_gli(factor(c("white", "Black"))), c(1L, 2L))
  expect_error(recode_ethnicity_gli(1:3), "character/factor")
})

# recode_gender_gli ------------------------------------------------------

test_that("recode_gender_gli maps common labels to GLI codes", {
  x = c("Male", "M", "male ", "man", "Boy", "Female", "F", "f", "Woman", "girl")
  expect_identical(
    recode_gender_gli(x),
    c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L)
  )
})

test_that("recode_gender_gli does not read female as male", {
  expect_identical(recode_gender_gli(c("female", "FEMALE")), c(2L, 2L))
})

test_that("recode_gender_gli returns NA for unknown or missing labels", {
  expect_identical(
    recode_gender_gli(c("1", "unknown", "", NA)),
    rep(NA_integer_, 4)
  )
})

test_that("recode_gender_gli accepts factors and rejects other types", {
  expect_identical(recode_gender_gli(factor(c("F", "M"))), c(2L, 1L))
  expect_error(recode_gender_gli(c(1, 2)), "character/factor")
})
