# lft_zarg ---------------------------------------------------------------

test_that("lft_zarg turns NULL into an all-NA numeric vector", {
  expect_identical(lft_zarg(NULL, 3, "x"), rep(NA_real_, 3))
})

test_that("lft_zarg accepts an all-NA logical vector as numeric", {
  expect_identical(lft_zarg(c(NA, NA), 2, "x"), c(NA_real_, NA_real_))
})

test_that("lft_zarg validates type and length", {
  expect_identical(lft_zarg(c(1, 2), 2, "x"), c(1, 2))
  expect_error(lft_zarg(c("1", "2"), 2, "x"), "`x` must be a numeric vector")
  expect_error(lft_zarg(c(TRUE, NA), 2, "x"), "`x` must be a numeric vector")
  expect_error(lft_zarg(c(1, 2, 3), 2, "x"), "same length")
})

# lft_bdr / lft_bdr_response ---------------------------------------------

test_that("lft_bdr is the change as percent of predicted", {
  expect_equal(lft_bdr(pre = 2, post = 2.4, pred = 4), 10)
  expect_equal(lft_bdr(c(2, 3), c(1.8, 3.5), c(4, 5)), c(-5, 10))
})

test_that("lft_bdr_response uses > 10% of predicted", {
  # +12.5%, +10% exactly (not > 10), -5%, NA
  res = lft_bdr_response(
    pre = c(2, 2, 2, NA), post = c(2.5, 2.4, 1.8, 2), pred = c(4, 4, 4, 4)
  )
  expect_identical(res, c(TRUE, FALSE, FALSE, NA))
})

test_that("lft_bdr_response accepts a custom threshold", {
  expect_identical(lft_bdr_response(2, 2.5, 4, threshold = 15), FALSE)
})

test_that("FEV1 or FVC response combines with `|` and keeps NA semantics", {
  fev1 = lft_bdr_response(c(2, NA, NA), c(2.1, 2, 2), c(4, 4, 4))
  fvc = lft_bdr_response(c(3, 3, 3), c(3.5, 3.5, 3), c(4, 4, 4))
  expect_identical(fev1 | fvc, c(TRUE, TRUE, NA))
})

# lft_patterns -----------------------------------------------------------

test_that("lft_patterns classifies spirometry-only patterns (figure 8)", {
  res = lft_patterns(
    fev1fvc_zscore = c(0, -2, 0, -2),
    fvc_zscore = c(0, 0, -2, -2)
  )
  expect_identical(res, c("normal", "obstructive", "prism", "possible mixed"))
})

test_that("lft_patterns flags isolated low FEV1 only when FEV1 is given", {
  expect_identical(lft_patterns(0, 0, fev1_zscore = -2), "prism")
  expect_identical(lft_patterns(0, 0, fev1_zscore = NA_real_), "normal")
  expect_identical(lft_patterns(0, 0), "normal")
})

test_that("lft_patterns uses TLC to define restriction (tables 5 and 6)", {
  res = lft_patterns(
    fev1fvc_zscore = c(-2, 0, 0, 0, 0, -2, 0, 0, -2),
    fvc_zscore = c(-2, -2, -2, -2, -2, -2, 0, 0, 0),
    fev1_zscore = c(-2, -2, -2, -2, -2, -2, -2, 0, 0),
    tlc_zscore = c(-2, -2, -2, -2, 0, 0, 0, 0, 0),
    rv_tlc_zscore = c(0, 0, 2, NA, 0, 0, 0, 0, 0),
    frc_tlc_zscore = c(0, 0, 0, NA, 0, 0, 0, 0, 0)
  )
  expect_identical(res, c(
    "mixed",               # low ratio + low TLC
    "simple restrictive",  # low TLC, ratios normal
    "complex restrictive", # low TLC, RV/TLC > ULN
    "restrictive",         # low TLC, ratios missing
    "non-specific",        # low FVC, normal ratio and TLC
    "obstructive",         # low FVC from air trapping, TLC normal
    "non-specific",        # isolated low FEV1, TLC normal
    "normal",
    "obstructive"
  ))
})

test_that("lft_patterns detects trapping from FRC/TLC alone", {
  res = lft_patterns(0, -2, tlc_zscore = -2, frc_tlc_zscore = 2)
  expect_identical(res, "complex restrictive")
})

test_that("lft_patterns falls back to spirometry per element when TLC is NA", {
  res = lft_patterns(c(0, 0), c(-2, -2), tlc_zscore = c(NA, -2))
  expect_identical(res, c("prism", "restrictive"))
  # an all-NA column read as logical
  res = lft_patterns(c(0, -2), c(-2, -2), tlc_zscore = c(NA, NA))
  expect_identical(res, c("prism", "possible mixed"))
})

test_that("lft_patterns uses strict limits at the LLN boundary", {
  expect_identical(lft_patterns(-1.645, -1.645), "normal")
  expect_identical(lft_patterns(-1.646, -1.645), "obstructive")
})

test_that("lft_patterns returns NA when ratio or FVC is missing", {
  expect_identical(lft_patterns(c(NA, 0), c(0, NA)), c(NA_character_, NA))
})

test_that("lft_patterns returns factor with all levels on request", {
  res = lft_patterns(c(0, -2), c(0, 0), stringsAsFactors = TRUE)
  expect_s3_class(res, "factor")
  expect_identical(as.character(res), c("normal", "obstructive"))
  expect_length(levels(res), 9)
})

test_that("lft_patterns validates inputs", {
  expect_error(lft_patterns(c("-2", "-10"), c(0, 0)), "`fev1fvc_zscore`")
  expect_error(lft_patterns(0, "0"), "`fvc_zscore`")
  expect_error(lft_patterns(c(0, 0), 0), "same length")
  expect_error(lft_patterns(0, 0, tlc_zscore = c(0, 0)), "same length")
})

test_that("lft_patterns handles zero-length input", {
  expect_identical(lft_patterns(numeric(0), numeric(0)), character(0))
})

# lft_volume_pattern -----------------------------------------------------

test_that("lft_volume_pattern follows figure 10", {
  res = lft_volume_pattern(
    tlc_zscore = c(-2, -2, -2, -2, 3, 3, 3, 0, 0, 0),
    rv_tlc_zscore = c(2, 2, 0, NA, 2, 0, NA, 0, 2, 0),
    frc_tlc_zscore = c(0, 0, 0, NA, 0, 0, NA, 2, 0, 0),
    fev1fvc_zscore = c(-2, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  expect_identical(res, c(
    "mixed", "complex restrictive", "simple restrictive", "restrictive",
    "hyperinflation", "large lungs", "possible hyperinflation",
    "hyperinflation", "air trapping", "normal"
  ))
})

test_that("lft_volume_pattern without FEV1/FVC cannot split mixed/complex", {
  expect_identical(lft_volume_pattern(-2, rv_tlc_zscore = 2), "restrictive")
})

test_that("lft_volume_pattern returns NA for missing TLC and validates", {
  expect_identical(lft_volume_pattern(c(NA, 0)), c(NA, "normal"))
  expect_error(lft_volume_pattern("0"), "`tlc_zscore`")
  expect_s3_class(lft_volume_pattern(0, stringsAsFactors = TRUE), "factor")
})

# lft_gas_transfer -------------------------------------------------------

test_that("lft_gas_transfer follows figure 11", {
  res = lft_gas_transfer(
    dlco_zscore = c(2, 0, -2, -2, -2, -2, -2, NA),
    va_zscore = c(0, 0, 0, -2, -2, NA, -2, 0),
    kco_zscore = c(0, 0, 0, 2, 0, 0, NA, 0)
  )
  expect_identical(res, c(
    "high", "normal", "low, normal VA", "low, low VA, high KCO",
    "low, low VA, low/normal KCO", "low", "low", NA
  ))
})

test_that("lft_gas_transfer works with DLCO only and validates", {
  expect_identical(lft_gas_transfer(c(-2, 0)), c("low", "normal"))
  expect_error(lft_gas_transfer("-2"), "`dlco_zscore`")
  expect_s3_class(lft_gas_transfer(0, stringsAsFactors = TRUE), "factor")
})

# lft_severity -----------------------------------------------------------

test_that("lft_severity grades z-scores into four bands", {
  res = lft_severity(c(0, -1.645, -1.7, -2.5, -2.51, -4, -4.1, NA))
  expect_identical(res, c(
    "normal", "normal", "mild", "mild", "moderate", "moderate", "severe", NA
  ))
})

test_that("lft_severity returns factor on request and validates", {
  res = lft_severity(c(-5, 0), stringsAsFactors = TRUE)
  expect_s3_class(res, "factor")
  expect_identical(levels(res), c("normal", "mild", "moderate", "severe"))
  expect_error(lft_severity("a"), "must be a numeric vector")
})
