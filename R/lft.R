# Interpretation of routine lung function tests following the ERS/ATS
# technical standard on interpretive strategies (Stanojevic et al., Eur Respir J
# 2022; 60: 2101499). All limits of normal are z-score based:
# LLN = 5th percentile (-1.645), ULN = 95th percentile (+1.645).
LFT_LLN = -1.645
LFT_ULN = 1.645

# validate an (optional) z-score argument; NULL becomes an all-NA vector
lft_zarg = function(x, n, name) {
  if (is.null(x)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(x) && all(is.na(x))) {
    x = as.numeric(x)
  }
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be a numeric vector", name))
  }
  if (length(x) != n) {
    stop(sprintf("`%s` must have the same length as the first argument", name))
  }
  return(x)
}

lft_factor = function(x, levels, stringsAsFactors) {
  if (stringsAsFactors) {
    x = factor(x, levels = levels)
  }
  return(x)
}

#' bronchodilator response expressed as percent of the predicted value
#'
#' @param pre,post numeric vectors of pre- and post-bronchodilator values
#' @param pred numeric vector of predicted values (same unit as `pre`/`post`)
#' @export
lft_bdr = function(pre, post, pred) {
  return(100*(post - pre)/pred)
}

#' positive bronchodilator responsiveness (ERS/ATS 2022): a change > 10% of
#' the predicted value
#'
#' Applies to a single variable (e.g. FEV1 or FVC). A positive response
#' in either FEV1 or FVC can be obtained by combining the results with `|`.
#'
#' @param pre,post numeric vectors of pre- and post-bronchodilator values
#' @param pred numeric vector of predicted values (same unit as `pre`/`post`)
#' @param threshold percent of predicted above which the response is positive
#' @return a logical vector, `NA` where the response cannot be determined
#' @examples
#' fev1 = lft_bdr_response(pre = 2.0, post = 2.5, pred = 4)
#' fvc = lft_bdr_response(pre = 3.0, post = 3.2, pred = 4.5)
#' fev1 | fvc  # ERS/ATS 2022 positive response in FEV1 or FVC
#' @export
lft_bdr_response = function(pre, post, pred, threshold = 10) {
  return(lft_bdr(pre, post, pred) > threshold)
}

#' ventilatory pattern of lung function impairment (ERS/ATS 2022, figure 8,
#' tables 5 and 6)
#'
#' Restriction is defined by TLC < LLN, so without TLC a low FVC with a
#' preserved FEV1/FVC is only reported as `"prism"` (possible restriction or
#' non-specific pattern) and a low FEV1/FVC with low FVC as `"possible mixed"`.
#' When TLC is available:
#' * FEV1/FVC < LLN and TLC < LLN: `"mixed"`
#' * TLC < LLN: `"simple restrictive"` when RV/TLC and FRC/TLC are normal,
#'  `"complex restrictive"` when either is > ULN, `"restrictive"` when neither
#'  ratio is available
#' * FEV1/FVC < LLN: `"obstructive"`
#' * FEV1 or FVC < LLN with normal FEV1/FVC and TLC: `"non-specific"`
#'
#' @param fev1fvc_zscore,fvc_zscore numeric vectors of FEV1/FVC and FVC zscore
#' @param fev1_zscore optional numeric vector of FEV1 zscore, used to detect an
#'  isolated low FEV1 with preserved FEV1/FVC and FVC
#' @param tlc_zscore,rv_tlc_zscore,frc_tlc_zscore optional numeric vectors of
#'  TLC, RV/TLC and FRC/TLC zscore; per-element `NA` falls back to the
#'  spirometry-only interpretation
#' @param stringsAsFactors a bool if true turn output result into a factor vector
#'  otherwise remains as a character/string vector
#' @export
lft_patterns = function(
  fev1fvc_zscore,
  fvc_zscore,
  fev1_zscore = NULL,
  tlc_zscore = NULL,
  rv_tlc_zscore = NULL,
  frc_tlc_zscore = NULL,
  stringsAsFactors = FALSE
) {
  n = length(fev1fvc_zscore)
  fev1fvc_zscore = lft_zarg(fev1fvc_zscore, n, "fev1fvc_zscore")
  fvc_zscore = lft_zarg(fvc_zscore, n, "fvc_zscore")
  fev1_zscore = lft_zarg(fev1_zscore, n, "fev1_zscore")
  tlc_zscore = lft_zarg(tlc_zscore, n, "tlc_zscore")
  rv_tlc_zscore = lft_zarg(rv_tlc_zscore, n, "rv_tlc_zscore")
  frc_tlc_zscore = lft_zarg(frc_tlc_zscore, n, "frc_tlc_zscore")

  obs = fev1fvc_zscore < LFT_LLN
  low_fvc = fvc_zscore < LFT_LLN
  low_spiro = low_fvc | (!is.na(fev1_zscore) & fev1_zscore < LFT_LLN)
  has_tlc = !is.na(tlc_zscore)
  low_tlc = tlc_zscore < LFT_LLN
  trapping = pmax(rv_tlc_zscore, frc_tlc_zscore, na.rm = TRUE) > LFT_ULN

  res = data.table::fcase(
    is.na(obs) | is.na(low_fvc), NA_character_,
    # lung volumes available: TLC defines restriction
    low_tlc & obs, "mixed",
    low_tlc & trapping, "complex restrictive",
    low_tlc & !trapping, "simple restrictive",
    low_tlc, "restrictive",
    has_tlc & obs, "obstructive",
    has_tlc & low_spiro, "non-specific",
    has_tlc, "normal",
    # spirometry only
    obs & low_fvc, "possible mixed",
    obs, "obstructive",
    low_spiro, "prism",
    default = "normal"
  )
  return(lft_factor(
    res,
    c("normal", "obstructive", "prism", "non-specific", "restrictive",
      "simple restrictive", "complex restrictive", "possible mixed", "mixed"),
    stringsAsFactors
  ))
}

#' lung volume pattern (ERS/ATS 2022, figure 10 and table 7)
#'
#' * TLC < LLN: `"mixed"` (RV/TLC or FRC/TLC > ULN and FEV1/FVC < LLN),
#'  `"complex restrictive"` (ratio > ULN, FEV1/FVC normal),
#'  `"simple restrictive"` (ratios normal), otherwise `"restrictive"`
#' * TLC > ULN: `"hyperinflation"` (ratio > ULN), `"large lungs"` (ratios
#'  normal), otherwise `"possible hyperinflation"`
#' * TLC normal: `"hyperinflation"` (FRC/TLC > ULN), `"air trapping"` (only
#'  RV/TLC > ULN), otherwise `"normal"`
#'
#' @param tlc_zscore numeric vector of TLC zscore
#' @param rv_tlc_zscore,frc_tlc_zscore numeric vectors of RV/TLC and FRC/TLC
#'  zscore, either may be `NULL`
#' @param fev1fvc_zscore optional numeric vector of FEV1/FVC zscore, used to
#'  separate mixed from complex restriction
#' @param stringsAsFactors a bool if true turn output result into a factor vector
#'  otherwise remains as a character/string vector
#' @export
lft_volume_pattern = function(
  tlc_zscore,
  rv_tlc_zscore = NULL,
  frc_tlc_zscore = NULL,
  fev1fvc_zscore = NULL,
  stringsAsFactors = FALSE
) {
  n = length(tlc_zscore)
  tlc_zscore = lft_zarg(tlc_zscore, n, "tlc_zscore")
  rv_tlc_zscore = lft_zarg(rv_tlc_zscore, n, "rv_tlc_zscore")
  frc_tlc_zscore = lft_zarg(frc_tlc_zscore, n, "frc_tlc_zscore")
  fev1fvc_zscore = lft_zarg(fev1fvc_zscore, n, "fev1fvc_zscore")

  low_tlc = tlc_zscore < LFT_LLN
  high_tlc = tlc_zscore > LFT_ULN
  high_frc = frc_tlc_zscore > LFT_ULN
  trapping = pmax(rv_tlc_zscore, frc_tlc_zscore, na.rm = TRUE) > LFT_ULN
  obs = fev1fvc_zscore < LFT_LLN

  res = data.table::fcase(
    is.na(tlc_zscore), NA_character_,
    low_tlc & trapping & obs, "mixed",
    low_tlc & trapping & !obs, "complex restrictive",
    low_tlc & !trapping, "simple restrictive",
    low_tlc, "restrictive",
    high_tlc & trapping, "hyperinflation",
    high_tlc & !trapping, "large lungs",
    high_tlc, "possible hyperinflation",
    high_frc, "hyperinflation",
    trapping, "air trapping",
    default = "normal"
  )
  return(lft_factor(
    res,
    c("normal", "large lungs", "possible hyperinflation", "hyperinflation",
      "air trapping", "restrictive", "simple restrictive",
      "complex restrictive", "mixed"),
    stringsAsFactors
  ))
}

#' gas transfer pattern (ERS/ATS 2022, figure 11)
#'
#' * DLCO > ULN: `"high"` (increased pulmonary blood volume, e.g. shunt,
#'  erythrocytosis, alveolar haemorrhage)
#' * DLCO < LLN with normal VA: `"low, normal VA"` (pulmonary vascular disease,
#'  emphysema with preserved volume, anaemia)
#' * DLCO and VA < LLN, KCO > ULN: `"low, low VA, high KCO"` (incomplete
#'  expansion or localised loss of lung volume)
#' * DLCO and VA < LLN, KCO <= ULN: `"low, low VA, low/normal KCO"` (loss of
#'  alveolar-capillary units, e.g. emphysema, ILD)
#'
#' @param dlco_zscore,va_zscore,kco_zscore numeric vectors of DLCO, VA and KCO
#'  zscore, `va_zscore` and `kco_zscore` may be `NULL` (result is then `"low"`)
#' @param stringsAsFactors a bool if true turn output result into a factor vector
#'  otherwise remains as a character/string vector
#' @export
lft_gas_transfer = function(
  dlco_zscore,
  va_zscore = NULL,
  kco_zscore = NULL,
  stringsAsFactors = FALSE
) {
  n = length(dlco_zscore)
  dlco_zscore = lft_zarg(dlco_zscore, n, "dlco_zscore")
  va_zscore = lft_zarg(va_zscore, n, "va_zscore")
  kco_zscore = lft_zarg(kco_zscore, n, "kco_zscore")

  low_va = va_zscore < LFT_LLN
  res = data.table::fcase(
    is.na(dlco_zscore), NA_character_,
    dlco_zscore > LFT_ULN, "high",
    dlco_zscore >= LFT_LLN, "normal",
    !low_va, "low, normal VA",
    low_va & kco_zscore > LFT_ULN, "low, low VA, high KCO",
    low_va & kco_zscore <= LFT_ULN, "low, low VA, low/normal KCO",
    default = "low"
  )
  return(lft_factor(
    res,
    c("normal", "high", "low", "low, normal VA", "low, low VA, high KCO",
      "low, low VA, low/normal KCO"),
    stringsAsFactors
  ))
}

#' severity of lung function impairment in lung function tests, where:
#' * zscore >= -1.64 (normal),
#' * zscore < -1.645 and >= -2.5 (mild)
#' * zscore < -2.5 and >= -4.0 (moderate)
#' * zscore < -4.0 (severe)
#'
#' @param zscore a numeric vector contains corresponding spirometry zscore
#' @param stringsAsFactors a bool if true turn output result into a factor vector
#'  otherwise remains as a character/string vector
#' @export
lft_severity = function(zscore, stringsAsFactors = FALSE) {
  if (!is.numeric(zscore)) {
    stop("`x` must be a numeric vector")
  }
  res = data.table::fcase(
    zscore >= -1.645, "normal",
    zscore >= -2.5, "mild",
    zscore >= -4.0, "moderate",
    zscore < -4.0, "severe",
    default = NA_character_
  )
  res = lft_factor(
    res, levels = c("normal", "mild", "moderate", "severe"),
    stringsAsFactors = stringsAsFactors
  )
  return(res)
}
