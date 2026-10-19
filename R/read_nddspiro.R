#' Read and Tidy ndd Spirometry XML Data
#'
#' Extracts and tidies spirometry measurement data for specified parameters
#' from ndd XML files.
#'
#' @param x an ndd Easyware XML file read by `xml2::read_xml()`.
#' @param parameters A character vector of parameter IDs to extract
#'  (e.g., `"FEV1"`, `"FVC"`). Defaults to common spirometry parameters.
#'  All parameters in the ndd XML file can be retrieved using `ls_params()`
#'  the function won't check input parameters for you
#' @param trace A positive integer specifying which trial(s) to extract (by rank),
#'  or NULL to include all trials.
#' @param pct_pred Logical. If TRUE (default), precentage of predicted will be
#'  derived (100*value/pred)
#' @return A `data.table` containing patient, test, and parameters data
#'
#' @details
#' This function parses ndd spirometry XML files to extract key test results,
#' patient demographics, test/trial metadata, and spirometry parameter values
#' (actual, predicted, LLN, z-score, units).
#'
#' - If `trace` is provided, only trials with the specified rank(s) are extracted.
#' - If `pct_pred = TRUE`, new columns % predicted will be added for each parameters,
#'  note that not all parameters have meaningful percentage predicted columns
#'
#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_double xml_text xml_integer
#' @importFrom data.table data.table dcast setnames setcolorder setorderv ":="
#'
#' @examples
#' \dontrun{
#' doc <- xml2::read_xml("example_ndd.xml")
#' dt <- read_spiro(doc, parameters = c("FEV1", "FVC"), trace = 1)
#' }
#' @export
read_spiro = function(
  x,
  parameters = c("FEV1", "FVC", "FEV1_FVC"),
  trace = NULL,
  pct_pred = TRUE
) {
  if (!inherits(x, "xml_document")) {
    stop("`x` must be an xml2 object read by `read_xml()` function from the `xml2` package")
  }

  if (is.null(trace)) {
    rank_cond = ""
  } else {
    if (!is.numeric(trace) || length(trace) != 1 || trace <= 0) {
      stop("argument `trace` must be a positive integer")
    }
    rank_cond = sprintf("[Rank <= %d]", as.integer(trace))
  }

  param_cond = paste0(
    "[", paste0("@ID = '", parameters, "'", collapse = " or "), "]"
  )
  rel_param_xpath = paste0("ResultParameters/ResultParameter", param_cond)
  # `//Trial[...]` would build a nodeset of every node and can hit its limit
  trial_xpath = paste0(
    "/descendant::Trial", rank_cond, "[", rel_param_xpath, "]"
  )
  param_xpath = paste0(trial_xpath, "/", rel_param_xpath)

  ns = xml2::xml_ns(x)
  trial_nodes = xml2::xml_find_all(x, trial_xpath, ns = ns)
  if (length(trial_nodes) == 0) {
    stop("No matching spirometry parameters found in the given XML file")
  }
  param_nodes = xml2::xml_find_all(x, param_xpath, ns = ns)

  # test level, expanded to one row per trial
  test_nodes = xml2::xml_find_all(trial_nodes, "ancestor::Test[1]", ns = ns)
  n_trial = xml2::xml_find_num(
    test_nodes,
    paste0("count(descendant::Trial", rank_cond, "[", rel_param_xpath, "])"),
    ns = ns
  )
  pd_nodes = xml2::xml_find_first(test_nodes, "PatientDataAtTestTime", ns = ns)
  pd_text = function(f) {
    xml2::xml_text(xml2::xml_find_first(pd_nodes, f, ns = ns))
  }
  dob = pd_text("DateOfBirth")
  testDate = xml2::xml_text(xml2::xml_find_first(test_nodes, "TestDate", ns = ns))
  res = data.table::data.table(
    patientID = xml2::xml_attr(
      xml2::xml_find_first(test_nodes, "ancestor::Patient[1]", ns = ns), "ID"
    ),
    height = as.numeric(pd_text("Height")),
    weight = as.numeric(pd_text("Weight")),
    gender = pd_text("Gender"),
    ethnicity = pd_text("Ethnicity"),
    dob = dob,
    age = as.numeric(
      difftime(as.Date(testDate), as.Date(dob), units = "days")
    )/365.25,
    testDate = testDate,
    testAutoID = xml2::xml_attr(test_nodes, "TestAutoID"),
    testType = xml2::xml_attr(test_nodes, "TypeOfTest"),
    stageType = xml2::xml_attr(test_nodes, "StageType")
  )[rep.int(seq_along(test_nodes), n_trial)]

  # trial level
  trial_text = function(f) {
    xml2::xml_text(xml2::xml_find_first(trial_nodes, f, ns = ns))
  }
  res[, `:=`(
    trialNumber = as.integer(trial_text("Number")),
    trialRank = as.integer(trial_text("Rank")),
    accepted = trial_text("Accepted")
  )]

  # parameter level
  pv = xml_child_text(
    x, param_xpath, param_nodes,
    c("DataValue", "PredictedValue", "LLNormalValue", "Z-Score", "Unit"), ns
  )
  n_param = xml2::xml_find_num(
    trial_nodes, paste0("count(", rel_param_xpath, ")"), ns = ns
  )
  long = data.table::data.table(
    trial = rep.int(seq_along(trial_nodes), n_param),
    parameter = xml2::xml_attr(param_nodes, "ID"),
    value = as.numeric(pv$DataValue),
    pred = as.numeric(pv$PredictedValue),
    lln = as.numeric(pv$LLNormalValue),
    zscore = as.numeric(pv[["Z-Score"]]),
    unit = pv$Unit
  )

  if (pct_pred) {
    long[, pctpred := 100*value/pred]
    value_vars = c("value", "pred", "pctpred", "lln", "zscore", "unit")
  } else {
    value_vars = c("value", "pred", "lln", "zscore", "unit")
  }

  id_vars = names(res)
  wide = data.table::dcast(
    long, trial ~ parameter, value.var = value_vars, sep = "|"
  )
  res = cbind(res, wide[, !"trial"])
  old_names = setdiff(names(res), id_vars)
  data.table::setnames(
    res, old_names, sub("^([^|]+)\\|(.*)$", "\\2_\\1", old_names)
  )
  params = intersect(parameters, long$parameter)
  data.table::setcolorder(
    res,
    c(id_vars, paste(rep(params, each = length(value_vars)), value_vars, sep = "_"))
  )
  data.table::setorderv(res, id_vars)

  return(res[])
}

# text of the named child elements of `nodes` (the nodes matched by
# `nodes_xpath`) from a single XPath query
xml_child_text = function(x, nodes_xpath, nodes, fields, ns) {
  kids = xml2::xml_find_all(x, paste0(nodes_xpath, "/*"), ns = ns)
  idx = rep.int(seq_along(nodes), xml2::xml_length(nodes))
  fid = match(xml2::xml_name(kids), fields)
  keep = which(!is.na(fid) & !duplicated(idx*(length(fields) + 1L) + fid))
  txt = xml2::xml_text(kids[keep])
  res = lapply(seq_along(fields), function(i) {
    out = rep(NA_character_, length(nodes))
    hit = fid[keep] == i
    out[idx[keep][hit]] = txt[hit]
    out
  })
  names(res) = fields
  return(res)
}
