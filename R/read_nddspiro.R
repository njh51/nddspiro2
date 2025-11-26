#' Read and Tidy ndd Spirometry XML Data
#'
#' Extracts and tidies spirometry measurement data for specified parameters from ndd XML files.
#'
#' @param x An XML document object, as returned by [xml2::read_xml()].
#' @param parameters A character vector of parameter IDs to extract (e.g., `"FEV1"`, `"FVC"`). 
#'  Defaults to common spirometry parameters. 
#'  All parameters in the ndd XML file can be retrieved using `get_params()`
#'  the function won't check input parameters for you
#' @param trace A positive integer specifying which trial(s) to extract (by rank), or NULL to include all trials.
#' @param wide Logical. If TRUE (default), returns data in wide format (one row per test/trial). If FALSE, returns long format (one row per parameter).
#' @param tidy_colorder Logical. If TRUE (default), orders columns in a tidy, grouped fashion (only relevant if `wide = TRUE`).
#' @param pct_pred Logical. If TRUE (default), precentage of predicted will be derived (100*value/pred)
#' @return A `data.table` containing patient, test, and parameter data, either in "long" or "wide" format.
#'
#' @details
#' This function parses ndd spirometry XML files to extract key test results, patient demographics,
#' test/trial metadata, and spirometry parameter values (actual, predicted, LLN, z-score, units).
#'
#' - If `trace` is provided, only trials with the specified rank(s) are extracted.
#' - If `wide = TRUE`, measurements are spread so that each parameter/metric appears as its own column.
#' - If `tidy_colorder = TRUE`, columns are ordered for readability in the wide format.
#' - If `pct_pred = TRUE`, new columns % predicted will be added for each parameters, note that not all 
#'  parameters have meaningful % columns 
#'
#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_double xml_text xml_integer
#' @importFrom data.table data.table dcast setnames setcolorder setorderv ":="
#'
#' @examples
#' \dontrun{
#' doc <- xml2::read_xml("example_ndd.xml")
#' dt <- read_spiro(doc, parameters = c("FEV1", "FVC"), trace = 1, wide = TRUE)
#' }
#' @export

read_spiro <- function(
    x,
    parameters = c("FEV1", "FVC", "FEV1_FVC"), 
    trace = NULL, 
    wide = TRUE,
    tidy_colorder = TRUE,
    pct_pred = TRUE
) {

    if (!inherits(x, "xml_document")) stop("`x` must be an xml2 object read by `read_xml()` function from `xml2` packages")

    if (!is.null(trace)) {
        if (!is.numeric(trace) || length(trace) != 1 || trace <= 0) stop("argument `trace` must be a positive integer")
        trial_xpath <- paste0("//Trial[", paste(paste("Rank = ", seq_len(trace)), collapse = " or "), "]")
    } else {
        trial_xpath <- "//Trial"
    }

    param_condtions <- paste(paste0("@ID = '", parameters, "'"), collapse = " or ")
    param_xpath <- paste0(trial_xpath, "/ResultParameters/ResultParameter[", param_condtions, "]")
    param_nodes <- xml2::xml_find_all(x, param_xpath)

    if (length(param_nodes) == 0) stop("No matching spirometry parameters found in the given XML file")

    trial_nodes <- xml2::xml_find_first(param_nodes, "ancestor::Trial[1]")
    test_nodes <- xml2::xml_find_first(param_nodes, "ancestor::Test[1]")
    patient_nodes <- xml2::xml_find_first(param_nodes, "ancestor::Patient[1]")
    pdtest_nodes <- xml2::xml_find_first(test_nodes, "./PatientDataAtTestTime")

    patientID <- xml2::xml_attr(patient_nodes, "ID")
    height <- xml2::xml_double(xml2::xml_find_first(pdtest_nodes, "./Height"))
    weight <- xml2::xml_double(xml2::xml_find_first(pdtest_nodes, "./Weight"))
    gender <- xml2::xml_text(xml2::xml_find_first(pdtest_nodes, "./Gender"))
    ethnicity <- xml2::xml_text(xml2::xml_find_first(pdtest_nodes, "./Ethnicity"))
    dob <- xml2::xml_text(xml2::xml_find_first(pdtest_nodes, "./DateOfBirth"))
    testAutoID <- xml2::xml_attr(test_nodes, "TestAutoID")
    testDate <- xml2::xml_text(xml2::xml_find_first(test_nodes, "./TestDate"))
    age <- as.numeric(difftime(as.Date(testDate), as.Date(dob), units = "days"))/365.25
    testType <- xml2::xml_attr(test_nodes, "TypeOfTest")
    stageType <- xml2::xml_attr(test_nodes, "StageType")
    trialNumber <- xml2::xml_integer(xml2::xml_find_first(trial_nodes, "./Number"))
    trialRank <- xml2::xml_integer(xml2::xml_find_first(trial_nodes, "./Rank"))
    accepted <- xml2::xml_text(xml2::xml_find_first(trial_nodes, "./Accepted"))
    parameter <- xml2::xml_attr(param_nodes, "ID")
    value <- xml2::xml_double(xml2::xml_find_first(param_nodes, "./DataValue"))
    pred <- xml2::xml_double(xml2::xml_find_first(param_nodes, "./PredictedValue"))
    lln <- xml2::xml_double(xml2::xml_find_first(param_nodes, "./LLNormalValue"))
    zscore <- xml2::xml_double(xml2::xml_find_first(param_nodes, "./Z-Score"))
    unit <- xml2::xml_text(xml2::xml_find_first(param_nodes, "./Unit"))

    res <- data.table::data.table(
        patientID, height, weight, gender, ethnicity, dob, age,
        testDate, testAutoID, testType, stageType, trialNumber,
        trialRank, accepted, parameter, value, pred, lln, zscore, 
        unit
    )

    if (pct_pred) res[, pctpred := 100*value/pred]

    if (wide) {
        name_vars <- "parameter"

        if (pct_pred) {
            value_vars <- c("value", "pred", "pctpred", "lln", "zscore", "unit")
        } else {
            value_vars <- c("value", "pred", "lln", "zscore", "unit")
        }
        
        id_vars <- setdiff(names(res), c(name_vars, value_vars))
        wide_formula <- paste(paste(id_vars, collapse = " + "), name_vars, sep = " ~ ")
        res <- data.table::dcast(res, wide_formula, value.var = value_vars)

        old_names <- grep("^(value|pred|pctpred|lln|zscore|unit)_", names(res), value = TRUE)
        new_names <- gsub(r"[^(value|pred|pctpred|lln|zscore|unit)(_)(.*)]", r"(\3_\1)", old_names)
        data.table::setnames(res, old_names, new_names)

        if (tidy_colorder) {
            param_order <- c(t(outer(unique(gsub("_.*", "", new_names)), value_vars, paste, sep = "_")))
            data.table::setcolorder(res, c(id_vars, param_order))
        }
    }

    res[]

}