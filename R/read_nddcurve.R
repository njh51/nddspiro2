#' Read and tidy ndd channel flow and volume data 
#' 
#' Extract channel flow and volume data with time from ndd xml file
#' 
#' @param x An XML document object, as returned by [xml2::read_xml()].
#' @param trace A positive integer specifying which trial(s) to extract (by rank)
#' 
#' @return A `data.table` containing patient ID, time, flow and volume data
#' 
#' @export

read_curve <- function(x, trace = NULL) {

    if (!inherits(x, "xml_document")) stop("`x` must be an xml2 object read by `read_xml()` function from `xml2` packages")

    if (!is.null(trace)) {
        if (!is.numeric(trace) || length(trace) != 1 || trace <= 0) stop("argument `trace` must be a positive number")
        trial_xpath <- paste0("//Trial[", paste(paste("Rank = ", seq_len(trace)), collapse = " or "), "]")
    } else {
        trial_xpath <- "//Trial"
    }

    trial_nodes <- xml2::xml_find_all(x, trial_xpath)
    flow_nodes <- xml2::xml_find_first(trial_nodes, "./ChannelFlow")
    volume_nodes <- xml2::xml_find_first(trial_nodes, "./ChannelVolume")

    channelFlow <- xml2::xml_text(xml2::xml_find_first(flow_nodes, "./SamplingValues"))
    channelVolume <- xml2::xml_text(xml2::xml_find_first(volume_nodes, "./SamplingValues"))

    timeZeroOffsetFlow <- xml2::xml_double(xml2::xml_find_first(flow_nodes, "./TimeZeroOffset"))
    timeZeroOffsetVol <- xml2::xml_double(xml2::xml_find_first(volume_nodes, "./TimeZeroOffset"))

    samplingFreqFlow <- xml2::xml_double(xml2::xml_find_first(flow_nodes, "./SamplingInterval"))
    samplingFreqVol <- xml2::xml_double(xml2::xml_find_first(volume_nodes, "./SamplingInterval"))

    test_nodes <- xml2::xml_find_first(trial_nodes, "ancestor::Test[1]")
    patientID <- xml2::xml_attr(xml2::xml_find_first(trial_nodes, "ancestor::Patient[1]"), "ID")

    testAutoID <- xml2::xml_attr(test_nodes, "TestAutoID")
    testDate <- xml2::xml_text(xml2::xml_find_first(test_nodes, "./TestDate"))
    testType <- xml2::xml_attr(test_nodes, "TypeOfTest")
    stageType <- xml2::xml_attr(test_nodes, "StageType")
    trialNumber <- xml2::xml_integer(xml2::xml_find_first(trial_nodes, "./Number"))
    trialRank <- xml2::xml_integer(xml2::xml_find_first(trial_nodes, "./Rank"))

    res <- data.table(
        patientID, testAutoID, testDate, testType, stageType,
        trialNumber, trialRank, timeZeroOffsetFlow, timeZeroOffsetVol,
        samplingFreqFlow, samplingFreqVol, channelFlow, channelVolume
    )

    by_vars <- c(
        "patientID", "testAutoID", "testDate", "testType", 
        "stageType", "trialNumber", "trialRank", "timeZeroOffsetFlow",
        "timeZeroOffsetVol", "samplingFreqFlow", "samplingFreqVol"
    )

    res <- res[
        ,
        {
            flow_vals <- as.numeric(unlist(strsplit(channelFlow, split = " ")))
            volume_vals <- as.numeric(unlist(strsplit(channelVolume, split = " ")))
            if (length(flow_vals) < length(volume_vals)) {
                length(flow_vals) <- length(volume_vals)
            } else {
                length(volume_vals) <- length(flow_vals)
            }
            list(flow = flow_vals, volume = volume_vals)
        },
        by = by_vars
    ]

    res[]

}