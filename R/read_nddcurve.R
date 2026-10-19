#' Read and tidy ndd channel flow and volume data
#'
#' Extract channel flow and volume data with time from ndd xml file
#'
#' @param x An ndd Easyware XML file read by `xml2::read_xml()`.
#' @param trace A positive integer specifying which trial(s) to extract (by rank)
#'
#' @return A `data.table` containing patient ID, time, flow and volume data
#'
#' @export
read_curve = function(x, trace = NULL) {
  if (!inherits(x, "xml_document")) {
    stop("`x` must be an xml2 object read by `read_xml()` function from the `xml2` package")
  }

  if (is.null(trace)) {
    trial_xpath = "/descendant::Trial"
  } else {
    if (!is.numeric(trace) || length(trace) != 1 || trace <= 0) {
      stop("argument `trace` must be a positive integer")
    }
    # `//Trial[...]` would build a nodeset of every node and can hit its limit
    trial_xpath = sprintf("/descendant::Trial[Rank <= %d]", as.integer(trace))
  }

  ns = xml2::xml_ns(x)
  find_first = function(nodes, xpath) {
    xml2::xml_find_first(nodes, xpath, ns = ns)
  }

  trial_nodes = xml2::xml_find_all(x, trial_xpath, ns = ns)
  flow_nodes = find_first(trial_nodes, "ChannelFlow")
  volume_nodes = find_first(trial_nodes, "ChannelVolume")
  test_nodes = find_first(trial_nodes, "ancestor::Test[1]")

  res = data.table::data.table(
    patientID = xml2::xml_attr(find_first(trial_nodes, "ancestor::Patient[1]"), "ID"),
    testAutoID = xml2::xml_attr(test_nodes, "TestAutoID"),
    testDate = xml2::xml_text(find_first(test_nodes, "TestDate")),
    testType = xml2::xml_attr(test_nodes, "TypeOfTest"),
    stageType = xml2::xml_attr(test_nodes, "StageType"),
    trialNumber = xml2::xml_integer(find_first(trial_nodes, "Number")),
    trialRank = xml2::xml_integer(find_first(trial_nodes, "Rank")),
    timeZeroOffsetFlow = xml2::xml_double(find_first(flow_nodes, "TimeZeroOffset")),
    timeZeroOffsetVol = xml2::xml_double(find_first(volume_nodes, "TimeZeroOffset")),
    samplingFreqFlow = xml2::xml_double(find_first(flow_nodes, "SamplingInterval")),
    samplingFreqVol = xml2::xml_double(find_first(volume_nodes, "SamplingInterval"))
  )

  # pad the shorter channel of each trial with NA
  flow = strsplit(
    xml2::xml_text(find_first(flow_nodes, "SamplingValues")), " ", fixed = TRUE
  )
  volume = strsplit(
    xml2::xml_text(find_first(volume_nodes, "SamplingValues")), " ", fixed = TRUE
  )
  n_flow = lengths(flow)
  n_volume = lengths(volume)
  n = pmax(n_flow, n_volume)
  row = rep.int(seq_along(n), n)
  pos = sequence(n)

  res = res[row]
  res[, `:=`(flow = NA_real_, volume = NA_real_)]
  data.table::set(
    res, which(pos <= n_flow[row]), "flow",
    as.numeric(unlist(flow, use.names = FALSE))
  )
  data.table::set(
    res, which(pos <= n_volume[row]), "volume",
    as.numeric(unlist(volume, use.names = FALSE))
  )

  return(res[])
}
