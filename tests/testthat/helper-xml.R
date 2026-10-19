# builders for small ndd EasyWare-like XML documents used in tests

xml_tag = function(tag, value) {
  if (is.null(value)) "" else sprintf("<%s>%s</%s>", tag, value, tag)
}

xml_param = function(id, value, pred = NULL, lln = NULL, zscore = NULL, unit = "L") {
  paste0(
    sprintf('<ResultParameter ID="%s">', id),
    xml_tag("DataValue", value),
    xml_tag("PredictedValue", pred),
    xml_tag("LLNormalValue", lln),
    xml_tag("Z-Score", zscore),
    xml_tag("Unit", unit),
    "</ResultParameter>"
  )
}

xml_channel = function(tag, values, offset = 0, interval = 0.01) {
  if (is.null(values)) {
    return("")
  }
  paste0(
    "<", tag, ">",
    xml_tag("SamplingInterval", interval),
    xml_tag("TimeZeroOffset", offset),
    xml_tag("SamplingValues", values),
    "</", tag, ">"
  )
}

xml_trial = function(number, rank, params = "", flow = NULL, volume = NULL,
                     accepted = "true") {
  paste0(
    "<Trial>",
    xml_tag("Number", number),
    xml_tag("Rank", rank),
    xml_tag("Accepted", accepted),
    xml_channel("ChannelFlow", flow, offset = 0.1),
    xml_channel("ChannelVolume", volume, offset = 0.2),
    "<ResultParameters>", paste0(params, collapse = ""), "</ResultParameters>",
    "</Trial>"
  )
}

xml_test = function(id, date, trials, height = 170, weight = 70,
                    gender = "Male", ethnicity = "Caucasian",
                    dob = "1990-01-01", type = "FVC", stage = "Pre") {
  paste0(
    sprintf('<Test TestAutoID="%s" TypeOfTest="%s" StageType="%s">', id, type, stage),
    xml_tag("TestDate", date),
    "<PatientDataAtTestTime>",
    xml_tag("Height", height),
    xml_tag("Weight", weight),
    xml_tag("Gender", gender),
    xml_tag("Ethnicity", ethnicity),
    xml_tag("DateOfBirth", dob),
    "</PatientDataAtTestTime>",
    "<Trials>", paste0(trials, collapse = ""), "</Trials>",
    "</Test>"
  )
}

xml_patient = function(id, tests) {
  paste0(
    sprintf('<Patient ID="%s"><Intervals><Interval><Tests>', id),
    paste0(tests, collapse = ""),
    "</Tests></Interval></Intervals></Patient>"
  )
}

xml_doc = function(patients) {
  xml2::read_xml(paste0(
    "<ndd><Patients>", paste0(patients, collapse = ""), "</Patients></ndd>"
  ))
}
