#' Extract Patient IDs from ndd EasyWare XML File
#'
#' This function retrieves all patient IDs from an ndd XML file.
#'
#' @param x An ndd Easyware XML file read by `xml2::read_xml()`.
#'
#' @return A character vector of patient IDs extracted from the XML file.
#'
#' @details
#' The function searches for all `<Patient>` nodes in the provided XML document
#' and extracts the value of their `ID` attribute. The input must be an object
#' of class `xml_document`, as produced by the `xml2::read_xml()` function.
#'
#' @examples
#' \dontrun{
#' doc <- xml2::read_xml("path/to/ndd_file.xml")
#' pids <- ls_pid(doc)
#' }
#'
#' @export
ls_pid = function(x) {
  if (!inherits(x, "xml_document")) {
    stop("`x` must be an xml2 object read by `read_xml()` function from the `xml2` package")
  }
  res = xml2::xml_find_all(x, xpath = "//Patient") |>
    xml2::xml_attr(attr = "ID")
  return(res)
}

#' Extract Unique Spirometry Parameter IDs from ndd EasyWare XML File
#'
#' This function retrieves all unique spirometry parameter IDs/variables
#' from an ndd XML file.
#'
#' @param x An ndd Easyware XML file read by `xml2::read_xml()`.
#'
#' @return A character vector of unique spirometry parameter IDs extracted
#'  from the XML file.
#'
#' @details
#' The function searches for all `<ResultParameter>` nodes in the provided
#' XML document and extracts the value of their `ID` attribute. Duplicate
#' parameter IDs are removed. The input must be an object of class `xml_document`,
#' as produced by the `xml2::read_xml()` function.
#'
#' @examples
#' \dontrun{
#' doc <- xml2::read_xml("path/to/ndd_file.xml")
#' params <- ls_params(doc)
#' }
#'
#' @export
ls_params = function(x) {
  if (!inherits(x, "xml_document")) {
    stop("`x` must be an xml2 object read by `read_xml()` function from the `xml2` package")
  }
  res = xml2::xml_find_all(x, xpath = "//ResultParameter") |>
    xml2::xml_attr(attr = "ID") |>
    unique()
  return(res)
}

#' Recode ethnicity to GLI-2012 numeric class
#'
#' Recode ethnicity class to numeric (1 to 5) for calculating spirometry
#' reference for gli2012 standard, where 1 = Caucasian, 2 = African-American/Black,
#' 3 = NE/north east Asian, 4 = SE/south east Asian, 5 = Other/mixed. Mixed is
#' matched first and "other" last, so "White - Other" is 1 and "Other Asian" is 5.
#'
#' @param x a character/factor vector contains sensible ethnicity class e.g.
#'  c("Caucasian", "African")
#' @export
recode_ethnicity_gli = function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("input `x` must be a character/factor vector containing sensible ethnicity class")
  }
  res = data.table::fcase(
    grepl("\\bmixed\\b", x, ignore.case = TRUE), 5L,
    grepl("\\b(white|caucasian)\\b", x, ignore.case = TRUE), 1L,
    grepl("\\b(african|black)\\b", x, ignore.case = TRUE), 2L,
    grepl("\\b(ne|north[-_ ]?east)[-_ ]?asian\\b", x, ignore.case = TRUE), 3L,
    grepl("\\b(se|south[-_ ]?east)[-_ ]?asian\\b", x, ignore.case = TRUE), 4L,
    grepl("\\bother\\b", x, ignore.case = TRUE), 5L,
    default = NA_integer_
  )
  return(res)
}

#' Recode gender to GLI numeric class
#'
#' Recode gender class to numeric (1 to 2) for calculating spirometry
#' reference for gli2012 standard where 1 = Male, 2 = Female.
#'
#' @param x a character/factor vector contains sensible gender class
#'  e.g. c("Male", "M", "Female", "F")
#' @export
recode_gender_gli = function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("input `x` must be a character/factor vector containing sensible gender class")
  }
  res = data.table::fcase(
    grepl("\\b(male|m|man|boy)\\b", x, ignore.case = TRUE), 1L,
    grepl("\\b(female|f|woman|girl)\\b", x, ignore.case = TRUE), 2L,
    default = NA_integer_
  )
  return(res)
}
