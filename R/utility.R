#' Extract Patient IDs from ndd XML File
#'
#' This function retrieves all patient IDs from an ndd XML file.
#'
#' @param x An XML document object read by `xml2::read_xml()`.
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
#' pids <- get_pid(doc)
#' }
#'
#' @export
get_pid <- function(x) {
  if (!inherits(x, "xml_document")) stop("`x` must be an xml2 object read by `read_xml()` function from `xml2` packages")
  res <- xml2::xml_find_all(x, xpath = "//Patient") |>
    xml2::xml_attr(attr = "ID")
  res
}

#' Extract Unique Spirometry Parameter IDs from ndd XML File
#'
#' This function retrieves all unique spirometry parameter IDs from an ndd XML file.
#'
#' @param x An XML document object read by `xml2::read_xml()`.
#'
#' @return A character vector of unique spirometry parameter IDs extracted from the XML file.
#'
#' @details
#' The function searches for all `<ResultParameter>` nodes in the provided XML document
#' and extracts the value of their `ID` attribute. Duplicate parameter IDs are removed.
#' The input must be an object of class `xml_document`, as produced by the `xml2::read_xml()` function.
#'
#' @examples
#' \dontrun{
#' doc <- xml2::read_xml("path/to/ndd_file.xml")
#' params <- get_params(doc)
#' }
#'
#' @export
get_params <- function(x) {
  if (!inherits(x, "xml_document")) stop("`x` must be an xml2 object read by `read_xml()` function from `xml2` packages")
  res <- xml2::xml_find_all(x, xpath = "//ResultParameter") |>
    xml2::xml_attr(attr = "ID") |> 
    unique()
  res
}