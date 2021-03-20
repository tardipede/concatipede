# Documenting data files in inst/extdata/ following advice from this thread:
# https://stackoverflow.com/questions/35373257/r-and-roxygen2-how-to-document-data-files-in-inst-extdata

#' Excel file 'sequences-test-matching.xlsx'
#'
#' This is an Excel file (.xlsx) typically used to test or demonstrate the
#' automatic matching capabilities of the concatipede package.
#'
#' This file represents the Excel template that could be produced by
#' \code{\link{concatipe_prepare}} after detecting the fasta files present in a
#' working directory.
#'
#' @name sequences-test-matching.xlsx
#'
#' @usage
#' system.file("extdata", "sequences-test-matching.xlsx", package = "concatipede")
#'
#' @examples
#' if (requireNamespace(readxl)) {
#'   path <- system.file("extdata", "sequences-test-matching.xlsx",
#'                       package = "concatipede")
#'   seqs <- read_xlsx(path)
#'   seqs
#' }
NULL
