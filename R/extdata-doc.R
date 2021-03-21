# Documenting data files in inst/extdata/ following advice from this thread:
# https://stackoverflow.com/questions/35373257/r-and-roxygen2-how-to-document-data-files-in-inst-extdata

#' @title Example files shipped with the package
#' @name example-files-shipped-with-the-package
#' 
#' @description
#' Several example files are shipped with the concatipede package.
#'
#' @section Excel file 'sequences-test-matching.xlsx':
#'
#' This is an Excel file (extension .xlsx) typically used to test or
#' demonstrate the automatic matching capabilities of the concatipede package.
#'
#' This file represents the Excel template that could be produced by
#' \code{\link{concatipede_prepare}} after detecting the fasta files present in a
#' working directory.
#'
#' @examples
#' # Excel file 'sequences-test-matching.xlsx'
#' if (requireNamespace("readxl")) {
#'   path <- system.file("extdata", "sequences-test-matching.xlsx",
#'                       package = "concatipede")
#'   seqs <- readxl::read_xlsx(path)
#'   seqs
#' }
NULL
