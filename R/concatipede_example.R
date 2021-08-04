# Inspired from readr::readr_example()

#' Get the path to one of concatipede example files
#'
#' Several example files are shipped with the concatipede package. This function facilitates the access to those files.
#'
#' \describe{
#'   \item{COI_Macrobiotidae.fas}{Example fasta file.}
#'   \item{ITS2_Macrobiotidae.fas}{Example fasta file.}
#'   \item{LSU_Macrobiotidae.fas}{Example fasta file.}
#'   \item{SSU_Macrobiotidae.fas}{Example fasta file.}
#'   \item{sequences-test-matching.xlsx}{This is an Excel file (extension .xlsx) typically used to test or demonstrate the automatic matching capabilities of the concatipede package. This file represents the Excel template that could be produced by \code{\link{concatipede_prepare}} after detecting the fasta files present in a working directory.}
#'   \item{Macrobiotidae_seqnames.xlsx}{This is an Excel file (extension .xlsx) that contains the correspondence table that can be used to concatenate the sequences contained in the example fasta files COI_Macrobiotidae.fas, ITS2_Macrobiotidae.fas, LSU_Macrobiotidae.fas, and SSU_Macrobiotidae.fas.}
#' }
#' 
#' @param example_file Basename of the target example file. If \code{NULL} (the default), the basenames of the available files are listed.
#'
#' @return The full path to access the example file, or a list of available example files if no \code{example_file} argument was provided.
#'
#' @examples
#' concatipede_example()
#' example <- concatipede_example("sequences-test-matching.xlsx")
#' if (requireNamespace("readxl")) {
#'   seqs <- readxl::read_xlsx(example)
#'   seqs
#' }
#'
#' @export

concatipede_example <- function(example_file = NULL) {
    if (is.null(example_file)) {
        list.files(system.file("extdata", package = "concatipede"))
    } else {
        system.file("extdata", example_file, package = "concatipede", mustWork = TRUE)
    }
}
