#' Read an Excel file
#'
#' This function loads a table from an Excel file.
#'
#' @param path The path to the Excel file to read.
#' @param sheet Optional, the sheet to read (either a string with the name of the sheet or an integer with its position). Default: 1.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @export

read_xl <- function(path, sheet = 1) {
    readxl::read_xlsx(path = path, sheet = sheet, col_names = TRUE)
}
