#' Write an Excel file
#'
#' This function writes an input table to an Excel file and returns its input (invisibly).
#'
#' @param x A data frame or tibble to write to an Excel file.
#' @param path The path to the Excel file to be written.
#'
#' @return The input table \code{x}, invisibly (so that the function can be part of a pipeline with the pipe operator).
#'
#' @export

write_xl <- function(x, path) {
    writexl::write_xlsx(x = x, path = path, col_names = TRUE,
                        format_headers = TRUE, use_zip64 = FALSE)
    invisible(x)
}
