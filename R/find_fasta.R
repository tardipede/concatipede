#' Find fasta files present in a folder
#'
#' @param dir Path to the directory which should be examined. If not provided,
#'     the current working directory (as returned by \code{\link{getwd}}) is
#'     used.
#' @param pattern Regular expression used by \code{\link{list.files}} to detect
#'     the fasta files. The default is to list all files ending in ".fa",
#'     ".fas", and ".fasta".
#' @param exclude Optional regular expression used to exclude some filenames
#'     from the list of detected files.
#'
#' @return A vector with the full paths to the found files.
#'
#' @examples
#' # Get the directory containing the package example files
#' dir <- system.file("extdata", package = "concatipede")
#' # List the fasta files containing in that directory
#' find_fasta(dir)
#' # Exclude some files
#' find_fasta(dir, exclude = "COI")
#' 
#' @export

find_fasta <- function(dir, pattern = "\\.fa$|\\.fas$|\\.fasta$", exclude) {
    # Parse the dir argument
    if (missing(dir)) {
        dir <- getwd()
    }
    if (length(dir) != 1) {
        stop("The `dir` argument must contain exactly one path.")
    }
    # Read files in the `dir` folder and create a list
    files <- list.files(path = dir, pattern = pattern)
    # Exclude the files matching the `exclude` pattern in their filename
    if (!missing(exclude)){
        to_keep <- !grepl(pattern = exclude, x = files)
        files <- files[to_keep]
    }
    # Build the full paths to the found files
    out <- file.path(dir, files)
    # Return
    out
}
