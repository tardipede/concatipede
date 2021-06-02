# See https://r-pkgs.org/man.html#multiple-man for information about how to
# document several functions in the same help file.

#' Writing alignments
#'
#' Alignments can be saved in `fasta`, `nexus`, and `phylip` formats.
#'
#' @param x Alignment to save (an object of class \code{DNAbin}).
#' @param path Path of the file to be written, without file extension (the
#'     appropriate extension is added automatically, i.e. the path will be
#'     extended with ".fasta", ".nexus", or ".phy" depending on the file format
#'     used).
#'
#' @return The input \code{x} (invisibly).
#'
#' @name write_aln
#'
#' @examples
#' \dontrun{
#'   # Path to an example alignment file
#'   pkg_aln <- system.file("extdata", "COI_Macrobiotidae.fas", package="concatipede")
#'   # Load the alignment into the R session
#'   aln <- ape::read.FASTA(pkg_aln)
#'   # Write the alignment in various formats
#'   # Note that the appropriate file extension is added by the writing functions.
#'   write_fasta(aln, "my-alignment")
#'   write_nexus(aln, "my-alignment")
#'   write_phylip(aln, "my-alignment")
#' }
NULL

#' Save an alignment in fasta format
#'
#' @rdname write_aln
#' @export

write_fasta <- function(x, path) {
    write.alignment(x, path, format = "fasta")
    invisible(x)
}

#' Save an alignment in nexus format
#'
#' @rdname write_aln
#' @export

write_nexus <- function(x, path) {
    write.alignment(x, path, format = "nexus")
    invisible(x)
}

#' Save an alignment in phylip format
#'
#' @rdname write_aln
#' @export

write_phylip <- function(x, path) {
    write.alignment(x, path, format = "phylip")
    invisible(x)    
}
