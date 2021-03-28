#' Save an alignment in fasta format
#'
#' @param x Alignment (an object of class \code{DNAbin}).
#' @param path Path of the file to be written (the ".fasta" extension is added automatically).
#'
#' @return The input \code{x} (invisibly).

write_fasta <- function(x, path) {
    write.alignment(x, path, format = "fasta")
    invisible(x)
}

#' Save an alignment in nexus format
#'
#' @param x Alignment (an object of class \code{DNAbin}).
#' @param path Path of the file to be written (the ".nexus" extension is added automatically).
#'
#' @return The input \code{x} (invisibly).

write_nexus <- function(x, path) {
    write.alignment(x, path, format = "nexus")
    invisible(x)
}

#' Save an alignment in phylip format
#'
#' @param x Alignment (an object of class \code{DNAbin}).
#' @param path Path of the file to be written (the ".phy" extension is added automatically).
#'
#' @return The input \code{x} (invisibly).

write_phylip <- function(x, path) {
    write.alignment(x, path, format = "phylip")
    invisible(x)    
}
