#' Load alignments and prepare template correspondence table for concatenate () function
#'
#' This function creates a template correspondence table that can also be saved in the working directory.
#'
#' @importFrom stats sd
#' @importFrom utils write.table
#'
#' @param fasta_files Optional, a vector of paths to the fasta files that should be merged. If this argument is missing, the function automatically detects and uses all the fasta files present in the working directory.
#' @param out Optional, a filename for the correspondence table template to save (without extension). No file is saved if \code{out} is not provided. In all cases, the function also returns a tibble with the correspondence table template (invisibly if \code{out} is provided).
#' @param excel Boolean, should the correspondence table template be saved in excel format? If \code{FALSE}, it is saved as a tab-separated text file instead. Default is \code{TRUE}. The correct file extension is automatically appended to the \code{out} argument. If \code{out} is missing, this argument has no effect.
#' @param exclude If no \code{fasta_files} argument has been passed, fasta files matching the \code{exclude} pattern will be ignored by the function when it automatically detects fasta files in the working directory.
#' 
#' @return A tibble with the correspondence table template (invisibly if an \code{out} argument was provided to save the table to a file).
#'
#' @examples
#' dir <- system.file("extdata", package = "concatipede")
#' fasta_files <- find_fasta(dir)
#' concatipede_prepare(fasta_files)
#' 
#' @export

concatipede_prepare <- function(fasta_files, out = "seqnames", excel = TRUE, exclude = "concatenated"){
    # Read files in the foldes and create a list if needed
    if (missing(fasta_files)) {
        fasta_files <- find_fasta(dir = getwd(), exclude = exclude)
    }
    # Check that all fasta files share the same folder
    dir_name <- unique(sapply(fasta_files, dirname))
    if (length(dir_name) > 1) {
        stop("All fasta files should be located in the same directory.")
    }
    # Extract basenames to use as column names later
    fasta_cols <- sapply(fasta_files, basename)
    if (anyDuplicated(fasta_cols)) {
        stop("Some files share the same name (",
             fasta_cols[anyDuplicated(fasta_cols)], ").")
    }
    
    # Load the fasta alignments, do some quality check and rename them with the original file name
    l <- list()
    maxlen <- 0
    for (i in 1:length(fasta_files)){
        dataset <- ape::read.FASTA(fasta_files[i])
        a <- sd(unlist(lapply(dataset, length)))
        if (a != 0) {
            warning("In file ", fasta_files[i], " not all sequences of same length \n")
        }
        l[[i]] <- assign(fasta_files[i], dataset)
        if (length(names(dataset)) > maxlen) {
            maxlen <- length(names(dataset))
        }
    }
    names(l) <- fasta_cols

    # Create data frame with sequence names
    df <- as.data.frame(matrix(0, ncol = length(l)+1, nrow = maxlen))
    colnames(df) <- c("name", names(l))
    for (i in 1:length(l)) {
        column <- names(l[i])
        seqnames <- c(names(l[[i]]), rep("", maxlen-length(names(l[[i]]))))
        df[,i+1] <- seqnames
    }
    
    # Save the template for the translation table as txt or excel file, as required
    if (!missing(out)) {
        if(excel) {
            writexl::write_xlsx(df, path = paste0(out, ".xlsx"), col_names = TRUE,
                                format_headers = TRUE)
        } else {
            write.table(df, sep = "\t", file = paste0(out, ".txt"), row.names = FALSE,
                        quote = FALSE)
        }
    }

    # Return df as a tibble
    df <- tibble::as_tibble(df)
    attr(df, "dir_name") <- dir_name
    if (!missing(out)) {
        return(invisible(df))
    }
    df
}
