### * Description

# Functions in this file are responsible for automatic matching of sequence
# names across fasta files.

# All functions from this file are exported.

# TODO Move dependencies for this file to the Suggests field of DESCRIPTION
# instead of Imports?

### * auto_match_seqs() @export

#' Build a template table with automatically matched sequence names
#'
#' The algorithm used to match sequences across fasta files based on their
#' names is outlined below.
#'
#' Let's assume a situation with N fasta files, with each fasta file i having
#' n_i sequence names. The problem of matching the names in the best possible
#' way across the fasta files is similar to that of identifying homologous
#' proteins across species, using e.g. reciprocal blast.
#'
#' The algorithm steps are:
#' \itemize{
#'
#'   \item For each pair of fasta files, identify matching names using a
#'   reciprocal match approach: two names match if and only if they are their
#'   reciprocal best match.
#'
#'   \item Those matches across fasta files define a graph.
#'
#'   \item We identify sub-graphs such that (i) they contain at most one
#'   sequence name per fasta file and (ii) all nodes in a given sub-graph are
#'   fully connected (i.e., they are all their best reciprocal matches across
#'   any pair of fasta files).
#' 
#' }
#'
#' @param x A table (data frame or tibble) typically produced by
#'     \code{\link{concatipede_prepare}}. It must be of the same format as a
#'     table returned by this function: a first column called "name" followed
#'     by one column per fasta file. Those columns have the name of their
#'     corresponding fasta file, and they contain the names of the sequences in
#'     this file, with one sequence name per cell. The number of rows in the
#'     number of sequences of the fasta file with the most sequences, and the
#'     columns for the other fasta files are filled with \code{NA} for padding.
#' @param method Method for string distance calculation. See
#'     \code{?stringdist::stringdist-metrics} for details. Default is
#'     \code{"lv"}.
#' @param xlsx Optional, a path to use to save the output table as an Excel
#'     file.
#'
#' @return A table (tibble) with the same columns as \code{x} and with sequence
#'     names automatically matched across fasta files. Sequence names which did
#'     not have a best reciprocal match in other fasta files are appended to
#'     the end of the table, so that the output table columns contain all the
#'     unique sequence names present in the corresponding column of the input
#'     table. The first column, "name", contains a suggested name for the row
#'     (not guaranteed to be unique). If a path was provided to the \code{xlsx}
#'     argument, an Excel file is saved and the table is returned invisibly.
#'
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @importFrom utils combn
#' 
#' @examples
#' xlsx_file <- system.file("extdata", "sequences-test-matching.xlsx",
#'                          package = "concatipede")
#' xlsx_template <- readxl::read_xlsx(xlsx_file)
#' auto_match_seqs(xlsx_template)
#' \dontrun{
#'   auto_match_seqs(xlsx_template, xlsx = "my-automatic-output.xlsx")
#' }
#'
#' @export

auto_match_seqs <- function(x, method = "lv", xlsx) {
    seqtable <- tibble::as_tibble(x)
    if (!colnames(seqtable)[1] == "name") {
        stop("The first column must be called \"name\".")
    }
    stopifnot(ncol(seqtable) > 2)
    ffiles <- colnames(seqtable)[2:ncol(seqtable)]
    # Store the dir_name attribute, if any
    dir_name <- attr(x, "dir_name")
    # Build a tidy table with unique ids for each sequence name
    ftibs <- lapply(ffiles, function(f) {
        out <- tibble::tibble(fasta_file = f, seq_name = na.omit(unique(seqtable[[f]])))
        out[["id"]] <- sapply(out[["seq_name"]], uuid)
        out
    })
    ftib <- dplyr::bind_rows(ftibs)
    # TODO Add a step where duplicates in sequence names within a file raise a
    # warning or an error.
    # Pairwise reciprocal matches
    pairs <- data.frame(t(combn(ffiles, m = 2)))
    colnames(pairs) <- c("file1", "file2")
    pairs <- tibble::as_tibble(pairs)
    pairs$best_matches <- lapply(1:nrow(pairs), function(i) {
        f1 <- pairs$file1[i]
        f2 <- pairs$file2[i]
        v1 <- ftib$seq_name[ftib$fasta_file == f1]
        v2 <- ftib$seq_name[ftib$fasta_file == f2]
        bm <- reciprocal_matches(v1, v2, method = method)
        bm
    })
    pairs$best_matches_id <- lapply(1:nrow(pairs), function(i) {
        f1 <- pairs$file1[i]
        f2 <- pairs$file2[i]
        bm <- pairs$best_matches[[i]]
        x <- ftib[ftib$fasta_file == f1, c("seq_name", "id")]
        colnames(x)[2] <- "id1"
        y <- ftib[ftib$fasta_file == f2, c("seq_name", "id")]
        colnames(y)[2] <- "id2"
        z <- dplyr::left_join(bm, x, by = c("x" = "seq_name"))
        z <- dplyr::left_join(z, y, by = c("y" = "seq_name"))
        z[, c("id1", "id2")]
    })
    # Build a graph containing all the reciprocal best matches
    g <- igraph::graph.data.frame(dplyr::bind_rows(pairs$best_matches_id), directed = FALSE)
    # Examine each subgraph of g to accept/reject it
    sg <- igraph::decompose(g)
    validated <- list()
    k <- 1
    for (i in seq_along(sg)) {
        m <- igraph::as_adjacency_matrix(sg[[i]])
        nodes <- colnames(m)
        # Check that there is at most one node from each fasta file
        nodes_ff <- ftib[ftib$id %in% nodes, ]
        valid <- (length(unique(nodes_ff$fasta_file)) == length(nodes_ff$fasta_file))
        # Check that all nodes are reciprocal best matches
        edges <- sum(m)
        expected_edges <- nrow(m) * (nrow(m) - 1)
        valid <- valid && (edges == expected_edges)
        if (valid) {
            validated[[k]] <- nodes
            k <- k + 1
        }
    }
    # Check that each node is present only once in the final matches
    z <- unlist(validated)
    stopifnot(length(z) == length(unique(z)))
    # Convert the `validated` list into a clean tibble
    rows <- list()
    id_to_ffile <- setNames(ftib$fasta_file, nm = ftib$id)
    id_to_seqname <- setNames(ftib$seq_name, nm = ftib$id)
    for (i in seq_along(validated)) {
        ids <- validated[[i]]
        seqnames <- id_to_seqname[ids]
        names(seqnames) <- id_to_ffile[ids]
        rows[[i]] <- seqnames[ffiles]
        names(rows[[i]]) <- ffiles
    }
    z <- dplyr::bind_rows(rows)
    # Add missing seq names
    for (f in ffiles) {
        file_seqnames <- ftib$seq_name[ftib$fasta_file == f]
        matched_seqnames <- na.omit(z[[f]])
        unmatched_seqnames <- file_seqnames[!file_seqnames %in% matched_seqnames]
        if (length(unmatched_seqnames) > 0) {
            new_rows <- tibble::tibble(x = unmatched_seqnames)
            colnames(new_rows) <- f
            z <- dplyr::bind_rows(z, new_rows)
        }
    }
    # Order by size of name set
    w <- apply(z, 1, function(x) mean(is.na(x)))
    z <- z[order(w), ]
    # Try to extract names from matching table
    my_lcs <- function(a, b) {
        a <- strsplit(a, "")[[1]]
        b <- strsplit(b, "")[[1]]
        lcs_info <- qualV::LCS(a, b)
        lcs_steps <- diff(lcs_info$va)
        if (length(lcs_steps) == 0) return("")
        lcs_mask <- c(uuid())
        for (i in seq_along(lcs_steps)) {
            if (lcs_steps[i] == 1) {
                lcs_mask[i+1] <- lcs_mask[i]
            } else {
                lcs_mask[i+1] <- uuid()
            }
        }
        segments <- table(lcs_mask)[unique(lcs_mask)]
        lcs_uuid <- names(segments)[which.max(segments)]
        lcs_indices <- lcs_info$va[lcs_mask == lcs_uuid]
        lcs <- a[lcs_indices]
        paste0(lcs, collapse = "")
    }
    find_lcs <- function(a) {
        Reduce(my_lcs, a)
    }
    trim_special <- function(x, specials = c("-_")) {
        specials <- strsplit(specials, "")[[1]]
        x <- strsplit(x, "")[[1]]
        test_trim <- TRUE
        while (test_trim & length(x) > 1) {
            test_trim <- FALSE
            for (s in specials) {
                if (length(x) > 1 && x[1] == s) {
                    x <- x[2:length(x)]
                    test_trim <- TRUE
                }
                if (length(x) > 1 && x[length(x)] == s) {
                    x <- x[1:(length(x)-1)]
                    test_trim <- TRUE
                }
                if (length(x) == 1 && x == s) {
                    x <- c()
                }
            }
        }
        return(paste0(x, collapse = ""))
    }
    proposed_names <- c()
    for (i in seq_len(nrow(z))) {
        name_set <- na.omit(unlist(z[i, ]))
        lcs <- find_lcs(name_set)
        proposed_names[i] <- trim_special(lcs)
    }
    # Return
    out <- tibble::tibble(name = proposed_names, z)
    attr(out, "dir_name") <- dir_name
    if (missing(xlsx)) return(out)
    writexl::write_xlsx(out, path = xlsx)
    return(invisible(out))
}
