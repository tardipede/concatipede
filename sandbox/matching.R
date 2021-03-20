### * Description

# Script to gather and tidy code from Matteo to perform sequence name matching

### * Setup

library(tidyverse)

### * Code from Matteo

### ** Modified from Matteo's comments on the issue #6 in Gitlab repo on 2021-03-16

# [The corresponding sequences.xlsx is saved in sandbox/]

# Correspondence table output from concatipede_prepare()
seqtable <- data.frame(readxl::read_xlsx("sequences.xlsx")) %>%
    as_tibble()
# Column 1 is the names so I don't use it for matching
V1 <- pull(seqtable, 2) %>% na.omit()
V2 <- pull(seqtable, 3) %>% na.omit()
v_dist <- adist(x = V1, y = V2, ignore.case = TRUE, partial = TRUE)
matches <- tibble(string_to_match = V1,
                  closest_match = V2[apply(v_dist, 1, which.min)],
                  distance = apply(v_dist, 1, min))
# Find id treshold by calculatind a density distribution of the distances and
# giving the treshold as the end of the first density peak
index <- pracma::findpeaks(density(matches$distance)$y)[1, 4]
dist_treshold <- ceiling(density(matches$distance)$x[index]) + 6 # 6 is for genbank accession number
# Get only "good" matches
good_matches <- matches[matches$distance <= dist_treshold, ]

# This is a way to extract the names from a row of matched sequences: (it still
# need to be cleaned)
matched_names <- c()
for (i in 1:nrow(good_matches)){
    to_match <- unlist(good_matches[i, 1:2])
    matched_names[i] <- GrpString::CommonPatt(unlist(to_match),low=100)[1, 1]
}

### ** Modified from e-mail from Matteo (2021-03-16)

## I made a function to match sequence names between two vectors.

## It does not report the names that does not have a good match in the other
## vector, however at the end they can be appended in the last rows of the
## correspondence table where it is clear they don't have any match.

## The problem is how to expand it between columns. I think it would be better
## to match each couple of columns in a template correspondence table and then
## find a way to merge the results

# Correspondence table output from concatipede_prepare()
seqtable <- as_tibble(data.frame(readxl::read_xlsx("sequences.xlsx")))
V1 <- pull(seqtable, 2) %>% na.omit() %>% unique()
V2 <- pull(seqtable, 3) %>% na.omit() %>% unique()

match_seqnames <- function(V1,V2){

    # Matching in the first direction
    v_dist <- adist(x = V1, y = V2, ignore.case = TRUE, partial = TRUE)
    matches <- tibble(string_to_match = V1,
                      closest_match = V2[apply(v_dist, 1, which.min)],
                      distance = apply(v_dist, 1, min))
    # Find id treshold by calculating a density distribution of the distances
    # and giving the treshold as the end of the first density peak
    index <- pracma::findpeaks(density(matches$distance)$y)[1,4]
    dist_treshold <- ceiling(density(matches$distance)$x[index]) + 6 # 6 is to allow for flexibility on genbank accession number
    # Get only "good" matches
    good_matches_1 <- matches[matches$distance <= dist_treshold,]
    colnames(good_matches_1)[1:2] <- c("V1","V2")

    # Matching in the second direction
    v_dist <- adist(x = V2, y = V1,ignore.case = TRUE, partial = TRUE)
    matches <- data.frame(string_to_match = V2,
                          closest_match = V1[apply(v_dist, 1, which.min)],
                          distance = apply(v_dist, 1, min))
    # Find id treshold by calculating a density distribution of the distances
    # and giving the treshold as the end of the first density peak
    index <- pracma::findpeaks(density(matches$distance)$y)[1,4]
    dist_treshold <- ceiling(density(matches$distance)$x[index])+6 # 6 is to allow for flexibility on genbank accession number
    # Get only "good" matches
    good_matches <- matches[matches$distance <= dist_treshold, ]
    good_matches <- good_matches[order(good_matches$distance), ]
    good_matches_2 <- good_matches[!duplicated(good_matches$closest_match), ]
    good_matches_2 <- tibble(V1 = good_matches_2$closest_match,
                             V2 = good_matches_2$string_to_match,
                             distance = good_matches_2$distance)

    # Combine the two dataframes and filter them for duplicate entries
    good_matches_comb <- rbind(good_matches_1, good_matches_2)
    good_matches_comb <- good_matches_comb[!duplicated(good_matches_comb), ]
    good_matches_comb <- good_matches_comb[order(good_matches_comb$distance), ]
    good_matches_comb <- good_matches_comb[!duplicated(good_matches_comb$V1), ]
    good_matches_comb <- good_matches_comb[!duplicated(good_matches_comb$V2), ]

    # Return
    return(good_matches_comb)
}

matches <- match_seqnames(V1, V2)

### ** Modified from e-mail from Matteo (2021-03-16)

## I attach another snippet of code I make in how to put together all the
## different alignments, but I couldn´t manage to complete it

# Generate a data frame with the possible columns combinations (excluding the
# first column)
col_numbers <- 2:ncol(seqtable)
col_numbers <- data.frame(t(combn(col_numbers, m = 2)))
colnames(col_numbers) <- c("col1", "col2")
col_numbers <- as_tibble(col_numbers)

# For each couple of columns, match the sequence names
correspondences <- list()
for (i in 1:nrow(col_numbers)){
  # extract the columns ad vectors and remove duplicates and NAs
  V1 <- pull(seqtable, col_numbers$col1[i]) %>% na.omit %>% unique
  V2 <- pull(seqtable, col_numbers$col2[i]) %>% na.omit %>% unique
  matchtable_temp <- match_seqnames(V1,V2)
  correspondences[[i]] <- matchtable_temp
}

# bind all the tables together
binded_tables <- bind_rows(correspondences)

# Create a network and identify all the separate clusters
gD <- igraph::simplify(igraph::graph.data.frame(binded_tables[,1:2],
                                                directed = FALSE))
lou <- igraph::cluster_louvain(gD)

# plot(lou, gD, vertex.label = NA, vertex.size=5, edge.arrow.size = .2) useless but makes a pretty plot

## identify and keep only clusters with numbers of components =<  number of alignments
Small <- which(table(lou$membership) <= ncol(seqtable)-1)

## Which nodes should be kept?
Keep <- igraph::V(gD)[(lou$membership %in% Small)]

gD2 <- igraph::induced_subgraph(gD, Keep)
lou2 <- igraph::cluster_louvain(gD2)

# Ideally only the clusters with a cyclic structure should be kept and then
# their components mapped to the original columns.  But I have no idea on how
# to do it.

### * Prototype for automatic matching across several fasta files

# Let's assume a situation with N fasta files, with each fasta file i having
# n_i sequence names.

# The problem of matching the names in the best possible way across fasta files
# is similar to that of identifying homologous proteins across species, using
# e.g. reciprocal blast.

# Suggested steps:

# 1 - For each pair of fasta files, identify matching names using a reciprocal
# match approach: two names match if and only if they are their reciprocal best
# match.

# 2 - Those matches across fasta files define a graph.

# 3 - We want to identify sub-graphs such that (i) they contain at most one
# sequence name per fasta file and (ii) all nodes in a given sub-graph are
# fully connected (i.e., they are all their best reciprocal matches across any
# pair of fasta files).

### ** reciprocal_matches()

#' Find the best reciprocal matches between x and y
#'
#' @param x,y Character vectors. They do not need to have the same length.
#' @param method Method for string distance calculation. See
#'     \code{?stringdist::stringdist-metrics}.
#'

reciprocal_matches <- function(x, y, method = "lv") {
    # Calculate the string distance matrix
    z <- stringdist::stringdistmatrix(x, y, method = "lv")
    # Find best matches in each direction
    bm1 <- apply(z, 1, function(w) {
        best <- min(w)
        if (sum(w == best) > 1) return(NA)
        which.min(w)
    })
    bm2 <- apply(z, 2, function(w) {
        best <- min(w)
        if (sum(w == best) > 1) return(NA)
        which.min(w)
    })
    # Find reciprocal best matches
    bm1 <- tibble(x = x,
                  best_y_for_x = y[bm1])
    bm2 <- tibble(y = y,
                  best_x_for_y = x[bm2])
    bm <- dplyr::left_join(bm1, bm2, by = c("best_y_for_x" = "y"))
    kept <- which(bm$x == bm$best_x_for_y)
    bm <- bm[kept, ]
    bm <- bm[, c("x", "best_y_for_x")]
    colnames(bm) <- c("x", "y")
    # Return
    return(bm)
}

### * Workbench

# Function to generate a random, unique id
uuid <- function(...) {
    paste0(sample(c(0:9, letters, toupper(letters)), 32, TRUE), collapse = "")
}

seqtable <- as_tibble(data.frame(readxl::read_xlsx("sequences.xlsx")))
ffiles <- colnames(seqtable)[2:ncol(seqtable)]

# Build a tidy table with unique ids for each sequence name
ftibs <- lapply(ffiles, function(f) tibble(fasta_file = f,
                                           seq_name = na.omit(unique(seqtable[[f]])),
                                           id = sapply(seq_name, uuid)))
ftib <- bind_rows(ftibs)
# TODO Add a step where duplicates in sequence names within a file raise a
# warning or an error.

# Pairwise reciprocal matches
pairs <- data.frame(t(combn(ffiles, m = 2)))
colnames(pairs) <- c("file1", "file2")
pairs <- as_tibble(pairs)
pairs$best_matches <- lapply(1:nrow(pairs), function(i) {
    f1 <- pairs$file1[i]
    f2 <- pairs$file2[i]
    v1 <- ftib$seq_name[ftib$fasta_file == f1]
    v2 <- ftib$seq_name[ftib$fasta_file == f2]
    bm <- reciprocal_matches(v1, v2)
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
g <- igraph::graph.data.frame(bind_rows(pairs$best_matches_id), directed = FALSE)

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
z <- bind_rows(rows)

# Add missing seq names
for (f in ffiles) {
    file_seqnames <- ftib$seq_name[ftib$fasta_file == f]
    matched_seqnames <- na.omit(z[[f]])
    unmatched_seqnames <- file_seqnames[!file_seqnames %in% matched_seqnames]
    if (length(unmatched_seqnames) > 0) {
        new_rows <- tibble(x = unmatched_seqnames)
        colnames(new_rows) <- f
        z <- bind_rows(z, new_rows)
    }
}


# Try to extract names from matching table
extracted_names =  split(z, seq(nrow(z))) %>%
  lapply(function(x){x = unlist(x[!is.na(x)])}) %>%
  lapply(GrpString::CommonPatt,low=100) %>%
  lapply(function(x){x = x[1,1]}) %>%
  lapply(function(x){if(substring(x, 1, 1) == "_"){x = substring(x, 2)}else{x = x}}) %>%
  lapply(function(x){if(substring(x, nchar(x), nchar(x)) == "_"){x = substring(x, 1,nchar(x)-1)}else{x = x}}) %>%
  unlist


z = data.frame(name = extracted_names, z)

# Save as an Excel file
writexl::write_xlsx(z, path = "prototype-matching-output.xlsx")
