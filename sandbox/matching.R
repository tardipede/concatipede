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

### * Suggestion of an algorithm

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
