### * Description

# Helper functions for matching.R

# No function in this file is exported.

### * uuid()

# Generate random unique identifiers
#
# @param ... All parameters are ignored (but the function does accept them, so
#     that it can be included in a \code{sapply} call for example).
#
# @return A string with 32 characters.
#
# @examples
# uuid()
# sapply(1:10, uuid)

uuid <- function(...) {
    paste0(sample(c(0:9, letters, toupper(letters)), 32, TRUE), collapse = "")
}

### * reciprocal_matches()

# Find the best reciprocal matches between x and y
#
# @param x,y Character vectors. They do not need to have the same length.
# @param method Method for string distance calculation. See
#     \code{?stringdist::stringdist-metrics}.
#
# @return A tibble with columns `x` and `y` containing the best reciprocal
#     matches. Strings which are not part of a best reciprocal match are not
#     returned.
#
# @examples
# x <- c("apple", "banana", "croissant", "dorado", "elephant", "fennec")
# y <- c("applet", "ananas", "crescent", "elefant", "fan", "tree", "apply")
# reciprocal_matches(x, y)

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
    bm1 <- tibble::tibble(x = x,
                  best_y_for_x = y[bm1])
    bm2 <- tibble::tibble(y = y,
                  best_x_for_y = x[bm2])
    bm <- dplyr::left_join(bm1, bm2, by = c("best_y_for_x" = "y"))
    kept <- which(bm$x == bm$best_x_for_y)
    bm <- bm[kept, ]
    bm <- bm[, c("x", "best_y_for_x")]
    colnames(bm) <- c("x", "y")
    # Return
    return(bm)
}
