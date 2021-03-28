### * Initial setup

# Create a temporary folder to store the package fasta files
tmp_dir <- tempfile()
dir.create(tmp_dir)
# Cd into this directory
old_dir <- getwd()
setwd(tmp_dir)
# Copy the package fasta files into this directory
example_files <- list.files(system.file("extdata", package="concatipede"),
                           full.names = TRUE)
file.copy(from = example_files, to = getwd())

### * Test

test_that("find_fasta() works as expected", {
    setwd(old_dir)
    # Find fasta files copied from inst/extdata/
    dir <- tmp_dir
    z <- find_fasta(dir)
    expect_true(length(z) == 4)
    # Exclude some files
    z <- find_fasta(dir, exclude = "SU")
    expect_true(length(z) == 2)
    # Error when giving more than one dir
    expect_error(find_fasta(c(dir, dir)),
                 "The `dir` argument must contain exactly one path\\.")
    # Use current dir when no dir argument is given
    setwd(tmp_dir)
    z <- find_fasta()
    expect_true(length(z) == 4)
    z <- find_fasta(exclude = "SU")
    expect_true(length(z) == 2)
    setwd(old_dir)
})

### * Clean-up

# Delete temporary directory
setwd(old_dir)
unlink(tmp_dir, recursive = TRUE)
