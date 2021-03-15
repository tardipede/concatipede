### * Setup

# Create a temporary folder to store the package fasta files
tmp_dir <- tempfile()
dir.create(tmp_dir)
# Cd into this directory
old_dir <- getwd()
setwd(tmp_dir)
# Copy the package fasta files into this directory
example_files = list.files(system.file("extdata", package="concatipede"),
                           full.names = TRUE)
file.copy(from = example_files, to = getwd())

### * Tests

test_that("concatipede_prepare() works", {
    # Check that the function completes without crashing
    expect_error(concatipede_prepare(filename = "test-template"),
                 NA)
    # Check that the expected output file exists
    expect_true(file.exists("test-template.xlsx"))
})

### * Clean-up

# Delete temporary directory
setwd(old_dir)
unlink(tmp_dir, recursive = TRUE)
