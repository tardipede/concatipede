### * Initial setup

# Create a temporary folder to store the package fasta files
tmp_dir <- tempfile()
dir.create(tmp_dir)
# Save old directory and cd into the tmp directory
old_dir <- getwd()
setwd(tmp_dir)
# Copy the package fasta files into this directory
example_files = list.files(system.file("extdata", package="concatipede"),
                           full.names = TRUE)
file.copy(from = example_files, to = getwd())

### * Test auto_match_seqs()

test_that("auto_match_seqs() does not crash", {
    xlsx_file <- system.file("extdata", "sequences-test-matching.xlsx",
                             package = "concatipede")
    xlsx_template <- readxl::read_xlsx(xlsx_file)
    expect_error({z <- auto_match_seqs(xlsx_template)}, NA)
    # Check that columns are in the same order
    expect_true(all(colnames(xlsx_template) == colnames(z)))
    # Check that no sequence name is lost
    cols <- colnames(xlsx_template)
    for (x in cols[cols != "name"]) {
        input <- na.omit(unique(xlsx_template[[x]]))
        output <- na.omit(unique(z[[x]]))
        expect_true(setequal(input, output))
    }
})

### * Clean-up

# Go back to old directory and delete temporary directory
setwd(old_dir)
unlink(tmp_dir, recursive = TRUE)
