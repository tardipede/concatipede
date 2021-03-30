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

### * Test concatipede_prepare()

test_that("concatipede_prepare() does not crash and can produce an xlsx file", {
    # Check that the function completes without crashing
    expect_error(concatipede_prepare(out = "test-template"),
                 NA)
    # Check that the expected output file exists
    expect_true(file.exists("test-template.xlsx"))
})

test_that("concatipede_prepare() can produce a tibble", {
    # Check that the function completes without crashing
    expect_error({z <- concatipede_prepare()},
                 NA)
    # Check that the output is as expected
    expect_s3_class(z, "tbl_df")
    expect_true(all(dim(z) == c(13, 5)))
    expect_true(setequal(colnames(z), c("name", "COI_Macrobiotidae.fas",
                                        "ITS2_Macrobiotidae.fas",
                                        "LSU_Macrobiotidae.fas",
                                        "SSU_Macrobiotidae.fas")))
    expect_true(colnames(z)[1] == "name")
    # Check that the output has an attribute with the directory name
    expect_true(attr(z, "dir_name") == tmp_dir)
})

### * Clean-up

# Delete temporary directory
setwd(old_dir)
unlink(tmp_dir, recursive = TRUE)
