### * Initial setup

# Create a temporary folder to store the package fasta files and cd there
prev_dir <- getwd()
tmp_dir <- tempfile()
dir.create(tmp_dir)
setwd(tmp_dir)
# Copy the package fasta files into this directory
example_files <- list.files(system.file("extdata", package="concatipede"),
                           full.names = TRUE)
file.copy(from = example_files, to = getwd())

### * Test

test_that("Functions work well together (short pipeline)", {
    stopifnot(!file.exists("my-template.xlsx"))
    # Short pipeline
    z <- find_fasta() %>%
        concatipede_prepare() %>%
        write_xl("my-template.xlsx")
    expect_true(file.exists("my-template.xlsx"))
    unlink("my-template.xlsx")
    # Check that the output is as expected
    expect_s3_class(z, "tbl_df")
    expect_true(all(dim(z) == c(13, 5)))
    expect_true(setequal(colnames(z), c("name", "COI_Macrobiotidae.fas",
                                        "ITS2_Macrobiotidae.fas",
                                        "LSU_Macrobiotidae.fas",
                                        "SSU_Macrobiotidae.fas")))
    expect_true(colnames(z)[1] == "name")
    # Check that the output has an attribute with the directory name
    expect_true(normalizePath(attr(z, "dir_name")) == normalizePath(tmp_dir))
})

test_that("Functions work well together (medium pipeline)", {
    stopifnot(!file.exists("my-template.xlsx"))
    stopifnot(!file.exists("matched-template.xlsx"))
    # Short pipeline
    z <- find_fasta() %>%
        concatipede_prepare() %>%
        write_xl("my-template.xlsx") %>%
        auto_match_seqs() %>%
        write_xl("matched-template.xlsx")
    expect_true(file.exists("my-template.xlsx"))
    expect_true(file.exists("matched-template.xlsx"))
    unlink("my-template.xlsx")
    unlink("matched-template.xlsx")
    # Check that the output is as expected
    expect_s3_class(z, "tbl_df")
    expect_true(all(dim(z) == c(13, 5)))
    expect_true(setequal(colnames(z), c("name", "COI_Macrobiotidae.fas",
                                        "ITS2_Macrobiotidae.fas",
                                        "LSU_Macrobiotidae.fas",
                                        "SSU_Macrobiotidae.fas")))
    expect_true(colnames(z)[1] == "name")
    # Check that the output has an attribute with the directory name
    expect_true(normalizePath(attr(z, "dir_name")) == normalizePath(tmp_dir))
})

test_that("Functions work well together (medium pipeline 2)", {
    stopifnot(!file.exists("my-template.xlsx"))
    stopifnot(!file.exists("matched-template.xlsx"))
    # Short pipeline
    expect_message({z <- find_fasta() %>%
                        concatipede_prepare() %>%
                        write_xl("my-template.xlsx") %>%
                        auto_match_seqs() %>%
                        write_xl("matched-template.xlsx") %>%
                        concatipede() },
                   "Loading the fasta files from the directory stored in the \"dir_name\" attribute of `df`")
    expect_true(file.exists("my-template.xlsx"))
    expect_true(file.exists("matched-template.xlsx"))
    unlink("my-template.xlsx")
    unlink("matched-template.xlsx")
    # Check that the output is as expected
    expect_s3_class(z, "DNAbin")
})

test_that("Functions work well together (long pipeline)", {
    stopifnot(!file.exists("my-template.xlsx"))
    stopifnot(!file.exists("matched-template.xlsx"))
    stopifnot(!file.exists("toto.fasta"))
    stopifnot(!file.exists("toto.nexus"))
    stopifnot(!file.exists("toto.phy"))
    # Short pipeline
    expect_message({z <- find_fasta() %>%
                        concatipede_prepare() %>%
                        write_xl("my-template.xlsx") %>%
                        auto_match_seqs() %>%
                        write_xl("matched-template.xlsx") %>%
                        concatipede() %>%
                        write_fasta("toto")},
                   "Loading the fasta files from the directory stored in the \"dir_name\" attribute of `df`")
    expect_true(file.exists("my-template.xlsx"))
    expect_true(file.exists("matched-template.xlsx"))
    expect_true(file.exists("toto.fasta"))
    unlink("my-template.xlsx")
    unlink("matched-template.xlsx")
    unlink("toto.fasta")
    # Check that the output is as expected
    expect_s3_class(z, "DNAbin")
    # Test the other alignment writers
    write_nexus(z, "toto")
    expect_true(file.exists("toto.nexus"))
    unlink("toto.nexus")
    write_phylip(z, "toto")
    expect_true(file.exists("toto.phy"))
    unlink("toto.phy")
})

### * Clean-up

# Go back to previous directory and delete temporary directory
setwd(prev_dir)
unlink(tmp_dir, recursive = TRUE)
