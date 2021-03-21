### * Initial setup

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

### * Test concatipede_prepare()

test_that("concatipede_prepare() does not crash and can produce an xlsx file", {
    # Check that the function completes without crashing
    expect_error(concatipede_prepare(filename = "test-template"),
                 NA)
    # Check that the expected output file exists
    expect_true(file.exists("test-template.xlsx"))
})

test_that("concatipede_prepare() can produce a tibble", {
    # Check that the function completes without crashing
    expect_error({z <- concatipede_prepare(tibble = TRUE)},
                 NA)
    # Check that the output is as expected
    expect_s3_class(z, "tbl_df")
    expect_true(all(dim(z) == c(13, 5)))
    expect_true(setequal(colnames(z), c("name", "COI_Macrobiotidae.fas",
                                        "ITS2_Macrobiotidae.fas",
                                        "LSU_Macrobiotidae.fas",
                                        "SSU_Macrobiotidae.fas")))
    expect_true(colnames(z)[1] == "name")
})

### * Test concatipede()

test_that("concatipede() does not crash and can produce an output file", {
    # Check that the function completes without crashing
    expect_error(concatipede(filename = "Macrobiotidae_seqnames.xlsx",
                             out = "my-output"),
                 NA)
    # Check that an output folder was created
    expect_true(dir.exists("my-output"))
    expect_true(file.exists(file.path("my-output", "my-output.fasta")))
    # Check that the fasta output looks ok
    z <- ape::read.FASTA(file.path("my-output", "my-output.fasta"))
    expect_true(length(z) == 13)
    expect_true(length(z[[1]]) == 2945)
})

test_that("concatipede() can accept a tibble as an input", {
    # Input tibble
    df <- structure(list(name = c("Macrobiotus_hannae", "Macrobiotus_wandae", 
                                  "Macrobiotus_caelestis", "Macrobiotus_polonicus", "Macrobiotus_kamilae", 
                                  "Macrobiotus_noongaris", "Sisubiotus_spectabilis", "Mesobiotus_romani", 
                                  "Mesobiotus_harmsworthi", "Mesobiotus_philippinicus", "Mesobiotus_insanis", 
                                  "Paramacrobiotus_areolatus", "Paramacrobiotus_fairbanksi"),
                         COI_Macrobiotidae.fas = c("MH057764_Macrobiotus_hannae_strain_PL010_COI", 
                                                   "MN482684_Macrobiotus_wandae_isolate_54_COI", "MK737922_Macrobiotus_caelestis_strain_KG007_COI", 
                                                   "MN888317_Macrobiotus_polonicus_strain_AT002_COI", "MK737920_Macrobiotus_kamilae_strain_IN030_COI", 
                                                   "MK737919_Macrobiotus_noongaris_strain_AU031_COI", "MN888324_Sisubiotus_spectabilis_strain_NO054_COI", 
                                                   "MH195149_Mesobiotus_romani_strain_EC002_COI", "MH195150_Mesobiotus_harmsworthi_COI", 
                                                   "KX129796_Mesobiotus_philippinicus_COI", "MF441491_Mesobiotus_insanis_COI", 
                                                   "MH675998_Paramacrobiotus_areolatus_strain_NO385_COI", "MH676011_Paramacrobiotus_fairbanksi_strain_PL018_COI"
                                                   ),
                         ITS2_Macrobiotidae.fas = c("MH063923_Macrobiotus_hannae_strain_PL010_ITS2", 
                                                    "MN435120_Macrobiotus_wandae_isolate_54_ITS2", "MK737072_Macrobiotus_caelestis_strain_KG007_ITS2", 
                                                    "MN888338_Macrobiotus_polonicus_strain_AT002_ITS2", "MK737067_Macrobiotus_kamilae_strain_IN030_ITS2", 
                                                    "MK737066_Macrobiotus_noongaris_strain_AU031_ITS2", "MN888344_Sisubiotus_spectabilis_strain_NO054_ITS2", 
                                                    "MH197150_Mesobiotus_romani_strain_EC002_ITS2", "MH197154_Mesobiotus_harmsworthi_ITS2", 
                                                    "KX129795_Mesobiotus_philippinicus_ITS2", "MF441490_Mesobiotus_insanis_ITS2", 
                                                    "MH666080_Paramacrobiotus_areolatus_strain_NO385_ITS2", "MH666090_Paramacrobiotus_fairbanksi_strain_PL018_ITS2"
                                                    ),
                         LSU_Macrobiotidae.fas = c("MH063924_Macrobiotus_hannae_strain_PL010_LSU", 
                                                   "MN435116_Macrobiotus_wandae_isolate_54_LSU", "MK737071_Macrobiotus_caelestis_strain_KG007_LSU", 
                                                   "MN888355_Macrobiotus_polonicus_strain_AT002_LSU", "MK737064_Macrobiotus_kamilae_strain_IN030_LSU", 
                                                   "MK737063_Macrobiotus_noongaris_strain_AU031_LSU", "MN888364_Sisubiotus_spectabilis_strain_NO054_LSU", 
                                                   "MH197151_Mesobiotus_romani_strain_EC002_LSU", "MH197264_Mesobiotus_harmsworthi_LSU", 
                                                   "KX129794_Mesobiotus_philippinicus_LSU", "MF441489_Mesobiotus_insanis_LSU", 
                                                   "MH664948_Paramacrobiotus_areolatus_strain_NO385_LSU", "MH664950_Paramacrobiotus_fairbanksi_strain_PL018_LSU"
                                                   ),
                         SSU_Macrobiotidae.fas = c("MN435112_Macrobiotus_wandae_isolate_54_SSU", 
                                                   "MH063922_Macrobiotus_hannae_strain_PL010_SSU", "MK737073_Macrobiotus_caelestis_strain_KG007_SSU", 
                                                   "MN888369_Macrobiotus_polonicus_strain_AT002_SSU", "MK737070_Macrobiotus_kamilae_strain_IN030_SSU", 
                                                   "MK737069_Macrobiotus_noongaris_strain_AU031_SSU", "MN888372_Sisubiotus_spectabilis_strain_NO054_SSU", 
                                                   "MH197158_Mesobiotus_romani_strain_EC002_SSU", "MH197146_Mesobiotus_harmsworthi_SSU", 
                                                   "KX1297932_Mesobiotus_philippinicus_SSU", "MF4414882_Mesobiotus_insanis_SSU", 
                                                   "MH664931_Paramacrobiotus_areolatus_strain_NO385_SSU", "MH664941_Paramacrobiotus_fairbanksi_strain_PL018_SSU"
                                                   )),
                    row.names = c(NA, -13L), class = c("tbl_df", "tbl", "data.frame"))
    # Check that the function completes without crashing
    expect_error(concatipede(df = df,
                             out = "my-output-df"),
                 NA)
    # Check that an output folder was created
    expect_true(dir.exists("my-output-df"))
    expect_true(file.exists(file.path("my-output-df", "my-output-df.fasta")))
    # Check that the fasta output looks ok
    z <- ape::read.FASTA(file.path("my-output-df", "my-output-df.fasta"))
    expect_true(length(z) == 13)
    expect_true(length(z[[1]]) == 2945)
})

test_that("concatipede() throws errors when the input is problematic", {
    expect_error(concatipede(out = "my-output"),
                 "Either `filename` or `df` must be provided.")
    expect_error(concatipede(filename = "my-file", df = mtcars, out = "my-output"),
                 "Only one of `filename` or `df` must be provided, not both.")
})

### * Test auto_match()

test_that("auto_match() does not crash", {
    xlsx_file <- system.file("extdata", "sequences-test-matching.xlsx",
                             package = "concatipede")
    xlsx_template <- readxl::read_xlsx(xlsx_file)
    expect_error({z <- auto_match(xlsx_template)}, NA)
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

# Delete temporary directory
setwd(old_dir)
unlink(tmp_dir, recursive = TRUE)
