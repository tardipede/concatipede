### * Description

# This script can be used to check the package on various platforms using the rhub package.

# See: https://cran.r-project.org/web/packages/rhub/vignettes/rhub.html

### * Setup

library(here)
library(crayon)
library(rhub)

# The first time rhub is used on your machine, you should validate your email
# with:
#   validate_email()

### * Check

# List platforms
platforms()

# Check
cran_prep <- check(platform = c("fedora-clang-devel",
                                "windows-x86_64-devel",
                                "macos-highsierra-release",
                                "ubuntu-gcc-release"),
                   email = "matthieu.bruneaux@gmail.com")
completed <- FALSE

while (!completed) {
    tryCatch({
        notes <- cran_prep$cran_summary()
        completed <- TRUE },
      error = function(e) {
          message("Results not ready yet, waiting 60 seconds before next try.")
          Sys.sleep(60)
          e
      })
}

fo <- here::here(paste0("rhub-check_", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".md"))
fo <- file(fo, open = "w")
cat(strip_style(capture.output(notes$print())), file = fo, sep = "\n")
cat("\n")
close(fo)
