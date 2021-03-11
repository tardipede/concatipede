[![GitLab pipeline status](https://gitlab.com/tardipede/concatipede/badges/master/pipeline.svg)](https://gitlab.com/tardipede/concatipede/commits/master)
[![R_CMD_CHECK](https://tardipede.gitlab.io/concatipede/r-cmd-check-badge.svg)](https://tardipede.gitlab.io/concatipede/R_CMD_CHECK.txt)
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)

Concatipede: an R package to concatenate fasta sequences easily  <img src="man/figures/concatipede_hexagon.png" width="120" align="right" />
==============================================================

This package allows to concatenate sequences of multiple MSAs based on a translation table that can be edited in excel.

This is convenient in case of concatenation of no more than few hundred terminals and a few tens on markers. Once created the correspondence table it makes very easy and fast to include or exclude taxa and markers from the concatenated alignment, as well as update it with new sequences.  

## How to install concatipede

For now you can install the package directly from Gitlab with:

```
# Run inside an R session
install.packages("devtools")
devtools::install_gitlab("tardipede/concatipede")
```

Please let us know if you encounter any issues during the package installation!

## How to use concatipede

You can get started with this [example vignette](https://tardipede.gitlab.io/concatipede/articles/010-package-usage.html).

## Contact
matteo.vecchi15@gmail.com
