
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epidemioCOVID

<!-- badges: start -->
<!-- badges: end -->

## Description

The goal of ‘epidemioCOVID’ is to perform alignment between user-input
of COVID strands in FASTA form and the reference sequence of COVID virus
and identify the mutation site of ’the user-input with a graphical
visualization of the mutation site in the sequence. This package can
analyze multiple sequence samples and propose the possible relation
between samples to construct an epidemiological transmission link if
exits.

This package has one analytic functions and one plotting functions, and
has not yet implemented the multiple sequence comparison function. This
package is developed under ‘R version 4.0.2 (2020-06-22)’ and platform
‘Platform: x86_64-w64-mingw32/x64’. (64-bit).

## Installation

You can install the development version of epidemioCOVID from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MingLIUUU/epidemioCOVID", build_vignettes = TRUE)
library("epidemioCOVID")
```

To run the shinyApp: Under construction

## Overview

`epidemioCOVID` contains 2 funtions.

To list all functions available in the package:

``` r
ls("package:epidemioCOVID")
```

``` r
data(package = "epidemioCOVID") # optional
browseVignettes("epidemioCOVID")
```

## Contributions

This package is written by Mingzheng Liu, with coding style taughted in
BCB410 class by Professor Anjali Silva.

## References

- \[Bioinformatics With Ease. (2021). Import Sequences From NCBI in R.\]
  (<https://www.youtube.com/watch?v=vRfbDyhERDY>)

- \[Silva, Anjali. “Anjalisilva/TestingPackage: A Simple R Package
  Illustrating Components of an R Package: 2019-2022 BCB410H - Applied
  Bioinformatics, University of Toronto, Canada.” GitHub.\]
  (<https://github.com/anjalisilva/TestingPackage>)

- \[L Zhou, T Feng, S Xu, F Gao, TT Lam, Q Wang, T Wu, H Huang, L Zhan,
  L Li, Y Guan, Z Dai*, G Yu* ggmsa: a visual exploration tool for
  multiple sequence alignment and associated data. Briefings in
  Bioinformatics. <DOI:10.1093/bib/bbac222>\]

- \[Hahsler M, Nagar A (2019). rBLAST: R Interface for the Basic Local
  Alignment Search Tool. R package version 0.99.2, URL:
  <https://github.com/mhahsler/rBLAST>.\]

## Acknowledgements:

This package was developed as part of an assessment for 2022 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. epidemioCOVID welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the GitHub issues.
