
<!-- README.md is generated from README.Rmd. Please edit that file -->

# klartext

[![R](https://github.com/m-pilarski/klartext/actions/workflows/r.yml/badge.svg)](https://github.com/m-pilarski/klartext/actions/workflows/r.yml)

This package is under active development and things might change without
backwards compatibility.

## Overview

klartext is a collection of tools to clean and normalize text.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("m-pilarski/klartext")
```

## Usage

``` r
library(klartext)


str_unify_char_latin("Ŧêśť ÄöÜ")
#> [1] "Test AoU"


str_blur_numbers(c(
  "The 2020 United States presidential election",
  "was the 59th quadrennial presidential election",
  "held on Tuesday, November 3, 2020. #2020"
))
#> [1] "The <NUM_CARDI> United States presidential election"      
#> [2] "was the <NUM_ORDI> quadrennial presidential election"     
#> [3] "held on Tuesday, November <NUM_CARDI>, <NUM_CARDI>. #2020"

example_emoji <- "😀😆😡💀"

str_convert_emoji(example_emoji)
#> [1] "<GRINNING_FACE><GRINNING_SQUINTING_FACE><POUTING_FACE><SKULL>"

str_convert_emoji(example_emoji, .col_description=subgroup)
#> [1] "<FACE_SMILING><FACE_SMILING><FACE_NEGATIVE><FACE_NEGATIVE>"

str_convert_emoji(example_emoji, .col_description=group)
#> [1] "<SMILEYS_EMOTION><SMILEYS_EMOTION><SMILEYS_EMOTION><SMILEYS_EMOTION>"
```
