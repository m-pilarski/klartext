
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
  "The two thousand and twenty United States presidential",
  "election was the 59th quadrennial presidential election",
  "held on Tuesday, November third, 2020. #2020"
))
#> [1] "The <NUM_CARDI> United States presidential"                   
#> [2] "election was the <NUM_ORDI> quadrennial presidential election"
#> [3] "held on Tuesday, November <NUM_ORDI>, <NUM_CARDI>. #2020"


example_emoji <- "😀😆😡💀"
str_convert_emoji(example_emoji)
#> [1] "<GRINNING_FACE><GRINNING_SQUINTING_FACE><POUTING_FACE><SKULL>"
str_convert_emoji(example_emoji, .col_description=subgroup)
#> [1] "<FACE_SMILING><FACE_SMILING><FACE_NEGATIVE><FACE_NEGATIVE>"
str_convert_emoji(example_emoji, .col_description=group)
#> [1] "<SMILEYS_EMOTION><SMILEYS_EMOTION><SMILEYS_EMOTION><SMILEYS_EMOTION>"


str_unify_spacing(c(
  "This    @test_at that\n #test_hash", 
  "<test-no-tag> <TEST_TAG> test!?!?",
  "An URL www.example.com/test ."
))
#> [1] "This @test_at that #test_hash"              
#> [2] "< test - no - tag > <TEST_TAG> test ! ? ! ?"
#> [3] "An URL www.example.com/test ."
```
