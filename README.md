
<!-- README.md is generated from README.Rmd. Please edit that file -->

# klartext

[![R](https://github.com/m-pilarski/klartext/actions/workflows/r.yml/badge.svg)](https://github.com/m-pilarski/klartext/actions/workflows/r.yml)

This package is under active development. Things might change without
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

example_emoji <- "😀😆😡💀"
str_convert_emoji(example_emoji)
#> [1] "<EMO_GRINNING_FACE><EMO_GRINNING_SQUINTING_FACE><EMO_POUTING_FACE><EMO_SKULL>"
str_convert_emoji(example_emoji, .resolution="subgroup")
#> [1] "<EMO_FACE_SMILING><EMO_FACE_SMILING><EMO_FACE_NEGATIVE><EMO_FACE_NEGATIVE>"
str_convert_emoji(example_emoji, .resolution="group")
#> [1] "<EMO_SMILEYS_EMOTION><EMO_SMILEYS_EMOTION><EMO_SMILEYS_EMOTION><EMO_SMILEYS_EMOTION>"


str_to_ascii("Ŧêśť – - — ⅛ … ÆÄöÜ ?¿")
#> [1] "Test - - -  1/8 ... AEAoU ??"


str_unify_spacing(c(
  "This    @test_at that\n #test_hash <test-no-tag>", 
  "<TEST_TAG> test!?!? An URL www.example.com/test ."
))
#> [1] "This @test_at that #test_hash < test - no - tag >"    
#> [2] "<TEST_TAG> test ! ? ! ? An URL www.example.com/test ."

str_blur_numbers(c(
  "The two thousand and twenty United States presidential",
  "election was the 59th quadrennial presidential election",
  "held on Tuesday, November third, 2020. #2020"
))
#> [1] "The <NUM_CARDI> United States presidential"                   
#> [2] "election was the <NUM_ORDI> quadrennial presidential election"
#> [3] "held on Tuesday, November <NUM_ORDI>, <NUM_CARDI>. #2020"

str_describe_numbers(c(
  "The 2020 United States presidential election was the", 
  "59thquadrennial presidential election held on Tuesday,",
  "November 3rd, 2020."
))
#> [1] "The two thousand and twenty United States presidential election was the"
#> [2] "59thquadrennial presidential election held on Tuesday,"                 
#> [3] "November third, two thousand and twenty."
```
