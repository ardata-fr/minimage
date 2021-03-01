
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minimage

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ardata-fr/minimage/workflows/R-CMD-check/badge.svg)](https://github.com/ardata-fr/minimage/actions)
<!-- badges: end -->

The goal of minimage is to let users minify size images.

The main function allows to compress the images contained in a directory
to another directory, optionally changing some compression parameters.

## Installation

You can install minimage from
[GitHub](https://github.com/ardata-fr/minimage/) with:

``` r
# install.packages("devtools")
devtools::install_github("ardata-fr/minimage")
```

## Example

``` r
library(minimage)
compress_images("test-files/", "test-new-files/", verbose = FALSE)
```

<div class="kable-table">

| input                  | size\_in | path\_out                  | size\_out | percent | algorithm | copied |
| :--------------------- | -------: | :------------------------- | --------: | ------: | :-------- | :----- |
| test-files/file\_1.png |    10386 | test-new-files/file\_1.png |      1714 |   83.50 | pngquant  | TRUE   |
| test-files/file\_2.png |    10409 | test-new-files/file\_2.png |      1825 |   82.47 | pngquant  | TRUE   |

</div>

``` r
compress_images("test-files/", "test-new-files/", png_quality = "90-100", 
                verbose = FALSE, overwrite = TRUE)
```

<div class="kable-table">

| input                  | size\_in | path\_out                  | size\_out | percent | algorithm | copied |
| :--------------------- | -------: | :------------------------- | --------: | ------: | :-------- | :----- |
| test-files/file\_1.png |    10386 | test-new-files/file\_1.png |      2299 |   77.86 | pngquant  | TRUE   |
| test-files/file\_2.png |    10409 | test-new-files/file\_2.png |      2288 |   78.02 | pngquant  | TRUE   |

</div>
