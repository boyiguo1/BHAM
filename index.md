# BHAM

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/boyiguo1/BHAM/workflows/R-CMD-check/badge.svg)](https://github.com/boyiguo1/BHAM/actions)

The **BHAM** package provides a scalable solution for fitting high-dimensional generalized additive model using spike-and-slab lasso priors or other regularized priors, including continuous spike-and-slab priors, Student' T priors and double exponential priors. It fits linear, logistic, poisson and Cox regression models. The specification of the additive functions follows a popular syntax implemented in [`mgcv`](https://cran.r-project.org/web/packages/mgcv/index.html). An arsenal of facilitating functions are provided, including cross-validation, model summary, and visualization.


## Getting Started

If you are new to **BHAM** we recommend starting with the [vignettes](https://boyiguo1.github.io/BHAM/articles/)


## Installation

Install the latest development version from **GitHub**

```r
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("boyiguo1/BHAM", build_vignettes = FALSE)
```

You can also set `build_vignettes=TRUE` but this will slow down the installation
drastically (the vignettes can always be accessed online anytime at
[boyiguo1.github.io/BHAM/articles](https://boyiguo1.github.io/BHAM/articles)).

We are currently streamlining the syntax of the package for better user experience. When we have a stable version, we will submit the package to CRAN. Please stay tuned!
