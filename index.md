# BHAM

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/boyiguo1/BHAM/workflows/R-CMD-check/badge.svg)](https://github.com/boyiguo1/BHAM/actions)

The **BHAM** package provides a wicked-fast model for high-dimensional generalized additive model fitting using spike-and-slab priors or reguarliarized priors.



an interface to fit Bayesian generalized
(non-)linear multivariate multilevel models using Stan. The formula syntax is
very similar to that of the package lme4 to provide a familiar and simple
interface for performing regression analyses.

A wide range of distributions and link functions are supported, allowing users
to fit -- among others -- linear, robust linear, count data, survival, response
times, ordinal, zero-inflated, hurdle, and even self-defined mixture models all
in a multilevel context. Further modeling options include non-linear and smooth
terms, auto-correlation structures, censored data, meta-analytic standard
errors, and quite a few more. In addition, all parameters of the response
distribution can be predicted in order to perform distributional regression.
Prior specifications are flexible and explicitly encourage users to apply prior
distributions that actually reflect their beliefs. Model fit can easily be
assessed and compared with posterior predictive checks and leave-one-out
cross-validation.

## Getting Started

If you are new to **brms** we recommend starting with the [vignettes](https://paul-buerkner.github.io/brms/articles/) and these
other resources:

* [Introduction to brms](https://www.jstatsoft.org/article/view/v080i01)
(Journal of Statistical Software)
* [Advanced multilevel modeling with brms](https://journal.r-project.org/archive/2018/RJ-2018-017/index.html)
(The R Journal)
* [Blog posts](https://paul-buerkner.github.io/blog/brms-blogposts/)
(List of blog posts about brms)
* [Ask a question](http://discourse.mc-stan.org/) (Stan Forums on Discourse)


## Installation


# Install the latest release from **CRAN**
# 
# ```r
# install.packages("brms")
# ```

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
