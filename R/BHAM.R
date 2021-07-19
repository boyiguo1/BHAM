#' BHAM: A package to fit Bayesian Hierarchical Additive Models for High Dimensional Application
#'
#'// TODO: Update this page
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name BHAM
#'
#' @importFrom glmnet glmnet
#'
#' @importFrom MASS theta.ml
#'
#' @importFrom tibble enframe
#'
#' @importFrom stats glm.control sd optimize is.empty.model coef deviance fitted terms
#' @importFrom stats .getXlevels make.link makepredictcall nobs
#' @importFrom stats model.matrix model.offset model.extract model.response model.weights
#' @importFrom  rlang .data
#' @importFrom survival coxph.control aeqSurv untangle.specials strata Surv attrassign coxph
#' @importFrom utils methods
#'
NULL
#> NULL





## quiets concerns of R CMD check re: the .'s that appear in pipelines
## See jennybc's example at https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
