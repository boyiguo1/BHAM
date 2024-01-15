## usethis namespace: start
#' BHAM: A package to fit Bayesian Hierarchical Additive Models for High Dimensional Application
#'
#'BHAM is a freely avaible R pakcage that implments Bayesian
#'hierarchical additive models for high-dimensional clinical and genomic data.
#'The package includes functions that generlized additive model, and Cox additive
#'model with the spike-and-slab LASSO prior. These functions implements scalable
#'and stable algorithms to estimate parameters. BHAM also provides utility
#'functions to construct additive models in high dimensional settings,
#'select optimal models, summarize bi-level variable selection results,
#'and visualize nonlinear effects. The package can facilitate flexible
#'modeling of large-scale molecular data, i.e. detecting succeptable
#'variables and inforing disease diagnostic and prognostic.
#'
#' @docType package
#' @name BHAM
#'
#' @importFrom glmnet glmnet
#' @importFrom MASS theta.ml
#' @importFrom tibble enframe
#' @importFrom stats glm.control sd optimize is.empty.model coef deviance fitted update predict.glm
#' @importFrom stats .getXlevels make.link makepredictcall nobs
#' @importFrom stats model.matrix model.offset model.frame model.extract model.response model.weights
#' @importFrom stats dbinom dnbinom dnorm dpois
#' @importFrom stats var predict reshape
#' @importFrom pROC auc
#' @importFrom rlang .data
#' @importFrom survival coxph.control aeqSurv untangle.specials strata Surv attrassign coxph
#' @importFrom utils methods
#'
#' @useDynLib BHAM, .registration = TRUE
#'
## usethis namespace: end
NULL
NULL
#> NULL





## quiets concerns of R CMD check re: the .'s that appear in pipelines
## See jennybc's example at https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
