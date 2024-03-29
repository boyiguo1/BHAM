% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_bgam.R
\name{tune.bgam}
\alias{tune.bgam}
\alias{cv.bgam}
\title{Cross-Validation for Bayesian Hierarchical Additive Models}
\usage{
tune.bgam(
  object,
  nfolds = 10,
  foldid = NULL,
  ncv = 1,
  s0 = NULL,
  verbose = TRUE
)

cv.bgam(object, nfolds = 10, foldid = NULL, ncv = 1, s0 = NULL, verbose = TRUE)
}
\arguments{
\item{object}{A model fitted using bgam, banlasso or bacoxph to be cross-validated.}

\item{nfolds}{Number of folds for cross-validation (default is 10).}

\item{foldid}{Optional vector specifying fold assignments. If provided, nfolds will be ignored.}

\item{ncv}{Number of cross-validation runs (default is 1).}

\item{s0}{Smoothing parameter (NULL by default).}

\item{verbose}{If TRUE, progress information is displayed (default is TRUE).}
}
\value{
An object containing the cross-validated results.
}
\description{
These functions perform K-fold cross-validation and calculates cross-validated
predictive measures or Bayesian hierarchical additive models and additive
Cox PH models.
}
