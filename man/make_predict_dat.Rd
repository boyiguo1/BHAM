% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper-functions.R
\name{make_predict_dat}
\alias{make_predict_dat}
\title{Title
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}}
\usage{
make_predict_dat(Smooth, dat)
}
\arguments{
\item{Smooth}{The smooth object from construct_smooth_data}

\item{dat}{The testing data to construct the new design matrix.}
}
\value{
a data frame containing the trasnformed desgin matrix for the testing data
}
\description{
Title
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
raw_dat <- sim_Bai(100, 5)$dat \%>\% data.frame
test_dat <- sim_Bai(100, 5)$dat \%>\% data.frame

sm_df <- data.frame(
 Var = setdiff(names(raw_dat), "y"),
 Func = "s",
 Args ="bs='cr', k=5"
)

dsn_smooth <- construct_smooth_data(sm_df, raw_dat)$Smooth

make_predict_dat(dsn_smooth, test_dat)

}
