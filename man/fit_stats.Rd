% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_fit.R
\name{fit_stats}
\alias{fit_stats}
\alias{fitstats}
\title{Test the fit of a model on a train and test set.}
\usage{
fit_stats(model, df_train, df_test)

fitstats(model, df_train, df_test)
}
\arguments{
\item{model}{An \code{\link{lm}} model.}

\item{df_train}{A data frame with the training data.}

\item{df_test}{A data frame with the test data.}
}
\value{
A data frame with the fit statistics.
}
\description{
Test the fit of a model on a train and test set.
}
