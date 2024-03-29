% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simdataQI.R
\name{simDataQI}
\alias{simDataQI}
\title{Simulated data for Quasi Independence test}
\usage{
simDataQI(n, dependence_factor = 0.65)
}
\arguments{
\item{n}{Number of observations of simulated data}

\item{dependence_factor}{Factor of dependence between X and Y.}
}
\value{
A dataframe with 3 values: X.ob, T.ob and Delta.ob \cr
X.ob is the entry time \cr
T.ob is the observed survival time \cr
Delta.ob is the status indicator if the observed failure time are subjected to right-censoring. 0 = censored, 1 = event.
}
\description{
Simulated data for Quasi Independence test
}
\examples{
data = simDataQI(100)
}
