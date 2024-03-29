% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kernelClass.R
\name{rationalQuadraticKernel}
\alias{rationalQuadraticKernel}
\title{Rational quadratic kernel function}
\usage{
rationalQuadraticKernel(sigma = 1, l = 1, alpha = -1, ...)
}
\arguments{
\item{sigma}{value of sigma of rational quadratic kernel}

\item{l}{value of lengthscale of rational quadratic kernel}

\item{alpha}{value of alpha of rational quadratic kernel}
}
\value{
a Gram Matrix object from class GramMatrix
}
\description{
It is a function to calculate the Gram matrix from rational quadratic kernel
}
\examples{
rational_quadratic_kernel = rationalQuadraticKernel(sigma=1, l=1, alpha = -1)
z = 1:10
gram_matrix = rational_quadratic_kernel(z,z)
}
