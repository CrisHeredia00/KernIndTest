% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kernelClass.R
\name{show,Kernel-method}
\alias{show,Kernel-method}
\title{Show method for kernel functions}
\usage{
\S4method{show}{Kernel}(object)
}
\arguments{
\item{object}{Kernel.}
}
\value{
None
}
\description{
Show method for kernel functions
}
\examples{
locally_periodic_kernel = locallyPeriodicKernel(sigma=1, l=1, p = 1)
show(locally_periodic_kernel)
}
