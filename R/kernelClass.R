## Kernel functions and Gram Matrix
## Kernel functions receives a list of hyper parameters and they calculate the Gram Matrix
## authors : Tamara Fernandez and Cristobal Heredia

## Kernel function class


#' Kernel class.
#'
#' It is a class that defines a kernel calculation using a function and its parameters.
#'
#' @slot  function. It receives one function that works as kernel function.
#' @slot kpar It is a list with the parameters of the kernel function
#'
#' @return a kernel class
# #'
# #'
# #' @examples
# #'
# #' @export
# #'
setClass(Class = "Kernel",
         representation("function",kpar="list"),
         validity=function(object){
           cat("~~~ Kernel: inspector ~~~ \n")
           if(length(formals(object)) != 2)
             stop ("Kernel function must have two parameters")
           return(TRUE)
         }
)

#' Kpar accessor
#'
#' @param object it receives a kernel class
#'
#' @return returns the list of kernel function parameters
#' @export
#'
#' @examples
#' gaussian_kernel = gaussianKernel(sigma=1, l=1)
#' get_kpar(gaussian_kernel)
setGeneric("get_kpar",function(object){standardGeneric ("get_kpar")})
setMethod("get_kpar","Kernel",function(object) return(object@kpar)) # Returns kpar list


## Gram Matrix class
setClass("GramMatrix",representation("matrix"),prototype=structure(.Data=matrix())) # It is a matrix


## Kernel functions
## Receives hyper-parameters and returns a Kernel class
## Kernel class calculates and returns Gram Matrix

#' Gaussian Kernel function
#'
#' It is a function to calculate the Gram matrix from gaussian kernel.
#'
#' @param sigma value of sigma of gaussian kernel
#' @param l value of lengthscale of gaussian kernel
#'
#' @return a Gram Matrix object from class GramMatrix
#' @export
#'
#' @examples
#' gaussian_kernel = gaussianKernel(sigma=1, l=1)
#' z = 1:10
#' gram_matrix = gaussian_kernel(z,z)
gaussianKernel <- function(sigma=1, l=1, ...)
  {
  rval <- function(x,y)
  {
    #Checking Data:
    if (is.vector(x) == TRUE & is.vector(y) == TRUE){
      if (length(x) != length(y)) stop("Vectors must be the same size") #No se si es cierto
    } else {
      stop("Kernel function receives vectors") #Añadir que pueda calcular matrices
    }

    if (is.numeric(x) != TRUE) stop("Kernel function must have receive a vector with numbers")
    if (is.numeric(y) != TRUE) stop("Kernel function must have receive a vector with numbers")

    # Defined for vectors
    # CALCULO DEL KERNEL
    K.fun=function(xi,xj) exp(-(xi-xj)^2/(2*l^2)) * sigma^2
    KMatrix = outer(x,y,FUN=K.fun)
    return(new("GramMatrix",KMatrix))
  }
  return(new("GaussianKernel",.Data=rval,kpar=list(sigma=sigma, l=l)))
}
setClass("GaussianKernel",prototype=structure(.Data=function(){},kpar=list()),contains=c("Kernel"))


#' Rational quadratic kernel function
#'
#' It is a function to calculate the Gram matrix from rational quadratic kernel
#' @param sigma value of sigma of rational quadratic kernel
#' @param l value of lengthscale of rational quadratic kernel
#' @param alpha value of alpha of rational quadratic kernel
#'
#' @return a Gram Matrix object from class GramMatrix
#' @export
#'
#' @examples
#' rational_quadratic_kernel = rationalQuadraticKernel(sigma=1, l=1, alpha = -1)
#' z = 1:10
#' gram_matrix = rational_quadratic_kernel(z,z)
rationalQuadraticKernel <- function(sigma=1, l=1, alpha = -1, ...)
{
  rval <- function(x,y)
  {
    #Checking Data:
    if (is.vector(x) == TRUE & is.vector(y) == TRUE){
      if (length(x) != length(y)) stop("Vectors must be the same size") #No se si es cierto
    } else {
      stop("Kernel function receives vectors") #Añadir que pueda calcular matrices
    }

    if (is.numeric(x) != TRUE) stop("Kernel function must have receive a vector with numbers")
    if (is.numeric(y) != TRUE) stop("Kernel function must have receive a vector with numbers")

    # Defined for vectors
    # CALCULO DEL KERNEL
    K.fun=function(xi,xj) (sigma^2)*((1 + ((xi-xj)^2)/(2*sigma*(l^2)))^-alpha)
    KMatrix = outer(x,y,FUN=K.fun)
    return(new("GramMatrix",KMatrix))
  }
  return(new("RationalQuadraticKernel",.Data=rval,kpar=list(sigma=sigma, l=l, alpha = alpha)))
}
setClass("RationalQuadraticKernel",prototype=structure(.Data=function(){},kpar=list()),contains=c("Kernel"))


#' Periodic Kernel function
#'
#' It is a function to calculate the Gram matrix from periodic kernel
#' @param sigma value of sigma of periodic kernel
#' @param l value of lengthscale of periodic kernel
#' @param p value of period of periodic kernel
#'
#' @return a Gram Matrix object from class GramMatrix
#' @export
#'
#' @examples
#' periodic_kernel = periodicKernel(sigma=1, l=1, p = 1)
#' z = 1:10
#' gram_matrix = periodic_kernel(z,z)
periodicKernel <- function(sigma=1, l=1, p=1,...)
{
  rval <- function(x,y)
  {
    #Checking Data:
    if (is.vector(x) == TRUE & is.vector(y) == TRUE){
      if (length(x) != length(y)) stop("Vectors must be the same size") #No se si es cierto
    } else {
      stop("Kernel function receives vectors") #Añadir que pueda calcular matrices
    }

    if (is.numeric(x) != TRUE) stop("Kernel function must have receive a vector with numbers")
    if (is.numeric(y) != TRUE) stop("Kernel function must have receive a vector with numbers")

    # Defined for vectors
    # CALCULO DEL KERNEL
    K.fun=function(xi,xj) (sigma^2)*exp(-(2*(sin(pi*abs(xi-xj)/p))^2)/l^2)
    KMatrix = outer(x,y,FUN=K.fun)
    return(new("GramMatrix",KMatrix))
  }
  return(new("PeriodicKernel",.Data=rval,kpar=list(sigma=sigma, l=l, p = p)))
}
setClass("PeriodicKernel",prototype=structure(.Data=function(){},kpar=list()),contains=c("Kernel"))


#' Locally Periodic Kernel function
#'
#' It is a function to calculate the Gram matrix from periodic kernel
#' @param sigma value of sigma of periodic kernel
#' @param l value of lengthscale of periodic kernel
#' @param p value of period of periodic kernel
#'
#' @return a Gram Matrix object from class GramMatrix
#' @export
#'
#' @examples
#' locally_periodic_kernel = locallyPeriodicKernel(sigma=1, l=1, p = 1)
#' z = 1:10
#' gram_matrix = locally_periodic_kernel(z,z)
locallyPeriodicKernel <- function(sigma=1, l=1, p=1,...)
{
  rval <- function(x,y)
  {
    #Checking Data:
    if (is.vector(x) == TRUE & is.vector(y) == TRUE){
      if (length(x) != length(y)) stop("Vectors must be the same size") #No se si es cierto
    } else {
      stop("Kernel function receives vectors") #Añadir que pueda calcular matrices
    }

    if (is.numeric(x) != TRUE) stop("Kernel function must have receive a vector with numbers")
    if (is.numeric(y) != TRUE) stop("Kernel function must have receive a vector with numbers")

    # Defined for vectors
    # CALCULO DEL KERNEL
    K.fun=function(xi,xj) (sigma^2)*exp(-(2*(sin(pi*abs(xi-xj)/p))^2)/l^2)*exp(-abs(xi-xj)^2/(2*l^2))
    KMatrix = outer(x,y,FUN=K.fun)
    return(new("GramMatrix",KMatrix))
  }
  return(new("LocallyPeriodicKernel",.Data=rval,kpar=list(sigma=sigma, l=l, p = p)))
}
setClass("LocallyPeriodicKernel",prototype=structure(.Data=function(){},kpar=list()),contains=c("Kernel"))


#' Show method for kernel functions
#'
#' @param object Kernel.
#'
#' @return None
#' @export
#'
#' @examples
#' locally_periodic_kernel = locallyPeriodicKernel(sigma=1, l=1, p = 1)
#' show(locally_periodic_kernel)
setMethod("show",signature(object="Kernel"),
          function(object)
          {
            switch(class(object),
                   "GaussianKernel" = cat(paste("Gaussian Kernel function.", "\n","Hyperparameters :\n", "\t sigma = ", get_kpar(object)$sigma, "\n","\t lengthscale = ", get_kpar(object)$l)),
                   "RationalQuadraticKernel" = cat(paste("Rational Quadratic Kernel function.", "\n","Hyperparameters :\n", "\t sigma = ", get_kpar(object)$sigma, "\n","\t lengthscale = ", get_kpar(object)$l,"\n", "\t alpha = ", get_kpar(object)$alpha)),
                   "PeriodicKernel" = cat(paste("Periodic Kernel function.", "\n","Hyperparameters :\n", "\t sigma = ", get_kpar(object)$sigma, "\n","\t lengthscale = ", get_kpar(object)$l, "\t period = ", get_kpar(object)$period)),
                   "LocallyPeriodicKernel" = cat(paste("Locally Periodic Kernel function.", "\n","Hyperparameters :\n", "\t sigma = ", get_kpar(object)$sigma, "\n","\t lengthscale = ", get_kpar(object)$l, "\t period = ", get_kpar(object)$period)),
            )
          }
        )

#Example of use
#a = gaussianKernel(sigma = 2, l = 1)


# new("gaussianKernel")
# b = rationalQuadraticKernel()
# show(a)
# show(b)
#z1 = 1:3
#z2 = 4:6
#a(z1,z2)
# b(z1,z2)

#asi es como internamente usa kernel kernlab (ignorar)
# if(!is(kernel,"kernel"))
# {
#   if(is(kernel,"function")) kernel <- deparse(substitute(kernel))
#   kernel <- do.call(kernel, kpar)
# }
# if(!is(kernel,"kernel")) stop("kernel must inherit from class `kernel'")
