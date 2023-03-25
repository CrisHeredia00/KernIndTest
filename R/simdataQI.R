#' Simulated data for Quasi Independence test
#'
#' @param n
#' Number of observations of simulated data
#' @param dependence_factor
#' Factor of dependence between X and Y.
#'
#' @return
#' A dataframe with 3 values: X.ob, T.ob and Delta.ob \cr
#' X.ob is the entry time \cr
#' T.ob is the observed survival time \cr
#' Delta.ob is the status indicator if the observed failure time are subjected to right-censoring. 0 = censored, 1 = event.
#' @export
#'
#' @examples
#' data = simDataQI(100)
simDataQI <- function(n, dependence_factor=0.65) {
  k <- 1
  XX <- YY <- TT <- CC <- delta <- rep(-1, n) #Vectors of length n
  while(k <= n){
    XX[k] <- runif(1, 0, 3.5)
    YY[k] <- 1.95 + dependence_factor * (XX[k] - 1.25)^2 + rnorm(1, sd = 0.1) #Crea los datos en funcion del X
    CC[k] <- runif(1, 0, 10)
    delta[k] <- (YY[k] <= CC[k])
    TT[k] <- pmin(YY[k], CC[k])
    if(XX[k] <= TT[k]) k = k+1 #Checking if X<T
  }
  data.frame(list(X.ob = XX, T.ob = TT, Delta = delta))
  #X.ob Is the entry time
  #T.ob Is the observed survival time
  #Delta.ob Is the status indicator if the observed failure time are subjected to right-censoring; 0 = censored, 1 = event.

  #Los datos no son independientes, es muy poco probable que sean quasi independientes
  #La nula es que son quasi independientes
}
