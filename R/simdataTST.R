#' #' Simulated Data Function for Two Sample Test
#'
#' @param n1
#' Number of observations of group 1
#' @param n2
#' Number of observations of group 2
#' @param rate
#' A list with four values: rates for t1, c1, t2, c2
#'
#' @return
#' A dataframe with 3 values: X, Delta and Group
#' @export
#'
#' @examples
#' data = simDataTST(100,100)

# Tengo dudas con los valores de rate
# Conversar si decidir rate como lista es comodo
simDataTST = function(n1=100, n2=100, rate = list("t1"=3,"c1"=1,"t2"=0.5,"c2"=0.1)){
  #First group
  T1=rexp(n1, rate = rate$t1)
  C1=rexp(n1, rate = rate$c1)
  X1=pmin(T1,C1)
  Delta1=1*(T1==X1)
  Data1=list(X1=X1,Delta1=Delta1)

  #Second group (alternative)
  T2=rexp(n2, rate = rate$t2)
  C2=rexp(n2, rate = rate$c2)
  X2=pmin(T2,C2)
  Delta2=1*(T2==X2)
  Data2=list(X2=X2,Delta2=Delta2)

  #Full data
  return(data.frame(X = c(X1,X2), Delta = c(Delta1,Delta2), Group = c(rep(1,n1),rep(0,n2))))
  #X: observed time
  #Delta: indicator of whether we actually observe the survival time of interest
  #Group: group membership

  #Los datos se crean bajo la hipotesis alternativa (los datos se generan bajo distintas distribuciones)
  #Para probar lo contrario, cambio los rate a los mismos valores
  #Para seguir probando mantenemos los rate cercano y podemos ver que pasa
}
