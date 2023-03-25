#' Quasi Independence Test
#'
#' This test was developed to determine if it exists an association between X and Y in the truncated setting.
#'
#' @include kernelClass.R testClass.R simdataQI.R
#' @param data
#' Data must be a data.frame with: X.ob,T.ob,Delta.
#' @param Nboot
#' WildBoostrap sample size.
#' @param alpha
#' Significance level
#' @param K.fun
#' K matrix kernel function.
#' @param L.fun
#' L matrix kernel function.
#' @param K.par
#' List of parameters of K matrix kernel function.
#' @param L.par
#' List of parameters of L matrix kernel function.
#'
#' @return
#' Returns a Quasi Independence Test object.
#' @export
#'
#' @examples
#' data <-  simDataQI(n=50, dependence_factor=0.65)
#'
#' QuasiIndependenceTest(data,Nboot=500,alpha=0.05,K.fun="gaussianKernel",L.fun="gaussianKernel",K.par=list("sigma"=1,"l"=1),L.par=list("sigma"=1,"l"=1))
QuasiIndependenceTest = function(data,Nboot=500,alpha=0.05,K.fun="gaussianKernel",L.fun="gaussianKernel",K.par=list("sigma"=1,"l"=1),L.par=list("sigma"=1,"l"=1))
{
  # Separating values into vectors
  X.ob = data[[1]]
  T.ob = data[[2]]
  Delta = data[[3]]

  #Checking section
  #Data type
  #Los errores no terminan la ejecuion!
  if (is.numeric(X.ob) != TRUE) stop("X.ob must be a vector containing numbers")
  if (is.numeric(T.ob) != TRUE) stop("T.ob must be a vector containing numbers")
  if (all(Delta %in% c(0,1)) == FALSE) stop("Delta must be a vector contaning only zeros or ones")
  if (is.numeric(Nboot) != TRUE) stop("Nboot must be a number")
  if (is.numeric(alpha) != TRUE) stop ("alpha must be a number")

  try(if (any(X.ob > T.ob)) stop("Left truncation time (X.ob) must be smaller than observed failure time (T.ob)"))

  #try(if (is.function(K.fun) != TRUE) stop ("K.fun must be a function"))
  #try(if (is.function(L.fun) != TRUE) stop ("L.fun must be a function"))

  # Test
  #De momento solo funcionan con la implementacion del paquete
  K.fun = do.call(K.fun,K.par)
  L.fun = do.call(L.fun,L.par)


  n=length(X.ob) #Number of observations
  Kxx=K.fun(X.ob,X.ob)
  Ltt=diag(Delta)%*%L.fun(T.ob,T.ob)%*%diag(Delta)

  ##B matrix
  R=function(x,z)max(sum((X.ob<=x)*(T.ob>=z)),1)
  B1=outer(X.ob,T.ob,FUN="<=")
  B2=t(outer(X.ob,X.ob,FUN="<="))
  B3=t(outer(T.ob,T.ob,FUN="<="))
  R1=outer(X.ob,T.ob,FUN=Vectorize(R))

  B.matrix=(B1*B2*B3)/R1

  #Statistic
  #Notar que no multiplica por PIc
  #T.stat: Our test-statistic
  T.stat=sum(diag(Kxx%*%Ltt-2*Kxx%*%Ltt%*%t(B.matrix)+Ltt%*%t(B.matrix)%*%Kxx%*%B.matrix))/n #Revisar si el orden afecta

  #Wild bootstrap
  #WBoot: Wild Bootstrap sample
  #Me parece que no es optimo
  BSLR=c()
  for(kk in 1:Nboot)
  {
    W=sample(c(1,-1),n,replace=TRUE,prob=c(1/2,1/2))
    WLtt=diag(W)%*%Ltt%*%diag(W)

    Kxx_WLzz = Kxx%*%WLtt
    BSLR=c(BSLR,sum(diag(Kxx_WLzz-2*B.matrix%*%(Kxx_WLzz)+B.matrix%*%Kxx%*%t(B.matrix)%*%WLtt))/n)
  }

  #Decision calculation
  quantile = quantile(BSLR,1-alpha)
  #print(quantile)
  result = isTRUE(T.stat<=quantile) #resultado true no rechaza hipotesis nula
  #print(result)
  p_value = mean(T.stat<BSLR)
  return(new("QuasiIndependenceTest",t.stat = T.stat, w.boot = BSLR, result = result, p.value = p_value, quantile = quantile))


  #return(list(T.stat=T.stat,WBoot=BSLR,result=result, pvalue = p_value))
}
