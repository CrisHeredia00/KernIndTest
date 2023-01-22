QuasiIndependenceTest = function(Nboot=500,X.ob,T.ob,Delta,alpha=0.05,K.fun="gaussianKernel",L.fun="gaussianKernel", kpar = list("sigma" = 1, "l" = 1))
{
  #Checking section
  #Data type
  try(if (is.numeric(Nboot) != TRUE) stop("Nboot must be a number"))
  try(if (is.numeric(X.ob) != TRUE) stop("X.ob must be a vector containing numbers"))
  try(if (is.numeric(T.ob) != TRUE) stop("T.ob must be a vector containing numbers"))
  try(if (all(Delta %in% c(0,1)) == FALSE) stop("Delta must be a vector contaning only zeros or ones"))
  try(if (is.numeric(alpha) != TRUE) stop ("alpha must be a number"))

  try(if (any(X.ob > T.ob)) stop("Left truncation time (X.ob) must be smaller than observed failure time (T.ob)"))


  #De momento las dos funciones usan el mismo kpar
  K.fun = do.call(K.fun,kpar)
  L.fun = do.call(L.fun,kpar)


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
  result = T.stat<=quantile
  p_value = mean(T.stat<BSLR)
  return(new("QuasiIndependenceTest",t.stat = T.stat, w.boot = BSLR, result = result, p.value = p_value))


  #return(list(T.stat=T.stat,WBoot=BSLR,result=result, pvalue = p_value))
}
# data <- simDataQI(50)
# X.ob=data[["X.ob"]]
# T.ob=data[["T.ob"]]
# Delta=data[["Delta"]]

QuasiIndependenceTest(500, X.ob, T.ob, Delta)
# a["w.boot"]
