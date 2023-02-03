#' A Reproducing-Kernel-Hilbert-Space log-rank test for the two-sample problem

# data must be a data.frame with: X, Delta, Group (with order)
KernelTwoSampleTest= function(data,Nboot=500,alpha=0.05,K.fun="gaussianKernel",K.par = list("sigma"=1,"l"=1))
{
  # Separating values into vectors
  data <- data[order(data[,1]), ]
  X = data[[1]]
  Delta = data[[2]]
  Group = data[[3]]

  #Checking data
  if (is.numeric(X) != TRUE) stop("X.ob must be a vector containing numbers")
  if (all(Delta %in% c(0,1)) == FALSE) stop("Delta must be a vector contaning only zeros or ones")
  if (all(Group %in% c(0,1)) == FALSE) stop("Group must be a vector contaning only zeros or ones")
  if (is.numeric(Nboot) != TRUE) stop("Nboot must be a number")
  #try(if (is.function(K.fun) != TRUE) stop ("K.fun must be a function"))
  #try(if (is.numeric(alpha) != TRUE) stop ("alpha must be a number"))


  # Test
  #De momento solo funciona con la implementacion del paquete
  K.fun = do.call(K.fun,K.par)

  n <- length(Delta)
  n0 <- sum(Group == 0)

  #We compute the processes Y_0, Y_1, and Y
  n.Y.1 <- c(n-n0, (n-n0) - cumsum(Group))[1:n]
  n.Y <- (n+1)-1:n
  n.Y.0 <- n.Y - n.Y.1
  #We compute the process L
  n.L = n.Y.0*n.Y.1/n.Y
  #we compute the measure dN_0/Y_0-dN_1/Y_1
  n.dmu = (-1)^Group*Delta/pmax(-1,n.Y.1*Group +n.Y.0*(1-Group), na.rm = TRUE)
  #Finally, we compute the vector $V$ used in equation (9)
  n.fm = n.L*n.dmu
  #Kaplan-meier
  KMEst = (1 - c(1, cumprod(1- Delta/((n+1)-1:n))))[1:n]

  #Since in our statistic censored data is not considered, we do the following to erase them from the vectors
  #it saves some time in the Wildbootstrap
  nonZero = which(n.fm != 0)
  timesKer = KMEst[nonZero]
  fmKer = n.fm[nonZero]

  #Compute the kernel evaluated at  \hat F(T_i). Matrix $\hat K$  is equation (9)
  Ker = outer(timesKer,timesKer,Vectorize(K.fun))


  #With these ingredients we can run the algorithm at the end of Section 5
  #step i) set Nboot, done
  #step ii) Wildbootstrap,
  out.Boot=rep(0,Nboot)
  W = rep(fmKer, Nboot)
  #sample the weights of all Nboots bootstrap samples
  W = matrix(W*sample(c(-1,1),size=length(fmKer)*Nboot,replace=TRUE,prob=c(1/2,1/2)), nrow = length(fmKer))
  #W = matrix(W*(rpois(length(fmKer)*Nboot,1)-1), nrow = length(fmKer))

  #use equation (22), whoever we can do all of them at the same time
  out.Boot = colSums(W * (Ker %*% W))
  #step 3
  quantile = quantile(out.Boot,1-alpha)
  #step 4, we compute the test-statistic by using 9 (we compute them all at the same time)
  stat = (fmKer)%*%Ker%*%(fmKer)
  stat=as.numeric(stat)
  #step 5
  result = (stat<=quantile)
  #extra step
  p_value = mean(stat<out.Boot)

  #We return false if the test-statistic is greater than the quantile (i.e. reject the null)
  return(new("TwoSampleTest",t.stat = stat, w.boot = out.Boot, result = result, p.value = p_value))


  #return(list(T.stat=stat,WBoot=out.Boot,result=result, pvalue = p_value))

}
# data <- simDataTST(n1=100, n2=100, rate = list("t1"=3,"c1"=3,"t2"=3,"c2"=3))
# KernelTwoSampleTest(data)
