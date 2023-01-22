## Test class and functions
## Test class are result from applying a statistical test
## authors : Tamara Fernandez and Cristobal Heredia

## Test Result class (Father)
setClass(Class = "TestResult",
         slots = c(
           t.stat = "numeric",
           w.boot = "numeric",
           result = "logical",
           p.value = "numeric"
         ),
         validity=function(object){
           cat("~~~ Test: inspector ~~~ \n")
           if(object@p.value < 0)
             stop ("p-value must be greater than cero")
           if(object@p.value > 1)
             stop ("p-value must be lower than one")
           return(TRUE)
         }
)

#Accessor
setMethod(
  f= "[",
  signature="TestResult",
  definition=function(x,i,j,drop){
    if(i=="t.stat"){return(x@t.stat)}else {}
    if(i=="w.boot"){return(x@w.boot)}else {}
    if(i=="result"){return(x@result)}else {}
    if(i=="p.value"){return(x@p.value)}else {}
  }
)
# a = new("TestResult")
# a["t.stat"]


setMethod("show",signature(object="TestResult"),
          function(object) cat(paste("Test Result class")))

#Quasi Independence
setClass(Class = "QuasiIndependenceTest", contains = "TestResult")
setMethod("show",signature(object="QuasiIndependenceTest"),
          function(object) {
            cat("*** Quasi independence test ***\n")
            cat("* alternative hypothesis: Not Quasi-independent *\n")
            cat("* null hypothesis: Quasi-independent *\n")
            cat(paste("p-value:", object@p.value,"\n"))
            if (isTRUE(object@result) && object@result == FALSE){
              cat("result: it rejects the null hypothesis\n")#p valor bajo
            } else {
              cat("result: there is no enough information to reject the null hypothesis\n") #p valor alto
            }
            cat(paste("t.stat: ", object@t.stat, "\n"))
          }
)
# new("QuasiIndependenceTest")
