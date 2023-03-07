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
             stop ("p-value must be greater or equal to zero")
           if(object@p.value > 1)
             stop ("p-value must be lower or equal to one")
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
            cat("*** Quasi Independence Test ***\n")
            cat("* alternative hypothesis: Not Quasi-independent *\n")
            cat("* null hypothesis: Quasi-independent *\n")
            cat(paste("p-value:", object@p.value,"\n"))
            cat(paste("t.stat: ", object@t.stat, "\n"))
            if (isTRUE(object@result) && object@result == TRUE){ #&& por como funciona R
              cat("result: it rejects the null hypothesis\n")#p valor bajo
            } else {
              cat("result: there is no enough information to reject the null hypothesis\n") #p valor alto
            }
          }
)
# new("QuasiIndependenceTest")


#Two Sample Test
setClass(Class = "TwoSampleTest", contains = "TestResult")
setMethod("show",signature(object="TwoSampleTest"),
          function(object) {
            cat("*** Two Sample Test ***\n")
            cat("* alternative hypothesis: The groups don't have the same distribution *\n")
            cat("* null hypothesis: The groups have the same distribution *\n")
            cat("p-value:", object@p.value,"\n")
            cat(paste("t.stat: ", object@t.stat, "\n"))
            if (isTRUE(object@result) && object@result == TRUE){ #&& por como funciona R
              cat("result: it rejects the null hypothesis\n")#p valor bajo
            } else {
              cat("result: there is no enough information to reject the null hypothesis\n") #p valor alto
            }
          }
)
# new("TwoSampleTest")



