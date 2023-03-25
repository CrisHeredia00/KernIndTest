## Test class and functions
## Test class are result from applying a statistical test
## authors : Tamara Fernandez and Cristobal Heredia

#' Test Result class
#'
#' @slot t.stat value of stadistic.
#' @slot w.boot vector of wild bootstrap values.
#' @slot result result of the test.
#' @slot p.value p value of the test.
#' @slot quantile quantile of the test.
#'
#' @return a test result class object
#' @export
setClass(Class = "TestResult",
         slots = c(
           t.stat = "numeric",
           w.boot = "numeric",
           result = "logical",
           p.value = "numeric",
           quantile = "numeric"
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

## Accessor
setMethod(
  f= "[",
  signature="TestResult",
  definition=function(x,i,j,drop){
    if(i=="t.stat"){return(x@t.stat)}else {}
    if(i=="w.boot"){return(x@w.boot)}else {}
    if(i=="result"){return(x@result)}else {}
    if(i=="p.value"){return(x@p.value)}else {}
    if(i=="quantile"){return(x@quantile)}else {}
  }
)
# a = new("TestResult")
# a["t.stat"]

#Show method
#' show method for test result
#'
#' @param object TestResult class.
#'
#' @return None
#' @export
setMethod("show",signature(object="TestResult"),
          function(object) cat(paste("Test Result class")))


#Graph method
setGeneric("graph_test", function(object) {
  standardGeneric("graph_test")
})

#'Graph method for test result objects
#'
#' @param object TestResult object.
#'
#' @return ggplot graph
#' @export
#'
#' @examples
#' data <- simDataQI(50, dependence_factor=0)
#' test_result = QuasiIndependenceTest(data)
#' graph_test(test_result)
setMethod("graph_test",signature(object="TestResult"),
          function(object){
            data = data.frame("data_x" = object@w.boot)
            max_stat = max(hist(unlist(object@w.boot), plot=FALSE, breaks = 30)$counts)
            #print(table(object@w.boot))
            ggplot2::ggplot(data) +
              ggplot2::geom_histogram(ggplot2::aes(x = data_x), color="black", fill="#999999", alpha = 0.5) +
              ggplot2::geom_vline(ggplot2::aes(xintercept = object@t.stat), color = "red", linewidth = 2) +
              ggplot2::geom_vline(ggplot2::aes(xintercept = unname(object@quantile)), color = "purple", linewidth = 2) +
              ggplot2::geom_text(ggplot2::aes(x=object@t.stat, y= max_stat*0.3, label=paste("\nStatistic")), colour="red", angle=90) +
              ggplot2::geom_text(ggplot2::aes(x=unname(object@quantile), y= max_stat*0.3, label=paste("\nQuantile ",names(object@quantile))), colour="purple", angle=90) +
              ggplot2::xlab("Data values") + ggplot2::ylab("Count") +
              ggplot2::theme_bw()
          }
          )

#Quasi Independence
setClass(Class = "QuasiIndependenceTest", contains = "TestResult")
#' Show method for Quasi Independence Test
#'
#' @param object Quasi Independence Test object.
#'
#' @return None
#' @export
#'
#' @examples
#' data <- simDataQI(50, dependence_factor=0)
#' test_result = QuasiIndependenceTest(data)
#' show(test_result)
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



#Two Sample Test
setClass(Class = "TwoSampleTest", contains = "TestResult")
#' Show method for Two Sample Test
#'
#' @param object Two Sample Test object.
#'
#' @return None
#' @export
#'
#' @examples
#' data <- simDataTST(50,50)
#' test_result = KernelTwoSampleTest(data)
#' show(test_result)
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





