#' @title Box plot for One Way ANOVA object
#' @description Creates a boxplot for a oneway object
#' @param x Object of class \code{oneway}
#' @param ... Parameters passed to the boxplot function
#' @return NULL
#' @examples
#' mileage <- oneway(hwy ~ class, cars)
#' plot(mileage)
#' @rdname plot.oneway
#' @export

plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  boxplot(x$anova$terms, x$anova$model, col="skyblue", ...)
}
