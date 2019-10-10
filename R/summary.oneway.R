#' @title Summary of One Way ANOVA
#' @description Prints summary of oneway ANOVA
#' @param x object of class \code{oneway}
#' @param ... Parameters passed to print function
#' @return NULL
#' @examples
#' \dontrun{
#' mileage <- oneway(hwy ~ class, cars)
#' summary(mileage)
#' @rdname summary.oneway
#' @export

summary.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("x must  be class 'oneway'")
  print(anova(x$anova), ...)
}
