#' @title Print a oneway object
#' @description Prints the results of a oneway ANOVA
#' @param x an object of class \code{oneway}
#' @param ... parameters passed to function
#' @return NULL
#' @examples
#' mileage <- oneway(hwy ~ class, cars)
#' print(mileage)
#' @rdname print.oneway
#' @export

print.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  cat("\nSummary Statistics\n",
      "====================================================\n", sep="")
  print(x$summarystats, ...)
  cat("\nAnova\n",
      "====================================================\n", sep="")
  print(summary.lm(x$anova))
}
