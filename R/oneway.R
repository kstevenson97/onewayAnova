#' @title One Way Analysis of Variance
#' @description \code{oneway} computes a oneway analysis of varaince
#' and includes group level summary statistics.
#' @param formula An object of class formula, relating the
#' dependent varaible to the grouping variable
#' @param data a data frame containing the varaibles in the model.
#' @return A list with 2 elements:
#' \item {oneway}{a list with the lm results}
#' \item {summarystats}{a data frame with the summary statistics}
#' @details
#' This functions computes a standard one-way ANOVA, means, and
#' standard deviations. Missing values are handled via listwise
#' deletion
#' @author Kenneth Stevenson <kstevenson@@wesleyan.edu>
#' @examples
#' mileage <- oneway(hwy ~ class, cars)
#' summary(mileage)
#' print(mileage)
#' plot(mileage)
#' @rdname oneway
#' @export

oneway <- function(formula, data) {

  # listwise deletion of missing values
  data_complete <- na.omit(data)

  # anova
  fit <- lm(formula, data_complete)

  stats <-  aggregate(formula,
                      data,
                      function(x) c(n = length(x), mean = mean(x), sd = sd(x)))


  result <- list(anova = fit, summarystats = stats)
  class(result) <- c("oneway", "list")
  return(result)

}
