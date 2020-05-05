#' Creates plot and runs linear regression
#'
#' This function creates a scatterplot if the number of subjects is less than 5 and not
#' greater than 5. If there are 5 or less variables, a linear regression will be performed
#' on the list of subjects specified, or sub.The linear regression will output a list of the
#' coefficients and the p-values.
#'
#' @param x A matrix of covariates.
#' @param y A vector of outcomes
#' @param sub A list of subjects
#' @return scatterplot of \code{x} and linear regression \code{y} an \code{x}.
#' @export
#' @examples
#' aGreatFunction(x1, y, sub)


myLinearRegression <- function(x, y, sub){

  if(ncol(x) < 5){
    GGally::ggpairs(x[sub], title = "Scatterplot of Covariate Pairs")

  } else if(ncol(x) > 5){
    print("Too many variables to plot")
  }

  modfit <- stats::lm(y ~ x[sub])
  modfitsum <- summary(modfit)
  coef <- coefficients(modfitsum)
  pval <- summary(modfitsum)$coefficients[,c(1,4)]

  return(list(coef,pval))


}


