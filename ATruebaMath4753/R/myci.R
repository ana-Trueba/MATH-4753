#' my ci
#'
#' Finds the 95% confidence interval of mu for a single sample.
#'
#' @param x a vector containing data from a sample.
#'
#' @return returns a vector with two numbers, the lower and upper limits of the confidence interval.
#' @export
#'
#' @examples myci(x)
#'
myci = function(x){
  mp=c(-1,1)
  l = length(x)
  ci = mean(x)+mp*qt(0.95/2,l)*sd(x)/sqrt(l)
  ci
}
