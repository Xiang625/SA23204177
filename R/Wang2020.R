#' @importFrom knitr kable
#' @importFrom xtable xtable
#' @importFrom MASS mvrnorm
#' @import RcppEigen
#' @import Rcpp
#' @import ggplot2
#' @import dplyr
#' @import bootstrap
#' @import boot
#' @import DAAG 
#' @import twosamples
#' @import coda
#' @import microbenchmark
#' @import rmarkdown
#' @useDynLib SA23204177

NULL

#' @title Testing of mean vector using R
#' @description Testing of mean vector using R
#' @param sam the matrix of samples
#' @param m trimming parameter
#' @param n sample size
#' @return \code{TRUE} (reject null hypothesis) or \code{FALSE}
#' @examples
#' \dontrun{
#' library(MASS)
#' set.seed(1234)
#' n1 <- n2 <- 50
#' p <- 200
#' mu1 <- rep(0, p)
#' mu2 <- mu1
#' mu2[1:10] <- 1
#' true.cov <- 0.4^(abs(outer(1:p, 1:p, "-"))) # AR1 covariance
#' sam1 <- mvrnorm(n = n1, mu = mu1, Sigma = true.cov)
#' sam2 <- mvrnorm(n = n2, mu = mu2, Sigma = true.cov)
#' Wang2020(sam1, m=5, n=n1)
#' Wang2020(sam2, m=5, n=n2)
#' }
#' @export
Wang2020 <- function(sam,m,n){
  n1 = n-m
  YY = sam%*%t(sam)
  W <- NULL
  for (k in 1:n1){
    W[k] = (T_n(k/n1,n1,YY,m)-k*(k+1)/n1/(n1+1)*T_n(1,n1,YY,m))^2
  }
  W_n = mean(W)
  test = T_n(1,n1,YY,m)^2/W_n
  reject = test > 54.70
  return(reject)
}