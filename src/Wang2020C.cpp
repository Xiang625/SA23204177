#include <RcppEigen.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppEigen)]]
//[[Rcpp::export]]
double T_nC(double r, int n1, const Eigen::MatrixXd& YY, int m){
  double t_n = 0;
  int n = r*n1;
  for (int i=1; i<=n; i++){
    for (int j=1; j<=i; j++){
      t_n = t_n + YY(i+m-1,j-1);
    }
  }
  return t_n;
}

//' @title Testing of mean vector using Rcpp and RcppEigen
//' @description Testing of mean vector using Rcpp and RcppEigen
//' @param sam the matrix of samples
//' @param m trimming parameter
//' @param n sample size
//' @return \code{TRUE} (reject null hypothesis) or \code{FALSE}
//' @examples
//' \dontrun{
//' library(MASS)
//' set.seed(1234)
//' n1 <- n2 <- 50
//' p <- 200
//' mu1 <- rep(0, p)
//' mu2 <- mu1
//' mu2[1:10] <- 1
//' true.cov <- 0.4^(abs(outer(1:p, 1:p, "-"))) # AR1 covariance
//' sam1 <- mvrnorm(n = n1, mu = mu1, Sigma = true.cov)
//' sam2 <- mvrnorm(n = n2, mu = mu2, Sigma = true.cov)
//' Wang2020C(sam1, m=5, n=n1)
//' Wang2020C(sam2, m=5, n=n2)
//' }
//' @export
//[[Rcpp::export]]
bool Wang2020C(const Eigen::MatrixXd& sam, int m, int n){
  double n1 = n - m;
  Eigen::MatrixXd YY = sam * sam.adjoint();
  double W = 0;
  for (int k=1; k <= n1; k++){
    W += pow(T_nC(k/n1,n1,YY,m)-k*(k+1)/n1/(n1+1)*T_nC(1,n1,YY,m),2);
  }
  double W_n = W / n1;
  double test = pow(T_nC(1,n1,YY,m),2)/W_n;
  bool reject = test > 54.70;
  return reject;
}
