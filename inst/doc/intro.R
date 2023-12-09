## ----eval=FALSE---------------------------------------------------------------
#  T_n <- function(r,n1,YY,m){
#    t_n = 0
#    n = floor(r*n1)
#    for (t in 1:n){
#      t_n = t_n + sum(YY[t+m,1:t])
#    }
#    return(t_n)
#  }
#  
#  Wang2020 <- function(sam,m,n){
#    n1 = n-m
#    YY = sam%*%t(sam)
#    W <- NULL
#    for (k in 1:n1){
#      W[k] = (T_n(k/n1,n1,YY,m)-k*(k+1)/n1/(n1+1)*T_n(1,n1,YY,m))^2
#    }
#    W_n = mean(W)
#    test = T_n(1,n1,YY,m)^2/W_n
#    reject = test > 54.70
#    return(reject)
#  }

## -----------------------------------------------------------------------------
library(SA23204177)
library(MASS)
set.seed(1234)
n1 <- n2 <- 50
p <- 200
mu1 <- rep(0, p)
mu2 <- mu1
mu2[1:10] <- 1
true.cov <- 0.4^(abs(outer(1:p, 1:p, "-"))) # AR1 covariance
sam1 <- mvrnorm(n = n1, mu = mu1, Sigma = true.cov)
sam2 <- mvrnorm(n = n2, mu = mu2, Sigma = true.cov)
Wang2020(sam1, m=5, n=n1)
Wang2020(sam2, m=5, n=n2)
Wang2020C(sam1, m=5, n=n1)
Wang2020C(sam2, m=5, n=n2)

## ----eval=TRUE----------------------------------------------------------------
library(microbenchmark)

tm2 <- microbenchmark(
  Wang2020 = Wang2020(sam1, m=5, n=n1),
  Wang2020C = Wang2020C(sam1, m=5, n=n1)
)
knitr::kable(summary(tm2)[,c(1,3,5,6)])

