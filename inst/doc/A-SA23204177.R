## -----------------------------------------------------------------------------
# 直接输出数据
test <- head(iris)
test

## -----------------------------------------------------------------------------
# 展示为表格
knitr::kable(test)

## -----------------------------------------------------------------------------
# 得到Latex形式的表格
xtable::xtable(test)

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()

## -----------------------------------------------------------------------------
# 将鸢尾花的类型映射给点的颜色和形状
fig_all <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, colour=Species, shape=Species)) + geom_point()
fig_all

## -----------------------------------------------------------------------------
setosa <- iris[iris$Species == "setosa",]

## ----message=FALSE------------------------------------------------------------
fig_setosa <- ggplot(setosa, aes(x=Sepal.Length, y=Sepal.Width))
fig_setosa + geom_point() + stat_smooth(method = lm)

## ----message=FALSE------------------------------------------------------------
# 同时绘制3种类型数据的拟合线
fig_all + geom_smooth(method = lm, fullrange = TRUE, se = FALSE)

## -----------------------------------------------------------------------------
mySample <- function(x,n){
  m <- length(x)
  p <- rep(1,m)/m
  cp <- cumsum(p)
  
  u <- runif(n)
  r <- x[findInterval(u,cp)+1]
  return(r)
} 

x <- mySample(1:6,2000) #test
table(x)

## -----------------------------------------------------------------------------
n <- 1000
u <- runif(n)
x <- log(2*u)
x[u>=0.5] <- -log(2*(1-u[u>=0.5]))
hist(x, prob = TRUE, ylim = c(0,0.5)) #density histogram of sample
y <- seq(-8, 8, .01)
lines(y, 0.5*exp(-abs(y))) #density curve f(x)

## -----------------------------------------------------------------------------
general.acc.rej <- function(size, xs, df, dgen, rgen, c){
  acc.rej <- function(size, df, rgen, dgen, c){
    ct = 1
    n = 1
    res = numeric(size)
    while(n <= size){
      y = rgen(1)
      u = runif(1)
      if (u < df(y)/dgen(y)/c){
        res[n] = y
        n = n + 1
      }
      ct = ct + 1
    }
    return(res)
  }
  
  
  sam = acc.rej(size = size, df = df, rgen = rgen, dgen = dgen, c)
  return(sam)
}

myBeta <- function(size, a, b){
  rgen <- function(size) runif(size, min = u.min, max = u.max)
  dgen <- function(x) dunif(x, min = u.min, max = u.max)
  df <- function (x) dbeta(x = x, shape1 = a, shape2 = b)
  
  xs = seq(0, 1, 0.01)
  u.max = max(xs)
  u.min = min(xs)
  y.max = max(df(xs))
  c = y.max/(u.max - u.min)
  
  sam <- general.acc.rej (size = size, xs = xs, df = df, dgen = dgen, rgen = rgen, c = c)
  hist(sam, probability = TRUE)
  lines(xs, df(xs))
}

a = 3
b = 2
size = 1000

myBeta(size = size, a = a, b = b)

## -----------------------------------------------------------------------------
re <- function(n){
  u1 <- runif(n, min = -1)
  u2 <- runif(n, min = -1)
  u3 <- runif(n, min = -1)
  x <- u3
  i <- (abs(u3)>=abs(u2)) & (abs(u3)>=abs(u1))
  x[i] <- u2[i]
  return(x)
}

x <- re(5000)
hist(x, prob = TRUE) #density histogram of sample
y <- seq(-1, 1, .01)
lines(y, 0.75*(1-y^2))
lines(density(x,bw = 0.1), col = 'red')

## -----------------------------------------------------------------------------
K = 100
n = 1e6
rho_test = c(0.3,0.6,1)
set.seed(111)
Est <- function(n, rho) {
  theta = runif(n, 0, pi/2)
  x = runif(n, 0, 1/2)
  # cross is a vector of true/false. Take the mean to find a proportion
  cross = x <= rho/2 * sin(theta)
  return(mean(cross))
}

for (rho in rho_test){
  E = c()
  for (k in 1:K){
    E[k] = 2*rho/Est(n, rho)
  }
  print(paste("rho =",rho,"Var =",var(E)))
}

## -----------------------------------------------------------------------------
set.seed(111)
n = 1e5
e = exp(1)
a = -(3*e-e^2-1)/((e^2-1)/2-(e-1)^2)

U <- runif(n)
T1 <- exp(U) #simple MC
T2 <- exp(U) + a * (e^(1-U) - (e-1)) #controlled
c(mean(T1), mean(T2), exp(1)-1) 

c(sd(T1)/sqrt(n), sd(T2)/sqrt(n))

## -----------------------------------------------------------------------------
g <- function(x){
  1/sqrt(2*pi)*x^2*exp(-x^2/2)
}

f1g <- function(x) g(x)/(1/pi/(1+x^2))
f2g <- function(x) g(x)/exp(1-x)

curve(f1g, from = 1, to = 10)
curve(f2g, from = 1, to = 10,add=T,col='red')

## -----------------------------------------------------------------------------
x <- rexp(1000,1) + 1
theta.hat <- mean(f2g(x))
theta.hat

## -----------------------------------------------------------------------------
# M <- 1000
# k <- 10
# N <- 50
# 
# T2 <- numeric(k)
# est <- matrix(0, N, 2)
# 
# for (i in 1:N) {
#   x <- rexp(1000,1) + 1
#   est[i, 1] <- mean(f2g(x))
#   for(j in 1:k) T2[j]<-mean(g(runif(M/k,(j-1)/k,j/k)))
#   est[i, 2] <- mean(T2)
# }
# round(apply(est,2,mean),4)

## -----------------------------------------------------------------------------
m = 1e5
n = 20
set.seed(111)

reject <- NULL
for(i in 1:m){
  x <- rchisq(n,2)
  mu.hat = mean(x)
  sigma.hat = var(x)
  reject[i] <- (mu.hat<=2-sigma.hat*qt(0.9755,m-1)) || (mu.hat>=2+sigma.hat*qt(0.9755,m-1))
} 
print(mean(reject))

## -----------------------------------------------------------------------------
m = 1e5
n = 10
set.seed(111)

# chi(1)
reject <- NULL
for(i in 1:m){
  x <- rchisq(n,1)
  mu.hat = mean(x)
  sigma.hat = var(x)
  reject[i] <- (mu.hat<=1-sigma.hat*qt(0.9755,m-1)) || (mu.hat>=1+sigma.hat*qt(0.9755,m-1))
} 
print(mean(reject))

# U(0,2)
for(i in 1:m){
  x <- runif(n,0,2)
  mu.hat = mean(x)
  sigma.hat = var(x)
  reject[i] <- (mu.hat<=1-sigma.hat*qt(0.9755,m-1)) || (mu.hat>=1+sigma.hat*qt(0.9755,m-1))
} 
print(mean(reject))

# Exp(1)
for(i in 1:m){
  x <- rexp(n)
  mu.hat = mean(x)
  sigma.hat = var(x)
  reject[i] <- (mu.hat<=1-sigma.hat*qt(0.9755,m-1)) || (mu.hat>=1+sigma.hat*qt(0.9755,m-1))
} 
print(mean(reject))

## -----------------------------------------------------------------------------
library(dplyr)
M = 1000
m = 1000
m1 = 950
m2 = 50
alpha = 0.1
set.seed(11)

FWER.Bon <- numeric(M)
FDR.Bon <- numeric(M)
TPR.Bon <- numeric(M)
FWER.BH <- numeric(M)
FDR.BH <- numeric(M)
TPR.BH <- numeric(M)
for (i in 1:M){
  x1 <- runif(m1)
  x2 <- rbeta(m2,0.1,1)
  x <- c(x1,x2)
  
  # Bonferroni校正
  p.adj1 = p.adjust(x, method = "bonferroni")
  # BH
  p.adj2 = p.adjust(x, method = "BH")
  
  FWER.Bon[i] = sum(p.adj1[1:m1]<alpha)>=1
  FDR.Bon[i] = sum(p.adj1[1:m1]<alpha)/sum(p.adj1<alpha)
  TPR.Bon[i] = sum(p.adj1[(m1+1):m]<alpha)/m2
  FWER.BH[i] = sum(p.adj2[1:m1]<alpha)>=1
  FDR.BH[i] = sum(p.adj2[1:m1]<alpha)/sum(p.adj2<alpha)
  TPR.BH[i] = sum(p.adj2[(m1+1):m]<alpha)/m2
}

result <- tribble(
  ~Method,~FWER, ~FDR, ~TPR,
  "Bonferroni", mean(FWER.Bon), mean(FDR.Bon), mean(TPR.Bon),
  "BH", mean(FWER.BH), mean(FDR.BH), mean(TPR.BH)
)
knitr::kable(result)

## -----------------------------------------------------------------------------
set.seed(11)
N = c(5,10,20)
B = 1000
m = 1000

for (n in N){
  x = rexp(n,2)
  lambda.boot = numeric(B)
  lambda.se = numeric(B)
  lambda.bias = numeric(B)
  for (j in 1:m){
    for (i in 1:B){
      xx = sample(x, n, replace = TRUE)
      lambda.boot[i] = 1/mean(xx)
    }
    lambda.se[j] = sd(lambda.boot)
    lambda.bias[j] = mean(lambda.boot) - 2
  }
  cat(c("n =",n,", bootstrap bias =",round(mean(lambda.bias), 3),", bootstrap se =",round(mean(lambda.se), 3),"\n"))
}



## -----------------------------------------------------------------------------
# theoretical
for (n in N){
  bias = round(2/(n-1), 3)
  se = round(2*n/(n-1)/sqrt(n-2), 3)
  cat(c("n =",n,", bias =",bias,", se =",se,"\n"))
}

## -----------------------------------------------------------------------------
library(bootstrap)
library(boot)

set.seed(12)
B <- 1000
n <- nrow(law)
R <- numeric(B)
t.star <- numeric(B)
m = 1000
cor.hat <- cor(law$LSAT,law$GPA)

for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  LSAT <- law$LSAT[i] #i is a vector of indices
  GPA <- law$GPA[i]
  R[b] <- cor(LSAT, GPA)
  R.b.se = numeric(m)
  for (j in 1:m){
    ii = sample(1:n, size = n, replace = TRUE)
    #LSAT.b = LSAT[ii]
    #GPA.b = GPA[ii]
    R.b.se[j] = cor(law$LSAT[ii], law$GPA[ii])
  }
  t.star[b] = (R[b] - cor.hat)/sd(R.b.se)
}

# alpha=0.05
t1 = quantile(t.star,0.025)
t2 = quantile(t.star,0.975)
cat("bootstrap t CI :","\n","[",cor.hat-t2*sd(t.star),",",cor.hat-t1*sd(t.star),"]","\n")

## -----------------------------------------------------------------------------
set.seed(11)
N = c(5,10,20)
B = 1000
m = 1000

for (n in N){
  x = rexp(n,2)
  lambda.boot = numeric(B)
  lambda.se = numeric(B)
  lambda.bias = numeric(B)
  for (j in 1:m){
    for (i in 1:B){
      xx = sample(x, n, replace = TRUE)
      lambda.boot[i] = 1/mean(xx)
    }
    lambda.se[j] = sd(lambda.boot)
    lambda.bias[j] = mean(lambda.boot) - 2
  }
  cat(c("n =",n,", bootstrap bias =",round(mean(lambda.bias), 3),", bootstrap se =",round(mean(lambda.se), 3),"\n"))
}



## -----------------------------------------------------------------------------
# theoretical
for (n in N){
  bias = round(2/(n-1), 3)
  se = round(2*n/(n-1)/sqrt(n-2), 3)
  cat(c("n =",n,", bias =",bias,", se =",se,"\n"))
}

## -----------------------------------------------------------------------------
library(bootstrap)
library(boot)

set.seed(12)
B <- 1000
n <- nrow(law)
R <- numeric(B)
t.star <- numeric(B)
m = 1000
cor.hat <- cor(law$LSAT,law$GPA)

for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  LSAT <- law$LSAT[i] #i is a vector of indices
  GPA <- law$GPA[i]
  R[b] <- cor(LSAT, GPA)
  R.b.se = numeric(m)
  for (j in 1:m){
    ii = sample(1:n, size = n, replace = TRUE)
    #LSAT.b = LSAT[ii]
    #GPA.b = GPA[ii]
    R.b.se[j] = cor(law$LSAT[ii], law$GPA[ii])
  }
  t.star[b] = (R[b] - cor.hat)/sd(R.b.se)
}

# alpha=0.05
t1 = quantile(t.star,0.025)
t2 = quantile(t.star,0.975)
cat("bootstrap t CI :","\n","[",cor.hat-t2*sd(t.star),",",cor.hat-t1*sd(t.star),"]","\n")

## -----------------------------------------------------------------------------
library(boot)
library(bootstrap)
x = c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
set.seed(11)
b.mean <- function(x,i) mean(x[i])
de <- boot(data=x, statistic=b.mean, R = 999)
boot.ci(de,type=c("norm","basic","perc","bca"))

## -----------------------------------------------------------------------------
n <- 88
Sigma.hat = cor(scor)
lambda.hat = eigen(Sigma.hat)$values
theta.hat = lambda.hat[1]/sum(lambda.hat)
theta.jack <- numeric(n)
for(i in 1:n){
  Sigma.jack = cor(scor[(1:n)[-i],])
  lambda.jack = eigen(Sigma.jack)$values
  theta.jack[i] = lambda.jack[1]/sum(lambda.jack)
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))

round(c(original=theta.hat,bias.jack=bias.jack,
se.jack=se.jack),3)

## ----include=FALSE------------------------------------------------------------
# models in Ex7.18
library(DAAG); attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits
L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)
L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)
L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)
L4 <- lm(log(magnetic) ~ log(chemical))
plot(log(chemical), log(magnetic), main="Log-Log", pch=16)
logyhat4 <- L4$coef[1] + L4$coef[2] * log(a)
lines(log(a), logyhat4, lwd=2)

## -----------------------------------------------------------------------------
n <- length(magnetic) #in DAAG ironslag

# for n-fold cross validation
# fit models on leave-two-out samples
index <- t(combn(n,2))
m <- nrow(index)
e1 <- e2 <- e3 <- e4 <- numeric(m)

vec.norm <- function(x) sqrt(sum(x^2))

for (k in 1:m) {
  y <- magnetic[-index[k,]]
  x <- chemical[-index[k,]]
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[index[k,]]
  e1[k] <- vec.norm(magnetic[index[k,]] - yhat1)
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[index[k,]] +
  J2$coef[3] * chemical[index[k,]]^2
  e2[k] <- vec.norm(magnetic[index[k,]] - yhat2)
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[index[k,]]
  yhat3 <- exp(logyhat3)
  e3[k] <- vec.norm(magnetic[index[k,]] - yhat3)
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[index[k,]])
  yhat4 <- exp(logyhat4)
  e4[k] <- vec.norm(magnetic[index[k,]] - yhat4)
}

c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))


## -----------------------------------------------------------------------------
cvm.test <- function(x,y){
  n = length(x)
  m = length(y)
  F_n <- ecdf(x)
  G_m <- ecdf(y)
  
  s1 = sum((F_n(x)-G_m(x))^2)
  s2 = sum((F_n(y)-G_m(y))^2)
  W = m*n/(m+n)^2*(s1+s2)
  return(W)
}

## -----------------------------------------------------------------------------
library(twosamples)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

W = cvm.test(x,y)
print(W)
print(cvm_test(x,y))

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}
# assume length(x)<=length(y)
count5test.ue <- function(x, y, R=999) {
  n1 <- length(x)
  n2 <- length(y)
  reps <- numeric(R)
  for (i in 1:R){
    k <- sample(1:n2, size = n1, replace = TRUE)
    y1 <- y[k]
    reps[i] = count5test(x,y1)
  }
  p <- mean(reps) > 0.5
}



## -----------------------------------------------------------------------------
# example 6.15
n1 <- 20
n2 <- 30
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 1000
alphahat <- mean(replicate(m, expr={
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x - mean(x) #centered by sample mean
  y <- y - mean(y)
  count5test.ue(x, y)
}))
print(alphahat)

## -----------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 1.5
power <- mean(replicate(m, expr={
  x <- rnorm(20, 0, sigma1)
  y <- rnorm(20, 0, sigma2)
  count5test.ue(x, y)
}))
print(power)

## -----------------------------------------------------------------------------
f <- function(N,b1,b2,b3,f0){
  x1 = rpois(N,1)
  x2 = rexp(N)
  x3 = rbinom(N,1,0.5)
  obs = b1*x1 + b2*x2 + b3*x3
  g <- function(x) mean(1/(1+exp(x+obs))) + f0 - 1
  
  a = uniroot(g, c(-20,20))$root
  #print(g(20))
  #print(g(-20))
  return(a)
}

## -----------------------------------------------------------------------------
N = 1e6
b1 = 0
b2 = 1
b3 = -1
f0 = c(0.1, 0.01, 0.001, 0.0001) 
set.seed(11)
a = numeric(4)
for (i in 1:4) {
  a[i] = f(N,b1,b2,b3,f0[i])
}
print(a)

## -----------------------------------------------------------------------------
plot(-log(f0),a)

## -----------------------------------------------------------------------------
laplace <- function(x) 0.5*exp(-abs(x))

chain <- function(sigma, n, x1){
  x = numeric(n)
  x[1] = x1
  u = runif(n)
  k = 0
  for (i in 2:n){
    xt = x[i-1]
    y = rnorm(1, xt, sigma)
    if (u[i] <= laplace(y)/laplace(xt)) x[i] = y
    else{
      x[i] = xt
      k = k+1
    }
  }
  return(list(x=x,k=k))
}

## -----------------------------------------------------------------------------
# simulation
sigmas = c(0.05,0.5,2.5,16)
n = 2000
set.seed(111)

theta0 <- c

rw1 = chain(sigmas[1],n,50)
rw2 = chain(sigmas[2],n,50)
rw3 = chain(sigmas[3],n,50)
rw4 = chain(sigmas[4],n,50)
kk = c(rw1$k,rw2$k,rw3$k,rw4$k)
rw = rbind(rw1$x,rw2$x,rw3$x,rw4$x)
print(kk/n)
for (i in 1:4){
  plot(rw[i,], type = "l", xlab=bquote(sigma == .(round(sigmas[i],3))), ylab="X", xlim=c(1,n), ylim=range(rw[i,]))
}

## -----------------------------------------------------------------------------
set.seed(111)
n = 5000
burn = 1000
da = matrix(0,n,2)
rho = 0.9
mu1 = mu2 = 0
sigma1 = sigma2 =1
s1 = sqrt(1-rho^2)*sigma1
s2 = sqrt(1-rho^2)*sigma2

da[1,] = c(mu1,mu2)
for (i in 2:n){
  y = da[i-1,2]
  m1 = mu1 + rho*(y-mu2)*sigma1/sigma2
  da[i,1] = rnorm(1,m1,s1)
  x = da[i,1]
  m2 = mu2 + rho*(x-mu1)*sigma2/sigma1
  da[i,2] = rnorm(1,m2,s2)
}

plot(da[(burn+1):n,1],da[(burn+1):n,2],xlab = bquote(X_t),ylab = bquote(Y_t),cex=0.5)

## -----------------------------------------------------------------------------
da.burn <- data.frame(da[(burn+1):n,])
lm(da.burn[,1]~da.burn[,2])
colnames(da.burn) <- c("X","Y")
colMeans(da.burn)
cov(da.burn)
cor(da.burn)

## -----------------------------------------------------------------------------
library(coda)

f <- function(x,sigma){
  if (any(x < 0)) return (0)
  stopifnot(sigma > 0)
  return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
}

lap.chain <- function(m, sigma){
  x <- numeric(m)
  x[1] <- rchisq(1, df=1)
  k <- 0
  u <- runif(m)
  for (i in 2:m) {
    xt <- x[i-1]
    y <- rchisq(1, df = xt)
    num <- f(y, sigma) * dchisq(xt, df = y)
    den <- f(xt, sigma) * dchisq(y, df = xt)
    if (u[i] <= num/den) x[i] <- y else {
      x[i] <- xt
      k <- k+1 #y is rejected
    }
  }
  return(x)
}

Gelman.Rubin <- function(psi){
  psi = as.matrix(psi)
  n = ncol(psi)
  k = nrow(psi)
  psi.means = rowMeans(psi)
  B = n*var(psi.means)
  psi.w = apply(psi, 1, "var")
  W = mean(psi.w)
  v.hat = W*(n-1)/n + B/(n*k)
  r.hat = v.hat/W
  return(r.hat)
}

## -----------------------------------------------------------------------------
m <- 10000
sigma <- 4
k = 4
burn = 1000

X = matrix(0, nrow = k, ncol = m)
for (i in 1:k){
  X[i,] = lap.chain(m,sigma)
}

psi = t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] = psi[i,]/(1:ncol(psi))
print(Gelman.Rubin(psi))

for (i in 1:k){
  plot(psi[i, (burn+1):n], type="l", xlab=i, ylab=bquote(psi))
}

## -----------------------------------------------------------------------------
la.mc = mcmc.list(as.mcmc(X[1,]),as.mcmc(X[2,]),as.mcmc(X[3,]),as.mcmc(X[4,]))

gelman.diag(la.mc)
gelman.plot(la.mc)

## -----------------------------------------------------------------------------
u = c(11,8,27,13,16,0,23,10,24,2)
v = u + 1
n = 10
# 似然函数
L <- function(lambda){
  ll = 0
  for (i in 1:10){
    t = max(1e-5,pexp(v[i],rate = lambda)-pexp(u[i],rate = lambda))
    ll = ll + log(t)
  }
  return(ll)
}
# 极大似然
optimize(L,lower=1e-3,upper=5,maximum=TRUE)

## -----------------------------------------------------------------------------
# EM
Ex <- function(u,v,lambda){
  a = u*exp(-lambda*u)-v*exp(-lambda*v)+(exp(-lambda*u)-exp(-lambda*v))/lambda
  b = pexp(v,lambda)-pexp(u,lambda)
  return(a/b)
}
EM <- function(u,v,max.it=1e4,eps=1e-5){
  i = 1
  lambda1 = 10
  lambda2 = 1
  x = numeric(10)
  while (abs(lambda1-lambda2) >= eps) {
    lambda1 = lambda2
    for (i in 1:10) {
      x[i] = Ex(u[i],v[i],lambda1)
    }
    lambda2 = 10/sum(x)
  }
  
  return(lambda2)
}

EM(u,v)

## -----------------------------------------------------------------------------
solve.game <- function(A) {
  #solve the two player zero-sum game by simplex method
  #optimize for player 1, then player 2
  #maximize v subject to ...
  #let x strategies 1:m, and put v as extra variable
  #A1, the <= constraints
  #
  min.A <- min(A)
  A <- A - min.A #so that v >= 0
  max.A <- max(A)
  A <- A / max(A)
  m <- nrow(A)
  n <- ncol(A)
  it <- n^3
  a <- c(rep(0, m), 1) #objective function
  A1 <- -cbind(t(A), rep(-1, n)) #constraints <=
  b1 <- rep(0, n)
  A3 <- t(as.matrix(c(rep(1, m), 0))) #constraints sum(x)=1
  b3 <- 1
  sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3, maxi=TRUE, n.iter=it)
  #the ’Answer’ is [x1,x2,...,xm | value of game]
  #
  #minimize v subject to ...
  #let y strategies 1:n, with v as extra variable
  a <- c(rep(0, n), 1) #objective function
  A1 <- cbind(A, rep(-1, m)) #constraints <=
  b1 <- rep(0, m)
  A3 <- t(as.matrix(c(rep(1, n), 0))) #constraints sum(y)=1
  b3 <- 1
  sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3, maxi=FALSE, n.iter=it)
  soln <- list("A" = A * max.A + min.A,
    "x" = sx$soln[1:m],
    "y" = sy$soln[1:n],
    "v" = sx$soln[m+1] * max.A + min.A)
  soln
}

## -----------------------------------------------------------------------------
A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
  2,0,0,0,-3,-3,4,0,0,
  2,0,0,3,0,0,0,-4,-4,
  -3,0,-3,0,4,0,0,5,0,
  0,3,0,-4,0,-4,0,5,0,
  0,3,0,0,4,0,-5,0,-5,
  -4,-4,0,0,0,5,0,0,6,
  0,0,4,-5,-5,0,0,0,6,
  0,0,4,0,0,5,-6,-6,0), 9, 9)

library(boot) #needed for simplex function
s <- solve.game(A + 2)
round(cbind(s$x, s$y), 7)

value.A = t(s$x)%*%(A)%*%(s$y)
value.B = t(s$x)%*%(A+2)%*%(s$y)
round(c(value.A,value.B),3)

## -----------------------------------------------------------------------------
library(dplyr)
library(Rcpp)
library(microbenchmark)

## -----------------------------------------------------------------------------
x = list(a=1, b=2)
print(x)
unlist(x)
as.vector(x)
is.vector(x)

## -----------------------------------------------------------------------------
dim(unlist(x))
dim(1:4)

## -----------------------------------------------------------------------------
A =diag(3)
is.matrix(A)
is.array(A)

## -----------------------------------------------------------------------------
y = list(name = "Wang", id = 1, isfemale = TRUE)
da = data.frame(y)
as.matrix(da)

## ----message=FALSE------------------------------------------------------------
da1 = da[-1,]
print(da1) 

da2 = select(da,-c(name,id,isfemale))
print(da2)

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}


## -----------------------------------------------------------------------------
A = matrix(1:9,nrow = 3,ncol = 3)
A = as.data.frame(A)
print(A)
sapply(A,scale01) # every column of a data frame
A["name"]=c("a","b","c")
print(A)
sapply(A[,sapply(A,is.numeric)],scale01)

## -----------------------------------------------------------------------------
# Compute the standard deviation of every column
vapply(iris[,-5],sd,0)
# Compute the standard deviation of every numeric column in a mixed data frame
vapply(iris[,vapply(iris,is.numeric,TRUE)],sd,0)

## -----------------------------------------------------------------------------
bivariate <- function(a=1,b=1,n=10,N=5000){
  X = matrix(0,N,2)
  X[1,] = c(0,0.5)
  for (i in 2:N){
    y = X[i-1,2]
    X[i,1] = rbinom(1,n,y)
    x = X[i,1]
    X[i,2] = rbeta(1,x+a,n-x+b)
  }
  return(X)
}

X = bivariate()
plot(X[,1],X[,2])

## -----------------------------------------------------------------------------
# Rcpp
# Define function "add"
sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix bivariateC(double a, double b, int n, int N) {
  NumericMatrix X(N,2);
  double x,y;
  X(0,0)=0;
  X(0,1)=0.5;
  for(int i=1;i<N;i++){
    y = X(i-1,1);
    X(i,0) = rbinom(1,n,y)[0];
    x = X(i,0);
    X(i,1) = rbeta(1,x+a,n-x+b)[0];
  }
  return X;
}
')
XC = bivariateC(1,1,10,5000)
plot(XC[,1],XC[,2])

## -----------------------------------------------------------------------------
ts <- microbenchmark(bivariateR=bivariate(),bivariateC=bivariateC(1,1,10,5000))
summary(ts)[,c(1,3,5,6)]

