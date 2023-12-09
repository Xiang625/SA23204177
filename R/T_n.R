T_n <- function(r,n1,YY,m){
  t_n = 0
  n = floor(r*n1)
  for (t in 1:n){
    t_n = t_n + sum(YY[t+m,1:t])
  }
  return(t_n)
}