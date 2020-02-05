# Numerically calculate inverse at a point. y can be a vector
inverse = function(fn, interval = NULL, lower = min(interval), upper = max(interval), ...){
  Vectorize(function(y, ...){
    uniroot(f=function(x){fn(x)-y}, lower=lower, upper=upper, ...)$root
  })
}


x = 1:10
y = sqrt(x)
sqrt.inv = inverse(sqrt, lower=1, upper=10)
sqrt.inv(y)


cumul_hazard = function(t, t0, alpha, beta, lambda = 1){
  
  lambda * log( 1 + ((t + t0) / alpha)^beta ) - 
    lambda * log( 1 + (t0 / alpha)^ beta)
  
  
}


alpha = 1
beta = 8
t0 = 0.75

t = seq(0, 10, by = 0.25)
grid = cumul_hazard(t, t0, alpha, beta)

inverse_cumulative_hazard = inverse(cumul_hazard, lower = 0, upper = 50)
inverse_cumulative_hazard(grid, alpha = alpha, beta = beta, t0 = t0)
# Not working

# Do this instead then apply
inverse = function (f, lower = -100, upper = 100) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
}