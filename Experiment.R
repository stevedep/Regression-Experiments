set.seed(300)
n=1000
x <- c(1:n) 

beta0 <- 3 # intercept
beta1 <- 1.8 # slope
beta2 <- 2 # slope
beta3 <- 2 # slope
rnorm(n, 20, 2)

l = list(x, x)

df = data.frame(x)

fun <- function(x) {
  df <<-  cbind(df, x) # <<- write to outer scope
}


df

lapply(l, fun       )


y <- beta0 +  beta1 * x + rnorm(n,mean = 0,sd = 4) # sd of error is 6
fit <- lm(y ~ x)
summary(fit)
