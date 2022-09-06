set.seed(300)
n=1000
x <- c(1:n) 

beta0 <- 30 # intercept
beta1 <- 4 # slope
beta2 <- 2 # slope
beta3 <- 0.8 # slope
x1 = rnorm(n, 20, 2)
x2 = rnorm(n, 50, 20)
x3 = rnorm(n, 80, 8)


y = beta0 + (beta1 * x1) + (beta2 * x2) + (beta3 * x3) 

l = list(x1, x2)

df = data.frame(y)

fun <- function(x) {
  df <<-  cbind(df, x) # <<- write to outer scope
}

lapply(l, fun)

colnames(df) = c("y", "x1", "x2")


fit <- lm(y ~ x1 + x2, data = df)
s = summary(fit)
fit$coefficients
s
s$coefficients[2,"Std. Error"] /s$coefficients[2,"Estimate"] * 100
s$coefficients[3,"Std. Error"] /s$coefficients[3,"Estimate"] * 100
