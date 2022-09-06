#https://rpubs.com/friendly/sampdist

library(purrr)
library(broom)
library(dplyr)
library(ggplot2) 
library(car)

therapy <- read.table(header=TRUE, text ='
name   sex perstest therapy
John    M 26 32
Susan   F 24 40
Mary    F 22 44
Paul    M 33 44
Jenny   F 27 48
Rick    M 36 52
Cathy   F 30 56
Robert  M 38 56
Lisa    F 30 60
Tina    F 34 68
')

true_mod <- lm(therapy ~ perstest, data=therapy)

tmod <- tidy(true_mod)
tmod

upper <- tmod$estimate +  1.96 * tmod$std.error
lower <- tmod$estimate -  1.96 * tmod$std.error
tmod <- cbind(tmod, lower, upper)
tmod

x <- therapy$perstest
n <- length(x)

samples <- 1000

# "true" parameters
b0 <- coef(true_mod)[1]   # 14
b1 <- coef(true_mod)[2]   # 1.2
sigma <- summary(true_mod)$sigma  # sqrt(80)
set.seed(1234)       # reproducibility

one_sample <- function(x, b0=14, b1=1.2, sigma=sqrt(80)) {
  n <- length(x)
  y <- b0 + b1 * x + rnorm(n, mean=0, sd=sigma)
  data.frame(x, y)
}
one_sample(x, b0, b1, sigma)
one_sample(x)

one_reg <- function(x) {
  simdata <- one_sample(x)
  lm(y ~ x, data=simdata)
}
one_reg(x)

all_coefs <- function(x, samples=5) {
  res <- replicate(n=samples, coef(one_reg(x)))
  rownames(res) <- c("b0", "b1")
  as.data.frame(t(res))
}

coefs <- all_coefs(x, samples=samples)


ggplot(coefs,  aes(x = b1) ) +
  geom_density(color="blue", fill = "blue", alpha = .2, size=1.5) +
  geom_vline( xintercept = b1, color="red", size=1.5) + 
  geom_vline(xintercept = lower[2]) +
  geom_vline(xintercept = upper[2]) +
  annotate("text", x=b1, y=0.05, label=paste("beta[1] ==", b1), 
           size=8, parse=TRUE) +
  stat_function(fun = dnorm, args = list(mean = b1, sd = .565)) +
  xlab("Slope (b1)") 
