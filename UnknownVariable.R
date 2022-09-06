library(ggplot2)
require(broom)
load("ds.rds")

lmM = lm(price~age+ km + CatalogPrice , data = dataset) #Create a linear regression with a quadratic coefficient#(lf)
f = summary(lmM) #Review the results#(lf)

n=10000

km = rnorm(n, mean=mean(dataset$km), sd=sd(dataset$km))
age = rnorm(n, mean=mean(dataset$age), sd=sd(dataset$age))
CatalogPrice = rnorm(n, mean=mean(dataset$CatalogPrice), sd=sd(dataset$CatalogPrice))
varx = rnorm(n, mean=0, sd=f$sigma)


u = rnorm(n, mean=0, sd=f$sigma)


price =  f$coef["(Intercept)", "Estimate"] + (age * f$coefficients["age", "Estimate"]) + (km * f$coefficients["km", "Estimate"]) + 
  (CatalogPrice * f$coefficients["CatalogPrice", "Estimate"]) + (varx * 1 ) #+  (vary * 3 )
l = list(km, age, CatalogPrice)
df = data.frame(price)

fun <- function(x) {
  df <<-  cbind(df, x) # <<- write to outer scope
}

r = lapply(l, fun)
colnames(df) = c("price", "km", "age", "CatalogPrice")

fit <- lm(price~age+ km + CatalogPrice , data = df[sample(n,200),]) 
tidy(fit)
summary(fit)

