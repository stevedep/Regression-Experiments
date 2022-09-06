#https://www.econometrics-with-r.org/4-5-tsdotoe.html
library(ggplot2)
load("ds.rds")

lmM = lm(price~age+ km + CatalogPrice , data = dataset) #Create a linear regression with a quadratic coefficient#(lf)
f = summary(lmM) #Review the results#(lf)

n=10000

km = rnorm(n, mean=mean(dataset$km), sd=sd(dataset$km))
age = rnorm(n, mean=mean(dataset$age), sd=sd(dataset$age))
CatalogPrice = rnorm(n, mean=mean(dataset$CatalogPrice), sd=sd(dataset$CatalogPrice))
u = rnorm(n, mean=0, sd=f$sigma)

price =  f$coef["(Intercept)", "Estimate"] + (age * f$coefficients["age", "Estimate"]) + (km * f$coefficients["km", "Estimate"]) + 
  (CatalogPrice * f$coefficients["CatalogPrice", "Estimate"]) + u

l = list(km, age, CatalogPrice)

df = data.frame(price)

fun <- function(x) {
  df <<-  cbind(df, x) # <<- write to outer scope
}

lapply(l, fun)

colnames(df) = c("price", "km", "age", "CatalogPrice")


ss = data.frame(1,1,1,1)
reeks <- c(1:3000)
for (i in reeks) {
  fit <- lm(price~age+ km + CatalogPrice , data = df[sample(n,100),]) 
  ss[i,1] = fit$coef["(Intercept)"]
  ss[i,2] = fit$coef["CatalogPrice"]
  ss[i,3] = fit$coef["age"]
  ss[i,4] = fit$coef["km"]
  
}


colnames(ss) = c('intercept', 'CatalogPrice', 'age', 'km')
g = ggplot(ss, aes(x=age)) + geom_density(color="blue", fill = "blue", alpha = .2, size=1.5) + geom_vline(xintercept = mean(ss$age), color = "blue") + geom_vline(xintercept = mean(ss$age) - sd(ss$age)*2, color = "red") + geom_vline(xintercept = mean(ss$age), color = "blue") + geom_vline(xintercept = mean(ss$age) + sd(ss$age)*2, color = "red") + geom_vline(xintercept = mean(ss$age) + sd(ss$age), color = "orange")  + geom_vline(xintercept = mean(ss$age) - sd(ss$age), color = "orange")

g + annotate("text", color="black", x=mean(ss$age), y=0, label=paste("beta[1] ==", mean(ss$age)), 
             size=8, parse=TRUE) + annotate("text", color= "black",x=mean(ss$age) + sd(ss$age), y=0.004, label=paste("sd_beta[1] ==", sd(ss$age)), 
                                            size=8, parse=TRUE) +  stat_function(fun = dnorm, color = "red", size=2, args = list(mean = f$coef["age","Estimate"], sd = f$coef["age","Std. Error"])) +
  xlab("Slope (b2)") 


#fit <- lm(price~age+ km + CatalogPrice , data = df[sample(n,200),]) 
#tidy(fit)
