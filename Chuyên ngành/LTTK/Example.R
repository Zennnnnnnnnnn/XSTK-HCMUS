#data
#x: iodine value sgd
#y: cetane number for a sample of 14 biofuels.
#The iodine value is the amount of iodine necessary to saturate a sample of 100 g of oil.

x=c(132.0, 129.0, 120.0, 113.2, 105.0, 92.0, 84.0, 83.2, 88.4, 59.0, 80.0, 81.5, 71.0, 69.2)
y=c(46.0, 48.0, 51.0, 52.1, 54.0, 52.0, 59.0, 58.7, 61.6, 64.0, 61.4, 54.6, 58.8, 58.0)

x#Least Squares Estimates
Sxx = sum((x - mean(x)) ^ 2)
Sxy = sum((x - mean(x)) * (y - mean(y)))
c(Sxx, Sxy)
beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)

#use the lm() function to check the work we had previously done.
mod <- lm(y ~ x)
coef(mod)
names(mod)
mod$fitted.values     # compute the fitted values
mod$residuals      # compute all the residuals

summary(mod)
anova(mod)

#add the fitted line to the scatterplot
plot(y ~ x,
     xlab = "iodine value (g)",
     ylab = "cetane number",
     main = "cetane number = 75.21 – 0.2094iodine value ",
     pch  = 20,
     cex  = 2,
     col  = "cadetblue2")
abline(mod, lwd = 3, col = "blue")

#Point prediction of the Y value that will result from a single new observation made at x=x_0=100
predict(mod, newdata = data.frame(x = 100))
#predict multiple values at once
predict(mod, newdata = data.frame(x = c(90,100, 120)))
  
confint(mod)
confint(mod, level = 0.99)


#confidence interval for the mean response
newd = data.frame(x = c(90,100, 120))
newd1= data.frame(x = 90)
predict(mod, newdata = newd1, 
        interval = c("confidence"), level = 0.99)

#Prediction Interval for New Observations
predict(mod, newdata = newd1, 
        interval = c("prediction"), level = 0.99)
