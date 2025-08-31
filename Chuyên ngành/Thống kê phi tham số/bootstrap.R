
score <- c(37,49,55,57,23,31,46)
mean <- mean(score)
mean

boot <- sample(score, size=7, replace=TRUE)
boot


# Bootstraping 

n <- length(score)
nboots <- 10000

boot.result <- numeric(nboots)      
for(i in 1:nboots)
{
  boot.samp <- sample(score, n, replace=TRUE)  
  boot.result[i] <- mean(boot.samp)
}
hist(boot.result)

# Confidence interval 
# Boostrap 95% T-pivot confidence intervals

set.seed(4123)
x.exp <- rexp(30, rate=1/25) # Mean = 25 
n <- length(x.exp) 
mean.exp <-mean(x.exp) 
sd.exp <- sd(x.exp)

nboots <- 10000
boot.t.result <- numeric(nboots) 
for(i in 1:nboots){
  boot.samp <- sample(x.exp, n, replace=TRUE)
  boot.t.result[i] <- (mean(boot.samp) - mean.exp)*sqrt(n)/sd(boot.samp)
}

hist(boot.t.result)

t.upper <- quantile(boot.t.result, 0.975) 
t.lower <- quantile(boot.t.result, 0.025)

lower95.limit <- mean.exp - t.upper*sd.exp/sqrt(n) 
upper95.limit <- mean.exp - t.lower*sd.exp/sqrt(n)

lower95.limit
upper95.limit

# Using bootstrap samples to estimate standard error

set.seed(4123)
x.exp <- rexp(30, rate=1/25) # mean = 25
n <- length(x.exp)

mean.exp <- mean(x.exp)
nboots <- 10000

boot.MSE.result <- numeric(nboots)      
for(i in 1:nboots)
{
  boot.samp <- sample(x.exp, n, replace=TRUE)  
  boot.MSE.result[i] <- (mean(boot.samp) - mean.exp)^2
}

SE <- sqrt(mean(boot.MSE.result))
lower95.limit <- mean.exp - qnorm(0.975)*SE
upper95.limit <- mean.exp + qnorm(0.975)*SE

lower95.limit
upper95.limit

# Confidence interval by percentile 

set.seed(4123)
x.exp <- rexp(30, rate=1/25) # mean = E(X) = 25
x.exp
boxplot(x.exp)


n <- length(x.exp)
mean.exp <- mean(x.exp)
nboots <- 10000
boot.result <- numeric(nboots)   

for(i in 1:nboots)
{
  boot.samp <- sample(x.exp, n, replace=TRUE)  
  boot.result[i] <- mean(boot.samp)
}
hist(boot.result)

mean.exp
quantile(boot.result, c(0.025,0.975))







