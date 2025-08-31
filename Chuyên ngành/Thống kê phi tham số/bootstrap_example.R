# Bootstrap example 
boot_dat <- read.table("ex_boot.txt", header = T)
head(boot_dat)
attach(boot_dat)

# Estimate the bias of S^2 
nboot <- 1000 
theta.hat <- var(x) # S^2
theta.star <- numeric(nboot)

# Perform the bootstrap to get the sampling distribution 
n <- length(x)
for(i in 1:nboot){
  x.star <- sample(x, n, replace = TRUE)
  theta.star[i] <- var(x.star)
}

# Histogram 
hist(theta.star)
abline(v = theta.hat, col = "red", lwd = 2)

s2_tilde <-mean( (x - mean(x))^2)
bias.boot <- mean(theta.star) - s2_tilde
bias.boot

# Boostrap for median

n <- length(x)
median.boot <- numeric(nboot)

for(i in 1:nboot) median.boot[i] <- median(sample(x, n, replace = T))

med.obs <- median(median.boot)


# Compute correlation coefficient between X & Y
nboot <- 1000

n <- length(x)
theta.hat <- cor(x, y)
theta.hat
theta.star <- numeric(nboot)

for (i in 1:nboot) {
  k.star <- sample.int(n, n, replace = TRUE)
  x.star <- x[k.star]
  y.star <- y[k.star]
  theta.star[i] <- cor(x.star, y.star)
}

a = -0.0980306 
b = 0.2362514
hist(theta.star)
abline(v = theta.hat, col = "red", lwd = 2)
abline(v = a, col = "blue")
abline(v = b, col = "blue")

# Bootstrap percentile confidence intervals

conf.level <- 0.95
alpha <- 1 - conf.level
quantile(theta.star, c(alpha / 2, 1 - alpha / 2))


