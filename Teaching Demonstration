# Central Limit Theorem (CLT)
set.seed(1) # reproduce the result
data <- runif(n=1000, min=1.2, max=5.6) # uniform random variable
hist(data, col='steelblue', main='Histogram') # histogram

n <- 1000
sample <- rep(0,n)
for (i in 1:n){
  sample[i] = mean(sample(data, 10, replace=TRUE))
}

mean(sample)
sd(sample)

hist(sample, col ='red', xlab='sample', main='Sample size = 10')

n = 1000
sample50 <- rep(0,n)
for (i in 1:n){
  sample50[i] = mean(sample(data, 50, replace=TRUE))
}
hist(sample50, col ='steelblue', xlab='sample', main='Sample size = 50')
mean(sample50)
sd(sample50)

sample <- rep(0,n)
par(mfrow=c(2,2)) # plot 4 figures together
for (i in 1:n){
  sample[i] = mean(sample(data, 10, replace=TRUE))
}
hist(sample, col ='red', xlab='sample', main='Sample size = 10', xlim=c(2,5))
for (i in 1:n){
  sample[i] = mean(sample(data, 50, replace=TRUE))
}
hist(sample, col ='red', xlab='sample', main='Sample size = 50', xlim=c(2,5))
for (i in 1:n){
  sample[i] = mean(sample(data, 100, replace=TRUE))
}
hist(sample, col ='red', xlab='sample', main='Sample size = 100', xlim=c(2,5))
for (i in 1:n){
  sample[i] = mean(sample(data, 1000, replace=TRUE))
}
hist(sample, col ='red', xlab='sample', main='Sample size = 1000', xlim=c(2,5))






# Law of Large Numbers (LLN)

# Create Variables for Observations
N <- 10000
set.seed(2)
# Create Variables for Iterations
# To simulate the coin flips.
# x: the sample flips as a 0 or 1.
# s: stores a running total of the occurrences of a value of “1”.
# r.avg: stores the running avg with each flip.
x <- sample(0:1, N, replace = T)
x
s <- cumsum(x)
s
r.avg <- s/(1:N)
r.avg

# Store the Means
r.stats <- round(cbind(x,s,r.avg), 3)
o <- 10
print(r.stats[1:o,])

# Graph the Results
# Create a plot chart to illustrate how the means of the sample approximately equals the population with large sample sizes.

# The plot uses line charts to reflect
# (1) the running averages of the coin flips
# and 
# (2) the expected average of the population (.5).

plot(r.avg, ylim=c(.30, .70), type = "l", xlab = "Observations", ylab = "Probability", lwd = 2, main = "Compare Probabilities")
lines(c(0,N), c(.50,.50),col="red", lwd = 2)

