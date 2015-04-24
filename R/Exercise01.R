# Theoretical mean and sd of the population
muexp <- 1 / lambda
sdexp <- 1 / lambda

# Asympthotic of the mean
library(ggplot2)
n <- 1000
lambda <- 0.2
means <- cumsum(rexp(n, rate=lambda)) / (1  : n) 
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
g <- g + geom_hline(yintercept = 0) + geom_line(size = 1) 
g <- g + geom_hline(yintercept=muexp,color="red",linetype="dashed", size=1.5)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g <- g + ggtitle("Asympthotic of Sample Mean to Population Mean")
print(g)

# Asympthotic of the standard deviation
n <- 1000
lambda <- 0.2
sds <- vector(mode="numeric", length=0)
for (i in 1:n) {
  sds[i] = sd(rexp(i,rate=lambda))
}
g <- ggplot(data.frame(x = 1 : n, y = sds), aes(x = x, y = y)) 
g <- g + geom_hline(yintercept = 0) + geom_line(size = 1) 
g <- g + geom_hline(yintercept=sdexp,color="red",linetype="dashed", size=1.5)
g <- g + labs(x = "Number of obs", y = "Cumulative std dev")
g <- g + ggtitle("Asympthotic of Sample StDev to Population StDev")
print(g)

# Do the simulation of the random 1000 exponentials
exps = rexp(nosim,lambda)
g <- qplot(x, geom="histogram", binwidth=5, 
      main="Exponential random value distribution", 
      xlab="Value of X", ylab="density",
      fill=I("blue"), col=I("black"))
print(g)

# Do the simulation of the average of 1000 samples size n=40   
nosim <- 1000
n <- 40
Xbardat <- data.frame(
  x = c(apply(matrix(rexp(nosim*n,lambda), 
                     nosim), 1, mean)
  ),
  size = factor(rep(c(n), rep(nosim, 1))))
XbarMean = mean(Xbardat$x)
Xbarsd = sd(Xbardat$x)
print(c(muexp,XbarMean))
print(c(sdexp/sqrt(n),Xbarsd))

# Plot 1
g <- ggplot(Xbardat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + geom_density(alpha=.2, fill="#FF6666")
g <- g + geom_vline(aes(xintercept=mean(muexp, na.rm=T)),  
                    color="red", linetype="dashed", size=1)
g <- g + ggtitle("Sample Mean - Density Chart")
g <- g + labs(x = "Sample Mean", y = "Density")
g + facet_grid(. ~ size)
print(g)


# Do the simulation of the variance of 1000 samples size n=40   
Xvardat <- data.frame(
  x = c(apply(matrix(rexp(nosim*n,lambda), 
                     nosim), 1, var)
  ),
  size = factor(rep(c(n), rep(nosim, 1))))
XvarMean = mean(Xvardat$x)
print(c(sdexp,sqrt(XvarMean)))

# Plot
g <- ggplot(Xvardat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=1, colour = "black", aes(y = ..density..)) 
g <- g + geom_density(alpha=.2, fill="#FF6666")
g <- g + geom_vline(aes(xintercept=mean(sdexp^2, na.rm=T)),  
                    color="red", linetype="dashed", size=1)
g <- g + ggtitle("Sample Variance - Density Chart")
g <- g + labs(x = "Sample Variance", y = "Density")
g + facet_grid(. ~ size)
print(g)

# Prepare the simulation of sample averages of size n=40
cfunc <- function(x, n) sqrt(n) * (mean(x) - muexp) / sdexp
XbarNormdat <- data.frame(
  x = c(apply(matrix(rexp(nosim*n,lambda), 
                     nosim), 1, cfunc, n)
  ),
  size = factor(rep(c(n), rep(nosim, 1))))
g <- ggplot(XbarNormdat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + labs(x = "Number of simulations", y = "Normal Density")
g <- g + ggtitle("Sample Mean - Normalized Density")
g <- g + stat_function(fun = dnorm, size = 2) + facet_grid(. ~ size)
print(g)