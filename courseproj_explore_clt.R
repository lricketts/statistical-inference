setwd("C:/Program Files/R/Coursera/statinfer/statinfer_courseproject")

library(ggplot2)

set.seed(100)

# Set rate equal to 0.2 for all simulations per Coursera instructions
lambda <- 0.2

# Mean and standard deviation of exponential distribution are equal to 1/lambda
mu <- 1/lambda
sigma <- 1/lambda
var = sigma ^ 2

# Number of simulations for this exercise will be 1,000
nosim <- 1000

# Create matrix of 1,000 sets of 40 random exponentials
sim <- matrix(rexp(nosim * 40, lambda), nosim)

# Calculate mean of 40 random exponentials, 1,000 times
dat <- data.frame(x = apply(sim, 1, mean),
                  y = apply(sim, 1, var))

# Plot histogram of 1,000 means of 40 random exponentials
#g = ggplot(dat, aes(x))
#g = g + geom_histogram(fill = "lightblue", color = "black", binwidth = 0.2)
#g = g + geom_vline(xintercept = mu, size = 2)
#g

# Simulated population mean is very close to our theoretical
paste("Sample mean:", mean(dat$x))

paste("Theoretical mean:", mu)

# Plot the sample distribution of 1,000 means of 40 random exponentials
# vertical lines represent theoretical mean (black) and sample variance (red)
# The two lines almost sit directly on top of each other, the simulation confirms
# that the sample size is large enough to provide a consistent estimator of the
# population mean.
g1 = ggplot(dat, aes(x)) 
g1 = g1 + geom_density(size = 2, fill = "salmon")
g1 = g1 + geom_vline(xintercept = mu, colour = "black", size = 1)
g1 = g1 + geom_vline(xintercept = mean(dat$x), colour = "red", size = 1)
g1 + ggtitle("Figure 1: Sample Mean Consistent Estimator of Population Mean")

# Sample variance is very close to our theoretical
paste("Sample variance:", mean(dat$y))

paste("Theoretical variance:", var)

# Sample distribution of 1,000 variances of 40 random exponentials
# vertical lines represent theoretical variance (black) and sample variance (red)
# The two lines almost sit directly on top of each other, the simulation confirms
# Our suspected theoretical variance.
g2 = ggplot(dat, aes(y)) 
g2 = g2 + geom_density(size = 2, fill = "salmon")
g2 = g2 + geom_vline(xintercept = var, colour = "black", size = 1)
g2 = g2 + geom_vline(xintercept = mean(dat$y), colour = "red", size = 1)
g2 + ggtitle("Figure 2: Sample Variance Consistent Estimator of Population Variance")


# Law of Large Numbers: An estimator is consistent if it converges to what
# you want to estimate, the Law of Large Numbers says that the sample mean of
# an iid sample is consistent for the population mean. We see that as the
# cumulative mean of 1,000 random exponentials converges on our population
# mean of 1/lambda = 5.
means <- cumsum(sim[,1]) / (1 : nosim)
g3 <- ggplot(data.frame(x = 1 : nosim, y = means), aes(x = x, y = y))
g3 <- g3 + geom_hline(yintercept = mu) + geom_line(size = 2)
g3 <- g3 + labs(x = "Number of obs", y = "Cumulative mean")
g3

# Normalize distribution of averages to compare with standard normal
normal_dist <- data.frame(x_norm = ((dat$x - mean(dat$x))/sigma) * sqrt(40))

# Normalized distribution of means fits the standard normal distribution quite
# well. This is a direct reflection of the Central Limit Theorem: the distribution
# of averages of iid variables (properly normalized) becomes that of a standard
# normal as the sample size increases.
g4 <- ggplot(normal_dist, aes(x = x_norm)) +
        geom_histogram(alpha = .50, binwidth = .3, fill = "darkorange4", 
                       colour = "black", aes(y = ..density..))
g4 <- g4 + stat_function(fun = dnorm, size = 2)
g4 + ggtitle("Figure 3: Normalized Sample Distribution Follows Normal Distribution")