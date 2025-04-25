#Problem 1

#Problem 1a

set.seed(0)
rand_nums <- rexp(n = 10000, rate = 2)
rand_nums_mean <- mean(rand_nums)
print(rand_nums_mean)

#Problem 1b

set.seed(0)
rand_draws <- rexp(n=200, rate=0.5)
hist(
  rand_draws,
  freq = FALSE,
  breaks = 30,
  main = "Histogram of 200 Exponential Draws with rate 0.5",
  xlab = "x",
  ylab = "Density"
)

x_vals <- seq(0, 10, length = 1000)
pdf_vals <- dexp(x_vals, rate = 0.5)
#lines(x_vals, pdf_vals, lwd = 2)
lines(x_vals, pdf_vals, col = "blue", lwd = 2)

#problem 1c

#rate = 1
lines(x_vals, (dexp(x_vals, rate = 1)), col = "red", lwd = 2)

#rate = 1/3
lines(x_vals, (dexp(x_vals, rate = 1/3)), col = "green", lwd = 2)

legend("topright", legend=c("rate = 1/2", "rate = 1", "rate = 1/3"),
       col=c("blue", "red", "green"), lwd=2,  title = "PDF")

#Problem 1d
rand_draws_sorted <- sort(rand_draws)
empirical_cdf <- ecdf(rand_draws_sorted)

#ecdf_manual <- (1/200)*seq(1, 200)
#print(ecdf_manual)

plot( empirical_cdf,
      xlab = "x",
      ylab = "Cumulative Probability",
      main = "Empirical vs Theoretical CDFs",
      do.points = FALSE,
      lwd =2,
      col = "black"
      
)

lines(x_vals, pexp(x_vals, rate = 1/2), col = "blue", lwd = 2)
lines(x_vals, (pexp(x_vals, rate = 1)), col = "green", lwd = 2)
lines(x_vals, (pexp(x_vals, rate = 1/3)), col = "red", lwd = 2)

legend("bottomright",
       legend = c("Empirical CDF", "Expon(β=1)", "Expon(β=2)", "Expon(β=3)"),
       col = c("black", "green", "blue", "red"),
       lwd = 2,
       lty = c(1,1,1,1),
       bty = "n")

#Problem 1e
rand_draws_median <- median(rand_draws)

#median when Beta = 1
median_beta_1 <- qexp(0.5, rate = 1)

#median when Beta = 2
median_beta_2 <- qexp(0.5, rate = 1/2)

#median when Beta = 3
median_beta_3 <- qexp(0.5, rate = 1/3)

print(rand_draws_median)
print(median_beta_1)
print(median_beta_2)
print(median_beta_3)

#Problem 1f

dx = 0.0005
upper_limit <- 100
x_vals_int <- seq(0, upper_limit, by = dx)
pdf_vals_int <- dexp(x_vals_int, rate = 1/2)
areas <- pdf_vals_int*dx
approx_integration <- sum(areas)
print(approx_integration)


#Problem 1g

dx_2 = 0.05
x_vals_int_2 <- seq(0, upper_limit, by = dx_2)
pdf_vals_int_2 <- dexp(x_vals_int_2, rate = 1/2)
expected_values <- x_vals_int_2 * pdf_vals_int_2
exp_value_areas <- expected_values*dx_2
approx_expected_values <- sum(exp_value_areas)
print(approx_expected_values)


f_expected <- function(x){
  x * dexp(x, rate = 1/2)
}

approx_integration_calculated <- integrate(f_expected, lower = 0, upper = Inf)
print(approx_integration_calculated$value)

#---------------------------------------------------------------------------------------------------


data=read.csv("https://github.com/StatisticsSU/STM/raw/main/assignment/bugs.csv",header =TRUE)
y=data$nBugs# number of bugs, a vector with n = 91 observations
View(data)

#problem 2a

lambda_estimate <- mean(y)
data_var <- var(y)
print(lambda_estimate)
print(data_var)

hist(
  y,20,
  ylim = c(0, 0.2),
  probability = TRUE,
  main = "Histogram of Number of Bugs",
  xlab = "Number of Bugs",
  ylab = "Density"
)

x_vals <- seq(0, max(y))
poisson_density <- dpois(x_vals, lambda = lambda_estimate)
lines(x_vals, poisson_density, col = "blue",lwd = 2, lty = 1)


#Problem 2b

mu_estimate <- mean(y)
nbinom_density_r1 <- dnbinom(x_vals, size = 1, mu = mu_estimate)
lines(x_vals, nbinom_density_r1, col = "red",lwd = 2, lty = 2)

nbinom_density_r3 <- dnbinom(x_vals, size = 3, mu = mu_estimate)
lines(x_vals, nbinom_density_r3, col = "purple",lwd = 2, lty = 3)

nbinom_density_r100 <- dnbinom(x_vals, size = 100, mu = mu_estimate)
lines(x_vals, nbinom_density_r100, col = "green",lwd = 2, lty = 4)

legend("topright", legend = c("Poisson (λ)", "Negative Binomial (r=1)", "Negative Binomial (r=3)", "Negative Binomial (r=100)"),
       col = c("blue","red",  "purple", "green"), lwd = 2, lty = c(1, 2, 3, 4))



# Problem 3a

# Step 1: Simulate 10,000 normal random variables
set.seed(123)
x <- rnorm(10000)

# Step 2: Transform using exponential
y <- exp(x)

# Step 3: Plot histogram with better readability
hist(y, 
     breaks = 50,                   # Reduce bin count for clarity
     probability = TRUE,           # Scale to show probability density
     xlim = c(0, 15),              # Focus on the range where most values fall
     main = "Histogram of Y = exp(X)",
     xlab = "Y",
     col = "lightblue", 
     border = "white")

# Problem 3b: Overlay theoretical PDF on histogram

# Step 1: Simulate data (if not already done)
set.seed(123)
x <- rnorm(10000)
y <- exp(x)

# Step 2: Plot histogram
hist(y,
     breaks = 50,
     probability = TRUE,
     xlim = c(0, 15),
     main = "Histogram of Y = exp(X) with Log-Normal PDF",
     xlab = "Y",
     col = "lightblue",
     border = "white")

# Step 3: Overlay log-normal theoretical PDF
curve(dlnorm(x, meanlog = 0, sdlog = 1),
      from = 0, to = 15,
      col = "red",
      lwd = 2,
      add = TRUE)
legend("topright", legend = "Theoretical PDF", col = "red", lwd = 2)


# Problem 3c: Monte Carlo estimate of E[Y] = E[exp(X)]

set.seed(123)

# Step 1: Simulate 10,000 values of X and compute Y = exp(X)
x <- rnorm(10000)
y <- exp(x)

# Step 2: Compute running mean estimates
sample_sizes <- 10:10000
running_means <- cumsum(y)[sample_sizes] / sample_sizes

# Step 3: Plot the running estimates
plot(sample_sizes, running_means,
     type = "l",
     col = "blue",
     lwd = 1.5,
     xlab = "Sample Size (m)",
     ylab = "Monte Carlo Estimate of E[Y]",
     main = "Convergence of Monte Carlo Estimate of E[Y]",
     ylim = c(1.4, 1.9))  # Zoom in on the region around exp(0.5)

# Step 4: Add the true expected value as a red dashed line
abline(h = exp(0.5), col = "red", lty = 2, lwd = 2)
legend("topright",
       legend = c("Estimate", "True E[Y] = exp(0.5)"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)








