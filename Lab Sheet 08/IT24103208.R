setwd("/Users/Krishan/Desktop/IT24103208Lab8")
getwd()

data <- read.table("Exercise - LaptopsWeights.txt", header = TRUE)
attach(data)  

N <- length(Weight.kg.)

pop_mean <- mean(Weight.kg.)
pop_var  <- var(Weight.kg.) * (N - 1) / N    # population variance (÷N)
pop_sd   <- sqrt(pop_var)

cat("\n[Population: Laptop Weights]\n")
cat("N             :", N, "\n")
cat("Pop. Mean     :", round(pop_mean, 6), "\n")
cat("Pop. Variance :", round(pop_var, 6), "\n")
cat("Pop. SD       :", round(pop_sd, 6), "\n")


set.seed(2120)        # reproducibility
num_samples <- 25
sample_size <- 6

samples <- matrix(NA, nrow = sample_size, ncol = num_samples)

for (i in 1:num_samples) {
  samples[, i] <- sample(Weight.kg., sample_size, replace = TRUE)
}
colnames(samples) <- paste0("s", 1:num_samples)

## Per-sample mean & variance
s.means <- apply(samples, 2, mean)
s.vars  <- apply(samples, 2, var)

## Mean & variance of the 25 sample means
mean_of_means <- mean(s.means)
var_of_means  <- var(s.means)
sd_of_means   <- sd(s.means)

## Theoretical values
theo_var_mean <- pop_var / sample_size
theo_sd_mean  <- pop_sd / sqrt(sample_size)

cat("\n[Sampling Results: 25 samples, size 6]\n")
cat("Mean of sample means   :", round(mean_of_means, 6), "\n")
cat("Variance of sample means:", round(var_of_means, 6), "\n")
cat("SD of sample means     :", round(sd_of_means, 6), "\n")

cat("\n[Theoretical expectations]\n")
cat("Expected Var(mean)  = σ²/n =", round(theo_var_mean, 6), "\n")
cat("Expected SD(mean)   = σ/√n  =", round(theo_sd_mean, 6), "\n")

## Optional: print the table of all 25 samples’ stats
result_table <- data.frame(
  Sample = paste0("s", 1:num_samples),
  SampleMean = round(s.means, 6),
  SampleVar  = round(s.vars, 6)
)
print(result_table)

