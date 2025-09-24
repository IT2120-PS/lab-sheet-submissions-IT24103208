setwd("C:\\Users\\Krishan\\OneDrive\\Desktop\\IT24103208")


# Problem 1: Learning platform and online test performance
# Given: 85% pass rate, sample size = 50
# X = number of students passed

# Parameters
n <- 50       # Number of students
p <- 0.85     # Probability of passing

# i. Distribution of X
# X follows a Binomial distribution
# X ~ Binomial(n=50, p=0.85)
cat("Problem 1:\n")
cat("i. Distribution: Binomial(n=50, p=0.85)\n\n")

# ii. Probability that at least 47 students passed
# P(X >= 47) = 1 - P(X <= 46)
prob_at_least_47 <- 1 - pbinom(46, size=n, prob=p)
cat("ii. Probability that at least 47 students passed:", prob_at_least_47, "\n\n")

# Problem 2: Call center receiving calls
# Average calls per hour = 12
# X = number of calls received in an hour

lambda <- 12   # Average number of calls per hour

cat("Problem 2:\n")
# i. Random variable X
cat("i. Random variable: X = number of calls received in an hour\n")

# ii. Distribution of X
cat("ii. Distribution: Poisson(lambda=12)\n")

# iii. Probability that exactly 15 calls are received
prob_15_calls <- dpois(15, lambda=lambda)
cat("iii. Probability that exactly 15 calls are received:", prob_15_calls, "\n")
