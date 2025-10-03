setwd("//Users//Krishan//Library//CloudStorage//OneDrive-SriLankaInstituteofInformationTechnology//PS - Stat//Lab 09")

# Lab Exercise 1: Test if average memes = 3 at 5% sig
memes <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)
t.test(memes, mu=3)  # Performs one-sample t-test

# Lab Exercise 2: Mice weights
weights <- c(17.6, 20.6, 22.2, 15.3, 20.9, 21.0, 18.9, 18.9, 18.9, 18.2)

# i. Test if mean < 25g at 5% sig
t.test(weights, mu=25, alternative="less")

# ii. Extract test statistic, p-value, and CI
res <- t.test(weights, mu=25, alternative="less")
res$statistic  # Test statistic
res$p.value    # p-value
res$conf.int   # Confidence interval

# Lab Exercise 3: Sugar levels ~ N(9.8, 0.05), n=30

# i. Generate 30 random sugar levels
sugar <- rnorm(30, mean=9.8, sd=0.05)

# ii. Test if mean > 10 at 5% sig
t.test(sugar, mu=10, alternative="greater")

# Exercise 1: Baking time ~ N(45, 2), n=25

# i. Generate random sample of 25 baking times
baking <- rnorm(25, mean=45, sd=2)

# ii. Test if average < 46 min at 5% sig
t.test(baking, mu=46, alternative="less")
