setwd("/Users/Krishan/Desktop/IT24103208")
getwd()

## 1 ##
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE)
names(Delivery_Times) <- "time"
View(Delivery_Times)   # use RStudio viewer instead of fix()

## 2 ##
# Basic histogram
hist(Delivery_Times$time,
     main = "Histogram for Delivery Times")

# Histogram with specified bins (also saves counts/breaks for later)
h <- hist(Delivery_Times$time,
          main = "Histogram for Delivery Times",
          breaks = seq(20, 70, length = 7),
          right = FALSE)

## 3 ##
# The histogram of delivery times shows that most delivery times fall in the middle range
# (around 40–55 minutes), and the shape is approximately symmetric.

## 4 ##
# Use the saved histogram to build the cumulative frequency polygon
breaks <- h$breaks
freq   <- h$counts

cum.freq <- cumsum(freq)

new <- c()
for (i in 1:length(breaks)) {
  if (i == 1) {
    new[i] <- 0
  } else {
    new[i] <- cum.freq[i - 1]
  }
}

plot(breaks, new, type = "l",
     main = "Cumulative Frequency Polygon for Time",
     xlab = "Time", ylab = "Cumulative Frequency",
     ylim = c(0, max(cum.freq)))
