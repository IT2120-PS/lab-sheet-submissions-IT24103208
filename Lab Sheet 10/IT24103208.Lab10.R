wd <- file.path(Sys.getenv("HOME"), "Desktop", "IT2120_Lab10")
if (!dir.exists(wd)) dir.create(wd, recursive = TRUE)
setwd(wd)


cat("\n--- Q1: Equal customers by weekday ---\n")
obs_q1 <- c(Mon=55, Tue=62, Wed=43, Thu=46, Fri=50)
exp_q1 <- rep(1/5, 5)  # equal probabilities
q1 <- chisq.test(obs_q1, p = exp_q1, rescale.p = TRUE)
print(q1)


if (q1$p.value < 0.05) {
  cat("Conclusion: Reject H0 (customers are NOT equal across weekdays)\n")
} else {
  cat("Conclusion: Fail to reject H0 (customers MAY be equal across weekdays)\n")
}


cat("\n--- Exercise: A,B,C,D equal? ---\n")

get_counts <- function() {
  if (file.exists("Data.csv")) {
    df <- try(read.csv("Data.csv", check.names = FALSE), silent = TRUE)
    if (!inherits(df, "try-error")) {
      # Format A: columns A,B,C,D
      abcd <- intersect(colnames(df), c("A","B","C","D"))
      if (length(abcd) == 4) {
        counts <- as.numeric(df[1, c("A","B","C","D")])
        names(counts) <- c("A","B","C","D")
        return(counts)
      }
      # Format B: Snack, Count
      if (all(c("Snack","Count") %in% colnames(df))) {
        s <- tapply(df$Count, df$Snack, sum)
        counts <- c(A=0,B=0,C=0,D=0)
        nm <- intersect(names(s), c("A","B","C","D"))
        counts[nm] <- s[nm]
        return(counts)
      }
    }
  }
  # Manual input (very simple)
  cat("Data.csv not found or format unknown. Type counts.\n")
  A <- as.numeric(readline("A: "))
  B <- as.numeric(readline("B: "))
  C <- as.numeric(readline("C: "))
  D <- as.numeric(readline("D: "))
  c(A=A,B=B,C=C,D=D)
}

obs_ex <- get_counts()
cat("Observed:\n"); print(obs_ex)

ex <- chisq.test(obs_ex, p = rep(1/4, 4), rescale.p = TRUE)
print(ex)

if (ex$p.value < 0.05) {
  cat("Conclusion: Reject H0 (A,B,C,D are NOT equally likely)\n")
} else {
  cat("Conclusion: Fail to reject H0 (A,B,C,D MAY be equally likely)\n")
}

cat("\nDone. Working dir:", getwd(), "\n")

