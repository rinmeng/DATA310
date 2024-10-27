library(DAAG)
data("fossum", package = "DAAG")

# Scatter plot of hdlngth (head length) against totlngth (total length)
plot(fossum$totlngth, fossum$hdlngth,
  xlab = "Total Length (cm)",
  ylab = "Head Length (mm)",
  main = "Head Length vs Total Length in Female Possums",
  pch = 16, col = "black"
)

model <- lm(hdlngth ~ totlngth, data = fossum)

# Output the model summary to see coefficients
summary(model)
anova(model)

plot(fossum$totlngth, fossum$hdlngth,
  xlab = "Total Length (cm)",
  ylab = "Head Length (mm)",
  main = "Head Length vs Total Length in Female Possums",
  pch = 16, col = "black"
)

abline(model, col = "red")


# F-statistic from ANOVA table
F_statistic <- 63.38

# Degrees of freedom
dfR <- 1 # Regression
dfE <- 41 # Error

# Critical F-value for a two-tailed test at alpha = 0.05
alpha <- 0.05
F_critical <- qf(1 - alpha, dfR, dfE)

# Output results
cat("Calculated F-statistic:", F_statistic, "\n")
cat("Critical F-value (two-tailed):", F_critical, "\n")

# Conclusion
if (F_statistic > F_critical) {
  cat("Reject the null hypothesis: There is a significant relationship between hdlngth and totlngth.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant relationship between hdlngth and totlngth.\n")
}


new_data <- data.frame(totlngth = 85)

predict(model, new_data, interval = "prediction", level = 0.95)

plot(fitted(model), resid(model),
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values",
  pch = 16, col = "black"
)
abline(h = 0, col = "red", lty = 2)
