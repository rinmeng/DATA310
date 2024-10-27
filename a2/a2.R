x <- c(1.8, 0.1, 0.2, 1.4, 2.4, 0.9, 0.9, 0.6, 0.2, 3.7)
y <- c(2.0, 0.4, 1.9, 2.7, 3.2, 1.1, 1.5, 1.1, 0.8, 4.1)

# Creating a data frame
df2 <- data.frame(x, y)

# Fitting the linear model
model <- lm(y ~ 0 + x, data = data)

# Viewing the summary of the model
summary(model)

res <- residuals(model)
res

mean(res)

plot(data$x, data$y,
     main = "Scatterplot with Fitted Line",
     xlab = "x", ylab = "y", pch = 19
)

abline(model, col = "red")


# Data
angle <- c(1.3, 4.0, 2.7, 2.2, 3.6, 4.9, 0.9, 1.1, 3.1)
distance <- c(0.43, 0.84, 0.58, 0.58, 0.70, 1.00, 0.27, 0.29, 0.63)

lm3 <- lm(distance ~ angle)
summary(lm3)
anova(lm3)

# Calculate means
x_bar <- mean(angle)
y_bar <- mean(distance)

# Calculate Sxx and Sxy
Sxx <- sum((angle - x_bar)^2)
Sxy <- sum((angle - x_bar) * (distance - y_bar))

# Calculate slope (beta1) and intercept (beta0)
beta1 <- Sxy / Sxx
beta0 <- y_bar - beta1 * x_bar

# Calculate residuals
residuals <- distance - (beta0 + beta1 * angle)

# Calculate the variance of the residuals
s_squared <- sum(residuals^2) / (length(angle) - 2)

# Standard errors
SE_beta1 <- sqrt(s_squared / Sxx)
SE_beta0 <- sqrt(s_squared * (1 / length(angle) + x_bar^2 / Sxx))

# Critical t-value for 95% CI with 7 degrees of freedom
t_value <- qt(0.975, df = length(angle) - 2)

# Confidence intervals
CI_beta1 <- c(beta1 - t_value * SE_beta1, beta1 + t_value * SE_beta1)
CI_beta0 <- c(beta0 - t_value * SE_beta0, beta0 + t_value * SE_beta0)

# Output the results
cat("95% Confidence Interval for Slope (beta1):", CI_beta1, "\n")
cat("95% Confidence Interval for Intercept (beta0):", CI_beta0, "\n")


# Data
angle <- c(1.3, 4.0, 2.7, 2.2, 3.6, 4.9, 0.9, 1.1, 3.1)
distance <- c(0.43, 0.84, 0.58, 0.58, 0.70, 1.00, 0.27, 0.29, 0.63)

# Calculate means
y_bar <- mean(distance)



# Calculate Sxy and Sxx
Sxy <- sum((angle - mean(angle)) * (distance - y_bar))
Sxx <- sum((angle - mean(angle))^2)

# Calculate beta coefficients
beta1 <- Sxy / Sxx
beta0 <- y_bar - beta1 * mean(angle)

# Predicted values
yhat <- beta0 + beta1 * angle

# Calculate SST
SST <- sum((distance - y_bar)^2)

# Calculate SSE
SSE <- sum((distance - yhat)^2)

# Calculate SSR
SSR <- SST - SSE

# Degrees of freedom
n <- length(distance)
dfR <- 1  # for regression (p - 1)
dfE <- n - 2  # for residual (n - p)
dfT <- n - 1  # total (n - 1)

# Mean Squares
MSR <- SSR / dfR
MSE <- SSE / dfE

# F-statistic
F_statistic <- MSR / MSE

# Print results
cat("SST:", SST, "\n")
cat("SSE:", SSE, "\n")
cat("SSR:", SSR, "\n")
cat("Mean Square Regression (MSR):", MSR, "\n")
cat("Mean Square Error (MSE):", MSE, "\n")
cat("F-statistic:", F_statistic, "\n")

# Predicted value at x0 = 2.5
x0 <- 2.5
y_hat_0 <- beta0 + beta1 * x0

# Calculate residuals and variance (s^2)
residuals <- distance - (beta0 + beta1 * angle)
ssquared <- sum(residuals^2) / (length(angle) - 2)

# Calculate t-value for 95% confidence interval (approximation)
t_value <- 2.3646  # Approximate value for 95% CI with df = 7 (n - 2)

# Confidence Interval for Expected Distance (Mean Response)
semr <- sqrt(ssquared * (1 / length(angle) + (x0 - x_bar)^2 / Sxx))
cimr <- c(y_hat_0 - t_value * semr, y_hat_0 + t_value * semr)

# Prediction Interval for a Single Observation
sepi <- sqrt(ssquared * (1 + 1 / length(angle) + (x0 - x_bar)^2 / Sxx))
piso <- c(y_hat_0 - t_value * sepi, y_hat_0 + t_value * sepi)

# Print results
cat("95% Confidence Interval for Expected Distance at x0 =", x0, ":", cimr, "\n")
cat("95% Prediction Interval for Single Observation at x0 =", x0, ":", piso, "\n")

