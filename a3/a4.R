
new_model <- lm(y ~ x7 + x8, data = table.b1)

coefficients <- summary(new_model)$coefficients
beta_7 <- coefficients["x7", "Estimate"]
SE_beta_7 <- coefficients["x7", "Std. Error"]

df <- df.residual(new_model)

t_crit <- qt(0.975, df)

CI_beta_7 <- c(beta_7 - t_crit * SE_beta_7, 
               beta_7 + t_crit * SE_beta_7)
print(CI_beta_7)

# new obs
new_obs <- data.frame(x7 = 56.0, x8 = 2100)

# predict
y_hat <- predict(new_model, new_obs)

SE_y_hat <- predict(new_model, new_obs, se.fit = TRUE)$se.fit

CI_y_hat <- c(y_hat - t_crit * SE_y_hat,
              y_hat + t_crit * SE_y_hat)
print(CI_y_hat)