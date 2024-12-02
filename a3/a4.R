library(MPV)

model_3.1 <- lm(y ~ x2 + x7 + x8, data = table.b1)
summary_model_3.1 <- summary(model_3.1)

anova_3.1 <- anova(model_3.1)
anova_3.1

t_stats <- summary_model_3.1$coefficients[, "t value"]
t_stats

# R squared
r_squared <- summary_model_3.1$r.squared
r_squared

# Adjusted R squared
adj_r_squared <- summary_model_3.1$adj.r.squared
adj_r_squared

model_reduced <- lm(y ~ x2 + x8, data = table.b1)
summary(model_reduced)

model_full <- model_3.1

# Get the residual sum of squares (RSS) for both models
RSS_full <- sum(residuals(model_full)^2)
RSS_reduced <- sum(residuals(model_reduced)^2)

# Get the number of parameters (coefficients) in the full and reduced models
p_full <- length(coef(model_full))  # including the intercept
p_reduced <- length(coef(model_reduced))  # including the intercept

# Get the degrees of freedom for the full model
df_full <- df.residual(model_full)

# Calculate the partial F-statistic
F_statistic <- ((RSS_reduced - RSS_full) / (p_full - p_reduced)) / (RSS_full / df_full)
F_statistic

