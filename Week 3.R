# load libraries
library(tidyverse)
library(broom)
library(effectsize)
library(rstatix)
library(car)
library(lmtest)
library(BayesFactor)

#load dataset
stores <- read_csv("MM333_week3_regression.csv", show_col_types = FALSE) %>%
  mutate(region = factor(region, levels = c("South", "North", "East", "West")))

# Examine the structure of the dataset
glimpse(stores)
# Obtain descriptive statistics
summary(stores)
# Count the number of observations
nrow(stores)
# Count the number of stores in each region
table(stores$region)
# Check for missing values
colSums(is.na(stores))
# Check for duplicate store IDs
sum(duplicated(stores$store_id))

# scatterplot
ggplot(stores, aes(x = advertising_spend_k,
                   y = sales_revenue_k)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Advertising Expenditure and Sales Revenue",
    x = "Advertising expenditure (£1,000s)",
    y = "Sales revenue (£1,000s)"
  ) +
  theme_minimal()

# Fit the simple linear regression model
simple <- lm(
  sales_revenue_k ~ advertising_spend_k,
  data = stores
)

# Examine the regression results
summary(simple)

# Display the regression results
summary(simple)

# Obtain 95% confidence intervals
confint(simple, level = 0.95)

# Create a new observation
new_store <- data.frame(
  advertising_spend_k = 15
)

# Predict sales revenue
predict(simple, newdata = new_store)

# Obtain a 95% confidence interval
predict(
  simple,
  newdata = new_store,
  interval = "confidence",
  level = 0.95
)

# Obtain a 95% prediction interval
predict(
  simple,
  newdata = new_store,
  interval = "prediction",
  level = 0.95
)
# Examine the residuals versus fitted values plot
plot(simple, which = 1)

# Load the required package
library(car)
# Perform the Durbin-Watson test
durbinWatsonTest(simple)

# Examine the Q-Q plot
plot(simple, which = 2)

# Option with 95%CI
qqPlot(simple$residuals)

# Perform the Shapiro-Wilk test
shapiro.test(simple$residuals)

# Examine the Scale-Location plot
plot(simple, which = 3)

# Load the required package
library(lmtest)

# Perform the Breusch-Pagan test
bptest(simple)

# Examine Cook's distance
plot(simple, which = 4)

# Calculate Cook's distance for each observation
cooks_d <- cooks.distance(simple)

# Calculate the screening threshold
threshold <- 4 / nobs(simple)

# Display the threshold
threshold

# Identify observations exceeding the threshold
which(cooks_d > threshold)

# Fit the multiple regression model
multiple <- lm(
  sales_revenue_k ~ advertising_spend_k + region,
  data = stores
)

# Examine the regression results
summary(multiple)

# Obtain 95% confidence intervals
confint(multiple)

# Compare R-squared and adjusted R-squared
summary(simple)$r.squared
summary(multiple)$r.squared

summary(simple)$adj.r.squared
summary(multiple)$adj.r.squared

# Compare AIC
AIC(simple, multiple)

# Calculate VIF
library(car)

vif(multiple)

# Fit the full multiple regression model
full <- lm(
  sales_revenue_k ~ advertising_spend_k +
    region +
    store_size_sq_m +
    training_hours +
    wait_time_minutes,
  data = stores
)

# Examine the regression results
summary(full)

# Obtain the AIC
AIC(full)

# Perform bidirectional stepwise regression
selected <- step(
  full,
  direction = "both",
  trace = 1
)

# Display the final model
formula(selected)

# Examine the regression results
summary(selected)

# Compare AIC values
AIC(simple, multiple, full, selected)

# Create a summary table
model_comparison <- tibble(
  Model = c("Simple", "Multiple", "Full", "Stepwise"),
  R_squared = c(
    summary(simple)$r.squared,
    summary(multiple)$r.squared,
    summary(full)$r.squared,
    summary(selected)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(simple)$adj.r.squared,
    summary(multiple)$adj.r.squared,
    summary(full)$adj.r.squared,
    summary(selected)$adj.r.squared
  ),
  AIC = c(
    AIC(simple),
    AIC(multiple),
    AIC(full),
    AIC(selected)
  )
)

# Display the comparison
model_comparison

# Check the number of observations
nobs(simple)
nobs(multiple)
nobs(full)
nobs(selected)

# Calculate BIC for both models
BIC(full)
BIC(selected)

# Calculate the approximate Bayes Factor
BF <- exp((BIC(full) - BIC(selected)) / 2)

# Display the result
BF

nobs(simple)
nobs(multiple)
nobs(full)
nobs(selected)
# Examine correlations between numeric predictors
stores %>% 
  select(
    advertising_spend_k,
    store_size_sq_m,
    training_hours,
    wait_time_minutes
  ) %>% 
  cor()
# Check multicollinearity in the full model
vif(full)

# Check multicollinearity in the selected model
vif(selected)

# Create a summary table of the regression models
model_summary <- tibble(
  Model = c("Simple", "Multiple", "Full", "Stepwise"),
  R_squared = c(
    summary(simple)$r.squared,
    summary(multiple)$r.squared,
    summary(full)$r.squared,
    summary(selected)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(simple)$adj.r.squared,
    summary(multiple)$adj.r.squared,
    summary(full)$adj.r.squared,
    summary(selected)$adj.r.squared
  ),
  AIC = c(
    AIC(simple),
    AIC(multiple),
    AIC(full),
    AIC(selected)
  )
)

# Display the results
model_summary

# Display regression coefficients and test statistics
summary(simple)

# Obtain 95% confidence intervals
confint(simple)
