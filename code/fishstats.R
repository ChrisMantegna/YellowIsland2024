#Friedman test

library(dplyr)
library(tidyr)

data_wide <- data %>%
  pivot_wider(names_from = quadrant, values_from = time_spent)

friedman.test(as.matrix(data_wide[ , -1]), groups = data_wide$trial, blocks = data_wide$fish)

# Post Hoc for Friedman
pairwise.wilcox.test(data$time_spent, data$quadrant, p.adjust.method = "bonferroni")


#Visualize
library(ggplot2)

ggplot(data, aes(x = quadrant, y = time_spent, color = as.factor(fish))) +
  geom_boxplot() +
  facet_wrap(~trial)

#Dirichlet
# Install the rstan package if not already installed
install.packages("rstan")
library(rstan)

# Example data: replace with your actual data
data <- data.frame(
  fish = rep(1:6, each = 4),
  trial = rep(1:4, times = 6),
  q1 = runif(24),
  q2 = runif(24),
  q3 = runif(24),
  q4 = runif(24)
)

# Normalize to proportions
data <- data %>%
  rowwise() %>%
  mutate(total = q1 + q2 + q3 + q4,
         p1 = q1 / total,
         p2 = q2 / total,
         p3 = q3 / total,
         p4 = q4 / total) %>%
  select(fish, trial, p1, p2, p3, p4)

# Stan model
stan_code <- "
data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of categories (quadrants)
  vector[K] y[N]; // observations (proportions)
  vector[K] alpha; // Dirichlet parameters
}
parameters {
  simplex[K] theta; // Dirichlet distribution parameter
}
model {
  theta ~ dirichlet(alpha);
  for (n in 1:N)
    y[n] ~ dirichlet(theta);
}
"

# Prepare data for Stan
stan_data <- list(
  N = nrow(data),
  K = 4,
  y = data[, 3:6],
  alpha = rep(1, 4) # Non-informative prior
)

# Fit the model
fit <- stan(model_code = stan_code, data = stan_data, iter = 2000, chains = 4)

# Print results
print(fit)
