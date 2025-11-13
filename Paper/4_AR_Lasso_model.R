#AR Model

# Install and load the bigtime package if not already installed
# install.packages("bigtime")
library(bigtime)

# Simulate a VAR model with Lasso sparsity (can be adapted for AR)
# For a single time series, this effectively becomes a high-order AR model with Lasso
#set.seed(123)

# sim_data <- simVAR(
#   VAR_type = "lasso",
#   sparsity_pattern = "lasso",
#   num_zero = 0.5, # Percentage of zero coefficients
#   T = 100,        # Number of observations
#   N = 1           # Number of time series (for AR, N=1)
# )

# Fit a sparse AR model using bigtime with Lasso regularization
# 'y' is the time series, 'p' is the maximum AR order


lasso_ar_model <- bigtime(sim_data$Y, p = 25, VAR_type = "lasso")

# Print the summary of the fitted model
summary(lasso_ar_model)