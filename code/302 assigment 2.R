"data <- data.frame(
  x = c(31.50310, 51.53221, 36.35908, 55.32070, 57.61869, 21.82226, 41.12422, 55.69676, 42.05740, 38.26459, 58.27333, 38.13337, 47.10283, 42.90534, 24.11699),
  y = c(71.58003, 109.06011, 73.78492, 117.04410, 119.29180, 46.50887, 86.81249, 114.34151, 87.65702, 80.27910, 118.17328, 82.94231, 99.51240, 88.53440, 55.74160)
)

mean_x <- mean(data$x)
mean_y <- mean(data$y)

sum_x_minus_mean_x <- sum((data$x - mean_x)*
                            (data$y - mean_y))
beta_hat_1 <- sum_x_minus_mean_x / sum((data$x - mean_x)**2)

beta_hat_0 <- mean_y - beta_hat_1 * mean_x

data$y_hat <- beta_hat_0 + beta_hat_1 * data$x

data$residual_square_y <- (data$y_hat - data$y) ** 2

MSE <- sum(data$residual_square_y) / 13




matrix_A <- matrix(c(1, 57.61869, 55.99300,
                     1, 21.82226, 29.84351,
                     1, 41.12422, 21.68238,
                     1, 55.69676, 33.11683), 
                   nrow = 4, byrow = TRUE)

transpose_matrix_A <- t(matrix_A)

XT_TIMES_X <- transpose_matrix_A %*% matrix_A

inverse_XT_TIMES_X <- solve(XT_TIMES_X)

hat_matrix <- matrix_A %*% inverse_XT_TIMES_X %*% transpose_matrix_A


data <- data.frame(
  x = c(31.50310, 51.53221, 36.35908, 55.32070, 57.61869, 21.82226, 41.12422, 55.69676, 42.05740, 38.26459, 58.27333, 38.13337, 47.10283, 42.90534, 24.11699),
  x_2 = c(55.99300, 29.84351, 21.68238, 33.11683, 58.18015, 55.58157, 47.71214, 45.62027, 59.77079, 46.22823, 48.34122, 41.76264, 43.76568, 31.56639, 25.88455),
  y = c(76.31838, 57.60531, 36.92869, 63.17989, 87.04391, 65.35706, 68.83830, 72.41664, 80.34171, 65.11045, 75.10450, 63.50490, 68.62384, 51.74278, 41.45067)
)

mean_y <- mean(data$y)
mean_x <- mean(data$x)
mean_x_2 <- mean(data$x_2)
cov_xy <- sum((data$x - mean(data$x)) * (data$y - mean(data$y))) / (length(data$x) - 1)
sample_cov_x_x<- sum((data$x - mean_x)**2)

# Calculate the sample mean of y
y_bar <- mean(y)

# Calculate the squared differences from the mean
squared_diff <- (y - y_bar)^2

# Calculate the sum of squared differences
sum_squared_diff <- sum(squared_diff)

# Calculate the sample standard deviation of y
sample_std_dev_y <- sqrt(sum_squared_diff / (length(y) - 1))

# Calculate the sample mean of x
mean_x <- mean(x)

# Calculate the squared differences between each x and the sample mean
squared_diff <- (x - mean_x)^2

# Sum up all the squared differences
sum_squared_diff <- sum(squared_diff)

# Calculate the sample variance of x
n <- length(x)
sample_variance_x <- sum_squared_diff / (n - 1)


beta_hat_1 <- (cov_xy - sample_std_dev_y) / sample_variance_x"

"data <- data.frame(
  x = c(31.50310, 51.53221, 36.35908, 55.32070, 57.61869, 21.82226, 41.12422, 55.69676, 42.05740, 38.26459, 58.27333, 38.13337, 47.10283, 42.90534, 24.11699),
  x_2 = c(55.99300, 29.84351, 21.68238, 33.11683, 58.18015, 55.58157, 47.71214, 45.62027, 59.77079, 46.22823, 48.34122, 41.76264, 43.76568, 31.56639, 25.88455),
  y = c(76.31838, 57.60531, 36.92869, 63.17989, 87.04391, 65.35706, 68.83830, 72.41664, 80.34171, 65.11045, 75.10450, 63.50490, 68.62384, 51.74278, 41.45067)
)

# Calculate sample covariance between x and y
cov_xy <- cov(data$x, data$y)

# Calculate sample covariance between x_2 and y
cov_x2y <- cov(data$x_2, data$y)

# Calculate sample variance of x
sample_var_x <- var(data$x)

# Calculate sample variance of x_2
sample_var_x2 <- var(data$x_2)

# Calculate sample standard deviation of y
sample_std_y <- sd(data$y)

# Calculate beta_hat_1
beta_hat_1 <- (cov_xy - sample_std_y) / sample_var_x

# Calculate beta_hat_2
beta_hat_2 <- (cov_x2y - sample_std_y) / sample_var_x2

# Calculate sample means
mean_y <- mean(data$y)
mean_x <- mean(data$x)
mean_x2 <- mean(data$x_2)

# Calculate beta_hat_0
beta_hat_0 <- mean_y - beta_hat_1 * mean_x - beta_hat_2 * mean_x2

# Print beta_hat_0
print(beta_hat_0)
# Print beta_hat_1 and beta_hat_2
print(beta_hat_1)
print(beta_hat_2)
"
"
# Degrees of freedom
df <- 12

# Significance level
alpha <- 0.05

# Calculate critical value
t_critical <- qt(1 - alpha/2, df)
"
# Create a data frame

data <- data.frame(
  x = c(31.50310, 51.53221, 36.35908, 55.32070, 57.61869, 21.82226, 41.12422, 55.69676, 42.05740, 38.26459, 58.27333, 38.13337, 47.10283, 42.90534, 24.11699),
  x2 = c(55.99300, 29.84351, 21.68238, 33.11683, 58.18015, 55.58157, 47.71214, 45.62027, 59.77079, 46.22823, 48.34122, 41.76264, 43.76568, 31.56639, 25.88455),
  y = c(76.31838, 57.60531, 36.92869, 63.17989, 87.04391, 65.35706, 68.83830, 72.41664, 80.34171, 65.11045, 75.10450, 63.50490, 68.62384, 51.74278, 41.45067)
)

# Optionally, set column names
colnames(data) <- c("x", "x2", "y")

# Perform linear regression
model <- lm(y ~ x + x2, data = data)

# Get the estimated coefficients
coefficients <- coef(model)
beta_hat_0 <- coefficients[1]  # Intercept coefficient (beta hat 0)
beta_hat_2 <- coefficients[3]  # Coefficient for x2 (beta hat 2)

# Print the estimated coefficients
print(paste("Estimated beta hat 0:", beta_hat_0))
print(paste("Estimated beta hat 2:", beta_hat_2))