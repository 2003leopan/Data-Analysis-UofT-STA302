#4a


# Observed data
n <- 30  # Sample size
y <- 27  # Number of successes

# Sample proportion
pi_hat <- y / n

# Significance level for 95% CI
alpha <- 0.05
z <- qnorm(alpha/2) 

# Wald Confidence Interval
se_wald <- sqrt(pi_hat * (1 - pi_hat) / n) 
ci_wald <- pi_hat + c(-1, 1) * z * se_wald

# Score Confidence Interval 
a <- pi_hat * (n / (n + z^2)) + (1/2) * (z^2 / (n + z^2))
b <- z / sqrt(n + z^2) * sqrt(pi_hat * (1 - pi_hat) * (n / (n + z^2)) + (1/4) * (z^2 / (n + z^2)))
ci_score <- a + c(-1, 1) * b 






#4b
# Simulation parameters
N <- 100000  # Number of simulations
n <- 30       # Sample size for each binomial experiment
true_pi <- 0.9 # True probability of success

# Function to calculate Wald CI (basically repeating the steps for the previous
# part)
wald_ci <- function(y, n, z) {
  pi_hat <- y / n
  se_wald <- sqrt(pi_hat * (1 - pi_hat) / n)
  return(pi_hat + c(-1, 1) * z * se_wald)
}

# Function to calculate Score CI
score_ci <- function(y, n, z) {
  pi_hat <- y / n
  a <- pi_hat * (n / (n + z^2)) + (1/2) * (z^2 / (n + z^2))
  b <- z / sqrt(n + z^2) * sqrt(pi_hat * (1 - pi_hat) * (n / (n + z^2)) + 
                                  (1/4) * (z^2 / (n + z^2)))
  return(a + c(-1, 1) * b)
}

# Simulate the data
set.seed(123) # Set a seed for reproducibility
sim_data <- rbinom(N, size = n, prob = true_pi)

# Calculate confidence intervals
alpha <- 0.05
z <- qnorm(1 - alpha/2) 

wald_intervals <- t(sapply(sim_data, wald_ci, n = n, z = z))
score_intervals <- t(sapply(sim_data, score_ci, n = n, z = z))

# Calculate coverage probabilities
wald_coverage <- mean(wald_intervals[,1] <= true_pi & wald_intervals[,2] >= true_pi)
score_coverage <- mean(score_intervals[,1] <= true_pi & score_intervals[,2] >= true_pi)













# Simulation parameters
N <- 100000
n <- 30
true_pi <- 0.9
alph <- 0.05 

# Generate data
y <- rbinom(N, n, true_pi)
pihat <- y / n

# Wald Confidence Interval
L.wald <- pihat - qnorm(1 - alph/2) * sqrt((pihat * (1 - pihat)) / n)
U.wald <- pihat + qnorm(1 - alph/2) * sqrt((pihat * (1 - pihat)) / n)
pi_in_wald_CI <- (true_pi > L.wald) * (true_pi < U.wald)
observedConfLevel_WaldCI <- mean(pi_in_wald_CI)

# Score Confidence Interval
nprime <- n + (qnorm(1 - alph/2))^2
w1 <- n / nprime
w2 <- ((qnorm(1 - alph/2))^2) / nprime
midpoint <- pihat * w1 + 0.5 * w2
L.score <- midpoint - qnorm(1 - alph/2) * sqrt((1 / nprime) * (pihat * (1 - pihat) * w1 + 0.25 * w2))
U.score <- midpoint + qnorm(1 - alph/2) * sqrt((1 / nprime) * (pihat * (1 - pihat) * w1 + 0.25 * w2))
pi_in_score_CI <- (true_pi > L.score) * (true_pi < U.score)
observedConfLevel_scoreCI <- mean(pi_in_score_CI)

# Print results
cat("Wald Coverage Probability:", observedConfLevel_WaldCI, "\n")
cat("Score Coverage Probability:", observedConfLevel_scoreCI, "\n")


#6a

install.packages("rpart")
library(rpart)
data(kyphosis)


# Jittered scatterplot for Age
plot(kyphosis$Age, kyphosis$Kyphosis, 
     xlab = "Age", ylab = "Kyphosis", 
     main = "Kyphosis vs. Age", pch = 19)

# Jittered scatterplot for Number
plot(kyphosis$Number, kyphosis$Kyphosis, 
     xlab = "Number", ylab = "Kyphosis", 
     main = "Kyphosis vs. Number", pch = 19)

# Jittered scatterplot for Start
plot(kyphosis$Start, kyphosis$Kyphosis, 
     xlab = "Start", ylab = "Kyphosis", 
     main = "Kyphosis vs. Start", pch = 19)




fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(0.65, 0.35), split = "information"))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,2), xpd = TRUE)
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)















# Fit the GLM with a binomial distribution and logit link
model <- glm(Kyphosis ~ Age + Number + Start, data = kyphosis, family = binomial)

# Display the summary of the model
summary(model)



#ODD RATIO
exp(coef(model))


#ESTIMATED PROBABILITY
median_age <- median(kyphosis$Age)
median_number <- median(kyphosis$Number)
median_start <- median(kyphosis$Start)

# Create a new data frame with the median values
new_data <- data.frame(Age = median_age, Number = median_number, Start = median_start)

# Predict the probability of kyphosis
predict(model, newdata = new_data, type = "response")