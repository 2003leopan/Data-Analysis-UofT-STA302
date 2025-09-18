ICOR_data <- read.csv("C:/Users/2003l/Downloads/Life Expectancy Data.csv")
# ICOR_nigeria_data <- data[data$Country == "Nigeria", ]
# ggplot(nigeria_data, aes(x = GDP, y = `Life.expectancy`)) + 
#   geom_point() + 
#   labs(title = "Life Expectancy vs. GDP in Nigeria", 
#        x = "GDP", 
#        y = "Life Expectancy")
GDP_data <- read.csv("C:/Users/2003l/Downloads/life-expectancy-vs-gdp-per-capita.csv")
Health_exp_data <- read.csv("C:/Users/2003l/Downloads/HEALTH EXP.csv")
install.packages("dplyr")
library(dplyr)
install.packages(c("tidyverse", "readr"))
library(tidyverse)
library(readr)
filtered1_health_data <- Health_exp_data %>% 
  select(Country, Year, `Life.expectancy`, `health.expenditure.per.capita`)
Health_exp_data <- Health_exp_data %>% 
  rename(
    life_expectancy = Life.expectancy,
    health_expenditure = health.expenditure.per.capita
  )
filtered_health <- Health_exp_data %>% 
  select(Country, 
         Year,  
         life_expectancy, 
         health_expenditurecolnames(ICOR_data)
  )

filtered_health_canada <- filtered_health %>%
  filter(Country == "Canada" & !is.na(health_expenditure))

install.packages("ggrepel")
library(ggrepel) # Load the ggrepel package

ggplot(filtered_health_canada, aes(x = health_expenditure, 
                                   y = life_expectancy)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text_repel(aes(label = Year), size = 3) + 
  labs(title = "Life Expectancy vs. Health Expenditure Per Capita (Canada)",
       x = "Health Expenditure Per Capita (PPP, Current Int. $)",
       y = "Life Expectancy (year)") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

model1 <- lm(life_expectancy ~ health_expenditure, data = filtered_health_canada)
#check for homoscedasticity (constant variance of residuals)
plot(model1, which=3)
# Histogram of residuals
hist(model1$residuals)

# Q-Q Plot
qqnorm(model1$residuals)
qqline(model1$residuals)


# Plot residuals against the order of observations
plot(model1$residuals, type = "l")

# Durbin-Watson test
library(car)
durbinWatsonTest(model1)

filtered_health_canada2<- filtered_health_canada[1:(nrow(filtered_health_canada)-5), ]


set.seed(123) # For reproducibility
train_index <- sample(1:nrow(filtered_health_canada2), size = 0.7*nrow(filtered_health_canada2))
train_data <- filtered_health_canada2[train_index,]
test_data <- filtered_health_canada2[-train_index,]

# Building the model (example with health expenditure)
model2 <- lm(life_expectancy ~ health_expenditure, data = train_data)

# Making predictions on the testing set
predictions <- predict(model2, newdata = test_data)

# Evaluating performance
install.packages("Metrics")
library(Metrics)
rmse <- rmse(test_data$life_expectancy, predict(model1, newdata = test_data))
r_squared <- summary(model2)$r.squared

# Print the results
print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Check for missing values in test_data
print(sum(is.na(test_data$life_expectancy)))  # Check Life.expectancy column
print(sum(is.na(test_data$health_expenditure))) # Check health.expenditure.per.capita

# Handle missing values (Choose one of the following methods)

# 1. Remove rows with missing values
test_data <- test_data[complete.cases(test_data), ] 

predictions <- predict(model2, newdata = test_data)

# Evaluating performance
rmse <- sqrt(mean((predictions - test_data$life_expectancy)^2))
r_squared <- summary(model2)$r.squared

# Print the results
print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))


filtered_ICOR_data <- ICOR_data %>% 
  select(Country, Year, `Life.expectancy`, `Income.composition.of.resources`)

filtered_ICOR_canada <- ICOR_data_Canada[, c("Life.expectancy", "Income.composition.of.resources")]

# Create the scatter plot
ggplot(ICOR_data, aes(x = `Income composition of resources`, y = `Life expectency`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a trendline
  geom_text_repel(aes(label = Year), size = 3) +  # Add year labels with repulsion
  labs(title = "Life Expectancy vs. Income Composition of Resources (Canada)",
       x = "Income Composition of Resources (ICOR)",
       y = "Life Expectancy (Years)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

print(colnames(ICOR_data))

# Remove data for 2008
ICOR_data_updated <- ICOR_data %>% 
  filter(Year != 2008, Year != 2007, Year != 2006)
  filter()
  filter()
  
ICOR_data_Canada <- ICOR_data_updated[ICOR_data_updated$Country == "Canada", ]

set.seed(123) # For reproducibility
train_index <- sample(1:nrow(filtered_ICOR_canada), size = 0.7*nrow(filtered_ICOR_canada))
train_data <- filtered_ICOR_canada[train_index,]
test_data <- filtered_ICOR_canada[-train_index,]

# --- Building the Model ---

model_ICOR <- lm(Life.expectancy ~ Income.composition.of.resources, data = train_data)

# --- Handling Missing Values (Choose one of the following methods) ---

# 1. Remove rows with missing values
test_data <- test_data[complete.cases(test_data), ] 


predictions <- predict(model_ICOR, newdata = test_data)

# --- Evaluating Performance ---

rmse <- sqrt(mean((predictions - test_data$Life.expectancy)^2))
r_squared <- summary(model_ICOR)$r.squared

# --- Print the Results ---

print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))







# Recreate the plot with the updated data
ggplot(ICOR_data_updated, aes(x = `Income composition of resources`, y = `Life expectency`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text_repel(aes(label = Year), size = 3) +
  labs(title = "Life Expectancy vs. Income Composition of Resources (Canada)",
       x = "Income Composition of Resources (ICOR)",
       y = "Life Expectancy (Years)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


model_updated <- lm(`Life expectency` ~ `Income composition of resources`, 
                    data = ICOR_data_updated)

# View the model summary
summary(model_updated)




GDP_data <- read.csv("C:/Users/2003l/Downloads/GDP.csv")

ggplot(GDP_data, aes(x = GDP, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a trendline
  geom_text_repel(aes(label = Year), size = 3) +  # Add year labels with repulsion
  labs(title = "Life Expectancy vs. GDP (Canada)",
       x = "GDP (USD)",
       y = "Life Expectancy (Years)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))






gdp_data_cleaned <- GDP_data[, c("Life.expectancy", "GDP")]


set.seed(123) # For reproducibility
train_index <- sample(1:nrow(gdp_data_cleaned), size = 0.7*nrow(gdp_data_cleaned))
train_data <- gdp_data_cleaned[train_index,]
test_data <- gdp_data_cleaned[-train_index,]

# --- Building the Model ---

model_GDP <- lm(Life.expectancy ~ GDP, data = train_data)

# --- Handling Missing Values (Choose one of the following methods) ---

# 1. Remove rows with missing values
test_data <- test_data[complete.cases(test_data), ] 
predictions <- predict(model_GDP, newdata = test_data)

# --- Evaluating Performance ---

rmse <- sqrt(mean((predictions - test_data$Life.expectancy)^2))
r_squared <- summary(model_GDP)$r.squared

# --- Print the Results ---

print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))








gdp_model1 <- lm(`Life.expectancy` ~ GDP, 
                    data = GDP_data)
summary(gdp_model1)
install.packages("tydiverse")
install.packages("ggplot2")
install.packages("ggrepel")
library(tidyverse)
library(ggplot2)
library(ggrepel)
GINI_data <- read.csv("C:/Users/2003l/Downloads/gini.csv")

ggplot(GINI_data, aes(x = Gini.index, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Optional: Add a trendline
  geom_text_repel(aes(label = Year), size = 3) +  # Add year labels with repulsion
  labs(title = "Life Expectancy vs. Gini index (Canada)",
       x = "Gini index (USD)",
       y = "Life Expectancy (Years)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))



# Create Gini category variable (using equal intervals in this example)
GINI_data$Gini_Category <- cut(GINI_data$Gini.index, breaks = c(30, 32, 34, 36), 
                               labels = c("Low", "Medium", "High"))

# Perform ANOVA
model <- aov(Life.expectancy ~ Gini_Category, data = GINI_data)
summary(model)

# Post-hoc test (if ANOVA is significant)
TukeyHSD(model)

boxplot(Life.expectancy ~ Gini_Category, 
        data = GINI_data,
        xlab = "Gini Index Category",
        ylab = "Life Expectancy",
        main = "Life Expectancy by Gini Index Category in Canada (2000-2015)", # Optional title 
        col = c("lightblue", "lightgreen", "lightpink") # Optional colors
)



plot(GINI_data, which=3) 




# Install the package (if not installed)
install.packages("writexl")

# Load the library
library(writexl)

# Export Data Frame to XLSX
write_xlsx(filtered_health_canada, "filtered_health_canada.xlsx")

library(dplyr)
filtered_ICOR_data_Canada <- ICOR_data %>%
  filter(Country == "Canada" & !is.na(Income.composition.of.resources))

write_xlsx(filtered_health, "filtered_health.xlsx")