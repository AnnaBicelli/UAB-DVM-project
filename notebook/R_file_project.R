library(tidyverse)
library(ggplot2)
library(caret)  
library(boot)   

# Upload the dataset
insurance_data <- read.csv (" insurance_dataset .csv ")
summary(insurance_data)

# Check null values
missing_values <- colSums(is.na(insurance_data))
print(missing_values)  


# Convert categorical variables into factors
insurance_data <- insurance_data %>%
  mutate(
    gender = as.factor(gender),
    smoker = as.factor(smoker),
    region = as.factor(region),
    medical_history = as.factor(medical_history),
    family_medical_history = as.factor(family_medical_history),
    exercise_frequency = as.factor(exercise_frequency),
    occupation = as.factor(occupation),
    coverage_level = as.factor(coverage_level)
  )

# Check the dataset
str(insurance_data)

# Relationship between variables
pairs(~ age + bmi + children + charges, data = insurance_data, main ='Scatterplot Matrix')

# correlation matrix between numeric variables
correlation_matrix <- cor(insurance_data %>%
                            select_if(is.numeric))
print(correlation_matrix)

# Heatmap 
library(reshape2)
library(ggcorrplot)
cor_melted <- melt(correlation_matrix)
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  theme_minimal() 

# Linear regression
full_model <- lm(charges ~ ., data = insurance_data)
summary(full_model)

# Selection of the most important variables 
step_model <- step(full_model, direction = "both")
summary(step_model)

model <- lm(charges ~ age + bmi + children + smoker + region, data = insurance_data)
summary(model)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(model)

# Check R^2
cat("R^2 full model:", summary(full_model)$r.squared, "\n") 
cat("R^2 important variables model:", summary(model)$r.squared, "\n") 

# Resampling techniques
set.seed(123)
num_samples <- 1000

# Bootstrap
bootstrap_fun <- function(data, indices) {
  sample_data <- data[indices, ]
  model_boot <- lm(charges ~ age + bmi + children + smoker + region, data = sample_data)
  return(coef(model_boot))
}
bootstrap_results <- boot(data = insurance_data, statistic = bootstrap_fun, R = num_samples)
print(bootstrap_results)
cat("Confidence Intervals (Percentile Method):\n")
bootstrap_conf_int <- apply(bootstrap_results$t, 2, quantile, probs = c(0.025, 0.975)) 

# Jackknife
jackknife_fn <- function (data,i) {
  data_subset <- data[-i, ]
  model <- lm( charges ~ age + bmi + children + smoker + region ,data = data_subset )
  return ( coef ( model ))
}

n <- nrow(insurance_data)
jackknife_results <- matrix(NA,nrow = n,ncol = length(coef(lm(charges ~ age + bmi + children + smoker + region,data=insurance_data))))

for (i in 1: n) {
  jackknife_results [i , ] <- jackknife_fn ( insurance_data , i )
}
jackknife_estimate <- colMeans( jackknife_results )
jackknife_se <- sqrt ((( n - 1) / n) * rowSums ((t( jackknife_results ) - jackknife_est ) ^2) )

print (" Jackknife Estimates :")
print ( jackknife_estimate )
print (" Jackknife Standard Errors :")
print ( jackknife_se)

smoker_charges <- insurance_data % >%
  group_by( smoker ) % >%
  summarise ( mean_charges = mean(charges))
print(smoker_charges )

ggplot ( insurance_data , aes (x = charges , fill = smoker )) +
  geom_histogram ( bins = 30 , alpha = 0.6 , position = " identity ")


