library(tidyverse)
library(GGally)
library(ggplot2)
library(leaps)

data <- read.csv('insurance.csv', header=TRUE)
data = mutate(data, norm_charges = log(charges))

# Figure.1
data <- transform(data, sex = as.factor(sex), 
                  children = as.factor(children), 
                  smoker = as.factor(smoker),
                  region = as.factor(region))
hist(data$charges, xlab = "Non-Normalized Charges (USD)", main = paste("Histogram of Non-Normalized Charges"))

# Figure.2
hist(data$norm_charges, xlab = "Normalized Charges (USD)", main = paste("Histogram of Normalized Charges"))

# Figure.3
quant_data <- subset(data, select = c(age, bmi, norm_charges))
pairs(quant_data)

# Figure.4
ggplot(data, aes(age, norm_charges, col = smoker))+
  geom_point() +
  labs( x = "Age", y = "Normalized Charges (USD)") + 
  ggtitle("Scatterplot of Age versus Normalized Charges") +
  theme(text = element_text(hjust = 0.5, size = 15)) +
  theme(plot.title = element_text(hjust= 0.5, size= 20))

# Figure.5
ggplot(data, aes(bmi, norm_charges, col = smoker)) +
  geom_point() + 
  labs( x = "BMI (kg/m^2)", y = "Normalized Charges (USD)") + 
  ggtitle("Scatterplot of BMI versus Normalized Charges") +
  theme(text = element_text(hjust = 0.5, size = 15)) +
  theme(plot.title = element_text(hjust= 0.5, size= 20))


# Figure.6
s <- regsubsets(norm_charges~.+age*smoker+bmi*smoker, data=subset(data, select = -c(charges)), method="exhaustive", nvmax = 14)
s.sum <- summary(s)
matrix_sum_df <- data.frame(
  num_param = c(2:15),
  Adj.R2 = s.sum$adjr2,
  CP = s.sum$cp,
  BIC = s.sum$bic)
matrix_sum_df

# Figure.7
matrix_sum_df <- matrix_sum_df[3:14,]
plot(matrix_sum_df$num_param, matrix_sum_df$Adj.R2, type = "b", frame = FALSE, pch = 19, col = "red", xlab="Number of Parameters", ylab = "Adjusted R-squared")
title("Adjusted R-sqaured v.s. Number of Parameters")
plot(matrix_sum_df$num_param, matrix_sum_df$CP, type = "b", frame = FALSE, pch = 19, col = "red", xlab="Number of Parameters", ylab = "CP")
title("CP v.s. Number of Parameters")
plot(matrix_sum_df$num_param, matrix_sum_df$BIC, type = "b", frame = FALSE, pch = 19, col = "red", xlab="Number of Parameters", ylab = "BIC")
title("BIC v.s. Number of Parameters")

# model selection
s.sum

# 14-parameter model
optimal_lm_14 <- lm(norm_charges~age+sex+children+smoker+region+age*smoker+bmi*smoker, data=data)
summary(optimal_lm_14)

# 13-parameter model
new_data <- data
new_data$region <- as.character(new_data$region)
new_data$region[new_data$region == "northwest" | new_data$region == "northeast"] <- "north"
new_data$region <- as.factor(new_data$region)
optimal_lm_13 <- lm(norm_charges~age+sex+children+smoker+region+age*smoker+bmi*smoker, data=new_data)
summary(optimal_lm_13)

# residual plot
pred_data = data
pred_data$predict = exp(optimal_lm$fitted.values)
pred_data$diff_sq = (pred_data$charges - pred_data$predict)^2
pred_data$diff <- pred_data$charges- pred_data$predict
pred_data$predict_norm = optimal_lm$fitted.values
pred_data$diff_norm <- pred_data$norm_charges - pred_data$predict_norm
plot(diff~predict, data = pred_data)

# Figure.8
coef_df <- data.frame(sort(abs(optimal_lm_13$coefficients), decreasing = TRUE))
colnames(coef_df) <- c("AbsCoeffs")
coef_df


