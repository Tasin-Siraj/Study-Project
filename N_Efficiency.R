rm(list = ls())
setwd("/Users/aungontasin/Desktop/Zalf/Dataset/Analysis/Data")
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(ModelMetrics)
set.seed(42)

soil_data <- read.csv("soil_variables.csv")

soil_data <- soil_data %>% select(-N_Fertilization_kg_ha,-Na_eff, -NH4_N, -NO3_N)
crop_types <- split(soil_data, soil_data$CropType_Long)

wheat <- crop_types$`Winter Soft Wheat`
wheat <- wheat %>% dplyr::select(where(is.numeric))

rye <- crop_types$`Winter Rye`
rye <- rye %>% dplyr::select(where(is.numeric))

barley <- crop_types$`Winter Barley`
barley <- barley %>% dplyr::select(where(is.numeric))


wheat <- na.omit(wheat)
wheat <- wheat %>% select(-Ert_dt_ha, -RP_Go)
shuffle_wheat <- sample(1:nrow(wheat))
wheat <- wheat[shuffle_wheat, ]
trainIndex <- createDataPartition(wheat$N_Efficiency, p = 0.8, list = FALSE)
trainData <- wheat[trainIndex, ]
testData <- wheat[-trainIndex, ]
model <- rpart(N_Efficiency ~., data = trainData, method = "anova")
rpart.plot(model)

actual_yield <- testData$N_Efficiency
predictions <- predict(model, newdata = testData)

R2 <- cor(actual_yield, predictions)^2
print(R2)
data <- data.frame(actual = actual_yield, predicted = predictions)
ggplot(data, aes(x = actual, y = predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "Actual", y = "Predicted", title = "Model Performance") +
    theme_minimal()

Imp_var <- varImp(model)
sorted_Ipm_var <- Imp_var %>% arrange(desc(Overall)) 
print(sorted_Ipm_var)
var_plot <- data.frame(variable = row.names(sorted_Ipm_var),
                       importance = sorted_Ipm_var$Overall)
ggplot(var_plot, aes(x = variable, y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    xlab("Variable") +
    ylab("Importance") +
    ggtitle("Variable Importance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



rye <- na.omit(rye)
rye <- rye %>% select(-RP_Go, -Ert_dt_ha)
shuffle_rye <- sample(1:nrow(rye))
rye <- rye[shuffle_rye, ]
trainIndex <- createDataPartition(rye$N_Efficiency, p = 0.8, list = FALSE)
trainData <- rye[trainIndex, ]
testData <- rye[-trainIndex, ]
model <- rpart(N_Efficiency ~., data = trainData, method = "anova")
rpart.plot(model)

actual_yield <- testData$N_Efficiency
predictions <- predict(model, newdata = testData)

R2 <- cor(actual_yield, predictions)^2
print(R2)
data <- data.frame(actual = actual_yield, predicted = predictions)
ggplot(data, aes(x = actual, y = predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
    labs(x = "Actual", y = "Predicted", title = "Model Performance") +
    theme_minimal()

Imp_var <- varImp(model)
sorted_Ipm_var <- Imp_var %>% arrange(desc(Overall)) 
print(sorted_Ipm_var)


var_plot <- data.frame(variable = row.names(sorted_Ipm_var),
                       importance = sorted_Ipm_var$Overall)
ggplot(var_plot, aes(x = variable, y = importance)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    xlab("Variable") +
    ylab("Importance") +
    ggtitle("Variable Importance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


barley <- na.omit(barley)
barley <- barley %>% select(-RP_Go, -Ert_dt_ha)
shuffle_barley <- sample(1:nrow(barley))
barley <- barley[shuffle_barley, ]
trainIndex <- createDataPartition(barley$N_Efficiency, p = 0.8, list = FALSE)
trainData <- barley[trainIndex, ]
testData <- barley[-trainIndex, ]
model <- rpart(N_Efficiency ~., data = trainData, method = "anova")
rpart.plot(model)

actual_yield <- testData$N_Efficiency
predictions <- predict(model, newdata = testData)

R2 <- cor(actual_yield, predictions)^2
print(R2)
data <- data.frame(actual = actual_yield, predicted = predictions)
ggplot(data, aes(x = actual, y = predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "purple") +
    labs(x = "Actual", y = "Predicted", title = "Model Performance") +
    theme_minimal()

Imp_var <- varImp(model)
sorted_Ipm_var <- Imp_var %>% arrange(desc(Overall)) 
print(sorted_Ipm_var)

var_plot <- data.frame(variable = row.names(sorted_Ipm_var),
                       importance = sorted_Ipm_var$Overall)
ggplot(var_plot, aes(x = variable, y = importance)) +
    geom_bar(stat = "identity", fill = "lightgreen") +
    xlab("Variable") +
    ylab("Importance") +
    ggtitle("Variable Importance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
