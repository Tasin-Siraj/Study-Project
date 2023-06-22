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

dim(soil_data)
summary(soil_data)
colnames(soil_data)
str(soil_data)

soil_data <- soil_data %>% select(-N_Fertilization_kg_ha,-Na_eff, -NH4_N, -NO3_N)
crop_types <- split(soil_data, soil_data$CropType_Long)

wheat <- crop_types$`Winter Soft Wheat`
wheat <- wheat %>% dplyr::select(where(is.numeric))

rye <- crop_types$`Winter Rye`
rye <- rye %>% dplyr::select(where(is.numeric))

barley <- crop_types$`Winter Barley`
barley <- barley %>% dplyr::select(where(is.numeric))

plot_missing_percent <- function(data) {
    missing_percent <- colSums(is.na(data)) / nrow(data) * 100
    missing_table <- data.frame(column = names(missing_percent), percent_missing = missing_percent)
    plot <- ggplot(missing_table, aes(x = column, y = percent_missing)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Variable", y = "Percentage Missing") +
        ggtitle("Percentage of Missing Values") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot)
}

plot_missing_percent(wheat)
plot_missing_percent(rye)
plot_missing_percent(barley)

plot_correlation <- function(data) {
    data <- na.omit(data)
    cor_matrix <- cor(data)
    corrplot(cor_matrix, method = "color", type = "lower",
             tl.col = "black", tl.srt = 45,
             col = colorRampPalette(c("#FFFFFF", "#0571B0", "#CA0020"))(100),
             title = "Correlation Matrix",
             mar = c(1, 1, 1, 1))
}
plot_correlation(wheat)
plot_correlation(rye)
plot_correlation(barley)

wheat <- na.omit(wheat)
wheat <- wheat %>% select(-N_Efficiency, -RP_Go)
shuffle_wheat <- sample(1:nrow(wheat))
wheat <- wheat[shuffle_wheat, ]
trainIndex <- createDataPartition(wheat$Ert_dt_ha, p = 0.8, list = FALSE)
trainData <- wheat[trainIndex, ]
testData <- wheat[-trainIndex, ]
model <- rpart(Ert_dt_ha ~., data = trainData, method = "anova")
rpart.plot(model)

actual_yield <- testData$Ert_dt_ha
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
rye <- rye %>% select(-RP_Go, -N_Efficiency)
shuffle_rye <- sample(1:nrow(rye))
rye <- rye[shuffle_rye, ]
trainIndex <- createDataPartition(rye$Ert_dt_ha, p = 0.8, list = FALSE)
trainData <- rye[trainIndex, ]
testData <- rye[-trainIndex, ]
model <- rpart(Ert_dt_ha ~., data = trainData, method = "anova")
rpart.plot(model)

actual_yield <- testData$Ert_dt_ha
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
barley <- barley %>% select(-RP_Go, -N_Efficiency)
shuffle_barley <- sample(1:nrow(barley))
barley <- barley[shuffle_barley, ]
trainIndex <- createDataPartition(barley$Ert_dt_ha, p = 0.8, list = FALSE)
trainData <- barley[trainIndex, ]
testData <- barley[-trainIndex, ]
model <- rpart(Ert_dt_ha ~., data = trainData, method = "anova")
rpart.plot(model)

actual_yield <- testData$Ert_dt_ha
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
