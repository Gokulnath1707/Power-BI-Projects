#Required Libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)

#Import Dataset
cars = car_dataset_india

#To understand the Dataset
names(cars)
head(cars)
tail(cars)


#Check Data Structure
str(cars)
summary(cars)
colSums(is.na(cars))  # Check missing values

#Impute Missing Values
cars <- cars %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
cars

#Remove Outliers Using Inter quartile range 
#Box Plot to detect outliers
boxplot(cars$Price, main = "Boxplot of Price")
#no outliers found

Q1 <- quantile(cars$Price, 0.25)
Q3 <- quantile(cars$Price, 0.75)
IQR <- Q3 - Q1
cars <- cars %>%
  filter(Price > (Q1 - 1.5 * IQR) & Price < (Q3 + 1.5 * IQR))

#converting categorical data into Numeric

cars$Fuel_Type <- as.factor(cars$Fuel_Type)
cars$Transmission <- as.factor(cars$Transmission)

#Optionally encode as numeric
cars$Fuel_Type <- as.numeric(as.factor(cars$Fuel_Type))

#Histogram
ggplot(cars, aes(x = Price)) + geom_histogram(binwidth = 200000, fill = "skyblue", color ="black")

gg#Box plot 
ggplot(cars, aes(x = Fuel_Type, y = Engine_CC)) + geom_boxplot(fill = "orange")

ggplot(cars, aes(x = Year, y= Service_Cost,Price))+geom_line()

#library(corrplot)
numeric_cols <- sapply(cars, is.numeric)
cor_matrix <- cor(cars[, numeric_cols], use = "complete.obs")
corrplot(cor_matrix, method = "color")

#regression analysis: package - (tidyr)
car_lm = lm(Price ~ Fuel_Type + Transmission, data = train_data)
summary(car_lm)

#Predict on test data
predictions <- predict(car_lm, newdata = test_data)
predictions

#RMSE
rmse <- sqrt(mean((test_data$Price - predictions)^2))
print(paste("RMSE:", rmse))

#Logistic regression

set.seed(123)
split = sample.split(cars$Price, SplitRatio = 0.7)
train_data = subset(cars, split == TRUE)
test_data = subset(cars, split == FALSE)

test_data$High_Price <- ifelse(test_data$Price > 500000, 1, 0)
test_data$High_Price <- as.factor(test_data$High_Price)

car_log = glm(High_Price ~ Fuel_Type + Transmission, data = train_data, family = "binomial")
summary(car_log)

#Predicting Model
prob <- predict(car_log, newdata = test_data, type = "response")
predicted_class <- ifelse(prob > 0.5, 1, 0)

# Ensure both are factors with same levels
predicted_class <- factor(predicted_class, levels = c(0,1))
test_data$High_Price <- factor(test_data$High_Price, levels = c(0,1))

# Confusion matrix — correct comparison!
confusionMatrix(predicted_class, test_data$High_Price)

#Random Forest : Regression method
#install(randomForest)
set.seed(123)
rf_model <- randomForest(
  High_Price ~ Year + Fuel_Type + Transmission + Mileage + Engine_CC + Seating_Capacity + Service_Cost,
  data = train_data,
  ntree = 100,      # Number of trees
  mtry = 3,         # Number of variables randomly sampled at each split
  importance = TRUE # Get feature importance
)
rf_model

print(rf_model)

rf_predictions <- predict(rf_model, newdata = test_data)

confusionMatrix(rf_predictions, test_data$High_Price)

varImpPlot(rf_model)
