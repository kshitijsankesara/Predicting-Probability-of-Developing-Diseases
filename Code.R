#Reading the data file 
HDM <- read.csv("HC Data Mining.csv")
View(HDM)


#Installing packages
install.packages("healthcareai")
library(healthcareai)
library(readxl)
library(ggplot2)
library(DBI)


#Structure
str(HDM)


#Generating models
model1 <- machine_learn(HDM, Id, outcome = Diabetes)
model1     #Diabetes

model2 <- machine_learn(HDM, Id, outcome = Coronary_Heart_Disease)
model2    #Coronary Heart Disease

model3 <- machine_learn(HDM, Id, outcome = Hypertension)
model3    #Hypertension


#Making predictions
prediction1 <- predict(model1)
View(prediction1)
plot(prediction1)

prediction2 <- predict(model2)
View(prediction2)
plot(prediction2)

prediction3 <- predict(model3)
View(prediction3)
plot(prediction3)


#Data Preparation
split_data1 <- split_train_test(d = HDM,
                               outcome = Diabetes,
                               p = .8)
View(split_data1)


split_data2 <- split_train_test(d = HDM,
                                outcome = Coronary_Heart_Disease,
                                p = .8)
View(split_data2)


split_data3 <- split_train_test(d = HDM,
                                outcome = Hypertension,
                                p = .8)
View(split_data3)


prepped_training_data1 <- prep_data(split_data1$train, Id, outcome = Diabetes,
                                   center = TRUE, scale = TRUE,
                                   collapse_rare_factors = FALSE)


prepped_training_data2 <- prep_data(split_data2$train, Id, outcome = Coronary_Heart_Disease,
                                   center = TRUE, scale = TRUE,
                                   collapse_rare_factors = FALSE)


prepped_training_data3 <- prep_data(split_data3$train, Id, outcome = Hypertension,
                                   center = TRUE, scale = TRUE,
                                   collapse_rare_factors = FALSE)


#Model Training

models1 <- tune_models(d = prepped_training_data1,
                      outcome = Diabetes,
                      metric = "PR")

models2 <- tune_models(d = prepped_training_data2,
                      outcome = Coronary_Heart_Disease,
                      metric = "PR")

models3 <- tune_models(d = prepped_training_data3,
                      outcome = Hypertension,
                     metric = "PR")


#Evaluating the models
evaluate(models1, all_models = TRUE)
evaluate(models2, all_models = TRUE)
evaluate(models3, all_models = TRUE)


#Interpret Models

interpret(models1, top_n = 30) %>% 
  plot()

interpret(models2, top_n = 30) %>% 
  plot()

interpret(models3, top_n = 30) %>% 
  plot()


#Variable Importance

get_variable_importance(models1, top_n = 20) %>%
  plot()

get_variable_importance(models2, top_n = 20) %>%
  plot()

get_variable_importance(models3, top_n = 20) %>%
  plot()


#Prediction on train data
Predict_models1 <- predict(models1)
View(Predict_models1)

Predict_models2 <- predict(models2)
View(Predict_models2)

Predict_models3 <- predict(models3)
View(Predict_models3)


#Testing the model on the Test data

test_predictions1 <- 
  predict(models1, 
          split_data1$test, 
          risk_groups = c(low = 30, moderate = 40, high = 20, extreme = 10)
  )
plot(test_predictions1)
View(test_predictions1)


test_predictions2 <- 
  predict(models2, 
          split_data2$test, 
          risk_groups = c(low = 30, moderate = 40, high = 20, extreme = 10)
  )
plot(test_predictions2)
View(test_predictions2)


test_predictions3 <- 
  predict(models3, 
          split_data3$test, 
          risk_groups = c(low = 30, moderate = 40, high = 20, extreme = 10)
  )
plot(test_predictions3)
View(test_predictions3)
