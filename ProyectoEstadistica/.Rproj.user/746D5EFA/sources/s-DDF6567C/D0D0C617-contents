library(caTools)
data = read.table(file.choose(),sep = ";", head = T)

set.seed(88)
split <- sample.split(data$covid_res, SplitRatio = 0.75)

set.seed(88)
split <- sample.split(data, SplitRatio = 0.75)

data_train <- subset(data, split == TRUE)
data_test <- subset(data, split == FALSE)

# Ajustando el modelo de Regresión Logística:
model1 <- glm (covid_res ~ sex + patient_type + intubed + pneumonia + age + pregnancy + diabetes + copd + asthma + inmsupr + hypertension + other_disease + cardiovascular + obesity+ renal_chronic + tobacco + contact_other_covid + icu, data = data_train, family = binomial)
summary(model1)

model2 <- glm (covid_res ~ sex + patient_type + intubed + pneumonia + age + pregnancy + diabetes +  cardiovascular + obesity+  tobacco + contact_other_covid + icu, data = data_train, family = binomial)
summary(model2)

model3 <- glm (covid_res ~ sex + patient_type + intubed + pneumonia + age + pregnancy + diabetes +   obesity+ tobacco +  contact_other_covid + icu, data = data_train, family = binomial)
summary(model3)

predict <- predict(model2, type = 'response')
predict2 <- as.data.frame(predict(model2, type = 'response'))
fix(predict2)

# Matriz de Confusion:
table(data_train$covid_res, predict > 0.5)

# La curva ROC
library(pROC)
test_prob = predict(model2, newdata = data_train, type = "response")
test_roc = roc(data_train$covid_res ~ test_prob, plot = TRUE, print.auc = TRUE)

(as.numeric(test_roc$auc))*100


# Matriz de Confusion:
table(data_train$covid_res, predict > 0.75)

# La curva ROC
library(pROC)
test_prob = predict(model2, newdata = data_train, type = "response")
test_roc = roc(data_train$covid_res ~ test_prob, plot = TRUE, print.auc = TRUE)

(as.numeric(test_roc$auc))*100


# Matriz de Confusion:
table(data_train$covid_res, predict > 0.63)

# La curva ROC
library(pROC)
test_prob = predict(model2, newdata = data_train, type = "response")
test_roc = roc(data_train$covid_res ~ test_prob, plot = TRUE, print.auc = TRUE)

(as.numeric(test_roc$auc))*100
