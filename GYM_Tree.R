library('dplyr')
library('corrplot')
library("PerformanceAnalytics")

library(e1071)

library(caTools)
library(caret)

base <- read.csv("C:/Users/joaop/Desktop/ML_Curso/MeusPythons/GYM/academia.csv")
base <- select(base,-date,-timestamp,-is_weekend, -is_holiday, -is_start_of_semester,-is_during_semester)

str(base)

#Ajustando temperatura para oC
base$temperature = (base$temperature-32)/1.8
base$temperature = round(base$temperature,)

percentil60 = quantile(base$number_people, probs = 0.60)

base$target = 'Cheia'

for (i in 1:nrow(base)) {
  if(base$number_people[i] <= percentil60) {
    base$target[i] <- 'Vazia'
  }
}

base <- select(base,-number_people)
str(base)

base$month = factor(base$month)
base$temperature = factor(base$temperature)
base$day_of_week = factor(base$day_of_week)
base$hour = factor(base$hour)
base$target = factor(base$target)
str(base)


set.seed(123)
divisao = sample.split(base$target, SplitRatio = 0.8)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#TREE
library(rpart)
classificador = rpart(formula = target ~ ., data = base_treinamento)
print(classificador)
library(rpart.plot)
rpart.plot(classificador)
previsoes = predict(classificador, newdata = base_teste[-5], type = 'class')
matriz_confusao = table(base_teste[, 5], previsoes)
print(matriz_confusao)
#install.packages('caret')
confusionMatrix(matriz_confusao)


#RANDOM FOREST
library(randomForest)
set.seed(1)
classificador = randomForest(x = base_treinamento[-5], y = base_treinamento$target, ntree = 32)
previsoes = predict(classificador, newdata = base_teste[-5], type = 'class')
matriz_confusao = table(base_teste[, 5], previsoes)
print(matriz_confusao)
#library(caret)
confusionMatrix(matriz_confusao)
