getwd()
setwd("D:/UVG/2022/Semestre 1 2022/Mineria de datos/HDT8")
#setwd("~/estudios/mineria de datos/HT7-Mineria")


df_test <- read.csv("test.csv")
df_train<- read.csv("train.csv")
df_test2 <- read.csv("sample_submission.csv")
df_test['SalePrice']<-df_test2$SalePrice

# combine by row
df_union <- rbind(df_train, df_test)
# update ID column
df_union$Id <- 1:nrow(df_union)

str(df_union)


porcentaje<-0.7
datos<-df_union
set.seed(123)


corte <- sample(nrow(datos),nrow(datos)*porcentaje)
df_train<-datos[corte,]
df_test<-datos[-corte,]


library(ggplot2)
library (dplyr)
library(caret)
library(e1071)
library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)

##limites de var categorica tipo de casa barata/ mediana / cara
summary(df_train$SalePrice)
priceRange <- max(df_train$SalePrice)-min(df_train$SalePrice)
baratoMax<- min(df_train$SalePrice)+(priceRange/3)
medianoMax <- baratoMax+(priceRange/3)
caroMax<-max(df_train$SalePrice)
min(df_train$SalePrice)
max(df_test$SalePrice)
(medianoMax)
baratoMax

summary(df_test$SalePrice)


df_train['tipoDeCasa']<- ifelse(df_train$SalePrice<baratoMax,"BARATA",ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,"MEDIA","CARA"))
df_test['tipoDeCasa']<- ifelse(df_test$SalePrice<baratoMax,"BARATA",ifelse(df_test$SalePrice>=baratoMax & df_test$SalePrice<medianoMax,"MEDIA","CARA"))


df_train_filtered<-df_train[,c(2,19,20,35,45,48,52,71,81,82)]
df_test_filtered<-df_test[,c(2,19,20,35,45,48,52,71,81,82)]

df_train_filtered$tipoDeCasa <- as.factor(df_train_filtered$tipoDeCasa)
df_test_filtered$tipoDeCasa <- as.factor(df_test_filtered$tipoDeCasa)


modelo.nn2 <- nnet(tipoDeCasa~.,data = datos,subset = corte, size=2, rang=0.1,
                   decay=5e-4, maxit=200) 
