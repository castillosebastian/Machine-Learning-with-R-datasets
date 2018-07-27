library(dplyr)
library(stringr)
library(psych)
setwd("~/R/Machine_learning_datasets")
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
sex <- table(insurance$sex)
prop.table(sex)
prop.table(table(insurance$region))
pairs.panels(insurance[c("age", "bmi", "children", "charges", "smoker")])
ins_model <- lm(charges ~ ., data = insurance)
ins_model
summary(ins_model)
# acentuando el impacto de age sobre charges mediante exponenciaciòn
insurance$age2 <- insurance$age^2

# redefiniendo nueva variable para reproducir el efecto de que ciertas variables
# solo tienen impacto sobre la v.dependiente cuando se supera cierto valor, como
# el caso del Peso respecto del Cobertura de Salud
insurance$bmi30 <- if_else(insurance$bmi >= 30, 1, 0)

# Agregar interacciòn entre smoke y obesity y generando nuevo modelo
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance) 
summary(ins_model2)

# Laboratorio de Regresion Lineal
library(MASS)
install.packages("ISLR")
library(ISLR)
library(dplyr)
library(ggplot2)
str(Boston)
df <- Boston
lmfit <- lm(medv~lstat, df)
summary(lmfit)
names(lmfit)
# Confidence Interval para concluir sobre la fuerza de la asociaciòn entre nuestro predictor 
# y variable dependiente, entendiendo por fuerza de la asociaciòn a valores alejados de 0
confint(lmfit)

# Visualizacion de Resultados del medelo
par(mfrow=c(2,2))
plot(lmfit)

# Probando si la relacion es lineal o no. Como se ve se aleja de la linealidad
plot(predict(lmfit), residuals(lmfit))


# Visualizacion del ggplot2
df %>% 
  mutate(modelo = predict(lmfit)) %>% 
  ggplot() +
  geom_point(aes(lstat, medv)) +
  geom_line(aes(lstat, modelo)) 
