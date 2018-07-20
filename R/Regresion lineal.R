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
