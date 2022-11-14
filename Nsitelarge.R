library(boot)
library(tibble)
library(glmnet)
library(coefplot)
library(MuMIn)
library(sjPlot)
library(performance)
library(dplyr)
library(pROC)
library(graphics)
library(caret)
library(rlist)
library(Metrics)

setwd("D:\\Projetos\\Mamiferosxfogo")

filename = "NSite_large"

df = read.csv("variaveis_mamiferos.csv", dec = ",", sep = ";")
df = select(df, c("pa_type", "NDVImean_5", "Dist_water", "Prob_fogo"))

riquezas = read.csv("sppRich_perSite - summary_outB.csv", dec = ".", sep = ",")

#Separando NSite
Nsitelarge = subset(riquezas, grepl("Nsite.large", riquezas$X))

df$Nsitelarge = Nsitelarge$Rhat

df = df %>% filter(Prob_fogo>=0)

df$pa_type = as.factor(df$pa_type)
df$id = 1:nrow(df)
treino = df %>% sample_frac(.8)
teste = anti_join(df, treino, by = "id")

df = df[,-6]
treino = treino[, -6]
teste = teste[,-6]

model = glm(Nsitelarge ~ pa_type + NDVImean_5 + Dist_water + Prob_fogo, data = treino, family = quasipoisson, na.action = "na.fail")

dd <- dredge(model)
avg = get.models(dd, subset = df == 5)[[1]]


sumario_model = summary(model)$coefficients
scale(sumario_model)
sumario_file = sprintf("%s\\sumario.csv",filename)
write.csv(sumario_model, sumario_file)



predicted=predict(model, teste, type="response")
reg_predict <- rep(0, nrow(teste))
reg_predict[predicted>.5] <- 1

teste$valor_predito=predicted

residuos_prob=(as.numeric(teste$Nsitelarge)-1)-predicted
source("metricas.R")