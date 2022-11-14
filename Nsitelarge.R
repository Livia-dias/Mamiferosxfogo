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


setwd("C:\\Queimadas\\R_\\fire-model\\Mamíferos")

df = read.csv("variaveis_mamiferos.csv", dec = ",", sep = ";")
df = select(df, c("pa_type", "NDVImean_5", "Dist_water", "Prob_fogo"))

riquezas = read.csv("sppRich_perSite - summary_outB.csv", dec = ".", sep = ",")
Nsite = subset(riquezas, grepl("Nsite\\[", riquezas$X))

Nsitelarge = subset(riquezas, grepl("Nsite.large", riquezas$X))

Nsite_ameaca = subset(riquezas, grepl("Nsite.threat", riquezas$X))

df$Nsite = Nsite$Rhat
df$Nsitelarge = Nsitelarge$Rhat
df$Nsitethreat = Nsite_ameaca$Rhat

df = df %>% filter(Prob_fogo>=0)

#riqueza ~ PAtype + NDVI500m + DistWater + Fogo, family=Poisson

#model_map_std=glm(HAS_FIRE ~ .,data = dados_treino_cave_map_std,family = binomial(link = "logit")

df$id = 1:nrow(df)
treino = df %>% sample_frac(.8)
teste = anti_join(df, treino, by = "id")


model = glm(Nsitelarge ~ pa_type + NDVImean_5 + Dist_water + Prob_fogo, data = treino, family = poisson)


sumario_model = summary(model)$coefficients
scale(sumario_model)
write.csv(sumario_model, "sumario_Nsitelarge.csv")

predicted=predict(model, teste, type="response")
reg_predict <- rep(0, nrow(teste))
reg_predict[predicted>.5] <- 1

teste$valor_predito=predicted

rmse = rmse(teste$Nsitelarge, teste$valor_predito)
write.csv(rmse, "rmse_Nsitelarge.csv")

anova = anova(model, test = "Chisq")
write.csv(anova, "anova_Nsitelarge.csv")
