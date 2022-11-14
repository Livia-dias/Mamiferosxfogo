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

df[, -1] = sapply(df[, -1], as.numeric)
df[,-1] = scale(df[,-1])
df$Nsite
#riqueza ~ PAtype + NDVI500m + DistWater + Fogo, family=Poisson

#model_map_std=glm(HAS_FIRE ~ .,data = dados_treino_cave_map_std,family = binomial(link = "logit")

df$id = 1:nrow(df)
treino = df %>% sample_frac(.8)
teste = anti_join(df, treino, by = "id")

options(na.action = "na.fail")

model = glm(Nsite ~ pa_type + NDVImean_5 + Dist_water + Prob_fogo, data = treino, family = poisson(link = "logit"))


dd_avg <- dredge(model, rank = AICc) # creates the 16 possible models
avg <-model.avg(dd_avg, subset = cumsum(weight) <= .95)

dd <- dredge(model)
model=get.models(dd,TRUE, subset = delta < 4)[[1]]

sumario_model = summary(model)$coefficients
scale(sumario_model)
write.csv(sumario_model, "sumario_Nsite.csv")

predicted=predict(model, teste, type="response")
reg_predict <- rep(0, nrow(teste))
reg_predict[predicted>.5] <- 1

teste$valor_predito=predicted

rmse = rmse(teste$Nsite, teste$valor_predito)
write.csv(rmse, "rmse_Nsite.csv")

anova = anova(model, test = "Chisq")
write.csv(anova, "anova_Nsite.csv")

plot(fitted.values(model), residuals.glm(model),
     #main = titulo_residuo_vs_fitted,
     xlab = "Valores Observados Ajustados", ylab = "Resíduais")
abline(0,0)


residuos_prob=(as.numeric(teste$Nsitethreat)-1)-predicted
plot(teste$valor_predito, residuos_prob,
     #main = titulo_predito_vs_residuo,
     xlab = "Previstos", ylab = "Resíduais")
