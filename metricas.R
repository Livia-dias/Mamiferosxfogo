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
library(corrplot)

valores_observados_plot = sprintf("%s\\residuos_ajustados.png", filename)
png(file = valores_observados_plot)
plot(fitted.values(model), residuals.glm(model),
     #main = titulo_residuo_vs_fitted,
     xlab = "Valores Observados Ajustados", ylab = "Resíduais")
abline(0,0)
dev.off()


valores_residuais_plot = sprintf("%s\\residuos_predicted.png", filename)
png(file = valores_residuais_plot)
plot(teste$valor_predito, residuos_prob,
     #main = titulo_predito_vs_residuo,
     xlab = "Previstos", ylab = "Resíduais")
dev.off()

#plot modelo
plot_file = sprintf("%s\\plot.png", filename)
png(file = plot_file)
par(mfrow = c(2,2))
plot(model)
dev.off()

#mean squared error
rmse = rmse(teste$Nsite, teste$valor_predito)
rmse_file = sprintf("%s\\rmse.csv", filename)
write.csv(rmse, rmse_file)

#Anova chi-squared
anova = anova(model, test = "Chisq")
anova_chisq_file = sprintf("%s\\anova_chisq.csv", filename)
write.csv(anova, anova_chisq_file)

termplot_file = sprintf("%s\\termplot.png",  filename)
png(file = plot_file)
par(mfrow = c(2, 2))
termplot(model, partial.resid = TRUE, col.res = "black", pch = 16)
dev.off()