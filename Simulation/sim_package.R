library(coxed)
library(tidyverse)

simdata <- sim.survdata(N=1000, T=100, num.data.frames=1)

model <- coxph(Surv(y, failed) ~ X1 + X2 + X3, data=simdata$data)

#Comparaison
dg = tibble(simdata$betas,model$coefficients,as.vector(summary(model)$coefficients[,5]))
colnames(dg) = c("Vrai_Coef","Est_Coef","p-value")
