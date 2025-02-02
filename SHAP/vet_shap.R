# Source: https://modeloriented.github.io/survex/articles/global-survshap.html

library(survex)
library(survival)
library(ranger)

vet <- survival::veteran

cph <- coxph(Surv(time, status) ~ ., data = vet, x = TRUE, model = TRUE)
exp <- explain(cph, data = vet[, -c(3,4)], y = Surv(vet$time, vet$status))
table(vet$status)
shap <- model_survshap(exp, veteran[c(1:4, 17:20, 110:113, 126:129), -c(3,4)])
plot(shap)
plot(shap, variable = "karno", geom = "profile")
plot(shap, geom = "beeswarm")
