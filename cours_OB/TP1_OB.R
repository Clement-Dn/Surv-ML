# Libraries
library(timereg)
library(tidyverse)
library(survival)

# Data loading & cleaning & VARs
data(melanoma)
df = melanoma

base.mel <- Surv(df$days, df$status == 1)

km_fit <- survfit(base.mel ~ 1)

# Rrésumé + tracer courbe
summary(km_fit)
plot(km_fit,
     main = "Courbe de survie Kaplan-Meier",
     xlab = "Temps (jours)",
     ylab = "Probabilité de survie")

# KM pour genre
km_fit_sex <- survfit(base.mel ~ df$sex)

plot(km_fit_sex, col = c("#b30c00", "#110094"),
     lty = 1:2,
     main = "Courbes de survie par sexe",
     xlab = "Temps (jours)",
     ylab = "Probabilité de survie")
legend("bottomleft",
       legend = c("Femme", "Homme"),
       col = c("#b30c00", "#110094"), lty = 1:2)

#log rank
log_rank_test <- survdiff(base.mel ~ df$sex)


#Cox
cox_model <- coxph(base.mel ~ df$sex)

#Cox autres var
cox_model <- coxph(base.mel ~ sex + thick + ulc, data = df)




# Exo 2
data(kidney)

boxplot(kidney$age ~ kidney$sex, 
        names = c("Homme", "Femme"),
        main = "Âge/Sexe",
        xlab = "Sexe",
        ylab = "Âge",
        col = c("lightblue", "pink"))

# Moyenne
tapply(kidney$age, kidney$sex, mean)

t.test(age ~ sex, data = kidney) # Pas de dif d'age sig

table(kidney$disease, kidney$sex)

barplot(prop.table(table(kidney$disease, kidney$sex), 2), 
        beside = TRUE, 
        col = c("red", "blue", "green", "purple"),
        legend.text = c("GN", "AN", "PKD", "Other"),
        main = "Maladie/Sexe",
        xlab = "Sexe",
        ylab = "Proportion")

anova_age_maladie <- aov(age ~ factor(disease), data = kidney)
summary(anova_age_maladie)

surv_object <- Surv(kidney$time, kidney$status)
km_sex <- survfit(surv_object ~ sex, data = kidney)
plot(km_sex, col = c("blue", "red"), 
     lty = 1:2, 
     main = "CdS / sexe",
     xlab = "Temps",
     ylab = "Proba de survie")
legend("bottomleft", legend = c("Homme", "Femme"), 
       col = c("blue", "red"), lty = 1:2)

survdiff(surv_object ~ sex, data = kidney)
km_disease <- survfit(surv_object ~ disease, data = kidney)
plot(km_disease, col = c("red", "blue", "green", "purple"), 
     lty = 1:4, 
     main = "CdS / maladie",
     xlab = "Temps (jours)",
     ylab = "Probabilité de survie")
legend("bottomleft", legend = c("GN", "AN", "PKD", "Other"), 
       col = c("red", "blue", "green", "purple"), lty = 1:4)

survdiff(surv_object ~ disease, data = kidney)
cox_mod <- coxph(surv_object ~ disease, data = kidney)
exp(cox_mod$coefficients)
cox_age <- coxph(surv_object ~ age, data = kidney)
cox_full <- coxph(surv_object ~ age + sex + disease, data = kidney)
anova(cox_full)
cox.zph(cox_full)
step(cox_full)




