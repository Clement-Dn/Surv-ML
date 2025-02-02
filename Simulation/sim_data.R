set.seed(42) 

#libraires
library(survival)
library(survminer)
library(tidyverse)

sim_data <- function(n, p, censor_rate, prop_zero) {
  
  # Gén Covar
  covariates <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1 ), nrow = n, ncol = p))
  colnames(covariates) <- paste0("Var", 1:p)
  
  # Gén coef avec zero
  beta <- runif(p, min = -10, max = 10)
  beta[sample(seq_along(beta), ceiling(prop_zero * p))] <- 0
  
  # Calcul effet covar
  effet_indi <- as.matrix(covariates) %*% beta
  
  # Gen de survie par terrible
  lambda <- 0.1  
  #survival_times <- -log(runif(n, min = 0, max = 1))/(lambda*effet_indi)

    # Construction du dataset final
  dataset <- data.frame(
    covariates
  )
  dataset = dataset %>%
    mutate(
      effet_indi = effet_indi,
      effet_indi = ifelse(effet_indi < 1,1,effet_indi ),
      u = runif(n, min = 0, max = 1),
      survival_times = -log(u)/(lambda*effet_indi),
      censure_adm = ifelse(is.infinite(survival_times), 1, 0),
      censure = ifelse(runif(nrow(dataset),min = 0, max = 1)<censor_rate  | censure_adm == 1,1,0),
      time = ifelse(censure == 1, runif(nrow(dataset),min = 0, max = max(survival_times)/2),survival_times)
           )
  print(paste("Les coef sont:", toString(round(beta,3))))
  
  return(list(data = dataset, coef = beta))
}
simulation = sim_data(n = 50000, p = 50, censor_rate = 0.1,prop_zero = 0.5)
df <- simulation$data %>%
  select(-c("effet_indi","u","survival_times","censure_adm"))

# modèle de Cox
cox_model <- coxph(Surv(time, censure) ~ ., data = df)
#summary(cox_model)


#Comparaison
dg = tibble(simulation$coef,cox_model$coefficients,as.vector(summary(cox_model)$coefficients[,5]))
colnames(dg) = c("Vrai_Coef","Est_Coef","p-value")

dg = dg %>% mutate(ratio = Vrai_Coef/Est_Coef)


# Kaplan-Meier
df = df %>% arrange(time)
km_fit <- survfit(Surv(df$time, df$censure) ~ 1)  # ~1 pas de strat

# Visualiser la courbe de Kaplan-Meier
ggsurvplot(km_fit, data = df, 
           xlab = "Temps",            # Légende de l'axe des x
           ylab = "Probabilité de survie" # Légende de l'axe des y
)

#Histogrammes
hist_time = ggplot(df, aes(x = time)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Simulation du temps de survie",
    x = "Temps ",
    y = "Fréquence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
hist_time_log = ggplot(df, aes(x = log(time))) + 
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Simulation du temps de survie",
    x = "log du Temps ",
    y = "Fréquence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


hist_coef_vrai = ggplot(dg, aes(x = Vrai_Coef)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Distribution des Coefficients",
    x = "Coefficient ",
    y = "Fréquence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

hist_coef_est = ggplot(dg, aes(x = Est_Coef)) + 
  geom_histogram(binwidth = 0.01, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Distribution des coefficients estimés",
    x = "Coefficient ",
    y = "Fréquence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

setwd("~/Surv-ML/Simulation")
save(hist_time,hist_time_log,hist_coef_vrai, hist_coef_est, file = "sim_data.RData")
setwd("~/Surv-ML")