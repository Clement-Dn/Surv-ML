# install.packages("survex")
library(survex)
library(survival)
library(ranger)
# 
setwd("~/Surv-ML/Simulation")
source("sim_function.R")

simulation = sim_data(n = 1500, p = 4, censor_rate = 0.1,prop_zero = 0)
df = simulation$data %>% select(c("time","censure",contains("Var"))) %>%
  mutate(censure = 1 - censure)

cox_model <- coxph(Surv(time, censure) ~ ., data = df, x = TRUE, model = TRUE)

exp <- explain(cox_model, data = df %>% select(-c("time","censure")), y = Surv(df$time, df$censure))
#Preparation of a new explainer is initiated 
#-> model label       :  coxph (  default  ) 
#-> data              :  500  rows  5  cols 
#-> target variable   :  500  values ( 442 events and 58 censored ) 
#-> times             :  51 unique time points , min = 0.00299895655715465 , median survival time = 3.26148404191879 , max = 75.8734423024736 
#-> times             :  (  generated from y as uniformly distributed survival quantiles based on Kaplan-Meier estimator  ) 
#-> predict function  :  predict.coxph with type = 'risk' will be used (  default  ) 
#-> predict survival function  :  predictSurvProb.coxph will be used (  default  ) 
#-> predict cumulative hazard function  :  -log(predict_survival_function) will be used (  default  ) 
#-> model_info        :  package survival , ver. 3.7.0 , task survival (  default  ) 
#A new explainer has been created!  

# Mesure du temps de début
start_time <- Sys.time()


shap <- model_survshap(exp, df %>% select(-c("time","censure")))

# Mesure du temps de fin
end_time <- Sys.time()

# Calcul du temps d'exécution
execution_time <- end_time - start_time
cat("Temps d'exécution:", execution_time, " secondes.\n")


# plot(shap)
# plot(shap, variable = "karno", geom = "profile")
# plot(shap, geom = "beeswarm")
