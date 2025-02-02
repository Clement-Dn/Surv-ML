# L'objectif de ce script est de créer une heat map du temps de calcul en
# fonction du nombre d'observation n et du nombre de parametre
#> Pour éviter de faire n*p calculs on va piocher k%

# libraries
library(survex)
library(survival)
library(ranger)
# fonction de sim
setwd("~/Surv-ML/Simulation")
source("sim_function.R")

sim_shap_time = function(n,p){
 
  simulation = sim_data(n, p, censor_rate = 0.1,prop_zero = 0)
  df = simulation$data %>% select(c("time","censure",contains("Var"))) %>%
    mutate(censure = 1 - censure)
  
  cox_model <- coxph(Surv(time, censure) ~ ., data = df, x = TRUE, model = TRUE)
  
  exp <- explain(cox_model, data = df %>% select(-c("time","censure")), y = Surv(df$time, df$censure),verbose = F)
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
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))  
  return(execution_time)
}

#sim_shap_time(n = 150, p =4)


sim_shap_time_total = function(range_n = c(100:200), range_p = c(5:10),nmb_cal, censor_rate = 0.1,prop_zero = 0){
  
  nmb_couple = length(range_n)*length(range_p)
  
  # Générer les couples (n, p)
  n_values = rep(range_n, each = length(range_p))  # Répéter les valeurs de n pour chaque p
  p_values = rep(range_p, times = length(range_n))  # Répéter les valeurs de p pour chaque n
  
  # Prendre un sous-ensemble basé sur la proportion demandée
  sample_indices = sample(1:nmb_couple, size = nmb_cal)
  couples = data.frame(n_ = n_values[sample_indices], p_ = p_values[sample_indices]) %>%
    filter(n_ > p_)
  cat("Nombre de calcul de Shap à faire:", nrow(couples), " sur",nmb_couple," possibles \n")
  
  couples = couples %>% mutate(time_ex = NA)
  
  for(i in 1:nrow(couples)){
    cat("Pour",couples[i,"n_"], "observations,",
        "pour",couples[i,"p_"], "variables,",i,"calculs sur",nmb_cal," \n")
    temps_ex = sim_shap_time(couples[i,"n_"],couples[i,"p_"])
    couples[i,"time_ex"] = temps_ex
    cat("Temps d'excution", temps_ex,"secondes, pour",couples[i,"n_"], "observations,",
        "pour",couples[i,"p_"], "variables,",i,"calculs sur",nmb_cal," \n")
    
  }
  return(couples)
  }

temps_data = sim_shap_time_total(range_n = c(100:1000),
                                  range_p = c(5:10),
                                  nmb_cal = 10,
                                  censor_rate = 0.1,
                                  prop_zero = 0
)

# heatmap
ggplot(temps_data, aes(x = n_, y = p_, color = time_ex)) +
  geom_point(size = 3) +  # Ajouter les points
  scale_color_gradient(low = "blue", high = "red") +  # Choisir un dégradé de couleurs
  labs(title = "Graphique de n, p et time_ex",
       x = "n",
       y = "p",
       color = "time_ex") +  # Ajouter les labels
  theme_minimal()  # Utiliser un thème minimal