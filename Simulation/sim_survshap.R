# install.packages("survex")
library(survex)

# Ajuster le modèle de Cox
cox_model <- coxph(Surv(time, censure) ~ ., data = df)

# Appliquer survshap pour obtenir les valeurs SHAP
shap_values <- survshap(cox_model, X = df)

# Affichage des valeurs SHAP pour les premières observations
shap_values$shap_values[1:10, ]

# Visualiser les résultats
# Visualisation globale des importances des features
plot(shap_values)

# Visualisation locale pour une observation spécifique (par exemple la première)
plot(shap_values, observation = 1)

