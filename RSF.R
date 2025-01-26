# Libraries
library(randomForestSRC)
library(survival)

# Data loading & cleaning & var
data("veteran")
# 1 = événement, 0 = censuré


# Appliquer le Random Survival Forest
rsf_model <- rfsrc(Surv(time, status) ~ ., 
                   data = veteran, 
                   ntree = 500,       
                   nodesize = 10,  
                   nsplit = 2,      
                   importance = TRUE)

print(rsf_model)

# Courbes de survie estimées pour les individus
plot(rsf_model, 
             subset = 1:5,         
             main = "CdS RSF",
             ylab = "Probabilité de survie", 
             xlab = "Temps")




