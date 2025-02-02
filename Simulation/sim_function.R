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
  #print(paste("Les coef sont:", toString(round(beta,3))))
  
  return(list(data = dataset, coef = beta))
}
