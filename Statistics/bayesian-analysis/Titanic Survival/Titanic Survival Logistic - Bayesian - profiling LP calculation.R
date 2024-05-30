library(profvis)

titanic_survival <- read.csv("/Users/chaitanya/Documents/Projects/Titanic Survival/train.csv", header = TRUE, na.strings = "")
titanic_survival_modeling_dataset <- titanic_survival[,c("Age", "Survived")]
titanic_survival_modeling_dataset <- titanic_survival_modeling_dataset[apply(titanic_survival_modeling_dataset, c(1), function(x){!any(is.na(x))}),]

p <- profvis({
  for(i in 1:100){
  # Parameters
  beta <- rep(0.1, ncol(titanic_survival_modeling_dataset) - 1 + 1)
  
  # Data
  x <- data.frame(titanic_survival_modeling_dataset[,(ncol(titanic_survival_modeling_dataset)-1)])
  y <- data.frame(titanic_survival_modeling_dataset[,ncol(titanic_survival_modeling_dataset)])
  
  # Prior probabilities
  beta.prior <- dnorm(beta, 0, 100, log = TRUE)
  
  # Posterior probabilities
  # logistic_pxi <-apply(x, c(1), function(x_i){beta[1] + (beta[2:length(beta)] * x_i)})
  logistic_pxi <- beta[1] + (beta[2:length(beta)] * x)
  e_logistic_pxi <- exp(logistic_pxi)
  
  # df <- rbind(x, y, logistic_pxi, e_logistic_pxi)
  # LL <- apply(df, c(1), function(df_i){df$y * df$log})
  LL <- sum(y*logistic_pxi) - sum(log(1 + e_logistic_pxi))
  LP <- LL + sum(beta.prior)
  }
})

print(p)