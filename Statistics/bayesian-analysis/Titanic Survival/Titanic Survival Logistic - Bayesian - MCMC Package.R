library(mcmc)

titanic_survival <- read.csv("/Users/chaitanya/Documents/Projects/Titanic Survival/train.csv", header = TRUE, na.strings = "")
titanic_survival_modeling_dataset <- titanic_survival[,c("Age", "Survived")]
titanic_survival_modeling_dataset <- titanic_survival_modeling_dataset[apply(titanic_survival_modeling_dataset, c(1), function(x){!any(is.na(x))}),]

# titanic_survival_modeling_dataset$Survived <- ifelse(titanic_survival_modeling_dataset$Age <=45, 1, 0)


  model <- function(beta, x, y){
    # Data
 
    
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
    
    
    return(LP)
  }
  
  beta.init <- rep(0.1, ncol(titanic_survival_modeling_dataset) - 1 + 1)
  out <- metrop(model, beta.init, 1e4, x = data.frame(titanic_survival_modeling_dataset[,(ncol(titanic_survival_modeling_dataset)-1)]),
                y = data.frame(titanic_survival_modeling_dataset[,ncol(titanic_survival_modeling_dataset)]),
                debug = TRUE)
  
  
  
  beta_mcmc <- out$final
  
  
  
  # Analytical model
  model <- glm(Survived ~ Age,family=binomial(link='logit'),data=titanic_survival_modeling_dataset)
  beta_analytical <- summary(model)$coefficients[,1]
  
  # Predicitions
  # ------------------------------------------------------------------------------------------------
  
  # Function to calculate probability of survival
  predict_survival <- function(beta, x) {apply(x[1:(ncol(x)-1)], c(1), function(x_i){1/(1+exp(-(beta[1] + (beta[2:length(beta)] * x_i))))})}
  
  # Predicted dataset
  pred_dataset <- titanic_survival_modeling_dataset
  
  # Predicitions using the MCMC betas
  pred_dataset$mcmc_p <- predict_survival(beta_mcmc, titanic_survival_modeling_dataset)
  pred_dataset$mcmc_pred <- ifelse(pred_dataset$mcmc_p >= 0.5, 1, 0)
  
  # Predictions using the analytical betas
  pred_dataset$analytical_p <- predict_survival(beta_analytical, titanic_survival_modeling_dataset)
  pred_dataset$analytical_pred <- ifelse(pred_dataset$analytical_p >= 0.5, 1, 0)
  
  # Summary of predictions
  sum(ifelse(pred_dataset$Survived == pred_dataset$mcmc_pred, 1, 0))/nrow(pred_dataset)
  sum(ifelse(pred_dataset$Survived == pred_dataset$analytical_pred, 1, 0))/nrow(pred_dataset)
  
  group_by(pred_dataset, Survived) %>% summarise(mcmc_p = mean(mcmc_p), analytical_p = mean(analytical_p)) %>% ungroup()
  
  
  
  # Plots
  # ------------------------------------------------------------------------------------------------
  ggplot(pred_dataset, aes(Age, Survived)) +
    geom_point() +
    geom_line(aes(Age, mcmc_p), color = "black") +
    geom_line(aes(Age, analytical_p), color = "red")
  
  plot_d <- data.frame(Age = -1000:1000, dummy = 0)
  plot_d$mcmc_p <- predict_survival(beta_mcmc, as.data.frame(plot_d$Age))
  plot_d$analytical_p <- predict_survival(beta_analytical, as.data.frame(plot_d$Age))
  
  ggplot(plot_d) +
    geom_line(aes(Age, mcmc_p), color = "black") +
    geom_line(aes(Age, analytical_p), color = "red")