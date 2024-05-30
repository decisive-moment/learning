library(rstan)
library(dplyr)
library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

titanic_survival <- read.csv("/Users/chaitanya/Documents/Projects/Titanic Survival/train.csv", header = TRUE, na.strings = "")
titanic_survival_modeling_dataset <- titanic_survival[,c("Age", "Survived")]
titanic_survival_modeling_dataset <- titanic_survival_modeling_dataset[apply(titanic_survival_modeling_dataset, c(1), function(x){!any(is.na(x))}),]

# titanic_survival_modeling_dataset$Survived <- ifelse(titanic_survival_modeling_dataset$Age <=45, 1, 0)

t <- 0

## Create Stan data
dat <- list(N        = nrow(titanic_survival_modeling_dataset),
            p        = 2,
            age      = titanic_survival_modeling_dataset[,(ncol(titanic_survival_modeling_dataset)-1)],
            y        = titanic_survival_modeling_dataset[,ncol(titanic_survival_modeling_dataset)])

## Load Stan file
fileName <- "/Users/chaitanya/Documents/Datalicious/GitHub Repositories/learning-bayesian-analysis/Titanic Survival/titanic_logistic.stan"
stan_code <- readChar(fileName, file.info(fileName)$size)
cat(stan_code)

## Run Stan
resStan <- stan(model_code = stan_code, data = dat,
                chains = 4, iter = 3000, warmup = 500, thin = 10)

## Show traceplot
traceplot(resStan, pars = c("beta"), inc_warmup = TRUE)

## Bayesian
print(resStan, pars = c("beta"))

## Extracting model parameters
stan_summary <- summary(resStan, pars = "beta")$summary
beta_mcmc_stan <- stan_summary[,"mean"]

# Analytical model
model <- glm(Survived ~ Age,family=binomial(link='logit'),data=titanic_survival_modeling_dataset)
beta_analytical <- summary(model)$coefficients[,1]


# Function to calculate probability of survival
predict_survival <- function(beta, x) {apply(x[1:(ncol(x)-1)], c(1), function(x_i){1/(1+exp(-(beta[1] + (beta[2:length(beta)] * x_i))))})}

# Predicted dataset
pred_dataset <- titanic_survival_modeling_dataset

# Predicitions using the MCMC betas
pred_dataset$mcmc_stan_p <- predict_survival(beta_mcmc_stan, titanic_survival_modeling_dataset)
pred_dataset$mcmc_stan_pred <- ifelse(pred_dataset$mcmc_stan_p >= 0.5, 1, 0)

sum(ifelse(pred_dataset$Survived == pred_dataset$mcmc_stan_pred, 1, 0))/nrow(pred_dataset)

group_by(pred_dataset, Survived) %>% summarise(mcmc_stan_p = mean(mcmc_stan_p)) %>% ungroup()

# Plots
plot_d <- data.frame(Age = -1000:1000, dummy = 0)
plot_d$mcmc_stan_p <- predict_survival(beta_mcmc_stan, as.data.frame(plot_d$Age))
plot_d$analytical_p <- predict_survival(beta_analytical, as.data.frame(plot_d$Age))


ggplot(plot_d) +
  geom_line(aes(Age, mcmc_stan_p), color = "black") +
  geom_line(aes(Age, analytical_p), color = "red")