# Chaitanya Anand
# Simulating 100 coin tosses of a biased coin
# Estimating the probability of heads using both Bayesian and Frequentist approaches

library(LaplacesDemon)

# Simulating coin toss to generate data
actual_theta <- 0.2
T <- 1

# Model function to specify the likelihood function
model <- function(parm, data)
{
  # Parameteres
  theta <- exp(parm)/(1+exp(parm))

  # Log Prior Densities
  theta.prior <- dnorm(theta, 1, 1, log = TRUE)
  
  # Log Likelihood
  LL <- (sum(data$y)*log(theta)) + ((data$T - sum(data$y)) * log(1-theta))
 
  # Log Posterior
  LP <- LL + theta.prior
  
  ret_list <- list(LP=LP, Dev=-2*LL, Monitor=LP, yhat=NA, parm=parm)
  return(ret_list)
}

b.results <- c()
f.results <- c()
y.history <- c(0)
initial.values <- 0.1
for(i in 1:10)
{
  y <- rep(0, T)
  for(i in 1:T) {y[i] <- rbinom(1, 1, actual_theta)}
  
  # Creating data list for LaplacesDemon
  parm.names <- "theta"
  mon.names <- "LP"
  MyData <- list(T=T, mon.names = mon.names, parm.names = parm.names, y=y)
  
  mcmc_samples <- LaplacesDemon(Model = model, Data = MyData, Initial.Values = initial.values, Iterations = 30000,
                                Algorithm = "HARM", Thinning = 1, Specs = list(alpha.star = 0.234))
  Consort(mcmc_samples)
  
  parm <- mcmc_samples$Summary2["theta", "Mean"]
  y.history <- c(y.history, y)
  b.results <- c(b.results, exp(parm)/(1+exp(parm)))
  f.results <- c(f.results, sum(y.history)/length(y.history))
  initial.values <- exp(parm)/(1+exp(parm))
}
bayesian.ans <- initial.values

paste("The Bayesian answer:", bayesian.ans)
paste("The Frequentist answer:", sum(y)/T)
paste("The actual theta:", actual_theta)

# plot(mcmc_samples, BurnIn = 10000, MyData)
