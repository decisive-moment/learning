data {
  int<lower=0> N;          // number of individuals
  int<lower=0> p;          // number of individuals
  int y[N];               // survival indicator
  real<lower=0> age[N];  // age
}
parameters {
  real beta[p];
}
transformed parameters {
  real<lower=0> odds[N];
  real<lower=0, upper=1> prob[N];

  for(i in 1:N)
  {
    odds[i] = exp(beta[1] + beta[2]*age[i]);
    prob[i] = odds[i] / (odds[i] + 1);
  }
}
model {
  y ~ bernoulli(prob);
}