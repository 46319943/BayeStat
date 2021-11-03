library(readr)
dat <- read_csv("ChickWeightForJAGS.csv")
View(ChickWeightForJAGS)

library("rjags")

mod_string = " model {
for (i in 1:length(Chick)) {
  weight[i] ~ dnorm(theta[Diet[i]], 1.0 / (sigma[Diet[i]])^2)
}

for (j in 1:max(Diet)) {
  theta[j] ~ dnorm(mu, 1.0 / tau^2)
  
  sigma_prec[j] ~ dgamma(1.0/2.0, 2*1.0/2.0)
  sigma[j] = sqrt(1.0 / sigma_prec[j])
}

mu ~ dnorm(0, 1.0 / 1e6)
tau_prec ~ dgamma(1.0/2.0, 3*1.0/2.0)
tau = sqrt(1.0 / tau_prec)

} "

set.seed(113)

data_jags = as.list(dat)

params = c("theta", 'mu', 'tau', 'sigma')

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic2 = dic.samples(mod, n.iter=1e3)