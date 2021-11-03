dat = read.csv(file="callers.csv", header=TRUE)
## set R's working directory to the same directory
## as this file, or use the full path to the file.

library("rjags")

mod_string = " model {
  	for (i in 1:length(calls)) {
  		calls[i] ~ dpois( days_active[i] * lam[i] )
  		log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
  	}
  	
  	b0 ~ dnorm(0.0, 1.0/1e2)
  	b[1] ~ dnorm(0.0, 1.0/1e2)
  	b[2] ~ dnorm(0.0, 1.0/1e2)
} "

set.seed(102)

data_jags = as.list(dat)

params = c("b0", "b")

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
dic = dic.samples(mod, n.iter=1e3)

x = c(29, 1)
loglam = mod_csim[, 'b0'] + mod_csim[, c(1,2)] %*% x
lam = exp(loglam)
(n_sim = length(lam))
y = rpois(n=n_sim, lambda = lam * 30)
mean(y >= 3)