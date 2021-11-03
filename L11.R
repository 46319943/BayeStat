dat = read.csv(file="pctgrowth.csv", header=TRUE)

library("rjags")

mod_string = " model {
for (i in 1:length(y)) {
  y[i] ~ dnorm(theta[grp[i]], 1.0 / sigma^2)
}

for (j in 1:max(grp)) {
  theta[j] ~ dnorm(mu, 1.0 / tau^2)
}

mu ~ dnorm(0, 1.0 / 1e6)

tau_prec ~ dgamma(1.0/2.0, 3*1.0/2.0)
tau = sqrt(1.0 / tau_prec)

sigma_prec ~ dgamma(1.0/2.0, 2*1.0/2.0)
sigma = sqrt(1.0 / sigma_prec)

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
dic = dic.samples(mod, n.iter=1e3)


means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
## dat is the data read from pctgrowth.csv

means_theta = colMeans(mod_csim)[4:8]

plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.
