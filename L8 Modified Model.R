data("PlantGrowth")
library("rjags")

mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    for (j in 1:3) {
        prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
    }
    
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

(pm_params = colMeans(mod_csim))
yhat = pm_params[1:3][data_jags$grp]
resid = data_jags$y - yhat
plot(resid)
plot(yhat, resid)

summary(mod_sim)
HPDinterval(mod_csim)

mean(mod_csim[,3] > mod_csim[,1])
mean(mod_csim[,3] > 1.1*mod_csim[,1])


dic2 = dic.samples(mod, n.iter=1e3)

#  fit the cell means model
mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)