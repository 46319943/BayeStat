library("car")
data("Anscombe")
head(Anscombe)
?Anscombe

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

data_jags = as.list(data.frame(Xc))

mod_string3 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ ddexp(0.0, 1.0)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)

params1 = c("b", "sig")

inits3 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod3 = jags.model(textConnection(mod_string3), 
                  data=data_jags, 
                  # inits=inits1, 
                  n.chains=3)
update(mod3, 3000) # burn-in

mod3_sim = coda.samples(model=mod3,
                        variable.names=params1,
                        n.iter=10000)
plot(mod3_sim)

gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_sim)

mod3_csim = do.call(rbind, mod3_sim) # combine multiple chains

dic.samples(mod3, n.iter=1e3)

par(mfrow=c(3, 1)) # arrange frame for plots
densplot(mod3_csim[,1:3], xlim=c(-2.0, 2.0))
