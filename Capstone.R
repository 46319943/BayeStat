library(readr)
dat <- read_csv("ChickWeightForJAGS.csv")
View(ChickWeightForJAGS)

library("rjags")

mod_string = " model {
for (i in 1:length(Chick)) {
  weight[i] ~ dnorm(theta[Diet[i]], 1.0 / sigma^2)
}

for (j in 1:max(Diet)) {
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

(pm_params = colMeans(mod_csim))
write.csv(pm_params,"pm_params.csv", row.names = FALSE)


summary(mod_sim)

(n_sim = nrow(mod_csim))

# 一号食谱鸡的体重分布
hist(
  rnorm(n_sim, mod_csim[, 'theta[1]'], mod_csim[, 'sigma'])
)

# 二号食谱鸡平均体重比一号食谱鸡平均体重高的概率
mean(mod_csim[, 'theta[2]'] > mod_csim[, 'theta[1]'])

# 一只三号食谱的鸡比一只一号食谱的鸡体重高的概率
mean(rnorm(n_sim, mod_csim[, 'theta[3]'], mod_csim[, 'sigma']) > rnorm(n_sim, mod_csim[, 'theta[1]'], mod_csim[, 'sigma']))

# 采用一个新的食谱，鸡的平均体重分布以及一只鸡的体重分布
hist(
  rnorm(n_sim, mod_csim[, 'mu'], 1 / mod_csim[, 'tau']^2)
)

hist(
  rnorm(n_sim,
        rnorm(n_sim, mod_csim[, 'mu'], 1 / mod_csim[, 'tau']^2),
        mod_csim[, 'sigma']
  )
)

# 使用正态分布会出现鸡的体重为0甚至是负数的情况，这是不符合常理的，这也是该模型的缺点。也许应该使用其他的分布来拟合鸡的体重。
