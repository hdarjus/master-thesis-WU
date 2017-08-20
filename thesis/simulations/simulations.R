library(tidyverse)
library(stochvollev)

ID <- as.integer(Sys.getenv("SGE_TASK_ID"))

set.seed(ID+300)

# Data generation

i <- 4

iN <- 1000
dPhi <- 0.95
dMu <- -9
dSigma2 <- 0.01

dat <- fnGenLogSV(iN, 0, 0.1, 0.95, -9, ID+300)

# Runs

iNsim <- 10000
result <- list()

lInit <- list(phi = dPhi, sigma2 = dSigma2, rho = 0, mu = dMu)
lPriors <- list(mu.mean = -9, mu.var = 1, phi.a = 20, phi.b = 1.5,
                sigma2.shape = 2.25,  # prior mean 0.05 and variance 0.01
                sigma2.rate = 0.0625,
                rho.a = 1, rho.b = 1)
result <- fnMCMCSampler(dat[, "y"], iNsim,
                     lInit = lInit, lPriors = lPriors,
                     iBurnin = iNsim %/% 2, sSigma2.Prior = "inv.gamma")

rho.samples <- result$samples$rho
start.burnin.cut <- sum(cumall(rho.samples[-1] == rho.samples[-length(rho.samples)])) + 1
print(start.burnin.cut)
result$samples <- lapply(result$samples, function (x, ind) x[ind:length(x)], start.burnin.cut)
result$h <- result$h[start.burnin.cut:iNsim, ]
result$weights <- result$weights[start.burnin.cut:iNsim]

print(mean(result$samples$rho))

result$h <- t(apply(result$h, 2, function (x, probs) quantile(x, probs = probs), c(.01, .05, .1, .25, .5, .75, .9, .95, .99)))

saveRDS(result, paste0("gamma_project_results_4_", ID, ".RDS"))
saveRDS(dat, paste0("dat_", ID, ".RDS"))

