library(stochvollev)

ID <- as.integer(Sys.getenv("SGE_TASK_ID"))

set.seed(1)

# Data generation

iN <- 1000
dPhi <- 0.95
dMu <- -9

params <- data.frame(n = NA_integer_, phi = NA_real_, mu = NA_real_, rho = NA_real_, sigma2 = NA_real_)
dat <- list()

for (dRho in c(-0.9, -0.6, -0.3, 0, 0.3, 0.6, 0.9)) {
  for (dSigma2 in c(0.01)) {
    params <- rbind(params, c(iN, dPhi, dMu, dRho, dSigma2))
    dat[[length(dat) + 1]] <- fnGenLogSV(iN, dRho, dSigma2, dPhi, dMu)
  }
}

params <- params[-1, ]

saveRDS(params, paste0("params_", ID, ".RDS"))
saveRDS(dat, paste0("gamma_project_dat_", ID, ".RDS"))

# Runs

iNsim <- 100000

i <- ID
lInit <- list(phi = params[i, "phi"], sigma2 = params[i, "sigma2"], rho = params[i, "rho"], mu = params[i, "mu"])
lPriors <- list(mu.mean = -9, mu.var = 1, phi.a = 20, phi.b = 1.5,
                sigma2.shape = 2.25,  # prior mean 0.05 and variance 0.01
                sigma2.rate = 0.0625,
                rho.a = 1, rho.b = 1)
results <- fnMCMCSampler(dat[[i]]$y, iNsim,
                     lInit = lInit, lPriors = lPriors,
                     iBurnin = iNsim %/% 2, sSigma2.Prior = "inv.gamma")

results$h <- t(apply(results$h, 2, function (x, probs) quantile(x, probs = probs), c(.01, .05, .1, .25, .5, .75, .9, .95, .99)))

saveRDS(results, paste0("gamma_project_results", ID, ".RDS"))
