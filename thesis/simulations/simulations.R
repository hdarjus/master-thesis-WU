library(stochvollev)

sink(file = "log.txt", type = "output", append = F)

set.seed(1)

# Data generation

iN <- 500
dPhi <- 0.95
dMu <- -9

params <- data.frame(n = NA_integer_, phi = NA_real_, mu = NA_real_, rho = NA_real_, sigma = NA_real_)
dat <- list()

for (dRho in c(-0.9, -0.6, -0.3, 0, 0.3, 0.6, 0.9)) {
  for (dSigma in c(0.01, 0.1)) {
    params <- rbind(params, c(iN, dPhi, dMu, dRho, dSigma))
    dat[[length(dat) + 1]] <- fnGenLogSV(iN, dRho, dSigma, dPhi, dMu)
  }
}

params <- params[-1, ]

saveRDS(params, "params.RDS")
saveRDS(dat, "gamma_project_dat.RDS")

# Runs

iNsim <- 10000
results <- list()

for (i in seq_len(nrow(params))) {
  ind <- length(results) + 1
  lInit <- list(phi = params[i, "phi"], sigma2 = (params[i, "sigma"])^2, rho = params[i, "rho"], mu = params[i, "mu"])
  results[[ind]] <- list()
    lPriors <- list(mu.mean = -9, mu.var = 1, phi.a = 20, phi.b = 1.5,
                    sigma2.shape = 2.25,  # prior mean 0.05 and variance 0.01
                    sigma2.rate = 0.0625,
                    rho.a = 1, rho.b = 1)
    results[[ind]] <- fnMCMCSampler(dat[[i]]$y, iNsim,
                         lInit = lInit, lPriors = lPriors,
                         iBurnin = iNsim %/% 2)
}

saveRDS(results, "gamma_project_results.RDS")
