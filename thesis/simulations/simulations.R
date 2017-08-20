library(stochvollev)

ID <- as.integer(Sys.getenv("SGE_TASK_ID"))

set.seed(ID)

# Data generation

iN <- 1000
dPhi <- 0.95
dMu <- -9
dSigma2 <- 0.01

dat <- readRDS("simdat.RDS")
dat <- dat[[ID]]

# Runs

iNsim <- 50000
results <- list()

lInit <- list(phi = dPhi, sigma2 = dSigma2, rho = c(-0.9, -0.6, -0.3, 0, 0.3, 0.6, 0.9)[ID], mu = dMu)
lPriors <- list(mu.mean = -9, mu.var = 1, phi.a = 20, phi.b = 1.5,
                sigma2.shape = 2.25,  # prior mean 0.05 and variance 0.01
                sigma2.rate = 0.0625,
                rho.a = 1, rho.b = 1)
results <- fnMCMCSampler(dat$y, iNsim,
                     lInit = lInit, lPriors = lPriors,
                     iBurnin = iNsim %/% 2, sSigma2.Prior = "inv.gamma")

results$h <- t(apply(results$h, 2, function (x, probs) quantile(x, probs = probs), c(.01, .05, .1, .25, .5, .75, .9, .95, .99)))

saveRDS(results, paste0("gamma_project_results", ID, ".RDS"))

