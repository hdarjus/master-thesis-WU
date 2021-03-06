library("zoo")
library("xts")
library("data.table")
library("tidyverse")
library("stochvollev")

run.version <- as.character(6)
thread.ind <- as.integer(Sys.getenv("SGE_TASK_ID"))
print(thread.ind)

source("data.R")
cutburning <- readRDS("cutburning.RDS")

row.ind <- ((thread.ind-1) %/% 2) + 1
ind.period <- as.numeric(cutburning[row.ind, 2])
ind.ticker <- as.numeric(cutburning[row.ind, 1])
ind.inits <- ((thread.ind-1) %% 2) + 1
ind.hyper <- 1

print(c(ind.hyper, ind.ticker, ind.inits, ind.period))

# data is assumed to be in an xts object called dat
if (!exists("dat")) {
  stop("dat doesn't exist")
}

# my own dataset
dat <- dat[, ind.ticker]

# delete rows with zero, there aren't many. there are no NAs
dat <- dat[dat > 0, ]

dat <- diff(log(dat))[-1]

# create data chunks of 3 years with quarterly moving window
dat.chunk.indices <- c()
for (start.year in seq(from = 2004, to = 2011, by = 1)) {
  for (start.month in c(1, 4, 7, 10)) {
    start.date <- as.Date(paste0(start.year, "-", start.month, "-01"))
    end.date <- as.Date(paste0(paste0(start.year+3, "-", start.month, "-01")))-1
    dat.chunk.indices <- c(dat.chunk.indices,
                           paste0(start.date, "/", end.date))
  }
}

dat <- dat[dat.chunk.indices[ind.period], ]

# priors
phi.grid <- matrix(c(20, 1.5),
                   ncol = 2, byrow = T)
sigma2.grid <- matrix(c(2.5, 0.025),
                      ncol = 2, byrow = T)
rho.grid <- matrix(c(1, 1),
                   ncol = 2, byrow = T)
mu.grid <- matrix(c(-9, 100), ncol = 2, byrow = T)
combinations <- nrow(phi.grid) * nrow(sigma2.grid) * nrow(rho.grid) * nrow(mu.grid)
hyperparam.grid <- matrix(rep(0, 4*2*combinations), ncol = 4*2)
                         
comb.count <- 0
for (phii in seq_len(nrow(phi.grid))) {
  for (sigma2i in seq_len(nrow(sigma2.grid))) {
    for (rhoi in seq_len(nrow(rho.grid))) {
      for (mui in seq_len(nrow(mu.grid))) {
        comb.count <- comb.count + 1
        hyperparam.grid[comb.count, ] <- c(phi.grid[phii, ],
                                           sigma2.grid[sigma2i, ],
                                           rho.grid[rhoi, ],
                                           mu.grid[mui, ])
      }
    }
  }
}

# setup
result <- list()
nsim <- 100000
priors <- list(
  phi.a = hyperparam.grid[ind.hyper, 1],
  phi.b = hyperparam.grid[ind.hyper, 2],
  sigma2.shape = hyperparam.grid[ind.hyper, 3],
  sigma2.rate = hyperparam.grid[ind.hyper, 4],
  rho.a = hyperparam.grid[ind.hyper, 5],
  rho.b = hyperparam.grid[ind.hyper, 6],
  mu.mean = hyperparam.grid[ind.hyper, 7],
  mu.var = hyperparam.grid[ind.hyper, 8]
)

initials <- as.list(expand.grid(
  phi = c(0.8),
  sigma2 = c(0.05),
  rho = c(0.2, -0.5),
  mu = c(-9)
))
initials <- lapply(initials, function (x, ind) x[ind], ind.inits)

result[["priors"]] <- priors
result[["initials"]] <- initials
result[["nsim"]] <- nsim
result[["data"]] <- dat
result[["dates"]] <- index(dat)
result[["years"]] <- dat.chunk.indices[ind.period]
result[["seed"]] <- thread.ind

# run
set.seed(thread.ind)
result[["result"]] <- fnMCMCSampler(dat - mean(dat), nsim, priors, initials, iBurnin = nsim %/% 3, sSigma2.Prior = "inv.gamma")

rho.samples <- result$result$samples$rho
start.burnin.cut <- sum(cumall(rho.samples[-1] == rho.samples[-length(rho.samples)])) + 1
print(start.burnin.cut)
result$result$samples <- lapply(result$result$samples, function (x, ind) x[ind:length(x)], start.burnin.cut)
result$result$h <- result$result$h[start.burnin.cut:nrow(result$result$h), ]
result$result$weights <- result$result$weights[start.burnin.cut:length(result$result$weights)]

# store only some quantiles of the posterior log variance
result$result$h <- t(apply(result$result$h, 2, function (x, probs) quantile(x, probs = probs), c(.01, .05, .1, .25, .5, .75, .9, .95, .99)))

# save results
dirname <- paste0("../results/", run.version, "/")
dir.create(dirname, showWarnings = F)

saveRDS(result, file = paste0(dirname,
                              "res_", formatC(ind.ticker, width=2, flag=0),
                              "_", formatC(ind.period, width=2, flag=0),
                              "_", formatC(ind.hyper, width=2, flag=0),
                              "_", formatC(ind.inits, width=2, flag=0),
                              "_", strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".RDS"))
