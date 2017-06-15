library("stochvollev")

# 44x3x6 = 792 threads
thread.ind <- 792
n.period <- 3
n.ticker <- 44
n.hyper <- 6
ind.period <- ((thread.ind-1) %/% (n.ticker*n.hyper)) + 1
ind.ticker <- ((thread.ind-1) %/% (n.period*n.hyper)) + 1
ind.hyper <- ((thread.ind-1) %/% (n.period*n.ticker)) + 1

source("data.R")

# data is assumed to be in an xts object called dat
if (!exists("dat")) {
  stop("dat doesn't exist")
}

# my own dataset
dat <- dat[, ind.ticker]

# delete rows with zero, there aren't many. there are no NAs
dat <- dat[dat > 0, ]

dat <- diff(log(dat))[-1]

# create data chunks of 4 years with an overlap of 0 year
dat.chunk.indices <- c()
for (start.year in seq(from = 2004, to = 2012, by = 4)) {
  dat.chunk.indices <- c(dat.chunk.indices,
                         paste0(start.year, "-01-01/", start.year+3, "-12-31"))
}

dat <- dat[dat.chunk.indices[ind.period], ]

phi.grid <- matrix(c(3, 1.5),
                   ncol = 2, byrow = T)
sigma2.grid <- matrix(c(2.5, 0.025,
                        2.5, 5),
                      ncol = 2, byrow = T)
rho.grid <- matrix(c(20, 1.5,
                     0.5, 0.5,
                     1.5, 20),
                   ncol = 2, byrow = T)
mu.grid <- matrix(c(-9, 100), ncol = 2, byrow = T)
combinations <- nrow(phi.grid) * nrow(sigma2.grid) * nrow(rho.grid) * nrow(mu.grid)
hyperparam.grid <- array(data = NA_real_,
                         dim = c(combinations, 4, 2),
                         dimnames = c("combinations", "params", "hyperparams"))
comb.count <- 0
for (phii in seq_len(nrow(phi.grid))) {
  for (sigma2i in seq_len(nrow(sigma2.grid))) {
    for (rhoi in seq_len(nrow(rho.grid))) {
      for (mui in seq_len(nrow(mu.grid))) {
        comb.count <- comb.count + 1
        hyperparam.grid[comb.count, 1, ] <- phi.grid[phii, ]
        hyperparam.grid[comb.count, 2, ] <- sigma2.grid[sigma2i, ]
        hyperparam.grid[comb.count, 3, ] <- rho.grid[rhoi, ]
        hyperparam.grid[comb.count, 4, ] <- mu.grid[mui, ]
      }
    }
  }
}


result <- list()
nsim <- 100
initials <- list(phi = 0.6, sigma2 = 0.01, rho = -0.4, mu = -9)
priors <- list(
  phi.a = hyperparam.grid[ind.hyper, 1, 1],
  phi.b = hyperparam.grid[ind.hyper, 1, 2],
  sigma2.shape = hyperparam.grid[ind.hyper, 2, 1],
  sigma2.rate = hyperparam.grid[ind.hyper, 2, 2],
  rho.a = hyperparam.grid[ind.hyper, 3, 1],
  rho.b = hyperparam.grid[ind.hyper, 3, 2],
  mu.mean = hyperparam.grid[ind.hyper, 4, 1],
  mu.var = hyperparam.grid[ind.hyper, 4, 2]
)
result[["priors"]] <- priors
result[["initials"]] <- initials
result[["nsim"]] <- nsim
result[["data"]] <- dat
result[["dates"]] <- index(dat)
result[["years"]] <- dat.chunk.indices[ind.period]
result[["seed"]] <- thread.ind
set.seed(thread.ind)
result[["result"]] <- fnMCMCSampler(dat - mean(dat), nsim, priors, initials, iBurnin = nsim %/% 10)

saveRDS(result, file = paste0("res_", formatC(ind.ticker, width=2, flag=0),
                              "_", formatC(ind.period, width=2, flag=0),
                              "_", formatC(ind.hyper, width=2, flag=0),
                              "_", strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".RDS"))
