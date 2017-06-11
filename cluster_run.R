source("data.R")

# data is assumed to be in a list object called data.chunks
if (!exists("data.chunks")) {
  stop("data.chunks doesn't exist")
}

library("parallel")
library("stochvollev")

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

nsim <- 200
initials <- list(phi = 0.6, sigma2 = 0.01, rho = -0.4, mu = -9)
runner <- function (chunk, n, initials, hyperparamgrid) {
  result <- list()
  for (i in seq_len(dim(hyperparamgrid)[1])) {
    subresult <- list()
    priors <- list(
      phi.a = hyperparamgrid[i, 1, 1],
      phi.b = hyperparamgrid[i, 1, 2],
      sigma2.shape = hyperparamgrid[i, 2, 1],
      sigma2.rate = hyperparamgrid[i, 2, 2],
      rho.a = hyperparamgrid[i, 3, 1],
      rho.b = hyperparamgrid[i, 3, 2],
      mu.mean = hyperparamgrid[i, 4, 1],
      mu.var = hyperparamgrid[i, 4, 2]
    )
    subresult[["priors"]] <- priors
    subresult[["initials"]] <- initials
    years <- lapply(chunk,
                    function (x, n, priors, initials) {
                      res <- fnMCMCSampler(x, n, priors, initials, iBurnin = n %/% 10)
                      res[["dates"]] <- index(x)
                      return(res)
                    },
                    n, priors, initials)
    subresult[["years"]] <- years
    result[[i]] <- subresult
  }
  return(result)
}

cl <- makeForkCluster(length(names(data.chunks)))
clusterSetRNGStream(cl, 42)
run.results <- parLapply(cl, data.chunks, runner, nsim, initials, hyperparam.grid)
stopCluster(cl)

saveRDS(run.results, file = paste0("results/run", strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".RDS"))
