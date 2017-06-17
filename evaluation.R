library("zoo")
library("xts")

# helper functions
plotparamtrace <- function (ticker, param, res = results) {
  dat <- res[[ticker]]
  traces <- lapply(dat, function (x, param) x$result$samples[[param]], param)
  toplot <- do.call("data.frame", traces)
  toplot <- zoo(toplot, order.by = seq_along(traces[[1]]))
  colnames(toplot) <- paste(c("'04", "'08", "'12"), "-", c("'08", "'12", "'16"))
  toplot <- toplot[, c(3, 2, 1)]
  plot(toplot,
       main = paste("Traceplots of", param, "of", ticker))
}

plotrhodensities <- function (res = results) {
  param <- "rho"
  dat <- res[[ticker]][[period]]
  opar <- par(no.readonly = T,
              mar = c(4, 5, 2, 2))
  layout(matrix(1:12, ncol = 3, byrow = T))
  for (i in 1:3) {
    plot(density(dat[[i]]$result$samples[[param]]),
         main = period,
         xlab = param, xlim = c(-1, 1), #ylim = c(0, 5),
         ylab = "Density")
    curve(dbeta((x+1)/2, dat[[i]]$prior$rho.a, dat[[i]]$prior$rho.b),
          add = T,
          col = "gray")
  }
  layout(1)
  par(opar)
}

plotsigma2densities <- function (ticker, period, xlim2, res = results) {
  param <- "sigma2"
  dat <- res[[ticker]][[period]]
  opar <- par(no.readonly = T,
              mar = c(4, 5, 2, 2))
  layout(matrix(1:6, nrow = 2, byrow = T))
  for (i in 1:6) {
    plot(density(dat[[i]]$result$samples[[param]]),
         main = paste0("Sigma^2 prior: InvGamma(",
                       dat[[i]]$prior$sigma2.shape,
                       ", ",
                       dat[[i]]$prior$sigma2.rate,
                       ")"),
         xlab = param, xlim = c(0, xlim2), #ylim = c(0, 5),
         ylab = "Density")
    curve(1/x^2*dgamma(1/x, shape = dat[[i]]$prior$sigma2.shape, rate = dat[[i]]$prior$sigma2.rate),
          add = T,
          col = "gray")
  }
  layout(1)
  par(opar)
}

periodends <- as.Date(c("2007-12-31", "2011-12-31"))
ploth <- function (ticker, start, name = ticker,
                   res = results, ps = periods, pends = periodends) {
  dat <- res[[ticker]]
  dxts <- as.zoo(dat[[ps[1]]]$data)
  hzoo <- zoo(exp(dat[[ps[1]]]$result$h/2),
              order.by = dat[[ps[1]]]$dates)
  for (p in ps[-1]) {
    hzoo <- rbind(hzoo,
                  zoo(exp(dat[[p]]$result$h/2),
                      order.by = dat[[p]]$dates))
    dxts <- rbind(dxts,
                  as.zoo(dat[[p]]$data))
  }
  opar <- par(no.readonly = T, mar = c(3, 4, 3, 2))
  layout(matrix(1:2, ncol = 1))
  plot(start*exp(cumsum(dxts)),
       ylab = "price", xlab = "",
       main = name)
  plot(hzoo,
       screens = 1,
       col = c("light gray", "dark gray", "black", "dark gray", "light gray"),
       ylab = "volatility", xlab = "",
       main = paste0("Posterior volatility of ", name))
  abline(v = pends, col = "turquoise", lty = 2)
  legend("topleft",
         legend = c("Median", "5-95% quantiles", "1-99% quantiles", "Period separator"),
         col = c("black", "dark gray", "light gray", "turquoise"),
         lty = c(1, 1, 1, 2),
         cex = .9, y.intersp = .85)
  layout(1)
  par(opar)
}

# data preparation

probs <- c(.01, .05, .5, .95, .99)

if (F) {
  results <- list()
  for (f in list.files("results/mcmc-run/", full.names = T)) {
    if (!(substr(f, start = 33, stop = 34) == "02"))
      next
    subresult <- readRDS(f)
    tosave <- list()
    # param quantiles
    tosave$quantiles <- lapply(subresult$result$samples, 
                               function (x, probs)
                                 quantile(x, probs = probs),
                               probs)
    # param densities
    tosave$densities <- lapply(subresult$result$samples, 
                               function (x)
                                 density(x))
    # h quantiles
    tosave$h <- subresult$result$h
    # priors
    tosave$prior <- subresult$priors
    if (is.null(results[[colnames(subresult$data)]]))
      results[[colnames(subresult$data)]] <- list()
    results[[colnames(subresult$data)]][[subresult$years]][[
      length(results[[colnames(subresult$data)]][[subresult$years]]) + 1]] <- tosave
  }
  
  tickers <- names(results)
  periods <- sort(names(results[[1]]))
  
  saveRDS(tickers, file = "results/tickers.RDS")
  saveRDS(periods, file = "results/periods.RDS")
} else {
  tickers <- readRDS("results/tickers.RDS")
  periods <- readRDS("results/periods.RDS")
}

# based on the densities I chose tickers 1, 10, 23, 34 plus the two indices
tick <- c(8, 10, 34, 35, 43, 44)
names <- c("SAICO Motor", "China Unicom",
           "Adidas", "Deutsche Telekom", "SSE50 Index", "DAX Index")
if (F) {
  results <- list()
  for (f in list.files("results/mcmc-run/", full.names = T)) {
    if (!(substr(f, start = 27, stop = 28) %in% formatC(tick, width = 2, flag = 0)))
      next
    if (!(substr(f, start = 33, stop = 34) == "02"))
      next
    subresult <- readRDS(f)
    if (is.null(results[[colnames(subresult$data)]]))
      results[[colnames(subresult$data)]] <- list()
    results[[colnames(subresult$data)]][[subresult$years]] <- subresult
  }
  
  saveRDS(results, file = "results/chosen_stocks.RDS")
} else {
  results <- readRDS("results/chosen_stocks.RDS")
}

# Questions:
# 1. China vs. Germany (leverage and volatility)
# 2. Leverage in time, does it change consistently?

# I'll use the hyparameter setting number 2.
# - least informative for rho
# - phi is expected to be positive, but less informative than in Omori informative
# - sigma2 I use the one from Omori. I tried another as well, but then the prior and the posterior were really far
# - quite non-informative for mu

# 1.
# volatility in china and germany telecom
ploth(tickers[tick[2]], name = names[2], start = 100)
ploth(tickers[tick[4]], name = names[4], start = 100)

# parameters in chinese and german stocks
plotparams <- function (periodind, sigmatop) {
  opar <- par(no.readonly = T, mar = c(2, 4, 2, 1))
  layout(matrix(1:12, ncol = 3, byrow = T))
  for (ti in c(1,2,4,3)) {
    dat <- results[[tickers[tick[ti]]]][[periods[periodind]]]
    # plot phi
    plot(density(dat$result$samples$phi),
         main = bquote(.(names[ti]) ~ phi),
         xlab = "phi", xlim = c(0, 1), #ylim = c(0, 5),
         ylab = "Density")
    curve(dbeta((x+1)/2, dat$prior$phi.a, dat$prior$phi.b),
          add = T,
          col = "gray")
    # plot sigma2
    plot(density(dat$result$samples$sigma2),
         main = bquote(.(names[ti]) ~ sigma^2),
         xlab = "sigma^2", xlim = c(0, sigmatop), #ylim = c(0, 5),
         ylab = "Density")
    curve(1/x^2*dgamma(1/x, shape = dat$prior$sigma2.shape, rate = dat$prior$sigma2.rate),
          add = T,
          col = "gray")
    # plot rho
    plot(density(dat$result$samples$rho),
         main = bquote(.(names[ti]) ~ rho),
         xlab = "rho", xlim = c(-1, 1), #ylim = c(0, 5),
         ylab = "Density")
    curve(dbeta((x+1)/2, dat$prior$rho.a, dat$prior$rho.b),
          add = T,
          col = "gray")
  }
  layout(1)
  par(opar)
}
## before the crisis
#plotparams(1, .8)

## through the crisis
#plotparams(2, .8)

## after the crisis
#plotparams(3, .8)

periodnames <- paste0(c(2004, 2008, 2012), "-", c(2008, 2012, 2016))

# rho
opar <- par(no.readonly = T, mar = c(2, 4, 2, 1), cex.main = 0.95)
layout(matrix(1:12, ncol = 4, byrow = T))  # periods x companies
for (periodind in 1:3) {
  for (ti in c(1,2,4,3)) {
    dat <- results[[tickers[tick[ti]]]][[periods[periodind]]]
    plot(density(dat$result$samples$rho),
         main = paste0(names[ti], " ", periodnames[periodind]),
         xlab = "", xlim = c(-1, 1), ylim = c(0, 4.5),
         ylab = "Density")
    curve(dbeta((x+1)/2, dat$prior$rho.a, dat$prior$rho.b),
          add = T,
          col = "gray")
  }
}
layout(1)
par(opar)

# sigma2
opar <- par(no.readonly = T, mar = c(2, 4, 2, 1), cex.main = 0.95)
layout(matrix(1:12, ncol = 4, byrow = T))  # periods x companies
for (periodind in 1:3) {
  for (ti in c(1,2,4,3)) {
    dat <- results[[tickers[tick[ti]]]][[periods[periodind]]]
    plot(density(dat$result$samples$sigma2),
         main = paste0(names[ti], " ", periodnames[periodind]),
         xlab = "", xlim = c(0, .6), ylim = c(0, 40),
         ylab = "Density")
    curve(1/x^2*dgamma(1/x, shape = dat$prior$sigma2.shape, rate = dat$prior$sigma2.rate),
          add = T,
          col = "gray")
  }
}
layout(1)
par(opar)

# phi
opar <- par(no.readonly = T, mar = c(2, 4, 2, 1), cex.main = 0.95)
layout(matrix(1:12, ncol = 4, byrow = T))  # periods x companies
for (periodind in 1:3) {
  for (ti in c(1,2,4,3)) {
    dat <- results[[tickers[tick[ti]]]][[periods[periodind]]]
    plot(density(dat$result$samples$phi),
         main = paste0(names[ti], " ", periodnames[periodind]),
         xlab = "", xlim = c(0.4, 1), ylim = c(0, 30),
         ylab = "Density")
    curve(dbeta((x+1)/2, dat$prior$phi.a, dat$prior$phi.b),
          add = T,
          col = "gray")
  }
}
layout(1)
par(opar)

# 2.
# in china 
plotrhodensities(tickers[tick[2]], periods[1])
plotrhodensities(tickers[tick[2]], periods[2])
plotrhodensities(tickers[tick[2]], periods[3])

# in germany
plotrhodensities(tickers[tick[5]], periods[1])
plotrhodensities(tickers[tick[5]], periods[2])
plotrhodensities(tickers[tick[5]], periods[3])

ploth.old <- function (ticker, hyperind, start, name = ticker,
                       res = results, ps = periods, pends = periodends) {
  dat <- res[[ticker]]
  dxts <- as.zoo(dat[[ps[1]]][[hyperind]]$data)
  hzoo <- zoo(exp(dat[[ps[1]]][[hyperind]]$result$h/2),
              order.by = dat[[ps[1]]][[hyperind]]$dates)
  for (p in ps[-1]) {
    hzoo <- rbind(hzoo,
                  zoo(exp(dat[[p]][[hyperind]]$result$h/2),
                      order.by = dat[[p]][[hyperind]]$dates))
    dxts <- rbind(dxts,
                  as.zoo(dat[[p]][[hyperind]]$data))
  }
  opar <- par(no.readonly = T, mar = c(3, 4, 3, 2))
  layout(matrix(1:2, ncol = 1))
  plot(start*exp(cumsum(dxts)),
       ylab = "price", xlab = "",
       main = name)
  plot(hzoo,
       screens = 1,
       col = c("light gray", "dark gray", "black", "dark gray", "light gray"),
       ylab = "volatility", xlab = "",
       main = paste0("Posterior volatility of ", name))
  abline(v = pends, col = "turquoise", lty = 2)
  legend("topleft",
         legend = c("Median", "5-95% quantiles", "1-99% quantiles", "Period separator"),
         col = c("black", "dark gray", "light gray", "turquoise"),
         lty = c(1, 1, 1, 2),
         cex = .9, y.intersp = .85)
  layout(1)
  par(opar)
}
