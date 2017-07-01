dat <- readRDS("results/run2017-06-10_23-15-40.RDS")
probs <- c(0.01, 0.05, 0.5, 0.95, 0.99)

# log volatility
# 1 company 1 period
comp.year.vol <- function (dat, company, year, prior.combi, probs, logvol = T) {
  year <- as.character(year)
  h <- dat[[company]][[prior.combi]]$years[[year]]$h
  if (!logvol)
    h <- exp(h/2)
  quantiles <- apply(h, 2, function (x, probs) quantile(x, probs = probs), probs)
  zoo(t(quantiles), order.by = dat[[company]][[prior.combi]]$years[[year]]$dates)
}

prior.combi <- 1
comp <- "BAS.DE"
opar <- par(no.readonly = T)
plot(comp.year.vol(dat, comp, 2000, prior.combi, probs),
     screens = 1,
     col = c("light gray", "dark gray", "black", "dark gray", "light gray"),
     ylab = "log volatility",
     xlab = "",
     main = "Log volatility quantiles")
par(opar)

# log volatility
# 1 company all periods
prior.combi <- 1
year.starts <- NULL
comp.vol <- NULL
for (year in names(dat[[comp]][[prior.combi]]$years)) {
  comp.vol <- rbind(comp.vol, comp.year.vol(dat, comp, year, 1, probs))
  year.starts <- c(year.starts, dat[[comp]][[prior.combi]]$years[[year]]$dates[1])
}

plot(comp.vol,
     screens = 1,
     col = c("light gray", "dark gray", "black", "dark gray", "light gray"),
     ylab = "log volatility",
     xlab = "",
     main = "Log volatility quantiles")
abline(v = year.starts[-1], col = "red")

# log volatility
# all companies all periods
prior.combi <- 1
opar <- par(mar = c(2.5, 3, 1.5, 1))
layout(matrix(1:length(dat), ncol = 1))
for (comp in names(dat)) {
  year.starts <- NULL
  comp.vol <- NULL
  for (year in names(dat[[comp]][[prior.combi]]$years)) {
    comp.vol <- rbind(comp.vol, comp.year.vol(dat, comp, year, 1, probs))
    year.starts <- c(year.starts, dat[[comp]][[prior.combi]]$years[[year]]$dates[1])
  }
  
  plot(comp.vol,
       screens = 1,
       col = c("light gray", "dark gray", "black", "dark gray", "light gray"),
       ylab = "log volatility",
       xlab = "",
       main = comp)
  abline(v = year.starts[-1], col = "red")
}
par(opar)

# traceplot
# all parameters 1 company 1 period
prior.combi <- 1
params <- c("mu", "phi", "sigma2", "rho")
comp <- "BAS.DE"
year <- as.character(2000)
opar <- par(mar = c(2.5, 5, 0.5, 1))
layout(matrix(1:4, ncol = 1))
for (param in params) {
  plot(dat[[comp]][[prior.combi]]$years[[year]]$samples[[param]],
       type = "l",
       ylab = param,
       xlab = "")
  abline(h = mean(dat[[comp]][[prior.combi]]$years[[year]]$samples[[param]]),
         lty = 2,
         col = "dark green")
}
par(opar)

# traceplot
# 1 parameter all companies 1 period
prior.combi <- 1
param <- "rho"
year <- as.character(2000)
opar <- par(mar = c(2.5, 4.5, 1.5, 1))
layout(matrix(1:length(dat), ncol = 1))
for (comp in names(dat)) {
  plot(dat[[comp]][[prior.combi]]$years[[year]]$samples[[param]],
       type = "l",
       ylab = param,
       xlab = "",
       main = comp)
  abline(h = mean(dat[[comp]][[prior.combi]]$years[[year]]$samples[[param]]),
         lty = 2,
         col = "dark green")
}
par(opar)

# prior - posterior density plots
# 1 parameter all companies all periods
param <- "rho"
prior.combi <- 1
opar <- par(mar = c(2.5, 4.5, 1.5, 1))
layout(matrix(1:(length(dat)*length(dat[[1]][[1]]$years)),
              nrow = length(dat), byrow = T))
for (comp in names(dat)) {
  for (year in names(dat[[comp]][[prior.combi]]$years)) {
    plot(density(dat[[comp]][[prior.combi]]$years[[year]]$samples[[param]]),
         ylab = param,
         xlim = c(-1, 1),
         main = paste0(comp, " ", year, "-", as.numeric(year)+3))
    curve(dbeta((x+1)/2,
                dat[[comp]][[prior.combi]]$priors$rho.a,
                dat[[comp]][[prior.combi]]$priors$rho.b),
          col = "dark green",
          add = T)
  }
}
par(opar)

# prior - posterior
# 1 parameter 1 company 1 period all prior combinations
param <- "rho"
comp <- "BAS.DE"
year <- as.character(2000)
opar <- par(mar = c(2.5, 4.5, 1.5, 1))
layout(matrix(1:6, nrow = 3, byrow = F))
prior.combi <- 0
for (s in 1:2) {
  for (r in 1:3) {
    prior.combi <- prior.combi + 1
    plot(density(dat[[comp]][[prior.combi]]$years[[year]]$samples[[param]]),
         ylab = param,
         xlim = c(-1, 1),
         main = paste0("Sigma prior rate=", dat[[comp]][[prior.combi]]$priors$sigma2.rate,
                       ",    Rho prior=Beta(",
                       dat[[comp]][[prior.combi]]$priors$rho.a, ", ",
                       dat[[comp]][[prior.combi]]$priors$rho.b, ")"))
    curve(dbeta((x+1)/2,
                dat[[comp]][[prior.combi]]$priors$rho.a,
                dat[[comp]][[prior.combi]]$priors$rho.b),
          col = "dark green",
          add = T)
  }
}
par(opar)
