library(data.table)
library(forcats)
library(xtable)
library(ggplot2)
library(tidyr)
library(zoo)

merged.h <- NULL
merged.samples <- NULL
merged.dat <- NULL

for (i in 1:14) {
  params <- readRDS(paste0("results/params_", i, ".RDS"))
  dat.list <- readRDS(paste0("results/gamma_project_dat_", i, ".RDS"))
  results <- readRDS(paste0("results/gamma_project_results", i, ".RDS"))
  
  probs <- c(.01, .05, .5, .95, .99)
  res.h <- NULL
  h <- results[[1]]$h
  quant <- t(apply(h, 2, function (x, probs) quantile(x, probs = probs), probs))
  for (pp in probs) {
    colind <- which(pp == probs)
    values <- quant[, colind]
    res.h <- bind_rows(res.h, data.frame(Quantile = as.integer(100*pp),
                                         Value = values,
                                         Data.ind = i,
                                         Time.ind = seq_along(values)))
  }
  merged.h <- bind_rows(merged.h, res.h)
  
  merged.samples <- results[[1]]$samples %>%
    rename(Phi = phi, Rho = rho, Mu = mu, Sigma2 = sigma2) %>%
    mutate(Draw.ind = seq_len(n()),
           Data.ind = i) %>%
    bind_rows(merged.samples)
  
  merged.dat <- NULL
  merged.dat <- data.table(coredata(dat.list[[i]])) %>%
    mutate(Time.ind = index(dat.list[[i]]),
           Data.ind = i) %>%
    bind_rows(merged.dat)
}
colnames(merged.dat)[1:4] <- c("Y", "H", "Eps", "Eta")
saveRDS(merged.dat, file = "dat.RDS")
saveRDS(gather(merged.samples, key = "Param", value = "Value", Phi:Mu, factor_key = T),
        file = "res-sam.RDS")
saveRDS(merged.h, file = "res-h.RDS")

rm(list = ls())


params <- readRDS("params.RDS")
dat <- readRDS("dat.RDS")
samples <- readRDS("res-sam.RDS")
h <- readRDS("res-h.RDS")


rownames(params) <- 1:14
print(xtable(params, caption = "Parameters used for simulation.", label = "tab:params"), booktabs = T, type = "latex")


ggplot(samples %>%
         filter(Param == "Phi") %>%
         select(Draw.ind, Data.ind, Value),
       aes(x = Draw.ind, y = Value)) +
  geom_line() +
  facet_grid(Data.ind ~ .)

ggplot(samples %>%
         filter(Param == "Phi") %>%
         select(Draw.ind, Data.ind, Value),
       aes(x = Value)) +
  geom_density(aes(y = ..density..)) +
  facet_wrap(~ Data.ind, ncol = 3)

ggplot(h, aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = as.factor(Quantile))) +
  facet_grid(Data.ind ~ .) +
  scale_color_manual("Quantile (%)", values = c("gray85", "gray70", "gray10", "gray70", "gray85"))

##########################################################################

for (i in seq(1, 14, by = 2)) {
  g <- ggplot(data = dat %>%
                mutate(Variance = exp(H)) %>%
                rename(Log.Return = Y) %>%
                select(Log.Return, Variance, Time.ind, Param.ind) %>%
                filter(Param.ind %in% c(i, i+1)) %>%
                gather(key = "Type", value = "Value", Log.Return, Variance),
              aes(x = Time.ind, y = Value)) +
    geom_line() +
    facet_grid(Type ~ Param.ind, scales = "free") +
    theme_bw()
  if (i == 1)
    g <- g + ggtitle("Traceplots of return and variance")
  print(g)
}



ggplot(h %>% filter(Prior != "simulated"), aes(x = Time.ind, y = exp(Value))) +
  geom_line(aes(color = factor(Quantile))) +
  facet_grid(Param.ind ~ Prior, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90"))



ggplot(h %>% filter(Prior == "gamma"), aes(x = Time.ind, y = exp(Value))) +
  geom_line(aes(color = factor(Quantile))) +
  facet_grid(Param.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "left") +
  scale_color_manual("Quantiles (%)", values = c("gray85", "gray70", "gray10", "gray70", "gray85")) +
  ggtitle("Posterior var with Gamma prior")

ggplot(dat, aes(x = Time.ind, y = exp(H))) +
  geom_line() +
  facet_grid(Param.ind ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("Simulated var")



ggplot(h %>% filter(Prior == "inv.gamma"), aes(x = Time.ind, y = exp(Value))) +
  geom_line(aes(color = factor(Quantile))) +
  facet_grid(Param.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "left") +
  scale_color_manual("Quantiles (%)", values = c("gray85", "gray70", "gray10", "gray70", "gray85")) +
  ggtitle("Posterior var with InvGamma prior")

ggplot(dat, aes(x = Time.ind, y = exp(H))) +
  geom_line() +
  facet_grid(Param.ind ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("Simulated var")



ggplot(h %>% filter(Prior != "inv.gamma"), aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = setattr(factor(Quantile), "levels", c("1", "5", "50", "95", "99", "sim")))) +
  facet_grid(Param.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90", "red"))



ggplot(h %>% filter(Prior != "gamma"), aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = setattr(factor(Quantile), "levels", c("1", "5", "50", "95", "99", "sim")))) +
  facet_grid(Param.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90", "red"))



ggplot(samples %>% filter(Param == "Phi"), aes(x = Draw.ind, y = Value)) +
  geom_line(color = "black") +
  facet_grid(Param.ind ~ Prior, scales = "free_y") +
  theme_bw() +
  ggtitle(expression(paste("Trace plots of ", phi)))



sim <- h %>% filter(Param.ind == 6, Prior == "simulated")
sim2 <- sim
sim$Prior <- factor("gamma", levels = c("gamma", "inv.gamma", "simulated"))
sim2$Prior <- factor("inv.gamma", levels = c("gamma", "inv.gamma", "simulated"))
ggplot(h %>% filter(Param.ind == 6, Prior != "simulated") %>% bind_rows(sim) %>% bind_rows(sim2),
       aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = setattr(factor(Quantile), "levels", c("1", "5", "50", "95", "99", "simulated")))) +
  facet_grid(Prior ~ .) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90", "red")) +
  ggtitle("Posterior and simulated volatilities")



ggplot(samples %>% filter(Param.ind == 6), aes(x = Draw.ind, y = Value)) +
  geom_line() +
  facet_grid(Param ~ Prior, scales = "free_y") +
  theme_bw() +
  ggtitle("Trace plots of the parameters in the converged scenario")



ggplot(samples %>% filter(Param.ind == 6, Param == "Phi"), aes(x = Value)) +
  geom_density(aes(color = Prior)) +
  stat_function(fun = function (x) dbeta((x+1)/2, 20, 1.5), mapping = aes(color = "prior")) +
  geom_vline(data = data.frame(X = 0.97), mapping = aes(colour = "simul", xintercept = X), show.legend = F) +
  xlim(0.6, 1) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual("", values = c(gamma = "purple", inv.gamma = "orange", prior = "gray", simul = "black"), labels = c(gamma = "Posterior Scen.Gamma", inv.gamma = "Posterior Scen.InvGamma", prior = "Prior", simul = "Simulated")) +
  ggtitle(expression(paste(phi, " densities")))

ggplot(samples %>% filter(Param.ind == 6, Param == "Sigma2"), aes(x = Value)) +
  geom_density(aes(color = Prior)) +
  stat_function(fun = function (x) 1/x^2*dgamma(1/x, 2.01, rate = 0.0101), mapping = aes(color = "prior.invg")) +
  stat_function(fun = function (x) dgamma(x, 0.01, rate = 1), mapping = aes(color = "prior.g")) +
  geom_vline(data = data.frame(X = 0.01), mapping = aes(xintercept = X, color = "simul"), show.legend = F) +
  xlim(0, 0.08) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual("", values = c("gamma" = "purple", "inv.gamma" = "orange", "prior.g" = "gray40", "prior.invg" = "gray70", "simul" = "green"), labels = c("gamma" = "Posterior Scen.Gamma", "inv.gamma" = "Posterior Scen.InvGamma", "prior.g" = "Prior Gamma", "prior.invg" = "Prior InvGamma", "simul" = "Simulated")) +
  ggtitle(expression(paste(sigma^2, " densities")))

ggplot(samples %>% filter(Param.ind == 6, Param == "Rho"), aes(x = Value)) +
  geom_density(aes(color = Prior)) +
  stat_function(fun = function (x) dbeta((x+1)/2, 1, 1), mapping = aes(color = "prior")) +
  geom_vline(data = data.frame(X = 0), mapping = aes(xintercept = X, color = "simul"), show.legend = F) +
  xlim(-1, 1) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual("", values = c("gamma" = "purple", "inv.gamma" = "orange", "prior" = "gray", "simul" = "green"), labels = c("gamma" = "Posterior Scen.Gamma", "inv.gamma" = "Posterior Scen.InvGamma", "prior" = "Prior", "simul" = "Simulated")) +
  ggtitle(expression(paste(rho, " densities")))



ggplot(data = samples %>% filter(Param.ind == 6, Param %in% c("Sigma2", "Phi")) %>% spread(Param, Value),
       mapping = aes(x = Phi, y = Sigma2)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low="blue", high="green") +
  facet_grid(Prior ~ .) +
  theme_bw() +
  ggtitle(expression(paste("Joint distribution of ", sigma^2, " and ", phi)))

