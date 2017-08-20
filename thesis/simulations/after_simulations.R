library(plotly)
library(tidyverse)
library(forcats)
library(zoo)

merged.h <- NULL
merged.samples <- NULL
merged.dat <- NULL

dat.list <- readRDS(paste0("simdat2.RDS"))

for (i in 1:7) {
  dataind <- as.integer(i)
  results <- readRDS(paste0("results5/gamma_project_results", i, ".RDS"))
  
  merged.h <- as_tibble(results$h) %>%
    mutate(Data.ind = dataind, Time.ind = seq_len(n())) %>%
    gather(key = "Quantile", value = "Value", 1:9, factor_key = T) %>%
    bind_rows(merged.h)
  
  merged.samples <- as_tibble(results$samples) %>%
    rename(Phi = phi, Rho = rho, Mu = mu, Sigma2 = sigma2) %>%
    mutate(Draw.ind = seq_len(n()), Data.ind = dataind) %>%
    add_column(Weights = results$weights) %>%
    bind_rows(merged.samples)
  
  merged.dat <- as_tibble(coredata(dat.list[[dataind]])) %>%
    rename(Y = y, H = h, Eps = eps, Eta = eta) %>%
    mutate(Time.ind = index(dat.list[[dataind]]), Data.ind = dataind) %>%
    bind_rows(merged.dat)
}

saveRDS(merged.dat, file = "dat.RDS")
saveRDS(gather(merged.samples, key = "Param", value = "Value", Phi:Mu, factor_key = T),
        file = "res-sam.RDS")
saveRDS(merged.h, file = "res-h.RDS")

rm(list = ls())


params <- readRDS("params.RDS")
dat <- readRDS("dat.RDS")
samples <- readRDS("res-sam.RDS")
h <- readRDS("res-h.RDS")

reweight <- function (x, weight) {
  sample(x, size = length(x), prob = weight, replace = T)
}

rownames(params) <- 1:7
print(xtable(params, caption = "Parameters used for simulation.", label = "tab:params"), booktabs = T, type = "latex")

# Good
dat %>%
  filter(Data.ind %in% c(1, 4, 7)) %>%
  group_by(Data.ind) %>%
  mutate(Price = 0.03*exp(cumsum(Y))) %>%
  ungroup() %>%
  ggplot(aes(x = Time.ind)) +
  #geom_area(aes(y = -exp(H/2), fill = "St.dev.")) +
  geom_area(aes(y = exp(H/2), fill = "St.dev.")) +
  #geom_line(aes(y = Y, color = "Return"), alpha = 0.9) +
  geom_line(aes(y = Price, color = "Price")) +
  facet_grid(Data.ind ~ ., scales = "free_y", labeller = labeller(Data.ind = c("1" = "rho = -0.9", "2" = "rho = -0.6", "3" = "rho = -0.3", "4" = "rho = 0", "5" = "rho = 0.3", "6" = "rho = 0.6", "7" = "rho = 0.9"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("Value") +
  scale_fill_manual("", values = c("St.dev." = "gray40")) +
  scale_color_manual("", values = c("Price" = "black")) +
  ggtitle("Simulated price process and standard devation")

# Good
## Param density plot
samples %>%
  filter(Param == "Rho") %>%
  group_by(Data.ind) %>%
  mutate(True = params[Data.ind, "rho"], Reweighted.Value = reweight(Value, Weights)) %>%
  ungroup() %>%
  ggplot() +
  stat_function(fun = function (x) dbeta((x+1)/2, 1, 1), mapping = aes(fill = "Prior"), geom = "area", alpha = 0.65, linetype = "blank") +
  geom_density(aes(x = Value, y = ..density.., fill = "Approximate posterior"), alpha = 0.65, linetype = "blank") +
  geom_density(aes(x = Reweighted.Value, y = ..density.., fill = "Posterior"), alpha = 0.65, linetype = "blank") +
  geom_vline(aes(xintercept = True, color = "Simulated")) +
  facet_wrap(~ Data.ind, ncol = 3, labeller = labeller(Data.ind = c("1" = "rho = -0.9", "2" = "rho = -0.6", "3" = "rho = -0.3", "4" = "rho = 0", "5" = "rho = 0.3", "6" = "rho = 0.6", "7" = "rho = 0.9"))) +
  xlim(-1, 1) +
  theme_bw() +
  theme(legend.position = "bottom", strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
  ylab("Density") +
  scale_fill_manual("Density", values = c("Approximate posterior" = "purple", "Prior" = "gray", "Posterior" = "blue")) +
  scale_color_manual("Point", values = c("Simulated" = "red")) +
  ggtitle(expression(paste(rho, " simulated value and densities")))

ggplot(samples %>% filter(Data.ind == dataind, Param == "Rho"), aes(x = Value)) +
  geom_density(aes(color = "Posterior")) +
  stat_function(fun = function (x) dbeta((x+1)/2, 1, 1), mapping = aes(color = "Prior")) +
  geom_vline(data = data.frame(X = params[dataind, "rho"]), mapping = aes(xintercept = X, color = "Simulated"), show.legend = F) +
  xlim(-1, 1) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual("", values = c("Posterior" = "black", "Prior" = "gray", "Simulated" = "green")) +
  ggtitle(expression(paste(rho, " densities")))

# Good
## Traceplot
samples %>%
  filter(Param == "Rho") %>%
  select(Draw.ind, Data.ind, Value) %>%
  ggplot(aes(x = Draw.ind, y = Value)) +
    geom_line() +
    facet_grid(Data.ind ~ .)

## Density plot
g <- samples %>%
  filter(Param == "Rho") %>%
  select(Draw.ind, Data.ind, Value) %>%
  ggplot(aes(x = Value)) +
    geom_density(aes(y = ..density..)) +
    facet_wrap(~ Data.ind, ncol = 3)
ggplotly(g)

ggplot(h, aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = Quantile)) +
  facet_grid(Data.ind ~ .) +
  scale_color_manual("Quantile (%)", values = c("gray85", "gray73", "gray60", "gray45", "gray10", "gray45", "gray60", "gray73", "gray85"))

##########################################################################

for (i in seq(1, 7)) {
  g <- dat %>%
    mutate(Variance = exp(H)) %>%
    rename(Log.Return = Y) %>%
    select(Log.Return, Variance, Time.ind, Data.ind) %>%
    filter(Data.ind %in% c(i, i+1)) %>%
    gather(key = "Type", value = "Value", Log.Return, Variance) %>%
    ggplot(aes(x = Time.ind, y = Value)) +
      geom_line() +
      facet_grid(Type ~ Data.ind, scales = "free") +
      theme_bw()
  if (i == 1)
    g <- g + ggtitle("Traceplots of return and variance")
  print(g)
}

# Good
## Variance
dat %>%
  rename(Value = H) %>%
  add_column(Quantile = "Simulated") %>%
  select(Data.ind, Time.ind, Quantile, Value) %>%
  bind_rows(h) %>%
  mutate(Data.ind = factor(as.character(Data.ind), labels = as.character(rev(1:7)))) %>%
  ggplot(aes(x = Time.ind, y = exp(Value))) +
  geom_line(aes(color = factor(Quantile, labels = c("1%", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "99%", "Simulated")))) +
  facet_grid(Data.ind ~ ., labeller = labeller(Data.ind = c("1" = "rho = -0.9", "2" = "rho = -0.6", "3" = "rho = -0.3", "4" = "rho = 0", "5" = "rho = 0.3", "6" = "rho = 0.6", "7" = "rho = 0.9"))) +
  theme_bw() +
  theme(legend.position = "bottom", strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
  xlab("Time") +
  ylab("Value") +
  scale_color_manual("Quantiles (%)", values = c("1%" = "gray85", "5%" = "gray73", "10%" = "gray60", "25%" = "gray45", "50%" = "gray10", "75%" = "gray45", "90%" = "gray60", "95%" = "gray73", "99%" = "gray85", "Simulated" = "red")) +
  ggtitle("Simulated and posterior variance")

ggplot(dat, aes(x = Time.ind, y = exp(H))) +
  geom_line() +
  facet_grid(Data.ind ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("Simulated var")



ggplot(h %>% filter(Prior == "inv.gamma"), aes(x = Time.ind, y = exp(Value))) +
  geom_line(aes(color = factor(Quantile))) +
  facet_grid(Data.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "left") +
  scale_color_manual("Quantiles (%)", values = c("gray85", "gray70", "gray10", "gray70", "gray85")) +
  ggtitle("Posterior var with InvGamma prior")

ggplot(dat, aes(x = Time.ind, y = exp(H))) +
  geom_line() +
  facet_grid(Data.ind ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("Simulated var")



ggplot(h %>% filter(Prior != "inv.gamma"), aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = setattr(factor(Quantile), "levels", c("1", "5", "50", "95", "99", "sim")))) +
  facet_grid(Data.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90", "red"))



ggplot(h %>% filter(Prior != "gamma"), aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = setattr(factor(Quantile), "levels", c("1", "5", "50", "95", "99", "sim")))) +
  facet_grid(Data.ind ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90", "red"))



ggplot(samples %>% filter(Param == "Phi"), aes(x = Draw.ind, y = Value)) +
  geom_line(color = "black") +
  facet_grid(Data.ind ~ Prior, scales = "free_y") +
  theme_bw() +
  ggtitle(expression(paste("Trace plots of ", phi)))


dataind <- 6

sim <- h %>% filter(Data.ind == dataind, Prior == "simulated")
sim2 <- sim
sim$Prior <- factor("gamma", levels = c("gamma", "inv.gamma", "simulated"))
sim2$Prior <- factor("inv.gamma", levels = c("gamma", "inv.gamma", "simulated"))
ggplot(h %>% filter(Data.ind == dataind, Prior != "simulated") %>% bind_rows(sim) %>% bind_rows(sim2),
       aes(x = Time.ind, y = Value)) +
  geom_line(aes(color = setattr(factor(Quantile), "levels", c("1", "5", "50", "95", "99", "simulated")))) +
  facet_grid(Prior ~ .) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual("Quantiles (%)", values = c("gray90", "gray70", "gray10", "gray70", "gray90", "red")) +
  ggtitle("Posterior and simulated volatilities")



ggplot(samples %>% filter(Data.ind == dataind), aes(x = Draw.ind, y = Value)) +
  geom_line() +
  facet_grid(Param ~ Prior, scales = "free_y") +
  theme_bw() +
  ggtitle("Trace plots of the parameters in the converged scenario")



ggplot(samples %>% filter(Data.ind == dataind, Param == "Phi"), aes(x = Value)) +
  geom_density(aes(color = Prior)) +
  stat_function(fun = function (x) dbeta((x+1)/2, 20, 1.5), mapping = aes(color = "prior")) +
  geom_vline(data = data.frame(X = 0.97), mapping = aes(colour = "simul", xintercept = X), show.legend = F) +
  xlim(0.6, 1) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual("", values = c(gamma = "purple", inv.gamma = "orange", prior = "gray", simul = "black"), labels = c(gamma = "Posterior Scen.Gamma", inv.gamma = "Posterior Scen.InvGamma", prior = "Prior", simul = "Simulated")) +
  ggtitle(expression(paste(phi, " densities")))

ggplot(samples %>% filter(Data.ind == dataind, Param == "Sigma2"), aes(x = Value)) +
  geom_density(aes(color = Prior)) +
  stat_function(fun = function (x) 1/x^2*dgamma(1/x, 2.01, rate = 0.0101), mapping = aes(color = "prior.invg")) +
  stat_function(fun = function (x) dgamma(x, 0.01, rate = 1), mapping = aes(color = "prior.g")) +
  geom_vline(data = data.frame(X = 0.01), mapping = aes(xintercept = X, color = "simul"), show.legend = F) +
  xlim(0, 0.08) +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual("", values = c("gamma" = "purple", "inv.gamma" = "orange", "prior.g" = "gray40", "prior.invg" = "gray70", "simul" = "green"), labels = c("gamma" = "Posterior Scen.Gamma", "inv.gamma" = "Posterior Scen.InvGamma", "prior.g" = "Prior Gamma", "prior.invg" = "Prior InvGamma", "simul" = "Simulated")) +
  ggtitle(expression(paste(sigma^2, " densities")))


## Joint density plot
samples %>%
  filter(Data.ind == dataind, Param %in% c("Sigma2", "Phi")) %>%
  spread(Param, Value) %>%
  ggplot(aes(x = Phi, y = Sigma2)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon") +
    scale_fill_gradient(low="blue", high="green") +
    theme_bw() +
    ggtitle(expression(paste("Joint distribution of ", sigma^2, " and ", phi)))

