library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(forcats)

set.seed(137)

# Explore results
calfolder <- getwd()
resfolder <- "../results/2/"  # results folder

# Convergence
## Select the best initial value

### Check which runs converged and use only 1 out of the 3 with different initial values.
if (FALSE) {
  library(zoo)
  library(xts)
  
  fnAcceptanceRatio <- function (x) mean(x[-1] != x[-length(x)])
  
  res.files <- list.files(resfolder)
  res.setups.path <- unique(substr(res.files, start = 1, stop = 13))
  # Collect data and results into data frames
  counter <- 0  # common identifier
  obs <- NULL  # observations
  h <- NULL  # variance estimates
  params <- NULL  # phi, rho, mu, sigma2
  # Read and handle files that correspond to the same dataset together
  for (filename.pat in res.setups.path) {
    counter <- counter + 1
    files.path <- list.files(resfolder, pattern = filename.pat)
    files.list <- list()
    for (file.path in files.path) {
      files.list[[length(files.list) + 1]] <- readRDS(paste0(resfolder, file.path))
    }
    # Use the datasets with the highest acceptance ratio, but only if it is also reasonable
    dataset.ar <- unlist(lapply(files.list, function (x, fn) fn(x$result$samples$phi), fnAcceptanceRatio))
    dataset.ind <- which.max(dataset.ar)
    if (dataset.ar[dataset.ind] < 0.2) {
      next()
    }
    
    dataset <- files.list[[dataset.ind]]
    obs <- bind_rows(obs,
                     data.frame(ID = counter,
                                Date = dataset$dates,
                                Period = dataset$years,  # to factor later
                                Company = colnames(dataset$data),  # to factor later
                                Values = as.numeric(coredata(dataset$data))))
    h <- data.frame(ID = counter,
                    Date = dataset$dates,
                    Period = dataset$years,  # to factor later
                    Company = colnames(dataset$data)) %>%  # to factor later
      bind_cols(as.data.frame(dataset$result$h)) %>%
      bind_rows(h)
    thinning <- seq(1, 100000, by = 40)  # I only use a part of the dataset for experimenting and formulating the results
    params <- bind_rows(params,
                        data.frame(ID = counter,
                                   Period = dataset$years,  # to factor later
                                   Company = colnames(dataset$data),  # to factor later
                                   Index = seq_along(thinning),
                                   Phi = dataset$result$samples$phi[thinning],
                                   Rho = dataset$result$samples$rho[thinning],
                                   Sigma2 = dataset$result$samples$sigma2[thinning],
                                   Mu = dataset$result$samples$mu[thinning]))
  }
  initials.df <- as.data.frame(do.call("rbind", lapply(files.list, function (x) unlist(x$initials))))
  priors.df <- as.data.frame(t(unlist(files.list[[1]]$priors)))
  saveRDS(initials.df, "initials.RDS")
  saveRDS(priors.df, "priors.RDS")
  
  obs$Period <- as.factor(obs$Period)
  obs$Company <- as.factor(obs$Company)
  h$Period <- as.factor(h$Period)
  h$Company <- as.factor(h$Company)
  params$Period <- as.factor(params$Period)
  params$Company <- as.factor(params$Company)
  saveRDS(obs, "observations.RDS")
  saveRDS(h, "volatility.RDS")
  saveRDS(params, "parameters.RDS")
  
  rm(list = ls())
}

# Read data
priors <- readRDS("priors.RDS")
initials <- readRDS("initials.RDS")
obs <- readRDS("observations.RDS")
vol <- readRDS("volatility.RDS")
pars <- readRDS("parameters.RDS")
comps <- readRDS("companies.RDS")

vol <- vol %>%
  gather(key = "Quantile", value = "Value", 5:9, factor_key = TRUE) %>%
  left_join(comps, by = "Company")
pars <- pars %>%
  gather(key = "Param", value = "Value", Phi:Mu, factor_key = TRUE) %>%
  left_join(comps, by = "Company")

## Check plots about the volatility

comp <- vol$Company[1]
ggplot(vol %>%
         filter(Company == comp) %>%
         select(Date, Period, Quantile, Value),
       aes(x = Date, y = Value)) +
  geom_line(aes(color = Quantile)) +
  facet_grid(Period ~ .) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), legend.position = "top") +
  scale_color_manual("Quantile (%)",
                     values = c("1%" = "gray80", "5%" = "gray50", "50%" = "gray10", "95%" = "gray50", "99%" = "gray80"),
                     labels = c("1%" = "1", "5%" = "5", "50%" = "50", "95%" = "95", "99%" = "99"))

## Check traceplots

ggplot(pars %>%
         filter(Company == comp, Param == "Phi") %>%
         select(Period, Index, Value),
       aes(x = Index, y = Value)) +
  geom_line() +
  facet_grid(Period ~ .) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), legend.position = "top")

fnAcceptanceRatio(filter(pars, Company == comp, Param == "Phi", Period == "2006-07-01/2009-06-30")$Value)

ggplot(pars %>%
         filter(Company == comp, Param == "Phi") %>%
         select(Period, Index, Value),
       aes(x = Value)) +
  geom_density(aes(y = ..density.., color = Period)) +
  scale_color_grey() +
  theme_bw() +
  theme(legend.position = "right") +
  guides(col = guide_legend(ncol = 1))

ggplot(pars %>%
         filter(Param == "Phi") %>%
         select(Period, Index, Value, Company, Country, Number),
       aes(x = Value)) +
  xlim(0.5, 1) +
  geom_density(aes(y = ..density.., color = Period)) +
  scale_color_grey() +
  facet_grid(Number ~ Country) +
  theme_bw() +
  theme(legend.position = "right") +
  guides(col = guide_legend(ncol = 1))


ggplot(pars %>%
         filter(Company == comp, Param == "Phi") %>%
         select(Period, Index, Value) %>%
         group_by(Period) %>%
         summarise(Avg = mean(Value), Stdev = sd(Value)) %>%
         select(Period, Avg, Stdev) %>%
         gather(key = "Stat", value = "Stat.Value", Avg, Stdev, factor_key = TRUE),
       aes(x = Period, y = Stat.Value)) +
  geom_col() +
  facet_grid(Stat ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "right")


ggplot(pars %>%
         filter(Param == "Rho") %>%
         select(Period, Index, Value, Company, Country, Number)) +
  geom_boxplot(aes(x = Number, group = Company, y = Value)) +
  facet_grid(Period ~ Country) +
  theme_bw() +
  theme(legend.position = "right") +
  guides(col = guide_legend(ncol = 1))

######################
tmp <- data.frame(a = rnorm(1000), b = 2+rbeta(1000, .5, .5))
qtmp <- as.data.frame(apply(tmp, 2, function (x) quantile(x, probs = seq(0, 1, by = .05))))
qtmp <- cbind(qtmp, Quantile = factor(rownames(qtmp), levels = paste0(100*rev(seq(0, 1, by = .05)), "%")))
rownames(qtmp) <- NULL
ggplot(gather(qtmp, key = "Colname", value = "Value", a, b, factor_key = T))

ggplot(gather(tmp, key = "Colname", value = "Value", a, b, factor_key = T),
       aes(x = Value, y = Colname)) +
  stat_density(aes(fill = ..density..), geom = "raster", position = "identity")
#####

ggplot(pars %>%
         filter(Param == "Rho") %>%
         select(Period, Index, Value, Company, Country, Number)) +
  stat_density(aes(x = Value, fill = ..density.., y = Number, group = Company), geom = "raster", position = "identity") +
  facet_grid(Period ~ Country) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), legend.position = "right",
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Company") +
  guides(col = guide_legend(ncol = 1))
