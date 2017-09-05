library(tidyverse)
library(forcats)

set.seed(137)

# Explore results
calfolder <- getwd()
resfolder <- "../results/final-mix/"  # results folder

### Check which runs converged and use only 1 out of the 3 with different initial values.
if (FALSE) {
  library(zoo)
  library(xts)
  
  #fnAcceptanceRatio <- function (x) mean(x[-1] != x[-length(x)])
  
  fnRemoveBurnin <- function (x) {
    start.index <- sum(cumall(x[-1] == x[-length(x)])) + 1
    if (start.index > 1)
      x[start.index:length(x)]
    else
      x
  }
  
  res.files <- list.files(resfolder)
  # Collect data and results into data frames
  counter <- 0  # common identifier
  obs <- NULL  # observations
  h <- NULL  # variance estimates
  params <- NULL  # phi, rho, mu, sigma2
  inits <- tibble(ID = integer(), Mu = numeric(), Phi = numeric(), Sigma2 = numeric(), Rho = numeric())
  # Read and handle files that correspond to the same dataset together
  for (file.path in res.files) {
    counter <- counter + 1
    dataset <- readRDS(paste0(resfolder, file.path))
    company <- gsub(pattern = "^X", replacement = "", x = colnames(dataset$data))
    company <- gsub(pattern = "\\.", replacement = " ", x = company)
    inits <- inits %>%
      add_row(ID = counter,
              Mu = dataset$initials$mu,
              Phi = dataset$initials$phi,
              Sigma2 = dataset$initials$sigma2,
              Rho = dataset$initials$rho)
    obs <- obs %>%
      bind_rows(tibble(ID = counter,
                       Date = dataset$dates,
                       Period = dataset$years,  # to factor later
                       Company = company,  # to factor later
                       Values = as.numeric(coredata(dataset$data))))
    h <- tibble(ID = counter,
                Date = dataset$dates,
                Period = dataset$years,  # to factor later
                Company = company) %>%  # to factor later
      bind_cols(as_tibble(dataset$result$h)) %>%
      bind_rows(h)
    rho <- dataset$result$samples$rho
    start.id <- sum(cumall(c(T, rho[-1] == rho[-length(rho)])))
    thinning <- as.integer(seq(start.id, length(rho), length.out = 2000))  # I only use a part of the dataset for experimenting and formulating the results
    params <- bind_rows(params,
                        tibble(ID = counter,
                               Period = dataset$years,  # to factor later
                               Company = company,  # to factor later
                               Index = seq_along(thinning),
                               Phi = dataset$result$samples$phi[thinning],
                               Rho = dataset$result$samples$rho[thinning],
                               Sigma2 = dataset$result$samples$sigma2[thinning],
                               Mu = dataset$result$samples$mu[thinning],
                               Weight = dataset$result$weights[thinning]/sum(dataset$result$weights[thinning])))
  }
  #initials.df <- as.data.frame(do.call("rbind", lapply(files.list, function (x) unlist(x$initials))))
  #priors.df <- as.data.frame(t(unlist(files.list[[1]]$priors)))
  saveRDS(inits, "initials.RDS")
  rm(inits)
  #saveRDS(priors.df, "priors.RDS")
  
  companies <- sort(unique(obs$Company))
  
  obs$Period <- factor(obs$Period, labels = sort(unique(obs$Period)))
  obs$Company <- factor(obs$Company, labels = companies)
  saveRDS(obs, "observations.RDS")
  rm(obs)
  
  comps <- tibble(Company = as_factor(companies), Country = as_factor(rep(c("CHN", "GER"), each = 10)), Number = rep(1:10, 2))
  h$Period <- factor(h$Period, labels = sort(unique(h$Period)))
  h$Company <- factor(h$Company, labels = companies)
  h <- h %>%
    gather(key = "Quantile", value = "Value", 5:13, factor_key = F) %>%
    mutate(Quantile = factor(Quantile, labels = c("1%", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "99%"))) %>%
    left_join(comps, by = "Company")
  saveRDS(h, "volatility.RDS")
  rm(h)
  
  params$Period <- factor(params$Period, labels = sort(unique(params$Period)))
  params$Company <- factor(params$Company, labels = companies)
  params <- params %>%
    gather(key = "Param", value = "Value", Phi:Mu, factor_key = TRUE) %>%
    left_join(comps, by = "Company")
  saveRDS(params, "parameters.RDS")
  saveRDS(comps, "companies.RDS")
  
  rm(list = ls())
  
  #pars %>% filter(Period == levels(Period)[1], Country == "GER", Param == "Rho") %>% select(Company, Value, Number) %>% group_by(Number) %>% summarise(Mean = mean(Value), Co = first(Company)) %>% select(Number, Co, Mean) %>% arrange(Mean)
  #for (i in 1:10) {pars[Period == levels(Period)[1] & Param == "Rho" & Company == as.matrix(chn.ranks)[i, "Co"], "Number"] <- i}
}

# Read data
priors <- readRDS("priors.RDS")
initials <- readRDS("initials.RDS")
obs <- readRDS("observations.RDS")
vol <- readRDS("volatility.RDS")
pars <- readRDS("parameters.RDS")
comps <- readRDS("companies.RDS")

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

pdf("rhos.pdf", width = 8*12/8, height = 13.90*12/8)
pars %>%
  filter(Param == "Rho") %>%
  select(Period, Index, Value, Company, Country, Number) %>%
  ggplot() +
  stat_density(aes(x = Value, fill = ..density.., y = Number, group = Company), geom = "raster", position = "identity") +
  geom_vline(xintercept = 0, color = "red", show.legend = F) +
  facet_grid(Period ~ Country) +
  theme_bw(base_size = 24) +
  theme(strip.text.y = element_text(angle = 0), legend.position = "right",
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.spacing = unit(0.1, "lines")) +
  ylab("Company") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  guides(col = guide_legend(ncol = 1)) +
  ggtitle(expression(paste(rho, " posterior distributions")))
dev.off()

pdf("negative-rhos.pdf", width = 8.00*12/8, height = 7.00*12/8)
pars %>%
  filter(Param == "Rho", Period %in% levels(Period)[seq(1, 32, by = 2)]) %>%
  group_by(Company, Period) %>%
  summarise(Country = first(Country), Is.Significant = quantile(Value, probs = 0.95) < 0) %>%
  group_by(Country, Period) %>%
  summarise(Significants = sum(Is.Significant)) %>%
  select(Country, Period, Significants) %>%
  ggplot(aes(x = Country, y = Significants)) +
  geom_col(aes(fill = Country), linetype = "blank") +
  facet_grid(. ~ Period, switch = "x") +
  theme_bw(base_size = 20) +
  theme(strip.text.x = element_text(angle = 90), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual("Country", values = c("CHN" = "red", "GER" = "black")) +
  scale_x_discrete(name = "Period", position = "top") +
  scale_y_continuous(name = "# of companies", limits = c(0, 10), breaks = 0:10) +
  ggtitle(expression(paste("Number of companies with significantly negative ", rho)))
dev.off()

pdf("positive-rhos.pdf", width = 8.00*12/8, height = 7.00*12/8)
pars %>%
  filter(Param == "Rho", Period %in% levels(Period)[seq(1, 32, by = 2)]) %>%
  group_by(Company, Period) %>%
  summarise(Country = first(Country), Is.Significant = quantile(Value, probs = 0.05) > 0) %>%
  group_by(Country, Period) %>%
  summarise(Significants = sum(Is.Significant)) %>%
  select(Country, Period, Significants) %>%
  ggplot(aes(x = Country, y = Significants)) +
  geom_col(aes(fill = Country), linetype = "blank") +
  facet_grid(. ~ Period, switch = "x") +
  theme_bw(base_size = 20) +
  theme(strip.text.x = element_text(angle = 90), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_manual("Country", values = c("CHN" = "red", "GER" = "black")) +
  scale_x_discrete(name = "Period", position = "top") +
  scale_y_continuous(name = "# of companies", limits = c(0, 10), breaks = 0:10) +
  ggtitle(expression(paste("Number of companies with significantly positive ", rho)))
dev.off()

pdf("rho-timeline.pdf", width = 8.00*12/8, height = 14.50*12/8)
pars %>%
  filter(Param == "Rho") %>%
  group_by(Company, Period) %>%
  summarise("5%" = quantile(Value, probs = 0.05),
            "50%" = quantile(Value, probs = 0.5),
            "95%" = quantile(Value, probs = 0.95)) %>%
  gather(key = "Quantile", value = "Value", "5%", "50%", "95%") %>%
  ggplot(aes(x = Period, y = Value, group = Quantile)) +
  geom_hline(yintercept = 0, show.legend = F, color = "red") +
  stat_summary(aes(color = Quantile), geom = "line", fun.data = "mean_se", show.legend = T) +
  facet_grid(Company ~ .) +
  theme_bw(base_size = 20) +
  theme(strip.text.y = element_text(angle = 0), axis.text.x = element_text(angle = 90), panel.spacing = unit(0.2, "lines")) +
  scale_color_manual("Quantile", values = c("5%" = "gray60", "50%" = "gray20", "95%" = "gray60")) +
  scale_x_discrete(labels = ifelse(substr(levels(pars$Period), 7, 7) %in% c("4", "0"), rep("", 32), levels(pars$Period))) +
  scale_y_continuous(breaks = c(-0.5, 0.5)) +
  ggtitle(expression(paste("Posterior distributions of ", rho, " per company")))
dev.off()

pdf("phi-timeline.pdf", width = 8.00*12/8, height = 6.50*12/8)
pars %>%
  filter(Param == "Phi",
         Period %in% levels(Period)[c(1, 13, 23, 32)],
         Company %in% levels(Company)[c(2, 6, 13, 18)]) %>%
  ggplot(aes(x = Value)) +
  stat_function(fun = function (x) dbeta((x+1)/2, 20, 1.5), mapping = aes(fill = "Prior"), geom = "area", alpha = 0.65, linetype = "blank") +
  geom_density(aes(x = Value, y = ..density.., fill = "Posterior"), alpha = 0.65, linetype = "blank") +
  facet_grid(Company ~ Period, labeller = labeller(.cols = c("2004-01-01/2006-12-31" = "2004-2006", "2007-01-01/2009-12-31" = "2007-2009", "2009-07-01/2012-06-30" = "2010-2012", "2011-10-01/2014-09-30" = "2012-2014"))) +
  theme_bw(base_size = 20) +
  theme(strip.text.y = element_text(angle = 0), axis.text.x = element_text(angle = 0), panel.spacing = unit(0.2, "lines")) +
  scale_fill_manual("", values = c("Prior" = "gray", "Posterior" = "blue")) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  ylab("Density") +
  ggtitle(expression(paste("Posterior distributions of ", phi, " per period and company")))
dev.off()


pdf("volatility.pdf", width = 8.00*12/8, height = 7.50*12/8)
vol %>%
  filter(Period %in% levels(Period)[c(1, 13, 23, 32)],
         Company %in% levels(Company)[c(2, 6, 13, 18)]) %>%
  mutate(Period = fct_relabel(Period, function (x, y) y[x], c("2004-01-01/2006-12-31" = "2004-2006", "2007-01-01/2009-12-31" = "2007-20", "2011-10-01/2014-09-30" = "2011-2014"))) %>%
  ggplot(aes(y = Value)) +
  geom_line(aes(x = Date, color = Quantile)) +
  facet_grid(Company ~ ., labeller = "label_value") +
  theme_bw(base_size = 20) +
  theme(strip.text.y = element_text(angle = 0), axis.text.x = element_text(angle = 0), panel.spacing = unit(0.2, "lines")) +
  scale_color_manual("Quantiles", values = c("1%" = "gray85", "5%" = "gray73", "10%" = "gray60", "25%" = "gray45", "50%" = "gray10", "75%" = "gray45", "90%" = "gray60", "95%" = "gray73", "99%" = "gray85")) +
  #scale_x_continuous(breaks = c(0, 0.5, 1)) +
  ylab("Log variance") +
  ggtitle("Posterior distribution of the log variance per company")
dev.off()
