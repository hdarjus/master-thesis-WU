
thread.ind <- as.integer(Sys.getenv("SGE_TASK_ID"))

if (thread.ind == 1) {
  folder <- "/gpfs/home/home/dhosszejni/master-thesis/results/1/"
  #folder <- "/home/hdarjus/temp_R/"
  for (f in list.files(folder)) {
    dat <- readRDS(paste0(folder, f))
    dat$result$h <- t(apply(dat$result$h, 2, function (x, probs) quantile(x, probs = probs), c(.01, .05, .5, .95, .99)))
    saveRDS(dat, paste0(folder, "new_", f))
  }
}
