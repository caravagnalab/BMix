# setwd('~/Documents/Hare/MOBSTER/')
# setwd("~/Documents/GitHub/test.dbpmm/Synthetic Tests/[Data] Batch 1.1")

ids = list.files('CSV/')
ids = c(paste0("./CSV/", ids),  paste0("./CSV-post-dbpmm/", ids))
ids

revolver:::setup_parallel(.8)
library(doParallel)
library(parallel)

# structure to save the results
results = NULL

# perform parallel inferences
r = foreach(num = seq(ids)) %dopar%
{
  library(BMix)
  data = read.csv(ids[num])
  sink(paste0('bmix.log.', num))
  
  fit = bmixfit(data, K.Binomials = 0:5, K.BetaBinomials = 0:5, samples = 5)
  
  save(fit, file = paste(ids[num], '.BMix.fit.RData', sep = ''))
  sink()
  #  results = append(results, list(fit))
  fit
  
  
}

results = r
save(results, file = "Results_fit_BMix_Popgen_batch_1_1.RData")

stop_parallel(cl)

# Analysis
load("Results_fit_BMix_Popgen_batch_1_1.RData")

pdf("BMix_fits.pdf", width = 10, height = 5)

ids = list.files('CSV/')
ids = c(paste0("./CSV/", ids),  paste0("./CSV-post-dbpmm/", ids))
ids

lapply(seq(ids), function(w) plot(w, data = read.csv(ids[num]), palette = 'Set1'), annotation = ids[num])
dev.off()


