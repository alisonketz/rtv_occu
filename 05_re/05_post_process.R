### -----------------------------------------------------------------
###
### Post process
###
### -----------------------------------------------------------------


out <- mcmcout$samples
fit_sum <- mcmcout$summary$all.chains
waic_05 <- mcmcout$WAIC
save(waic_05,file = "results/waic_05.Rdata")

#traceplots and density plots
MCMCtrace(out, filename = "figures/traceplot.pdf")
samples <- mcmc(as.data.frame(rbind(out[[1]],out[[2]],out[[3]])))

sink("results/results.txt")
print(fit_sum)
gelman.diag(out, multivariate = FALSE)
coda::HPDinterval(samples)
sink()

