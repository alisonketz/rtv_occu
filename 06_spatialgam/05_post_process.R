### -----------------------------------------------------------------
###
### Post process
###
### -----------------------------------------------------------------


out <- mcmcout$samples
fit_sum <- mcmcout$summary$all.chains
waic_04 <- mcmcout$WAIC
save(waic_04,file = "results/waic_04.Rdata")

#traceplots and density plots
MCMCtrace(out, filename = "figures/traceplot.pdf")
samples <- mcmc(as.data.frame(rbind(out[[1]],out[[2]],out[[3]])))

sink("results/results.txt")
print(fit_sum)
gelman.diag(out, multivariate = FALSE)
coda::HPDinterval(samples)
sink()

