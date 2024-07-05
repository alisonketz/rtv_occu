### -----------------------------------------------------------------
###
### Post process
###
### -----------------------------------------------------------------


out <- mcmcout$samples
fit_sum <- mcmcout$summary$all.chains
waic_03 <- mcmcout$WAIC
save(waic_03,file = "results/waic_03.Rdata")

#traceplots and density plots
MCMCtrace(out, filename = "figures/traceplot.pdf")

sink("results/results.txt")
print(fit_sum)
gelman.diag(out, multivariate = FALSE)
sink()
out$chain2
samples <- mcmc(as.data.frame(rbind(out[[1]],out[[2]],out[[3]])))
coda::HPDinterval(samples)

