### -----------------------------------------------------------------
###
### Post process
###
### -----------------------------------------------------------------


out <- mcmcout$samples
fit_sum <- mcmcout$summary$all.chains
waic_02 <- mcmcout$WAIC
save(waic_02,file = "results/waic_02.Rdata")

#traceplots and density plots
MCMCtrace(out, filename = "figures/traceplot.pdf")

sink("results/results.txt")
print(fit_sum)
gelman.diag(out)
sink()


