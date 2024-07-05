### -----------------------------------------------------------------
###
### Post process
###
### -----------------------------------------------------------------


out <- mcmcout$samples
fit_sum <- mcmcout$summary$all.chains
waic_01 <- mcmcout$WAIC


#traceplots and density plots
MCMCtrace(out)

sink("results.txt")
print(fit_sum)
gelman.diag(out)
sink()


