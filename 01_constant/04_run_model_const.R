modelcode <- nimbleCode( { 
    
    #prior
    #probability of detection
        p ~ dbeta(1,1)

    for (i in 1:n_stands){ 
      #probability of detection
      psi[i] ~ dunif(0,1)         # Occupancy probability of stand i
      # Likelihood
      # Occurrence in stand i
      z[i] ~ dbern(psi[i])
    }
      # logit(psi[i]) <- logit(int_psi) #+ beta_lpsi * covA[i]
    for (j in 1:n_obs){
        # Occurrence in sample j
        y[j] ~ dbern(z[stand[j]] * p)
        # y[i,j] ~ dbern(z[i] * theta[i,j])
        # theta[i,j] ~ dbeta(1,1)
        # logit(theta[i,j]) <- logit(int_theta[j]) #+ beta_ltheta * covB[i,j
    }

    # Derived quantities
    # sum.z <- sum(z[1:n.pond])   # Total # of occupied ponds in sample
    # sum.a <- sum(tmp[1:n.pond]) # Total # of ponds with presence in <=1 of the 5 samples
})


### Parameters monitored 
params <- c("psi", "p") 

### MCMC setting
n_iter <- 5000
n_thin <- 1
n_burnin <- 1000
n_chains <- 3

### Data
nimData <- list(y = df_long$y
                # z = df_long$z
                )

nimConsts <- list(n_obs = nrow(df_long),
                 n_stands = n_stands,
                stand = df_long$stand
                 )

initsFun <- function()list(p = .05,
                            psi = rep(.1,n_stands),
                            z = rep(1,nrow(df_long))
                          )
nimInits <- initsFun()

Rmodel <- nimbleModel(code = modelcode,
                      constants = nimConsts,
                      data = nimData,
                      inits = initsFun()
                      )

#identify params to monitor
parameters <- c(
  "psi",
  "p"
)
starttime <- Sys.time()
confMCMC <- configureMCMC(Rmodel,
                          monitors = parameters,
                          thin = n_thin,
                          useConjugacy = FALSE,
                          enableWAIC = TRUE)
# confMCMC$removeSamplers('b_period')
# confMCMC$addSampler(target = "b_period",  type = "RW_block")
nimMCMC <- buildMCMC(confMCMC)
Cnim <- compileNimble(Rmodel)
CnimMCMC <- compileNimble(nimMCMC,
                          project = Rmodel)
mcmcout <- runMCMC(CnimMCMC,
                   niter = n_iter,
                   nburnin = n_burnin,
                   nchains = n_chains,
                   inits = initsFun,
                   samplesAsCodaMCMC = TRUE,
                   summary = TRUE,
                   WAIC = TRUE
                   )

runtime <- difftime(Sys.time(),
                    starttime,
                    units = "min")

###
### save model run
###

save(runtime, file = "runtime_const.Rdata")
save(mcmcout, file = "mcmcout_const.Rdata")


mcmcout$summary
