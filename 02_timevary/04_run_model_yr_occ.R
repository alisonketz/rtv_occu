
modelcode <- nimbleCode({
   # Priors for detection probability p and occupancy probability psi
    for (j in 1:n_stands) { 
        p[j] ~ dbeta(pprior[1], pprior[2])
        for (t in 1:n_years) {
            psi[j, t] ~ dbeta(1, 1)  # Occupancy probability of stand j in year t
            Z[j, t] ~ dbern(psi[j, t])  # Occupancy state of stand j in year t
        }
    }

    # Likelihood
    for (i in 1:n_obs) {
        y[i] ~ dbern(Z[stand[i], yr[i]] * p[stand[i]])  # Detection observation for plot i in stand j at time t
    }

    # Derived quantities (if any)
    # sum.z <- sum(Z[ , ])  # Total number of occupied stands (optional)
})


### Parameters monitored 
params <- c("psi", "p") 

### MCMC setting
n_iter <- 10000
n_thin <- 1
n_burnin <- 5000
n_chains <- 3

# Create the data list for NIMBLE
nimData <- list(
  y = df_long$y
)

# Define constants for the model
nimConsts <- list(
  n_obs = nrow(df_long),
  n_stands = n_stands,
  n_years = n_years,
  stand = as.integer(df_long$stand),
  yr = as.integer(df_long$year),
  pprior = pprior
  )

# Define initial values for the model
initsFun <- function() list(
  p = runif(n_stands,0,.05),
  psi = matrix(runif(n_stands * n_years), nrow = n_stands, ncol = n_years),
  Z = Z_init
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

save(runtime, file = "results/runtime.Rdata")
save(mcmcout, file = "results/mcmcout.Rdata")
