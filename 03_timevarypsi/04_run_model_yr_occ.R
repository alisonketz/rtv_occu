
# Define the NIMBLE model code
modelcode <- nimbleCode({
  
  # Prior for detection probability
  beta0_p ~ dnorm(beta0_prior[1], 1 / beta0_prior[2]^2)
  # for (k in 1:n_covs_p) {
  #   beta_p[k] ~ dnorm(0, 1 / 2.89)
  # }
  
  # Prior for occupancy probability
  beta0_psi ~ dnorm(0, 1 / 2.89)
  for (k in 1:n_covs_psi) {
    beta_psi[k] ~ dnorm(0, 1 / 2.89)
  }

  for (j in 1:n_stands) {
    for (t in 1:n_years) {
      logit(psi[j, t]) <- beta0_psi +
                          beta_psi[1] * age_surv[j, t] + # nolint: indentation_linter.
                          # beta_psi[2] * dist_of_t[j, t] +
                          beta_psi[2] * dist_of_s[j, t]
      Z[j, t] ~ dbern(psi[j, t])  # Occupancy state of stand j in year t
    }
  }

  # Likelihood
  for (j in 1:n_stands) {
    for (t in 1:n_years) {
      for (i in 1:num_plots[j,t]) {
        logit(p_detect[j, t, i]) <- beta0_p #+ beta_p[1] * age_surv_array[j, t, i] #+  beta_p[2] * ht_array[j, t, i] 
        y_array[j, t, i] ~ dbern(Z[j, t] * p_detect[j, t, i])
      }
    }
  }

  # Derived quantities (if any)
  # sum.z <- sum(Z[ , ])  # Total number of occupied stands (optional)
})

### Parameters monitored 
params <- c("psi", "p_detect", "beta0_p", "beta0_psi", "beta_p", "beta_psi")

### MCMC setting
n_iter <- 50000
n_thin <- 1
n_burnin <- 25000
n_chains <- 3

# Create the data list for NIMBLE
nimData <- list(
  y_array = y_array,
  # age_surv_array = scaled_age_surv_array,
  # ht_array = ht_array,
  age_surv = age_surv_matrix,
  # dist_of_t = dist_of_t_matrix,
  dist_of_s = dist_of_s_matrix
)

# Define constants for the model
nimConsts <- list(
  # n_obs = nrow(df_long),
  n_stands = n_stands,
  n_years = n_years,
  # stand = as.integer(df_long$stand),
  num_plots = num_plots,
  # yr = as.integer(df_long$year),
  beta0_prior = beta0_prior,
  n_covs_psi = 2#,
  # n_covs_p = 1
   )

# Define initial values for the model
initsFun <- function() list(
  psi = matrix(runif(n_stands * n_years), nrow = n_stands, ncol = n_years),
  Z = Z_init,
  beta0_p = rnorm(1, beta0_prior[1], beta0_prior[2]),
  # beta_p = runif(1,0,.00001),
  beta_psi = rnorm(2,0,1),
  beta0_psi = rnorm(1,0,1)
)
nimInits <- initsFun()

Rmodel <- nimbleModel(code = modelcode,
                      constants = nimConsts, # nolint: indentation_linter.
                      data = nimData,
                      inits = initsFun()
                      )# nolint: indentation_linter.

#identify params to monitor
parameters <- c(
  # "psi",
  "beta0_p",
  "beta0_psi",
  # "beta_p",
  "beta_psi"
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
