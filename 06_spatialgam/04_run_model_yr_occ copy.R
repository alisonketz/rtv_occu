
# Define the NIMBLE model code
modelcode <- nimbleCode({
  
  # Prior for detection probability
  # beta0_p ~ dnorm(beta0_p_prior[1], 1 / beta0_p_prior[2]^2)
  # # beta0_p ~ dnorm(0, 1 / 2.89)
  # for (k in 1:n_covs_p) {
  #   beta_p[k] ~ dnorm(0, 1 / 2.89)
  # }
  # Prior for occupancy probability
  beta0_psi ~ dnorm(0, 1 / 2.89)
  # for (k in 1:n_covs_psi) {
  #   beta_psi[k] ~ dnorm(0, 1 / 2.89)
  # }
  # Prior for occupancy of detected nests along transects between plots
  # beta0_transect ~ dnorm(0, 1 / 2.89)
  # for (k in 1:n_covs_transect) {
  #   beta_transect[k] ~ dnorm(0, 1 / 2.89)
  # }
  ### Spatial GAM surface for occurrence events along transects
  for (t in 1:n_years) {
    for (k in 1:n_knots) {
      spatial_gam_pp[k, t] <- inprod(b_sp[k, t, 1:spat_cols[t]], Z_sp[k, 1:spat_cols[t], t])
    }
  }
  # Precision prior for point process
  tau_sp ~ dgamma(1,1)

  # GAM Spatial coefficients
  for (k in 1:n_knots) {
    for (t in 1:n_years) {
      for (j in 1:spat_cols[t]) {
        b_sp[k, t, j] ~ ddexp(0, tau_sp)
      }
    }
  }
  ### Long format Spatial GAM for occurrence events along transects
 # Long format Spatial GAM for occurrence events along transects
  for (i in 1:n_obs_trans) {
    # Calculate the spatial contribution using the inner product
    spatial_effect[i] <- inprod(b_sp[1:n_knots, stand_yr[i], 1:spat_cols[stand_yr[i]]], Z_spatial[1:n_knots, 1:spat_cols[stand_yr[i]], stand_yr[i]])
    
    # Predict intensity using the fitted GAM
    lambda[i] <- exp(beta0_psi + spatial_effect[i]) # + beta_psi[1] * age_surv_points_trans[i] + beta_psi[2] * ht_points_trans[i]
    y_points_trans[i] ~ dpois(lambda[i])  # Spatial likelihood for occurrences
  }
  
  #### Long format Occupancy likelihood for detected nests
  # for (i in 1:n_obs_trans){
  #   # Detected nests occupancy probability
  #   logit(prob_transect[i]) <- beta0_transect + beta_p[1] * age_surv_points_trans[i]+ beta_p[2] * ht_points_trans[i]
  #   # Observed occupancy state
  #   y_nests[i] ~ dbern(prob_transect[i])
  # }
})

# Create the data list for NIMBLE
nimData <- list(
  # y_array = y_array,
  y_nests = df_long_trans$y,
  y_points_trans = rep(1,nrow(df_long_trans)),
  age_surv_points_trans = df_long_trans$age_surv,
  ht_points_trans = df_long_trans$htlivcrown
  # age_surv_array = scaled_age_surv_array,
  # ht_array = ht_array,
  # age_surv = age_surv_matrix,
  # dist_of_t = dist_of_t_matrix,
  # dist_of_s = dist_of_s_matrix

)

# Define constants for the model
nimConsts <- list(
  # n_obs = nrow(df_long),
  # n_stands = n_stands,
  n_years = n_years,
  # stand = as.integer(df_long$stand),
  # num_plots = num_plots,
  # yr = as.integer(df_long$year),
  beta0_p_prior = beta0_p_prior,
  n_covs_psi = 2,
  n_covs_p = 2,
  Z_sp = Z_spatial_array,
  spat_cols = spat_cols,
  n_obs_trans = nrow(df_long_trans),
  n_knots = n_knots,
  stand_yr = df_long_trans$year
   )

# Define initial values for the model
initsFun <- function() list(
  psi = matrix(runif(n_stands * n_years), nrow = n_stands, ncol = n_years),
  Z = Z_init,
  beta0_p = rnorm(1, beta0_p_prior[1], beta0_p_prior[2]),
  beta_p = runif(2,0,.00001),
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
  "beta_p",
  "beta_psi".
  "b_sp"
)


### MCMC setting
n_iter <- 200000
n_thin <- 1
n_burnin <- 100000
n_chains <- 3


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
