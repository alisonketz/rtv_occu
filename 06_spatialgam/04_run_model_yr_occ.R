
# Define the NIMBLE model code
modelcode <- nimbleCode({
  
  # Prior for detection probability
  beta0_p ~ dnorm(beta0_p_prior[1], 1 / beta0_p_prior[2]^2)
  # beta0_p ~ dnorm(0, 1 / 2.89)
  for (k in 1:n_covs_p) {
    beta_p[k] ~ dnorm(0, 1 / 2.89)
  }
  # Prior for occupancy probability
  beta0_psi ~ dnorm(0, 1 / 2.89)
  for (k in 1:n_covs_psi) {
    beta_psi[k] ~ dnorm(0, 1 / 2.89)
  }
  # Prior for occupancy of detected nests along transects between plots
  beta0_transect ~ dnorm(0, 1 / 2.89)
  for (k in 1:n_covs_transect) {
    beta_transect[k] ~ dnorm(0, 1 / 2.89)
  }
  ### Spatial GAM surface for occurrence events along transects
  for (j in 1:n_stands) {
    for (t in 1:n_years) {
       spatial_gam_pp[j, t,1:n_knots] <- inprod(b_pp[j,t,1:n_knots], Z_spatial[t,1:n_knots])
    }
  }
  # Precision prior for point process
  tau_pp ~ dgamma(1,1)

  #point process coefficients
  for (k in 1:n_knots) {
    for(s in 1:n_stands){
      for(t in 1:n_years){
        b_pp[s,t,k] ~ ddexp(0, tau_pp)
      }
    }
  }

 ### Spatial GAM for occurrence events along transects
  # for (j in 1:n_stands) {
  #   for (t in 1:n_years) {
  #     for (p in 1:n_points[j, t]) {
  #       # Predict intensity using the fitted GAM
  #       lambda[j, t, p] <- exp(beta0_psi + beta_psi[1] * age_surv_points[j, t, p] + beta_psi[2] * ht_points[j, t, p] + spatial_gam_pp[j, t])
  #       y_points[j, t, p] ~ dpois(lambda[j, t, p])  # Point process likelihood for occurrences
  #     }
  #   }
  # }

  ### Long format Spatial GAM for occurrence events along transects
  for (i in 1:n_obs_trans){
          # Predict intensity using the fitted GAM
          lambda[i] <- exp(beta0_psi + beta_psi[1] * age_surv_points_trans[i] + beta_psi[2] * ht_points_trans[i] + spatial_gam_pp[stand_trans[i], stand_yr[i]])
          y_points_trans[i] ~ dpois(lambda[i])  # Point process likelihood for occurrences
  }
  #### Long format Occupancy likelihood for detected nests
  for (i in 1:n_obs_trans){
    # Detected nests occupancy probability
    logit(prob_transect[i]) <- beta0_transect + beta_p[1] * age_surv_points_trans[i]+ beta_p[2] * ht_points_trans[i]
    # Observed occupancy state
    y_nests[i] ~ dbern(prob_transect[i])
  }


  # Occupancy likelihood for detected nests
  # for (j in 1:n_stands) {
  #   for (t in 1:n_years) {
  #     for (p in 1:n_points[j, t]) {
  #       # Detected nests occupancy probability
  #       logit(prob_transect[j, t, p]) <- beta0_transect + beta_transect[1] * nest_cov1[j, t, p] + beta_transect[2] * nest_cov2[j, t, p]
  #       nest_occupy[j, t, p] ~ dbern(prob_transect[j, t, p])  # Occupancy state of detected nests
  #       # Observed occupancy state
  #       y_nests[j, t, p] ~ dbern(nest_occupy[j, t, p])
  #     }
  #   }
  # }

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
      if(num_plots[j,t] > 0){
        for (i in 1:num_plots[j,t]) {
          logit(p_detect[j, t, i]) <- beta0_p + beta_p[1] * age_surv_array[j, t, i] + beta_p[2] * ht_array[j, t, i] 
          y_array[j, t, i] ~ dbern(Z[j, t] * p_detect[j, t, i])
        }
      }
    }
  }

  # Derived quantities (if any)
  # sum.z <- sum(Z[ , ])  # Total number of occupied stands (optional)
})

### Parameters monitored 
params <- c("psi", "p_detect", "beta0_p", "beta0_psi", "beta_p", "beta_psi", "b_pp")

### MCMC setting
n_iter <- 200000
n_thin <- 1
n_burnin <- 100000
n_chains <- 3

# Create the data list for NIMBLE
nimData <- list(
  y_array = y_array,
  age_surv_array = scaled_age_surv_array,
  ht_array = ht_array,
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
  beta0_p_prior = beta0_p_prior,
  n_covs_psi = 2,
  n_covs_p = 2
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
