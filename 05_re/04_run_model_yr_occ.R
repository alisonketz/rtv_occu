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

  ### Hyperparameter priors for random effect precision
  # tau_plot ~ dgamma(0.001, 0.001)  # Precision for the plot random effect
  # sigma_plot <- 1 / sqrt(tau_plot)  # Standard deviation for the plot random effect
  # Prior for the standard deviation using a half-Cauchy distribution
  sigma_plot ~ T(dt(0, scale_plot, 1), 0, )  # Truncated t-distribution at 0
  tau_plot <- 1 / (sigma_plot^2)  # Precision for the plot random effect


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
      if(num_plots[j,t]>0){
        for (i in 1:num_plots[j,t]) {
                plot_effect[j, t, i] ~ dnorm(0, tau_plot)  # Random effect for each plot
                logit(p_detect[j, t, i]) <- beta0_p + beta_p[1] * age_surv_array[j, t, i] + beta_p[2] * ht_array[j, t, i] + plot_effect[j, t, i] 
                y_array[j, t, i] ~ dbern(Z[j, t] * p_detect[j, t, i])
        }
      }
    }
  }

  # Derived quantities (if any)
  # sum.z <- sum(Z[ , ])  # Total number of occupied stands (optional)
})

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
  n_covs_p = 2,
  scale_plot = 1.5  # Scale parameter for the half-Cauchy prior
  )

# Define initial values for the model
initsFun <- function() list(
  psi = matrix(runif(n_stands * n_years), nrow = n_stands, ncol = n_years),
  Z = Z_init,
  plot_effect = array(0, dim = c(n_stands, n_years, max(num_plots))),
  tau_plot = 1,  # Initial value for the precision parameter
  beta0_p = rnorm(1, beta0_p_prior[1], beta0_p_prior[2]),
  beta_p = runif(2,0,.00001),
  beta_psi = rnorm(2,0,1),
  beta0_psi = rnorm(1,0,1),
  sigma_plot = runif(1,.1,.2)
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
  "beta_psi",
  "sigma_plot"
)

### MCMC settings
n_iter <- 100000
n_thin <- 10
n_burnin <- 50000
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
