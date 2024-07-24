###
### setup data for running model
###

df_long$state <- as.factor(df_long$state)
levels(df_long$state) <- c(0,0,1,0,1)
df_long$y <- as.numeric(as.character(df_long$state))
df_long$y[is.na(df_long$y)] <- 0
df_long$year <- df_long$year - min(df_long$year) + 1
df_long$standno_t <- as.factor(df_long$standno_t)
df_long$stand <- as.numeric(df_long$standno_t)


df_long_trans$state <- as.factor(df_long_trans$state)
levels(df_long_trans$state) <- c(0,0,1,0,1)
df_long_trans$y <- as.numeric(as.character(df_long_trans$state))
df_long_trans$y[is.na(df_long_trans$y)] <- 0
df_long_trans$year <- df_long_trans$year - min(df_long_trans$year) + 1
df_long_trans$standno_t <- as.factor(df_long_trans$standno_t)
df_long_trans$stand <- as.numeric(df_long_trans$standno_t)



### to check how many plots are within each stand
extract_plotnos <- function(df) {
  # Group by 'standno_t' and create a list of unique 'plotno' for each group
  result <- df %>%
    group_by(standno_t) %>%
    summarise(plotnos = length(unique(plotno[!is.na(plotno)])))
  return(result)
}
result <- extract_plotnos(df_long)
max(result$plotnos)

df_long <- df_long  %>% mutate(plotno = as.numeric(as.character(plotno)))

### Create a nested data frame with unique plots per stand
df_long <- df_long %>%
  group_by(standno_t) %>%
  mutate(plot_id = as.numeric(factor(plotno, levels = unique(plotno)))) %>%
  ungroup()

df_long$z <- NA
df_long$z[df_long$y == 1] <- 1

n_stands <- length(unique(df_long$stand))
n_years <- max(df_long$year)
n_plots <- max(df_long$plot_id, na.rm = TRUE)

# Create unique combinations of stand, year, and plot
unique_combinations <- df_long %>%
  select(stand, year, plot_id) %>%
  distinct()

Z_init <- array(0, dim = c(n_stands, n_years))

# Populate Z_init array based on the presence of observations
for (i in 1:nrow(unique_combinations)) {
  stand_index <- unique_combinations$stand[i]
  year_index <- unique_combinations$year[i]
  Z_init[stand_index, year_index] <- 1
}
# sum(Z_init,na.rm = TRUE)

### Ensure that each combination of stand_id, year, and plot_id is unique
df_long <- df_long %>%
  distinct(stand, year, plot_id, .keep_all = TRUE)

df_long <- df_long %>% mutate(stand_id = as.integer(stand),
                              plot_id = as.integer(plot_id),
                              year_id = as.integer(year))


### Detection probability 
### prior distributions moment matching
beta_moments <- nimbleFunction(run = function(mu = double(0), sigma = double(0)) {
    alpha  <-  (mu^2-mu^3-mu*sigma^2)/sigma^2
	  beta  <-  (mu-2*mu^2+mu^3-sigma^2+mu*sigma^2)/(sigma^2)
    ans <- c(alpha,beta)
    return(ans)
    returnType(double(1)) # return type declaration
})

#### values from Piasecki et al, Huggins model based on double observers
mup <- .05
lup <- 0.0000001
pup <- .12
sdp <- (pup - mup)/4

### prior without covariates
pprior <- beta_moments(mu = mup,sigma = sdp)

### prior for the intercept with covariates
beta0_p_prior <- c(logit(mup), (logit(pup) - logit(mup))/2)

ggsave("figures/prior_beta0_hist.png",
    ggplot(data = data.frame(x = expit(rnorm(1000,beta0_p_prior[1],beta0_p_prior[2]))), aes(x = x))+
    geom_histogram(bins = 20)+
    theme_bw()+
    xlab("beta0")+
    ggtitle("Prior for beta0"),height = 7, width = 7)


###-----------------------------------------------------------------------------
### Occupancy ~ “age_surv” (age at the initial time of survey) + 
### “dist_of_t”  (distance of the tree from old growth, 
### there’s also a stand measurement “dist_of_s”)
###-----------------------------------------------------------------------------

df_stand <- df_long %>% group_by(stand,year) %>% summarise(
  age_surv = first(age_surv),
  dist_of_s = first(dist_of_s),
  dist_of_t = mean(dist_of_t),
  ht = min(htlivcrown - htnest1_m), .groups = "drop")

df_stand
plot(df_stand$stand,df_stand$dist_of_s)
plot(df_stand$stand,df_stand$dist_of_t)
plot(df_stand$stand,df_stand$ht)

df_stand$dist_of_s <- as.numeric(scale(df_stand$dist_of_s))
df_stand$dist_of_t <- as.numeric(scale(df_stand$dist_of_t))
df_stand$ht <- as.numeric(scale(df_stand$ht))


df_stand$dist_of_s[is.na(df_stand$dist_of_s)] <- mean(df_stand$dist_of_s[!is.na(df_stand$dist_of_s)])
df_stand$dist_of_t[is.na(df_stand$dist_of_t)] <- mean(df_stand$dist_of_t[!is.na(df_stand$dist_of_t)])
df_stand$ht[is.na(df_stand$ht)] <- mean(df_stand$ht[!is.na(df_stand$ht)])



# Reshape group-level data into matrices for NIMBLE
age_surv_matrix <- matrix(NA, nrow = n_stands, ncol = n_years)
dist_of_t_matrix <- matrix(NA, nrow = n_stands, ncol = n_years)
dist_of_s_matrix <- matrix(NA, nrow = n_stands, ncol = n_years)
ht_matrix <- matrix(NA, nrow = n_stands, ncol = n_years)

# Loop through each stand and year
for (j in 1:n_stands) {
  stand_data <- df_stand %>% filter(stand == j)
  for (t in 1:n_years) {
    if (t %in% stand_data$year) {
      age_surv_matrix[j, t] <- stand_data %>% filter(year == t) %>% pull(age_surv)
      dist_of_t_matrix[j, t] <- stand_data %>% filter(year == t) %>% pull(dist_of_t)
      dist_of_s_matrix[j, t] <- stand_data %>% filter(year == t) %>% pull(dist_of_s)
      ht_matrix[j, t] <- stand_data %>% filter(year == t) %>% pull(ht)
    }
  }
}

temp_flat_age_surv <- as.vector(age_surv_matrix)
scaled_temp_flat_age_surv <- scale(temp_flat_age_surv)
age_surv_matrix_scaled <- matrix(scaled_temp_flat_age_surv, nr = n_stands, nc = n_years)
age_surv_matrix_scaled[is.na(age_surv_matrix_scaled)] = 0
dist_of_s_matrix[is.na(dist_of_s_matrix)] = 0
ht_matrix[is.na(ht_matrix)] = 0


df_long_trans$dist_of_t<- scale(df_long_trans$dist_of_t)
df_long_trans$dist_of_s<- scale(df_long_trans$dist_of_s)
df_long_trans$htlivcrown<- scale(df_long_trans$htlivcrown)
df_long_trans$age_surv<- scale(df_long_trans$age_surv)



###-----------------------------------------------------------------------------
### Detection ~ “age_surv” (if we can have the same covariates in each model) +
### “htnest1_m” (this would be the nest height, but if we have data, that means we found a nest so might not be that interesting, 
### what might be more interesting could be likelihood of detecting
### a vole in a nest as the ratio between the nest and live crown [“htnest1_m” - “htlivcrown”]). 
###-----------------------------------------------------------------------------

# df_plot <- df_long %>% group_by(stand,year,plot_id) %>% summarise(
#   age_surv = first(age_surv),
#   dist_of_s = first(dist_of_s),
#   dist_of_t = mean(dist_of_t),
#   ht = min(htnest1_m - htlivcrown), .groups = "drop")

# df_plot$dist_of_s <- as.numeric(scale(df_plot$dist_of_s))
# df_plot$dist_of_t <- as.numeric(scale(df_plot$dist_of_t))
# df_plot$ht <- as.numeric(scale(df_plot$ht))

# # df_long <- df_long %>% mutate(ht = min(htnest1_m - htlivcrown,na.rm = TRUE))
# df_long$ht <- as.numeric(scale(df_long$htlivcrown))

# df_long$ht[is.na(df_long$ht)] <- 0

# df_long$age_surv = as.numeric(scale(df_long$age_surv))

# df_stand$age_surv = as.numeric(scale(df_stand$age_surv))


df_long$ht <- df_long$htlivcrown
df_long$ht[is.na(df_long$ht)] <- mean(df_long$ht[!is.na(df_long$ht)])
df_long$ht <- as.numeric(scale(df_long$ht))


# Define the unique stands, years, and plots
stands <- unique(df_long$stand)
years <- unique(df_long$year)
plots <- unique(df_long$plot_id)

# Constants
n_obs <- nrow(df_long)
n_stands <- length(stands)
n_years <- length(years)
n_plots <- length(plots)

# Create a function to reshape data into an array
# reshape_to_array <- function(df, value_col) {
#   df %>%
#     select(stand, year, plot_id, !!sym(value_col)) %>%
#     spread(key = year, value = !!sym(value_col)) %>%
#     select(-stand) %>%
#     as.matrix()
# }

# Reshape each covariate and the response variable
y_array <- array(NA, dim = c(n_stands, n_years, n_plots))
age_surv_array <- array(NA, dim = c(n_stands, n_years, n_plots))
ht_array <- array(NA, dim = c(n_stands, n_years, n_plots))

for (s in 1:length(stands)) {
  for (t in 1:length(years)) {
    for (i in 1:length(plots)) {
      y_value <- df_long %>%
        filter(stand == stands[s], year == years[t], plot_id == plots[i]) %>%
        pull(y)
      age_surv_value <- df_long %>%
        filter(stand == stands[s], year == years[t], plot_id == plots[i]) %>%
        pull(age_surv)
      ht_value <- df_long %>%
        filter(stand == stands[s], year == years[t], plot_id == plots[i]) %>%
        pull(ht)
      y_array[s, t, i] <- ifelse(length(y_value) == 0, NA, y_value)
      age_surv_array[s, t, i] <- ifelse(length(age_surv_value) == 0, NA, age_surv_value)
      ht_array[s, t, i] <- ifelse(length(ht_value) == 0, NA, ht_value)
    }
  }
}

num_plots <- df_long %>%
  group_by(stand, year) %>%
  summarise(num_plots = n(), .groups = 'drop') %>%
  pivot_wider(names_from = year,values_from = num_plots) %>%
  select(-stand) %>% as.matrix()

temp_flat_age_surv <- as.vector(age_surv_array)
scaled_temp_flat_age_surv <- scale(temp_flat_age_surv)
scaled_age_surv_array <- array(scaled_temp_flat_age_surv, dim = c(n_stands, n_years, n_plots))
scaled_age_surv_array[is.na(scaled_age_surv_array)] = 0
ht_array[is.na(ht_array)] = 0

table(which(y_array == 1, arr.ind = TRUE)[,1])
#  1  2  3  5  6  7  8  9 10 11 13 14 16 17 22 33 34 36 38 40 41 42 44 51 53 61 68 73 75 
#  4  4  3 12  4  2  2  2  1  5  1  4  2  1  6  1  3  1  2  1  1  4  1  3  2  1  1  2  1 


num_plots[is.na(num_plots)] = 0
y_array[is.na(y_array)] = 0
num_plots
