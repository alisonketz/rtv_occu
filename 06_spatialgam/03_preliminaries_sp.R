sum(df_long_trans$y)

df_long_trans %>% contains('gps')

### checking whether there are multiple observations for every year-stand combo
### answer = there is not
### to move this forward, we could just use space, not accounting for year to
### model the probability of nest occurence across the stands

# Get all unique stands and years
all_stands <- unique(df_long_trans$stand)
all_years <- unique(df_long_trans$year)

# Create a complete grid of all stand-year combinations
complete_grid <- expand.grid(stand = all_stands, year = all_years)

existing_combinations <- df_long_trans %>%
  group_by(stand, year) %>%
  summarise(n = n(), .groups = 'drop')

missing_combinations <- anti_join(complete_grid, existing_combinations, by = c("stand", "year"))

###--------------------------------------------
### setting up basis expansions across space
### --------------------------------------------
library(fields)
temp1 <- df_long_trans %>% filter(year==1) %>% 
    select(gps_y19_x,gps_y19_y,y,age_surv,htlivcrown,dist_of_t,dist_of_s) %>% 
    rename(gps_x = gps_y19_x,gps_y = gps_y19_y) %>%
    drop_na(gps_x)
temp1

temp2 <- df_long_trans %>% filter(year==2) %>% 
    select(gps_y20_x,gps_y20_y,y,age_surv,htlivcrown,dist_of_t,dist_of_s) %>% 
    rename(gps_x = gps_y20_x,gps_y = gps_y20_y) %>%
    drop_na(gps_x)
temp2

temp3 <- df_long_trans %>% filter(year==3) %>% 
    select(gps_y21_x,gps_y21_y,y,age_surv,htlivcrown,dist_of_t,dist_of_s) %>% 
    rename(gps_x = gps_y21_x,gps_y = gps_y21_y) %>%
    drop_na(gps_x)
temp3

temp4 <- df_long_trans %>% filter(year==4) %>% 
    select(gps_y22_x,gps_y22_y,y,age_surv,htlivcrown,dist_of_t,dist_of_s) %>% 
    rename(gps_x = gps_y22_x,gps_y = gps_y22_y) %>%
    drop_na(gps_x)
temp4

temp5 <- df_long_trans %>% filter(year==5) %>% 
    select(gps_y23_x,gps_y23_y,y,age_surv,htlivcrown,dist_of_t,dist_of_s) %>% 
    rename(gps_x = gps_y23_x,gps_y = gps_y23_y) %>%
    drop_na(gps_x)
temp5

trans_ls <- list(temp1,temp2,temp3,temp4,temp5)

Z_spatial <- lapply(trans_ls,function(df_temp){

    n_knots <- 10

    # Extract the GPS coordinates
    gps_coords <- df_temp %>% select(gps_x, gps_y) %>% distinct()

    # Generate spatial knots based on available GPS points
    spatial_knots <- cover.design(gps_coords, n_knots)$design

    # Compute distance matrix between knots
    knots_dist <- dist(spatial_knots, "euclidean", diag = TRUE, upper = TRUE)

    # Create the penalty matrix for thin-plate splines
    omega_all <- as.matrix(knots_dist^2 * log(knots_dist))
    omega_all[is.nan(omega_all)] <- 0

    svd_omega_all <- svd(omega_all)
    sqrt_omega_all <- t(svd_omega_all$v %*% (t(svd_omega_all$u) * sqrt(svd_omega_all$d)))

    # Compute the basis functions for the data points
    cov_dist2 <- rdist(spatial_knots, gps_coords)
    Z_spatial <- as.matrix(cov_dist2^2 * log(cov_dist2))

    return(Z_spatial)
})


spat_cols <- sapply(Z_spatial,ncol)

Z_spatial_array <- array(NA,c(n_knots,max(spat_cols),n_years))
for(t in 1:n_years){
    Z_spatial_array[1:n_knots,1:spat_cols[t],t]<- Z_spatial[[t]]
}

