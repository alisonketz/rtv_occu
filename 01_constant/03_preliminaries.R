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
df_long <- df_long  %>% mutate(plotno = as.numeric(as.character(plotno)))


df_long_trans$state <- as.factor(df_long_trans$state)
levels(df_long_trans$state) <- c(0,0,1,0,1)
df_long_trans$y <- as.numeric(as.character(df_long_trans$state))
df_long_trans$y[is.na(df_long_trans$y)] <- 0
df_long_trans$year <- df_long_trans$year - min(df_long_trans$year) + 1
df_long_trans$standno_t <- as.factor(df_long_trans$standno_t)
df_long_trans$stand <- as.numeric(df_long_trans$standno_t)
df_long_trans <- df_long_trans  %>% mutate(plotno = as.numeric(as.character(plotno)))



### Create a nested data frame with unique plots per stand
df_long <- df_long %>%
  group_by(standno_t) %>%
  mutate(plot_id = as.numeric(factor(plotno, levels = unique(plotno)))) %>%
  ungroup()

### Create a nested data frame with unique plots per nest
df_long_trans <- df_long_trans %>%
  group_by(standno_t) %>%
  mutate(plot_id = as.numeric(factor(nesttag, levels = unique(nesttag)))) %>%
  ungroup()


df_long$z <- NA
df_long$z[df_long$y == 1] <- 1
df_long_trans$z <- NA
df_long_trans$z[df_long_trans$y == 1] <- 1


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
