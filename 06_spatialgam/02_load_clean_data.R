###########################################################################################
###
### Load data
###
###########################################################################################


# Read the CSV file while specifying column types (assuming column 192 should be logical)
col_types <- cols(
  .default = col_guess(),
  y23_spec3 = col_character(),  # Specify column 192 as character initially
  y22_spec3 = col_character()  # Specify column 192 as character initially
)

df_rtv <- read_csv('../../../data/2023_Master_240419.csv', col_types = col_types, show_col_types = FALSE)

##########################################################
### problems in loading
### all of the values in this column are NA
##########################################################
# problems(df_rtv)
# names(df_rtv[,192])
# dim(df_rtv)
# sum(is.na(df_rtv$y23_spec3))

# table(df_rtv$standno_n)

#  If (plotno_t>0 or 
#     y19_treety == “Nest_fixed” or 
#     y20_treety == “Nest_fixed” or 
#     y21_treety == “Nest_fixed” or 
#     y22_treety == “Nest_fixed”), then plotno_t, else NA 

plotno <- ifelse(df_rtv$plotno_t>0 |
    df_rtv$y19_treety == 'Nest_fixed' | 
    df_rtv$y20_treety == 'Nest_fixed' | 
    df_rtv$y21_treety == 'Nest_fixed' | 
   df_rtv$ y22_treety == 'Nest_fixed',df_rtv$plotno_t,NA)


table(df_rtv$plotno_t)

names(df_rtv)

sum(!is.na(df_rtv$y19_treety))
table(df_rtv$y19_treety)
table(df_rtv$y20_treety)
table(df_rtv$y21_treety)
table(df_rtv$y22_treety)


###-----------------------------------------------------------------------------
### Occupancy ~ “age_surv” (age at the initial time of survey) + 
### “dist_of_t”  (distance of the tree from old growth, 
### there’s also a stand measurement “dist_of_s”)
###-----------------------------------------------------------------------------
### Detection ~ “age_surv” (if we can have the same covariates in each model) +
### “htnest1_m” (this would be the nest height, but if we have data, that means we found a nest so might not be that interesting, 
### what might be more interesting could be likelihood of detecting
### a vole in a nest as the ratio between the nest and live crown [“htnest1_m” - “htlivcrown”]). 
###-----------------------------------------------------------------------------

df_vole <- df_rtv %>% dplyr::select(unique_id,
                                    standno_t,
                                    treetag_t,
                                    nesttag,
                                    y19_volesi,
                                    y20_volesi,
                                    y21_volesi,
                                    y22_volesi,
                                    y23_volesi,
                                    age_surv,
                                    dist_of_t,
                                    dist_of_s,
                                    htnest1_m,
                                    htlivcrown,
                                    y19_x, y19_y,
                                    y20_x, y20_y,
                                    y21_x, y21_y,
                                    y22_x, y22_y,
                                    y23_x, y23_y
                                    )
df_vole$plotno <- plotno
df_vole <- df_vole %>% relocate(plotno, .after = standno_t)

df_vole <- df_vole %>% rename(gps_y19_x = y19_x, gps_y19_y = y19_y,
                                    gps_y20_x = y20_x, gps_y20_y = y20_y,
                                    gps_y21_x = y21_x, gps_y21_y = y21_y,
                                    gps_y22_x = y22_x, gps_y22_y = y22_y,
                                    gps_y23_x = y23_x, gps_y23_y = y23_y
)

df_vole_plot <- df_vole %>% drop_na(plotno)
df_vole_trans <- df_vole %>% filter(is.na(plotno))



### there are 181 duplicated rows for unique_id
### it looks like this is due to there being multiple nests in those trees
###
# df_vole[duplicated(df_vole$unique_id),]
# print(df_vole[df_vole$unique_id == "123-6066",], col = "all" )
# df_vole[df_vole$unique_id == "103-1893",]
# #some of them have more than 2 rows even, that are duplicated. 
# df_vole[df_vole$unique_id == "508-6578",]
# df_vole %>% dplyr::filter(
#   is.na(y19_volesi) & is.na(y20_volesi) & is.na(y21_volesi) & is.na(y22_volesi) & is.na(y23_volesi))


# Define the priority of statuses
status_priority <- c("Occupied", "Recent", "OldSign", "NoSign", "NoNest", NA)

# Function to select the highest priority status
select_highest_priority <- function(values) {
  # Match the values to their priority index
  priority_index <- match(values, status_priority)
  # Return the value with the lowest index (highest priority)
  values[which.min(priority_index)]
}

# Function to select the highest priority nesttag or return NA if all are NA
select_highest_nesttag <- function(values) {
  if (all(is.na(values))) {
    return(NA)
  } else {
    return(max(values, na.rm = TRUE))
  }
}

# Identify duplicated unique_id
duplicated_ids <- df_vole_trans %>%
  group_by(unique_id) %>%
  filter(n() > 1) %>%
  pull(unique_id)

# Aggregate the duplicated rows
aggregated_df <- df_vole_trans %>%
  filter(unique_id %in% duplicated_ids) %>%
  group_by(unique_id) %>%
  reframe(
    standno_t = first(standno_t),
    plotno = first(plotno),
    treetag_t = first(treetag_t),
    nesttag = select_highest_nesttag(nesttag),
    age_surv = first(age_surv),
    dist_of_t = first(dist_of_t),
    dist_of_s = first(dist_of_s),
    htnest1_m = first(htnest1_m),
    htlivcrown = first(htlivcrown),
    across(starts_with("y"), ~ select_highest_priority(.)),
    across(starts_with("gps"), ~ first(.)),
    .groups = 'drop'
  )

# Combine the aggregated rows with the rest of the dataframe
# df_vole <- df_vole %>%
#   filter(!unique_id %in% duplicated_ids) %>%
#   bind_rows(aggregated_df)


#why are there missing stand ages? 
### imputing missing stand ages and htlivcrown grouped by stand ages
stands <- unique(df_vole_plot$standno_t)

for (i in stands){
  temp <- df_vole_plot$age_surv[df_vole_plot$standno_t==i]
  temp2 <- df_vole_plot$htlivcrown[df_vole_plot$standno_t==i]
  mn_stand_age <- mean(temp,na.rm = TRUE)
  mn_stand_ht <- mean(temp2,na.rm = TRUE)
  df_vole_plot$age_surv[df_vole_plot$standno_t==i & is.nan(df_vole_plot$age_surv)] <- mn_stand_age
  df_vole_plot$htlivcrown[df_vole_plot$standno_t==i & is.nan(df_vole_plot$htlivcrown)] <- mn_stand_ht
}

df_vole_plot$age_surv[is.nan(df_vole_plot$age_surv)] <- mean(df_vole_plot$age_surv[!is.nan(df_vole_plot$age_surv)])
df_vole_plot$age_surv[is.na(df_vole_plot$age_surv)] <- mean(df_vole_plot$age_surv[!is.na(df_vole_plot$age_surv)])



#why are there missing stand ages? 
### imputing missing stand ages and htlivcrown grouped by stand ages
stands <- unique(df_vole_trans$standno_t)

for (i in stands){
  temp <- df_vole_trans$age_surv[df_vole_trans$standno_t==i]
  temp2 <- df_vole_trans$htlivcrown[df_vole_trans$standno_t==i]
  mn_stand_age <- mean(temp,na.rm = TRUE)
  mn_stand_ht <- mean(temp2,na.rm = TRUE)
  df_vole_trans$age_surv[df_vole_trans$standno_t==i & is.nan(df_vole_trans$age_surv)] <- mn_stand_age
  df_vole_trans$htlivcrown[df_vole_trans$standno_t==i & is.nan(df_vole_trans$htlivcrown)] <- mn_stand_ht
}

df_vole_trans$age_surv[is.na(df_vole_trans$age_surv)] <- mean(df_vole_trans$age_surv[!is.na(df_vole_trans$age_surv)])


###---------------------------------------------------------------------------------------
# The exact columns/information to designate occupancy are way over in columns EX-FT. 
#  Specifically, I’d pull the 5 columns for vole sign: “y19_volesi”  “y20_volesi”  “y21_volesi”  “y22_volesi”  “y23_volesi” 
#  and (personally) I’d make 2 columns – year and state in a new frame retaining 
#  the unique_id or column combo for reference: “standno_t” (stand), “treetag_t” (tree), “nesttag” (nest), 
# and maybe we want the plot pulled from the prior workflow (each stand-tree combination should be unique). 
###---------------------------------------------------------------------------------------

### Select the columns of interest
selected_df <- df_vole_plot %>%
  select(unique_id, standno_t, treetag_t, nesttag, plotno,age_surv, dist_of_t, dist_of_s,htnest1_m, htlivcrown, starts_with("y"), starts_with("gps"))

### Pivot longer to convert vole sign columns into year and state
df_long <- selected_df %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    names_prefix = "y",
    values_to = "state"
  ) %>%
 mutate(
   # Convert to full year
    year = as.numeric(str_remove(str_remove(year, "y"), "_volesi")) + 2000 
  )

### Convert year to numeric
df_long <- df_long %>%
  mutate(year = as.numeric(year))


### Select the columns of interest for transect data
selected_df <- df_vole_trans %>%
  select(unique_id, standno_t, treetag_t, nesttag, plotno,age_surv, dist_of_t, dist_of_s,htnest1_m, htlivcrown, starts_with("y"), starts_with("gps"))

### Pivot longer to convert vole sign columns into year and state
df_long_trans <- selected_df %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    names_prefix = "y",
    values_to = "state"
  ) %>%
 mutate(
   # Convert to full year
    year = as.numeric(str_remove(str_remove(year, "y"), "_volesi")) + 2000 
  )

### Convert year to numeric
df_long_trans <- df_long_trans %>%
  mutate(year = as.numeric(year))

### adjusting the age of the stand 
### based on knowning the age of the stand at the start of the study

df_long <- df_long %>%
  mutate(age_surv = case_when(
    year == 2020 ~ age_surv + 1,
    year == 2021 ~ age_surv + 2,
    year == 2022 ~ age_surv + 3,
    year == 2023 ~ age_surv + 4,
    TRUE ~ age_surv
  ))

df_long_trans <- df_long_trans %>%
  mutate(age_surv = case_when(
    year == 2020 ~ age_surv + 1,
    year == 2021 ~ age_surv + 2,
    year == 2022 ~ age_surv + 3,
    year == 2023 ~ age_surv + 4,
    TRUE ~ age_surv
  ))

df_long_trans$age_surv

### removing lines where there's no geographic information i.e. no gps point in the year of the data
# rm1<- which(is.na(df_long_trans$gps_y19_x)) [which(which(is.na(df_long_trans$gps_y19_x)) %in% which(df_long_trans$year == 2019))]
# rm2 <- which(is.na(df_long_trans$gps_y20_x)) [which(which(is.na(df_long_trans$gps_y20_x)) %in% which(df_long_trans$year == 2020))]
# rm3 <- which(is.na(df_long_trans$gps_y21_x)) [which(which(is.na(df_long_trans$gps_y21_x)) %in% which(df_long_trans$year == 2021))]
# rm4 <- which(is.na(df_long_trans$gps_y22_x)) [which(which(is.na(df_long_trans$gps_y22_x)) %in% which(df_long_trans$year == 2022))]
# rm5 <- which(is.na(df_long_trans$gps_y23_x)) [which(which(is.na(df_long_trans$gps_y23_x)) %in% which(df_long_trans$year == 2023))]


# df_long_trans <- df_long_trans[-c(rm1,rm2,rm3,rm4,rm5),]


table(df_long$standno_t)
head(df_long)
df_long%>%filter(standno_t == 613)
rm = c()
for (i in 1:nrow(df_long)){
  if(is.na(df_long$gps_y19_x[i]) & df_long$year[i] == 2019 & is.na(df_long$state[i])) rm = c(rm,i)
  if(is.na(df_long$gps_y20_x[i]) & df_long$year[i] == 2020 & is.na(df_long$state[i])) rm = c(rm,i)
  if(is.na(df_long$gps_y21_x[i]) & df_long$year[i] == 2021 & is.na(df_long$state[i])) rm = c(rm,i)
  if(is.na(df_long$gps_y22_x[i]) & df_long$year[i] == 2022 & is.na(df_long$state[i])) rm = c(rm,i)
  if(is.na(df_long$gps_y23_x[i]) & df_long$year[i] == 2023 & is.na(df_long$state[i])) rm = c(rm,i)
}
df_long <- df_long[-rm,]
dim(df_long)

rm = c()
for (i in 1:nrow(df_long_trans)){
  if(is.na(df_long_trans$gps_y19_x[i]) & df_long_trans$year[i] == 2019 & is.na(df_long_trans$state[i])) rm = c(rm,i)
  if(is.na(df_long_trans$gps_y20_x[i]) & df_long_trans$year[i] == 2020 & is.na(df_long_trans$state[i])) rm = c(rm,i)
  if(is.na(df_long_trans$gps_y21_x[i]) & df_long_trans$year[i] == 2021 & is.na(df_long_trans$state[i])) rm = c(rm,i)
  if(is.na(df_long_trans$gps_y22_x[i]) & df_long_trans$year[i] == 2022 & is.na(df_long_trans$state[i])) rm = c(rm,i)
  if(is.na(df_long_trans$gps_y23_x[i]) & df_long_trans$year[i] == 2023 & is.na(df_long_trans$state[i])) rm = c(rm,i)
}
df_long_trans <- df_long_trans[-rm,]
