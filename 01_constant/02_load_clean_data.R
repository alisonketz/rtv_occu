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

df_vole <- df_rtv %>% dplyr::select(unique_id,standno_t,treetag_t,nesttag,y19_volesi,y20_volesi,y21_volesi,y22_volesi,y23_volesi)
df_vole$plotno <- plotno
df_vole <- df_vole %>% relocate(plotno, .after = standno_t)


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
duplicated_ids <- df_vole %>%
  group_by(unique_id) %>%
  filter(n() > 1) %>%
  pull(unique_id)

# Aggregate the duplicated rows
aggregated_df <- df_vole %>%
  filter(unique_id %in% duplicated_ids) %>%
  group_by(unique_id) %>%
  summarize(
    standno_t = first(standno_t),
    plotno = first(plotno),
    treetag_t = first(treetag_t),
    nesttag = select_highest_nesttag(nesttag),
    across(starts_with("y"), ~ select_highest_priority(.)),
    .groups = 'drop'
  )

# Combine the aggregated rows with the rest of the dataframe
df_vole <- df_vole %>%
  filter(!unique_id %in% duplicated_ids) %>%
  bind_rows(aggregated_df)

# View the result
print(df_vole)
df_vole[df_vole$unique_id == "508-6578",]

###---------------------------------------------------------------------------------------
# The exact columns/information to designate occupancy are way over in columns EX-FT. 
#  Specifically, I’d pull the 5 columns for vole sign: “y19_volesi”  “y20_volesi”  “y21_volesi”  “y22_volesi”  “y23_volesi” 
#  and (personally) I’d make 2 columns – year and state in a new frame retaining 
#  the unique_id or column combo for reference: “standno_t” (stand), “treetag_t” (tree), “nesttag” (nest), 
# and maybe we want the plot pulled from the prior workflow (each stand-tree combination should be unique). 
###---------------------------------------------------------------------------------------

### Select the columns of interest
selected_df <- df_vole %>%
  select(unique_id, standno_t, treetag_t, nesttag, plotno, starts_with("y"))

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

### removing plotno that are NA for now,
### in more complicated models I will use alternative poisson distribution
df_long <- df_long  %>%  drop_na(plotno)