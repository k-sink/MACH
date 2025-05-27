# Katharine Sink 
# April 2025

library(tidyverse)
library(zoo)
library(broom)
library(Hmisc)

# directory for files 
data = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/ALLVAR"
files = list.files(data, pattern = "basin_\\d{8}_MACH.csv$", full.names = TRUE)

# for anything using runoff, use complete record basins (n = 623)
basin = read_csv("D:/University of Texas at Dallas/Dissertation/complete_sites.csv")
basin_id = basin %>% pull(SITENO)

#############################################################################
#### SENSITIVITY ####
#############################################################################
# time varying sensitivity using moving window regressions 
# linear regression to estimate how runoff efficiency responds to changes in aridity 
# rolling regression function for sensitivity (slope of Q/P ~ PET/P)

rolling_sens = function(df, window_size = 5) {  # rolling linear regression in years based on window length
  df = df %>% 
    mutate(DATE = as.Date(DATE), WYR = wYear(DATE)) %>% 
    filter(WYR > 1980 & WYR < 2024) %>% # drop incomplete water years
    group_by(WYR) %>% filter(sum(!is.na(OBSQ)) >=365) %>% # make sure each water year is complete for runoff
    reframe(OBSQ = sum(OBSQ, na.rm = TRUE), # annual total runoff
            PRCP = sum(PRCP, na.rm = TRUE), # annual total prcp
            PET = sum(PET, na.rm = TRUE)) %>%  # annual total pet
    mutate(q_p = OBSQ/PRCP,  # annual runoff efficiency
           pet_p = PET/PRCP) # annual aridity index
  
  # remove infinity or NA
  df = df %>% filter(is.finite(q_p), is.finite(pet_p)) %>% arrange(WYR)
  
  if (nrow(df) < window_size) return(NULL) 
  
  # calculate rolling regression slope
  # x is aridity index, y is runoff efficiency
  # slope of regression is sensitivity
  roll_df = rollapply(1:nrow(df), width = window_size, FUN = function(idx) {
    sub_df = df[idx, ]
    if (nrow(sub_df) < window_size) return(NA)
    fit = lm(q_p ~ pet_p, data = sub_df)
    return(coef(fit)[2])
  }, align = "right", fill = NA)
  
  df_out = df %>% 
    slice(window_size:nrow(df)) %>% 
  mutate(sensitivity = roll_df[(window_size):length(roll_df)]) %>% 
  select(WYR, sensitivity)

return(df_out)
}

all_results = list()

for (file in files) {
  site = str_extract(basename(file), "\\d{8}")
  df = read_csv(file, show_col_types = FALSE)
  
  result = rolling_sens(df)
  if (!is.null(result)) {
    result$site = site
    all_results[[length(all_results) + 1]] = result
  }
}

combined_df = bind_rows(all_results)

#############################################################################
#### ELASTICITY ####
#############################################################################
# regression model over all sites and multiple time periods

# define 5 year periods 
get_periods = function(start_year = 1980, end_year = 2023, width = 5) {
  starts = seq(start_year, end_year - width + 1, by = width)
  tibble(
    period_start = starts,
    period_end = starts + width - 1,
    label = paste0(starts, "-", starts + width - 1)
  )
}

periods = get_periods()

# tibble to vector 
sel_basins = basin %>% pull(SITENO)
# get basin numbers from all files in list.files
sitenos = str_extract(basename(files), "\\d{8}")
# filter files using basin id for complete basin record
complete = files[sitenos %in% sel_basins]

# loop
results = purrr::map_dfr(complete, function(file) {
  site_id = str_extract(basename(file), "\\d{8}")
  df = read_csv(file, show_col_types = FALSE) 
  df$DATE = as.Date(df$DATE) 
  df = df %>% mutate(YR = lubridate::year(DATE)) 

    # aggregate annual data 
 annual_df = df %>% 
  group_by(YR) %>% 
  dplyr::summarise(PRCP = sum(PRCP), 
                  OBSQ = sum(OBSQ), 
                  TAIR = mean(TAIR), 
                  PET = sum(PET), 
                  .groups = "drop") %>% 
  mutate(PETvP = PET/PRCP, AI = case_when(
    PETvP <= 1 ~ "humid", 
    PETvP > 1 & PETvP <=2 ~ "semi-humid", 
    PETvP > 2 & PETvP <=4 ~ "semi-arid", 
    PETvP > 4 ~ "arid")) %>% 
  filter(PRCP >= 100, OBSQ > 0)
  
# skip site if not enough data
  if (nrow(annual_df) < 5) return(NULL)  # not enough years to regress
  
  # create the periods and loop over each 5 year period
  period_results = purrr::map_dfr(1:nrow(periods), function(i) {
    p_start = periods$period_start[i]
    p_end = periods$period_end[i]
    p_label = periods$label[i]
       
    period_df = annual_df %>% 
      filter(YR >= p_start & YR <= p_end)
    
    if (nrow(period_df) < 3) return(NULL)

    # log transform and normalize temp instead of log 
    period_df = period_df %>% 
      mutate(log_OBSQ = log(OBSQ), log_PRCP = log(PRCP), norm_TAIR = scale(TAIR)[,1])
    
    # fit log-log regression
    fit = tryCatch({
      lm(log_OBSQ ~ log_PRCP + norm_TAIR, data = period_df)
    }, error = function(e) NULL)

    if (is.null(fit)) return(NULL)

    tidy(fit) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        SITENO = site_id,
        period = p_label, 
        aridity = names(sort(table(period_df$AI), decreasing = TRUE))[1]
      )
  })

  period_results
})

# estimate = regression coefficient, quantifies the elasticity of runoff to change in predictor variable
# example if log(TAIR) is -0.2 then 1% increase in TAIR means 0.2% decrease in runoff 

# get average coefficient for each predictor across all sites and periods
results %>% group_by(term) %>%
  summarize(mean_estimate = mean(estimate, na.rm = TRUE),
            sd_estimate = sd(estimate, na.rm = TRUE), n = n())

# average coefficient for each predictor across all sites by period
results %>% group_by(period, term) %>%
  summarize(mean_estimate = mean(estimate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = term, values_from = mean_estimate)

results %>% group_by(period, term, aridity) %>%
  summarize(mean_estimate = mean(estimate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = term, values_from = mean_estimate)

ggplot(results, aes(x = period, y = estimate, color = term, group = term)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
 # stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Regression Coefficients by Period", y = "Estimate") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################################
#### HYDROCLIMATE VOLATILITY ####
#############################################################################
library(SPEI)
library(ggplot2)

# create different temporal aggregations
all_data = all_data %>% mutate(
  YR = year(DATE), 
  MNTH = month(DATE), 
  SEASON = case_when(
    MNTH %in% c(12, 1, 2) ~ "DJF", 
    MNTH %in% c(3, 4, 5) ~ "MAM", 
    MNTH %in% c(6, 7, 8) ~ "JJA", 
    MNTH %in% c(9, 10, 11) ~ "SON"
  )
)

# based on description in supplemental info from Swain et al (2025)
# https://www.nature.com/articles/s43017-024-00624-z
# function to calculate spei over 3 month window

# output folder
output_folder = "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Whiplash"

# function to calculate whiplash events
calc_whiplash = function(data) {
  
  # monthly data for sub-seasonal SPEI
  # aggregate to monthly totals 
  data = data %>% mutate(DATE = as.Date(DATE), MNTH = month(DATE), YR = year(DATE)) %>% 
    arrange(DATE) %>% 
    group_by(YR, MNTH) %>% 
    reframe(PRCP = sum(PRCP), PET = sum(PET)) %>% drop_na() %>% 
    ungroup()
  
  # get monthly climatic balance precipitation minus potential et
    data = data %>% mutate(balance = PRCP - PET)

  # calculate 3-month SPEI using function from SPEI package
  spei_result = spei(data$balance, scale = 3)
  data$SPEI3 = as.numeric(spei_result$fitted)
  
  # drop na values
  data = data %>% filter(!is.na(SPEI3))

  # calculate 3 subseasonal SPEI differences (t - t-1, t - t-2, t - t-3)
   spei_data = data %>% 
     mutate(diff_1m = SPEI3 - lag(SPEI3, 1), 
             diff_2m = SPEI3 - lag(SPEI3, 2), 
             diff_3m = SPEI3 - lag(SPEI3, 3)
             )
 # select largest magnitude change for transition, store each month 
   # if positive (dry to wet transitions), select maximum, 
   # if negative (wet to dry transitions), select minimum
   spei_data = spei_data %>% 
     rowwise() %>% 
     mutate(transition = {
       diffs = c(diff_1m, diff_2m, diff_3m)
       if (all(is.na(diffs))) {
         NA
       } else {
         max_diff = max(diffs, na.rm = TRUE)
         min_diff = min(diffs, na.rm = TRUE)
         if (abs(min_diff) > abs(max_diff)) min_diff else max_diff
       }
     })
   
   # set baseline threshold to 1980-2010 period 
   baseline_data = spei_data %>% 
     filter(YR >= 1980 & YR <= 2010) %>% 
     pull(transition)
   
   # baseline for local whiplash occurrence, calculate 99.4th and 0.6th percentile threshold 
   # resulting in one exceedance each for wet-dry and dry-wet transitions per decade
   dry_to_wet_threshold = quantile(baseline_data, 0.994, na.rm = TRUE)
   wet_to_dry_threshold = quantile(baseline_data, 0.006, na.rm = TRUE)
   
   # whiplash event occurs if stored SPEI temporal difference exceeds local baseline threshold
   spei_data = spei_data %>% 
     mutate(whiplash_type = case_when(
       transition >= dry_to_wet_threshold ~ "dry_wet", 
       transition <= wet_to_dry_threshold ~ "wet_dry", 
       TRUE ~ NA_character_
     ))
   
   return(spei_data)
}

# loop through each CSV file
for (file in files) {
  # Read data from the CSV file
  data = read_csv(file)
  
  data = data %>% select(DATE, PRCP, PET)
  data$DATE = as.Date(data$DATE)
  data$PRCP = as.numeric(data$PRCP)
  data$PET = as.numeric(data$PET)

  result = calc_whiplash(data)
  
  # round all numeric columns to 2 digits
  result = result %>% 
    mutate(across(where(is.numeric), ~round(.x, 2)))
    

  # get SITENO from the filename 
  siteno = sub("basin_(\\d+)_MACH.csv", "\\1", basename(file))
  
 write_csv(result, file.path(output_folder, paste0("whiplash_", siteno, ".csv")))
}


files = list.files("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Whiplash", full.names = TRUE)

summary_df <- purrr::map_dfr(files, function(file) {
  df <- readr::read_csv(file)
  siteno <- sub("whiplash_(\\d+)\\.csv", "\\1", basename(file))
  df %>%
    count(whiplash_type) %>%
    pivot_wider(names_from = whiplash_type, values_from = n, values_fill = 0) %>%
    mutate(SITENO = siteno)
})



# plot the number of wet-to-dry events across basins
ggplot(result, aes(x = YR, y = wet_to_dry_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Wet-to-Dry Hydroclimate Whiplash Events", 
       x = "Basin ID", y = "Event Count")

# plot the number of dry-to-wet events across basins
ggplot(results, aes(x = SITENO, y = dry_to_wet_count)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Dry-to-Wet Hydroclimate Whiplash Events", 
       x = "Basin ID", y = "Event Count")

# Combine the event counts and map them spatially if you have longitude/latitude data
# Example for mapping
ggplot(results, aes(x = longitude, y = latitude, color = wet_to_dry_count)) +
  geom_point() +
  scale_color_gradient(low = "yellow", high = "blue") +
  labs(title = "Spatial Distribution of Wet-to-Dry Events")


#############################################################################
#### LAND COVER CHANGE ####
#############################################################################
# shannon diversity index is metric that quantifies how evenly distributed the land cover classes are
# the more even distribution, the higher the index
# a positive change means land cover has become more diverse (more even spread across types)
# a negative change means land cover has become less diverse (more dominated by fewer classes)
# zero change means no change in diversity 
shannon_index = function(props) {
  props = props[props > 0] # remove zeros to avoid log(0)
  -sum(props * log(props))
}

LC_files = "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Land_cover"
file_list = list.files(LC_files, pattern = "^ANLCD\\d{4}\\.csv$", full.names = TRUE)
output_folder = "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/LC_change"

# extract years and sort
years = str_extract(basename(file_list), "\\d{4}") %>% as.integer() %>% sort()

# identify start years using 5 year periods from 1985 to 2020
# i.e. january 1 1985 to december 31 1989 is 1985-1990, 5 year period
start_years = seq(1985, 2015, by = 5)

# define 5 year windows
windows = tibble(
  start_year = start_years,
  end_year = start_years + 5
)

# process each 5 year window
for (i in seq_len(nrow(windows))) {
  start_year = windows$start_year[i]
  end_year = windows$end_year[i]
  
  # read two files for time window
  df_start = read_csv(file.path(LC_files, paste0("ANLCD", start_year, ".csv")))
  df_end = read_csv(file.path(LC_files, paste0("ANLCD", end_year, ".csv")))
  
  # join by siteno
  merged = inner_join(df_start, df_end, by = "SITENO", 
                      suffix = c(paste0("_", start_year), paste0("_", end_year)))
  
  # compute absolute change (over period) and rate of change (per year)
  # change is based on end year - start year so positive means increase, negative means decrease
  lc_codes = c(11,12,21,22,23,24,31,41,42,43,52,71,81,82,90,95)
  for (lc in lc_codes) {
   merged[[paste0(lc, "_abs_change")]] = round(merged[[paste0(lc, "_", end_year)]] - merged[[paste0(lc, "_", start_year)]], 2)
   merged[[paste0(lc, "_rate_change")]] = round(merged[[paste0(lc, "_abs_change")]] / 5, 2)
  }

  # compute Shannon diversity index change
  merged = merged %>%
    rowwise() %>%
    mutate(
      shannon_start = shannon_index(c_across(all_of(paste0(lc_codes, "_", start_year))) / 100), 
      shannon_end = shannon_index(c_across(all_of(paste0(lc_codes, "_", end_year))) / 100), 
      shannon_change = round(shannon_end - shannon_start, 2)
    ) %>%
    ungroup()

  # export to CSV
  output = merged %>%
    select(SITENO, ends_with("_abs_change"), ends_with("_rate_change"), shannon_change)

  write_csv(output, file.path(output_folder, paste0("landcover_change_", start_year, "_to_", end_year, ".csv")))
}

#################################
# generalized group changes
# Define LC codes and groups
lc_codes <- c(11,12,21,22,23,24,31,41,42,43,52,71,81,82,90,95)

lc_groups <- list(
  water = 11, 
  ice_snow = 12,
  barren = 31, 
  urban = c(21, 22, 23, 24), 
  forest = c(41, 42, 43), 
  shrub = 52, 
  grassland = 71, 
  farm = c(81, 82), 
  wetlands = c(90, 95)
)

group_results <- list()

# Create lookup vector
code_to_group <- unlist(lc_groups)
names(code_to_group) <- rep(names(lc_groups), lengths(lc_groups))

for (i in seq_len(nrow(windows))) {
  start_year <- windows$start_year[i]
  end_year <- windows$end_year[i]

  df_start <- read_csv(file.path(LC_files, paste0("ANLCD", start_year, ".csv")),
                       col_types = cols(),
                       name_repair = "minimal")
  df_end <- read_csv(file.path(LC_files, paste0("ANLCD", end_year, ".csv")),
                     col_types = cols(),
                     name_repair = "minimal")

  # Ensure column names are characters (e.g., "11", not X11 or numeric)
  names(df_start) <- as.character(names(df_start))
  names(df_end)   <- as.character(names(df_end))

 # START YEAR
df_start_grouped <- df_start %>%
  select(SITENO, all_of(as.character(lc_codes))) %>%
  pivot_longer(-SITENO, names_to = "code", values_to = "value") %>%
  mutate(code = as.integer(code),
         group = code_to_group[as.character(code)]) %>%
  filter(!is.na(group)) %>%
  group_by(SITENO) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  group_by(SITENO, group) %>%
  summarise(percent = 100 * sum(value, na.rm = TRUE) / unique(total), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = percent, names_prefix = paste0("g_", start_year, "_"))

# END YEAR
df_end_grouped <- df_end %>%
  select(SITENO, all_of(as.character(lc_codes))) %>%
  pivot_longer(-SITENO, names_to = "code", values_to = "value") %>%
  mutate(code = as.integer(code),
         group = code_to_group[as.character(code)]) %>%
  filter(!is.na(group)) %>%
  group_by(SITENO) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  group_by(SITENO, group) %>%
  summarise(percent = 100 * sum(value, na.rm = TRUE) / unique(total), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = percent, names_prefix = paste0("g_", end_year, "_"))

  # Join start and end year data
  merged <- full_join(df_start_grouped, df_end_grouped, by = "SITENO")

  # Calculate change and rate
  for (group in names(lc_groups)) {
    start_col <- paste0("g_", start_year, "_", group)
    end_col   <- paste0("g_", end_year, "_", group)
    abs_col   <- paste0(group, "_abs_change")
    rate_col  <- paste0(group, "_rate_change")

    merged[[abs_col]]  <- round(merged[[end_col]] - merged[[start_col]], 2)
    merged[[rate_col]] <- round((merged[[end_col]] - merged[[start_col]]) / 5, 2)
  }

  group_results[[paste0(start_year, "_to_", end_year)]] <- merged
}
# Save all to CSVs
output_dir = "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/LC_change"
dir.create(output_dir, showWarnings = FALSE)

for (period in names(group_results)) {
  write_csv(
    group_results[[period]],
    file.path(output_dir, paste0("landcover_group_change_", period, ".csv"))
  )
}
#####################
# directional land cover class changes

# create general groups 
# ignore water, ice/snow, and wetlands since not much change in these classes
lc_groups = list(
  water = 11, 
  ice_snow = 12, 
  barren = 31,
  urban = c(21, 22, 23, 24), 
  forest = c(41, 42, 43), 
  shrub = 52, 
  grassland = 71, 
  farm = c(81, 82), 
  wetlands = c(90, 95))

# get category from LC code
get_lc = function(code) {
  for (cat in names(lc_groups)) {
    if (code %in% lc_groups[[cat]]) return(cat)
  } 
  return(NA) # if not in one of the groups
}

# read and group land cover data
load_lc_yr = function(file_path) {
  df = read_csv(file_path)
  df_long = df %>% 
    pivot_longer(-SITENO, names_to = "code", values_to = "percent") %>% 
    mutate(code = as.numeric(code), 
           category = sapply(code, get_lc)) %>% 
    filter(!is.na(category)) %>% 
    group_by(SITENO, category) %>% 
    summarise(percent = sum(percent), .groups = "drop")
  
  return(df_long)
}

# determine transitions between categories
# get percentage of land cover groups 
lc_2015 = load_lc_yr("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Land_cover/ANLCD2015.csv")
lc_2020 = load_lc_yr("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Land_cover/ANLCD2020.csv")

# find percent change among each group
lc_change = lc_2010 %>% left_join(lc_2015, by = c("SITENO", "category")) %>% 
  rename(percent_2010 = percent.x, 
         percent_2015 = percent.y) %>% 
  mutate(
    percent_2010 = round(as.numeric(percent_2010), 2), 
    percent_2015 = round(as.numeric(percent_2015), 2),
    change = round(percent_2015 - percent_2010, 2))

write_csv(lc_change, 
          "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/LC_change/landcover_change_groups_2010_2015.csv")

# get largest increase and decrease between all groups in each basin
# can give estimate of overall directional change
top_changes = lc_change %>% 
  group_by(SITENO) %>% 
  summarise(increase = category[which.max(change)], 
            max_increase = round(max(change), 2), 
            decrease = category[which.min(change)], 
            max_decrease = round(min(change), 2), 
            .groups = "drop")

write_csv(top_changes, 
          "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/LC_change/landcover_top_changes_groups_2010_2015.csv")

######################
### RUNOFF CHANGE ###
#####################
# changes in runoff 
# get 5 year runoff totals 
runoff_files = list.files("D:/University of Texas at Dallas/CombinedDataset/Formatted_data/ALLVAR", 
                         pattern = "^basin_\\d{8}_MACH.csv$", full.names = TRUE)

# manual 5-year window definition
breaks = seq(1985, 2015, by = 5)
labels = paste0(breaks, "_to_", breaks + 5)

assign_period = function(year) {
  match = which(breaks <= year & year < breaks + 5)
  if (length(match) == 0) return(NA)
  return(labels[match])
}

# computes total runoff over 5 year period (not between)
compute_5yr_totals = function(file_path) {
  df = read_csv(file_path)
  
  SITENO = str_extract(basename(file_path), "\\d{8}")
  
  df = df %>%
    mutate(DATE = as.Date(DATE),
           YR = year(DATE),
           SITENO = SITENO,
           period = sapply(YR, assign_period)) %>%
    filter(!is.na(period))
  
  df_5yr = df %>%
    group_by(SITENO, period) %>%
    summarise(Q_total = sum(OBSQ, na.rm = TRUE), .groups = "drop")
  
  return(df_5yr)
}

runoff_data = lapply(runoff_files, compute_5yr_totals) %>% bind_rows()

###########################
## USE THIS FOR COMPARISON TO LAND COVER CHANGES !
## SENSITIVITY OF RUNOFF TO CHANGES 

# annual runoff totals 
# calculate annual totals in runoff and compute difference between 5 year points
# function to get annual totals
compute_annual_totals = function(file_path) {
  df = read_csv(file_path, show_col_types = FALSE)
  SITENO = str_extract(basename(file_path), "\\d{8}")
  
  df = df %>%
    mutate(DATE = as.Date(DATE),
           YR = year(DATE),
           SITENO = SITENO) %>%
    group_by(SITENO, YR) %>%
    summarise(Q_total = sum(OBSQ, na.rm = TRUE), 
              PRCP_total = sum(PRCP, na.rm = TRUE), 
              .groups = "drop")
  
  return(df)
}

# combine annual totals across sites
annual_runoff = lapply(runoff_files, compute_annual_totals) %>% bind_rows()

# pivot to wide format for year-on-year difference
runoff_wide = annual_runoff %>%
  pivot_wider(names_from = YR, values_from = Q_total, names_prefix = "Q_")

Q_change = "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Q_change"

# list of numeric variables to calculate changes for
numeric_vars = names(annual)[!names(annual) %in% c("SITENO", "YR")]

# pivot wider for each variable
runoff_wide = annual %>%
  pivot_wider(names_from = YR, values_from = all_of(numeric_vars), names_sep = "_")

# define 5-year periods (e.g., Q_1990 - Q_1985)
periods = seq(1985, 2020, by = 5)

# loop through each 5-year period and each variable
for (i in seq_along(periods[-length(periods)])) {
  start_year = periods[i]
  end_year = periods[i + 1]
  
  runoff_wide_period = runoff_wide %>% select(SITENO)  
  
  for (var in numeric_vars) {
    col_start = paste0(var, "_", start_year)
    col_end   = paste0(var, "_", end_year)
    col_diff  = paste0(var, "_change")

    runoff_wide_period[[col_diff]] = round(runoff_wide[[col_end]] - runoff_wide[[col_start]], 2)
  }
  
  # save to CSV
  write_csv(runoff_wide_period, file.path(Q_change, paste0("change_", start_year, "_to_", end_year, ".csv")))
}

# runoff change only
# compute 5-year period change 
# for (i in seq_along(periods[-length(periods)])) {
  #start_year = periods[i]
  #end_year = periods[i + 1]
   #runoff_wide = runoff_wide %>% 
    #mutate(
     # !!paste0("Q_abs_change_", start_year, "_to_", end_year) := round(as.numeric(.data[[paste0("Q_", end_year)]] - .data[[paste0("Q_", start_year)]]), 2) #,
      #!!paste0("Q_rate_change_", start_year, "_to_", end_year) := round(as.numeric((.data[[paste0("Q_", end_year)]] - .data[[paste0("Q_", start_year)]]) / 5), 2)
  #  )
  # output per-period CSV to match land cover format
  #output = runoff_wide %>%
   # select(SITENO, starts_with(paste0("Q_abs_change_", start_year)) , starts_with(paste0("Q_rate_change_", start_year))) %>%
    #rename(
     # Q_abs_change = paste0("Q_abs_change_", start_year, "_to_", end_year),
      #Q_rate_change = paste0("Q_rate_change_", start_year, "_to_", end_year)
    #)
#  write_csv(output, file.path(Q_change,paste0("runoff_change_", start_year, "_to_", end_year, ".csv")))
#}

############
# read in attributes
site_info = read_csv("F:/MACH/MACH_explorer/data/attributes/site_info.csv")
site_info = site_info %>% select(SITENO, basin_slope, elev_mean, NHD_drain_area_sqkm)

# define 5 year periods
periods = c("1985_to_1990", "1990_to_1995", "1995_to_2000", 
             "2000_to_2005", "2005_to_2010", "2010_to_2015", "2015_to_2020")

# combined lc change and runoff change files into one 
merged_datasets = list()

for (period in periods) {
  
  # read runoff change and land cover change files
  runoff_file = paste0("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Q_change/change_", period, ".csv")
  lc_file = paste0("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/LC_change/landcover_change_groups_", period, ".csv")
  
  runoff_df = read_csv(runoff_file)
  lc_df = read_csv(lc_file)
  
  # drop rows where runoff change is NA or zero
  runoff_df = runoff_df %>% dplyr::filter(!is.na(OBSQ_change))
  
  # select only SITENO, category, and change columns from csv for land cover groups and then pivot
  lc_wide = lc_df %>% 
    select(SITENO, category, change) %>% 
    pivot_wider(names_from = category, values_from = change, names_glue = "{category}_change")
  
  # merge runoff and land cover changes
  combined = inner_join(runoff_df, lc_wide, by = "SITENO")
  
  # add static site attributes
 # combined = inner_join(combined, site_info, by = "SITENO")
  
  # add period label for tracking
  combined$period = period
  
  # store in the list
  merged_datasets[[period]] = combined
}

# combine all periods into one dataframe
full_dataset = bind_rows(merged_datasets)

full_dataset = full_dataset %>% drop_na()

library(randomForest)
# exclude non-predictor columns like SITENO and period
# predictors (features) include PRCP, PET, ET, forest, etc changes
# target is obsq change, how runoff changed during that period
# each tree tries to predict OBSQ_change based on different random subsets of predictors and data
# final prediction is average of all tree outputs (for regression)
# handles non linear relationships, capturing interactions, robust to overfitting

# predicted = model predictions for training data, rsq = r squared per tree where final is overall
# mse = mean squared error per tree, ntrees = default is 500, y = observed response variable

# evaluate over all periods to get overall relationships across space and time
rf_global = randomForest(
  OBSQ_change ~ PET_change + PRCP_change + ET_change + TAIR_change + PETvP_change + EvP_change + 
  barren_change + farm_change + forest_change + grassland_change + ice_snow_change + 
  shrub_change + urban_change + water_change + wetlands_change, 
  data = full_dataset, 
  importance = TRUE
)

print(rf_global)
# model evaluation 
# get predicted runoff change
preds = predict(rf_global, newdata = full_dataset)
r_squared = cor(preds, full_dataset$OBSQ_change)^2
rmse_val = sqrt(mean((preds - full_dataset$OBSQ_change)^2))

# view and sort variable importance
# %IncMSE is percent increase in MSE, how much worse the model performs when given variable is randomly shuffled
# for each tree, builds tree on bootstrapped sample, evaluates MSE with out of bag data (data not used in training)
# randomly shuffles one predictor in the OOB data, runs tree again, calculates new MSE, percent increase due to shuffling recorded
# repeated for all trees then averaged
importance_df = as.data.frame(importance(rf_model))
importance_df$Variable = rownames(importance_df)
varImpPlot(rf_global, type = 1)

# calculate sensitivity by period to evaluate temporal variability in sensitivity 
rf_period = list()
importance_list = list()

merged_datasets = lapply(merged_datasets, drop_na)

for (period in periods) {
  df = merged_datasets[[period]]
  
  # fit random forest
  rf_model = randomForest(
    OBSQ_change ~ PET_change + PRCP_change + ET_change + TAIR_change + PETvP_change + EvP_change + 
      barren_change + farm_change + forest_change + grassland_change + ice_snow_change + 
      shrub_change + urban_change + water_change + wetlands_change, 
    data = df, 
    importance = TRUE
  )
  
  rf_period[[period]] = rf_model
  
  # get r sqaured
  r2 = rf_model$rsq[length(rf_model$rsq)]  # last OOB
  
  # Extract variable importance (%IncMSE)
  importance_df = as.data.frame(importance(rf_model))
  importance_df$variable = rownames(importance_df)
  importance_df$period = period
  importance_df$r2 = r2
  importance_df = importance_df %>%
    select(period, r2, variable, `%IncMSE`) %>%
    arrange(desc(`%IncMSE`))
  
  importance_list[[period]] = importance_df
}

importance_all_periods = bind_rows(importance_list)

ggplot(importance_all_periods, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~period) +
  theme_minimal() +
  labs(title = "Variable Importance by Period", x = "Variable", y = "% Increase in MSE")


## get most influential variable by site using all time periods 

site_info = site_info %>% select(SITENO, dec_lat_va, dec_long_va)

# create list of dataframes per site (not all sites will show due to incomplete runoff data drop NA)
site_list = split(full_dataset, full_dataset$SITENO)

rf_per_site = list()
importance_per_site = list()

for (s in names(site_list)) {
  df = site_list[[s]]
  
  # skip if not enough data
  if (nrow(df) < 2) next
  
  # fit model
  rf_model = randomForest(
    OBSQ_change ~ PET_change + PRCP_change + ET_change + TAIR_change + PETvP_change + EvP_change + 
      barren_change + farm_change + forest_change + grassland_change + ice_snow_change + 
      shrub_change + urban_change + water_change + wetlands_change,
    data = df,
    importance = TRUE
  )
  
  # Store model
  rf_per_site[[s]] = rf_model
  
  # Get variable importance
  imp = as.data.frame(importance(rf_model))
  imp$variable = rownames(imp)
  imp$SITENO <- s
  imp <- imp %>%
    select(SITENO, variable, `%IncMSE`) %>%
    arrange(desc(`%IncMSE`)) %>%
    slice(1)  # Keep only top variable
  
  importance_per_site[[s]] <- imp
}

dominant_vars = bind_rows(importance_per_site)

map_data = left_join(dominant_vars, site_info, by = "SITENO")

# recode variables for easier map display
map_importance = map_data %>% 
  mutate(group = case_when(
    variable %in% c("barren_change", "farm_change", "forest_change", "grassland_change", "ice_snow_change", 
                    "shrub_change", "urban_change", "water_change", "wetlands_change") ~ "land_cover", 
    TRUE ~ variable
  ))

ggplot(map_importance, aes(x = dec_long_va, y = dec_lat_va, color = group)) +
  geom_point(size = 3) +
  theme_minimal() + theme(axis.title = element_blank()) +
  labs(title = "Dominant Influential Variable at Each Site (All Periods Combined)",
       color = "Top Variable") +
  scale_color_brewer(palette = "Set3")