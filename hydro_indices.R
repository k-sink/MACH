# Katharine Sink

# climate and hydro indices
library(tidyverse)
library(EflowStats)
library(dataRetrieval)
library(smwrBase)


#######################################################################
 ## FORMAT DISCHARGE FILES ##
#######################################################################
#remotes::install_gitlab("water/analysis-tools/smwrData", host = "code.usgs.gov")
#remotes::install_gitlab("water/analysis-tools/smwrBase", host = "code.usgs.gov")

# list of gauges to retrieve info using dataRetrieval function
# use gauges with complete years 
# gauges = read.csv("D:/University of Texas at Dallas/CombinedDataset/Complete_Q.csv")
gauges = read.csv("D:/University of Texas at Dallas/CombinedDataset/Attributes.csv")
# imports as integer, convert to character
gauges = as.character(gauges$SITENO)
# add leading zero to gauges with 7 characters to make it an 8 digit value
gauges = str_pad(gauges, max(nchar(gauges)), side = "left", pad = "0")

#######################################################################

# discharge files in cfs, save just date and discharge value from original USGS files 
discharge_path = "D:/University of Texas at Dallas/CombinedDataset/Discharge"
streamflow_files = list.files(path = discharge_path, pattern = "\\.csv$", full.names = TRUE)
output_folder = "D:/University of Texas at Dallas/CombinedDataset/Discharge2"

# loop through each streamflow file
for (streamflow_file in streamflow_files) {
  
  # Read the streamflow data
  streamflow_data <- read_csv(streamflow_file, col_types = cols(
    agency_cd = col_character(), 
    site_no = col_character(),  # Ensure site_no is read as a character
    dateTime = col_date(), 
    X_00060_00003 = col_double(),
    X_00060_00003_cd = col_character(),
    tz_cd = col_character()
    ))

  streamflow_data = select(streamflow_data, c("dateTime", "X_00060_00003"))
  streamflow_data = streamflow_data %>% rename("DATE" = "dateTime", "RUN" = "X_00060_00003")

    # Create output file path
  output_file <- file.path(output_folder, basename(streamflow_file))
  
  # Save the updated streamflow data
  write_csv(streamflow_data, output_file)
}

##################################################################

# function to create the full date sequence for each file
create_complete_dates = function() {
  # Create a complete sequence of dates from 01/01/1980 to 12/31/2023 (16071 days)
  seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "day")
}

# function to process each CSV file and add NA if missing discharge data 
process_csv_file = function(file_path) {
  # create the full date sequence
  complete_dates = create_complete_dates()
  
  # Read the CSV file
  df = read_csv(file_path, col_types = list(col_date(), col_double()))
  
  # ensure the 'DATE' column is in Date format
  df$DATE = as.Date(df$DATE, format = "%m/%d/%Y")
  
  # merge the CSV file data with the complete date sequence
  # use full_join to preserve all the dates, even those missing in the CSV file
  df_complete = full_join(data.frame(DATE = complete_dates), df, by = "DATE")
  
  # fill missing discharge values with NaN
  df_complete$RUN[is.na(df_complete$RUN)] = NaN

 # order the columns 
  df_complete = df_complete %>% dplyr::select(DATE, RUN)
  
   # return the processed data frame in desired order
  return(df_complete)
}

# directory where the CSV files are stored
input_dir = "D:/University of Texas at Dallas/CombinedDataset/Discharge2"

# save discharge cfs with NA for missing dates
output_folder = "D:/University of Texas at Dallas/CombinedDataset/Discharge_cfs"

# get the list of all CSV files in the directory
csv_files = list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# loop through each file, process it, and save the updated file
for (file_path in csv_files) {
  # process the CSV file
  df_processed = process_csv_file(file_path)
  
  # Generate output file name
  output_file_name = paste0("basin_", tools::file_path_sans_ext(basename(file_path)), ".csv")
  output_file_path = file.path(output_folder, output_file_name)
  
  # write the updated data frame back to a CSV file 
 write.csv(df_processed, output_file_path, row.names = FALSE)
}

#######################################################################
 ## HYDRO STATISTICS ##
#######################################################################
# mean daily discharge 
# runoff ratio 
# slope of flow duration curve (between log transformed 33rd and 66th streamflow percentiles)
# mean half flow date (date on which cumulative discharge since October 1st reaches half of annual discharge, day of year)
# Q5 5% flow quantile, low flow (mm/day)
# Q95 95% flow quantile, high flow (mm/day)
# high_q_freq, frequency of high flow days (>9 times the median daily flow, days/yr)
# high_q_dur, average duration of high flow events (number of consecutive days >9 times median daily flow, days)
# low_q_freq, frequency of low flow days (<0.2 times the mean daily flow, days/yr)
# low_q_dur, average duration of low flow events (number of consecutive days <0.2 times mean daily flow, days)
# zero_q_freq, frequency of days with Q = 0mm (%)

csv_folder = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/MACH/OBSQ"
output_file = "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Overall/hydro_stats2.csv"

# list of discharge files (mm/day)
csv_files = list.files(path = csv_folder, pattern = "basin_\\d{8}_obsq.csv$", full.names = TRUE)


# Function to compute water year
compute_water_year = function(date) {
  year = year(date)
  month = month(date)
  ifelse(month >= 10, year + 1, year)  # Assign next calendar year to water year if date is in Oct-Dec
}

# determine if the year is a leap year 
# functions taken from EflowStats 
is.leapyear = function(year, yearType = 'calendar', MNTH = NULL) {
  if (yearType=='calendar' || is.null(MNTH) || MNTH<3 || MNTH>6) {
     return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) 
  }else if ((yearType=='water') && !is.null(MNTH) && (MNTH>=3 && MNTH <=6)) {
        return((((year+1) %% 4 == 0) & ((year+1) %% 100 != 0)) | ((year+1) %% 400 == 0)) 
  }
}

# aggregate to annual streamflow and precipitation
#year = MACH_data %>% group_by(SITENO, WYR) %>% reframe(OBSQ = sum(OBSQ, na.rm = TRUE), PRCP = sum(PRCP, na.rm = TRUE))
# get long term mean per site
#mean = year %>% group_by(SITENO) %>% summarise(OBSQ_mean = mean(OBSQ), PRCP_mean = mean(PRCP))

# get deviations from mean for each year
#diff = year %>% left_join(mean, by = "SITENO") %>% 
 # mutate(dQ = OBSQ - OBSQ_mean, dP = PRCP - PRCP_mean,  
     #     dQ_norm = dQ/OBSQ_mean, dP_norm = dP/PRCP_mean) # normalized difference 

#stream_elast = diff %>% group_by(SITENO) %>% 
 # reframe(elast = coef(lm(dQ_norm ~ dP_norm))[2])



# create empty dataframe to store results
summary_stats = tibble(SITENO = character(), q_mean = numeric(), FDC_slope = numeric(), mean_hf_date = numeric(), 
                           Q5 = numeric(), Q95 = numeric(), high_q_freq = numeric(), high_q_dur = numeric(), 
                           low_q_freq = numeric(), low_q_dur = numeric(), zero_q = numeric())

# function to calculate hydro statistics for each file
compute_stats = function(data) {
  data = data %>% dplyr::select(SITENO, DATE, WYR, OBSQ)

  q_mean = mean(data$OBSQ)
  # flow duration curve slope
  Q33 = quantile(data$OBSQ, probs = 0.33, na.rm = TRUE)
  Q66 = quantile(data$OBSQ, probs = 0.66, na.rm = TRUE)
  FDC_slope = (log10(Q66) - log10(Q33)) / (0.66 - 0.33)
  
  # mean half flow date
  # half_flow = data %>% group_by(year(DATE)) %>%  
     half_flow = data %>% group_by(WYR) %>% 
     summarise(hf_date = {
      cum_discharge = cumsum(OBSQ)
      total_discharge = sum(OBSQ, na.rm = TRUE)
      doy = yday(DATE[min(which(cum_discharge >= 0.5 * total_discharge))])
      doy
    })
  mean_hf_date = mean(half_flow$hf_date, na.rm = TRUE)
  
  # Q5 and Q95
  Q5 = quantile(data$OBSQ, probs = 0.05, na.rm = TRUE)
  Q95 = quantile(data$OBSQ, probs = 0.95, na.rm = TRUE)
  
  # high flow metrics, frequency and duration
  median_flow = median(data$OBSQ, na.rm = TRUE)
  high_flow_events = rle(data$OBSQ > 9 * median_flow)
 # high_q_freq = sum(high_flow_events$values) / length(unique(year(data$DATE)))
  high_q_freq = sum(high_flow_events$values) / length(unique(data$WYR))
  high_q_dur = mean(high_flow_events$length[high_flow_events$values], na.rm = TRUE)
  
  # low flow metrics, frequency and duration
  mean_flow = mean(data$OBSQ, na.rm = TRUE)
  low_flow_events = rle(data$OBSQ < 0.2 * mean_flow)
#  low_q_freq = sum(low_flow_events$values) / length(unique(year(data$DATE)))
   low_q_freq = sum(low_flow_events$values) / length(unique(data$WYR))
   low_q_dur = mean(low_flow_events$lengths[low_flow_events$values], na.rm = TRUE)
  
  # zero discharge frequency
  zero_q = (sum(data$OBSQ == 0, na.rm = TRUE) / nrow(data)) * 100
  
  # return a named vector for the statistics
  return(list(q_mean = q_mean, FDC_slope = FDC_slope, mean_hf_date = mean_hf_date, Q5 = Q5, Q95 = Q95, 
            high_q_freq = high_q_freq, high_q_dur = high_q_dur, low_q_freq = low_q_freq, 
            low_q_dur = low_q_dur, zero_q = zero_q))
}
  
# loop through each file and compute statistics
for (file in csv_files) {
  # extract gauge ID from filename
  SITENO = gsub("basin_|_obsq.csv", "", basename(file))

    # Read the CSV file
  data = read_csv(file, col_types = cols(SITENO = col_character(), DATE = col_character(), 
                                              OBSQ = col_double()))
  data = data %>% mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))
 
   # Compute water year and add month
  data = data %>% mutate(WYR = compute_water_year(DATE), MNTH = month(DATE))
 
  # arrange date 
  data = data %>% arrange(DATE) 
  data = data %>% drop_na(OBSQ) 
  
  # count number of days in each water year 
  # only use complete water years, ignore years with missing days
 # ndays_yr = data %>% group_by(YR) %>% summarise(ndays = n())
  ndays_yr = data %>% group_by(WYR) %>% summarise(ndays = n())
  
  # determine number of target days (leap year vs non leap year)
 # ndays_target = ndays_yr %>% mutate(ndays_target = ifelse(is.leapyear(YR ,"calendar", 12), 366, 365))
    ndays_target = ndays_yr %>% mutate(ndays_target = ifelse(is.leapyear(WYR ,"water", 12), 366, 365))
  
   # merge target days with original, remove incomplete years
#  data = data %>% left_join(ndays_target, by = "YR") %>% 
    data = data %>% left_join(ndays_target, by = "WYR") %>% 
    filter(ndays == ndays_target) %>% 
  #  dplyr::select(-c(YR, MNTH, ndays, ndays_target))
      dplyr::select(-c(MNTH, ndays, ndays_target))

data = as.data.frame(data)
  
  # compute statistics (named vector)
  stats = compute_stats(data)
  
  # convert stats to a tiblle and add siteno
  new_row = tibble(SITENO = SITENO, !!!stats)
  
  print(new_row)
  
  # Append to summary dataframe
  summary_stats = bind_rows(summary_stats, new_row)
}

# Convert columns to numeric where necessary
summary_stats = summary_stats %>%
  mutate(across(-SITENO, as.numeric))

# Save final summary to CSV
write.csv(summary_stats, output_file, row.names = FALSE)


#######################################################################
 ## MAGNIFICENT SEVEN (FDSS) ##
#######################################################################
#EflowStats package designed to work with NWIS data, Hydrologic Indicator and Alteration Stats
# can be used with dataRetrieval to calculate HIT and MAG7 statistics for any USGS stream gage
# own time series can be used with data.frame, class Date in first column and streamflow values of class Numeric in second
# Richter (1996) 32 indices encompasses most of redundant indices 
# Archfield (2014) 7 fundamental daily streamflow statistics (FDSS) can accurately classify better than 32 indices
# MAG7 magnificent seven based on lmoments

# use discharge data that has only available data, not NA values added

input_dir = "D:/University of Texas at Dallas/CombinedDataset/Discharge2"
output_file = "D:/University of Texas at Dallas/CombinedDataset/IHA/magnifSeven.csv"

csv_files = list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
all_results = list()

# Process each file
for (file_path in csv_files) {
  # Extract site number from filename (assumes format: "00000000.csv")
  site_no = basename(file_path) %>% str_remove(".csv")
  
  # Read the CSV file
  df = read_csv(file_path, col_types = cols(DATE = col_character(), RUN = col_double()))
  df = df %>% mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))
 
   # Compute water year
  df = df %>% mutate(WYR = compute_water_year(DATE))
 # add month column 
  df = df %>% mutate(MNTH = month(DATE))

  df = dplyr::arrange(df, DATE)
  df = tidyr::drop_na(df)
  # get the first water year 
  #first_year = df$WYR[1]
  # get the last water year
  #last_year = df$WYR[nrow(df)]
  
  # count number of days in each water year 
  # only use complete water years, ignore years with missing days
  ndays_yr = df %>% group_by(WYR) %>% summarise(ndays = n())

  # count the number of days in the first and last water years 
#ndays_first_year = nrow(df[df$WYR == first_year,])
#ndays_last_year = nrow(df[df$WYR == last_year,])

 # get the target number of days (complete number for a year)
#ndays_first_year_target <- ifelse(is.leapyear(first_year, "water", 10), 366, 365)
#ndays_last_year_target <- ifelse(is.leapyear(last_year,  "water", 10), 366, 365)

  ndays_target = ndays_yr %>% mutate(ndays_target = ifelse(is.leapyear(WYR ,"water", 10), 366, 365))
  
#  if(ndays_first_year < ndays_first_year_target){
 #   df = df[df$WYR != first_year,]  # remove the first year if it does not have complete data
 # }
#  if(ndays_last_year < ndays_last_year_target){
 #   df = df[df$WYR != last_year,]  # remove the last year if it does not have complete data
#  }

  # merge target days with original
  df = df %>% left_join(ndays_target, by = "WYR")
  
  # remove incomplete years
  df = df %>% filter(ndays == ndays_target)
  
  # remove the water year and month columns
  df = dplyr::select(df, -c(WYR, MNTH, ndays, ndays_target))
    
  # make sure data frame 
  df = as.data.frame(df)

  # calculate the seven indices from Archfield
  x = calc_magnifSeven(df, "water", 10)
 
  # store results in list with site number as the first column
  result_df = as.data.frame(cbind(site_no, x))
  all_results[[site_no]] = result_df
}

  final = bind_rows(all_results)
  final = pivot_wider(final, id_cols = site_no, names_from = indice, values_from = statistic)
  write_csv(final, output_file)

#######################################################################
 ## CLIMATE AVERAGES ##
#######################################################################
# mean annual values 

year = MACH_data %>% group_by(SITENO, YR) %>% reframe(PRCP = sum(PRCP), TMIN = mean(TMIN), 
             TMAX = mean(TMAX), TMEAN = mean(TMEAN), PET = sum(PET), AET = sum(AET))

year9120 = year %>% dplyr::select(SITENO, YR, PRCP, TMIN, TMAX, TMEAN, PET, AET) %>% 
  dplyr::filter(YR %in% 1991:2020)

# 30 year mean annual maximum temperature (1981-2010, 1991-2020)
climate9120 = year9120 %>% group_by(SITENO) %>% 
  reframe(PPT9120 = mean(PRCP), TMIN9120 = mean(TMIN), TMAX9120 = mean(TMAX), 
                TAV9120 = mean(TMEAN), PET9120 = mean(PET), AET9120 = mean(AET)) %>% 
  mutate(across(-SITENO, ~round(.x, 2)))


climate9120 = write_csv(climate9120, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Overall/climate9120.csv")

####################################################################
### CLIMATE INDICES ###
###################################################################
# Core climate extreme indices from ETCCDI (Expert Team on Climate Change Detection and Indices)
# CLIVAR/CCI/JCOMM  http://etccdi.pacificclimate.org/list_27_indices.shtml
# 27 core indices

library(SPEI)
library(furrr)
library(zoo)

#### PRECIPITATION ####

## RX1day ##
# monthly max 1 day precip
RX1day = MACH_data %>% group_by(SITENO, YR, MNTH) %>% reframe(RX1day = max(PRCP))
RX1day = pivot_wider(RX1day, names_from = YR, values_from = PRCP)
write.csv(RX1day, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/RX1day.csv")

## RX5day ##
# monthly maximum consecutive 5 day precipitation 
plan(multisession)

# function to calculate the maximum 5 day consecutive precip for each month (5 day rolling sum)
calc_Rx5day = function(precip_data) {
  precip_data = precip_data %>% mutate(rolling_sum = zoo::rollsum(PRCP, 5, align = "right", fill = NA))
  
  # get max rolling sum for the month
  max_5day = max(precip_data$rolling_sum, na.rm = TRUE)
  
  return(max_5day)
}

precip = MACH_data %>% dplyr::select(SITENO, DATE, YR, MNTH, DY, PRCP)

Rx5day = precip %>% group_by(SITENO, YR, MNTH) %>% arrange(DATE) %>% 
  group_split() %>% furrr::future_map_dfr(~data.frame(RX5day = calc_Rx5day(.)), .progress = TRUE)
write.csv(names, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Rx5day.csv")

## SPI ##
# standardized precipitation index
# drought index that captures how observed precipitation deviates from climatological average over given time period
# drought indices >0 wetter than normal, <0 drier than normal 
# <-2 extreme drought, <-1.5 to -2 severe drought
month = MACH_data %>% group_by(SITENO, YR, MNTH) %>% reframe(PRCP = sum(PRCP))
#month = month %>% mutate(date = as.Date(paste(YR, MNTH, "1", sep = "-")))
#month = month %>% dplyr::select(-c(YR, MNTH))

# spi and spei functions are identical except for distribution function and data, 
# time ordered values of precipitation (SPI) and gamma or climatic balance
# precipitation minus potential evapotranspiration (SPEI) and log-logistic
calc_spi = function(precip) {
  spi_result = spi(precip, scale = 1)
  return(spi_result$fitted)
}

spi_1 = month %>% group_by(SITENO) %>% mutate(SPI = calc_spi(PRCP)) %>% ungroup()
# remove -inf values which occur when month is zero total precipitation 
spi = spi_1 %>% filter(!is.infinite(SPI)) %>% group_by(SITENO, YR) %>% reframe(SPI = mean(SPI))
spi = pivot_wider(spi, names_from = YR, values_from = SPI)
write.csv(spi, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/SPI.csv")

## SPEI (standardized precipitation evapotranspiration index) ##

# use water balance, wb = precip - pet for calculating index instead of precipitation 
month2 = MACH_data %>% group_by(SITENO, YR, MNTH) %>% reframe(PRCP = sum(PRCP), PET = sum(PET))
month2 = month2 %>% mutate(WB = PRCP - PET)

# remove site because no PET data available 
month2 = month2 %>% filter(SITENO != "10336740")

calc_spei = function(wb) {
  spei_result = spei(wb, scale = 1)
  return(spei_result$fitted)
}

spei_1 = month2 %>% group_by(SITENO) %>% mutate(SPEI = calc_spei(WB)) %>% ungroup()
# remove -inf values which occur when month is zero total precipitation 
spei = spei_1 %>% filter(!is.infinite(SPEI)) %>% group_by(SITENO, YR) %>% reframe(SPEI = mean(SPEI))
spei = pivot_wider(spei, names_from = YR, values_from = SPEI)
write.csv(spei, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/SPEI.csv")

## Rnnmm ##
# user defined threshold 
# annual count of days when precipitation is zero mm (R00mm) 
R00mm = MACH_data %>% group_by(SITENO, YR) %>% tally(PRCP == 0)
R00mm = pivot_wider(R00mm, names_from = YR, values_from = n)
write.csv(R00mm, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/R00mm.csv")

## Rnnmm ##
# user defined threshold 
# annual count of days when precipitation is greater than or equal to 1mm (wet day)
R01mm = MACH_data %>% group_by(SITENO, YR) %>% tally(PRCP >= 1)
R01mm = pivot_wider(R01mm, names_from = YR, values_from = n)
write.csv(R01mm, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/R01mm.csv")

## R10mm ##
# annual count of days when precipitation is greater than or equal to 10mm (R10mm)
R10mm = MACH_data %>% group_by(SITENO, YR) %>% tally(PRCP >= 10)
R10mm = pivot_wider(R10mm, names_from = YR, values_from = n)
write.csv(R10mm, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/R10mm.csv")

## R20mm ##
# annual count of days when precipitation is greater than or equal to 20mm (R20mm)
R20mm = MACH_data %>% group_by(SITENO, YR) %>% tally(PRCP >= 20)
R20mm = pivot_wider(R20mm, names_from = YR, values_from = n)
write.csv(R20mm, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/R20mm.csv")

## PRCPTOT ##
## annual total precipitation in wet days (PRCPTOT) when daily precip is greater than or equal to 1mm
PRCPTOT = MACH_data %>% group_by(SITENO, YR) %>% filter(PRCP >= 1)
PRCPTOT = PRCPTOT %>% group_by(SITENO, YR) %>% reframe(PRCP = sum(PRCP))
PRCPTOT = pivot_wider(PRCPTOT, names_from = YR, values_from = PRCP)
write.csv(PRCPTOT, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/PRCPTOT.csv")

## R95pTOT ##
# annual total precipitation when daily precipitation is greater than 95th percentile (R95pTOT)
R95pTOT = MACH_data %>% group_by(SITENO, YR) %>% filter(PRCP >= 1)
R95pTOT = MACH_data %>% group_by(SITENO, YR) %>% filter(PRCP > quantile(PRCP, 0.95))
R95pTOT = R95pTOT %>% group_by(SITENO, YR) %>% reframe(PRCP = sum(PRCP))
R95pTOT = pivot_wider(R95pTOT, names_from = YR, values_from = PRCP)
write.csv(R95pTOT, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/R95pTOT.csv")

## R99pTOT ##
# annual total precipitation when daily precipitation is greater than 99th percentile (R99pTOT)
R99pTOT = MACH_data %>% group_by(SITENO, YR) %>% filter(PRCP >= 1)
R99pTOT = MACH_data %>% group_by(SITENO, YR) %>% filter(PRCP > quantile(PRCP, 0.99))
R99pTOT = R99pTOT %>% group_by(SITENO, YR) %>% reframe(PRCP = sum(PRCP))
R99pTOT = pivot_wider(R99pTOT, names_from = YR, values_from = PRCP)
write.csv(R99pTOT, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/R99pTOT.csv")

## CDD ##
# maximum length of dry spell, max number of consecutive dry days with precip < 1mm (CDD)
longest_dd = function(prcp) {
  result = rle(prcp < 1)  # run length encoding computes lengths and values of runs 
  max(result$lengths[result$values == TRUE], na.rm = TRUE)  
}
CDD = MACH_data %>% group_by(SITENO, YR) %>% summarise(max_streak = longest_dd(PRCP), .groups = "drop")
CDD = pivot_wider(CDD, names_from = YR, values_from = max_streak)
write.csv(CDD, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/CDD.csv")  

## CWD ##
# maximum length of wet spell, max number of consecutive wet days with precip >= 1mm (CWD)
longest_wd = function(prcp) {
  result = rle(prcp >= 1)  # run length encoding computes lengths and values of runs 
  max(result$lengths[result$values == TRUE], na.rm = TRUE)  
}
CWD = MACH_data %>% group_by(SITENO, YR) %>% summarise(max_streak = longest_wd(PRCP), .groups = "drop")
CWD = pivot_wider(CWD, names_from = YR, values_from = max_streak)
write.csv(CWD, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/CWD.csv")  

## SDII ##
# simple precipitation intensity index (SDII)
# sum of precipitation in wet days (>= 1mm) divided by number of wet days in period, mean precip for wet days

# filter days with 1mm or greater precip
wd_prcp = MACH_data %>% filter(PRCP >= 1)
# annual total precip
wd_prcp = wd_prcp %>% group_by(SITENO, YR) %>% reframe(PRCP = sum(PRCP))
SDII = left_join(wd_prcp, R1mm, by = c("SITENO", "YR")) # join tables before wide pivot
SDII = SDII %>% mutate(SDII = PRCP/n)
SDII = SDII %>% dplyr::select(-c(PRCP, n))
SDII = pivot_wider(SDII, names_from = YR, values_from = SDII)
write.csv(SDII, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/SDII.csv")  

#### TEMPERATURE ####

## FD ##
# number of frost days, annual count of days when daily minimum temperature (TMIN) is less than 0C
FD = MACH_data %>% group_by(SITENO, YR) %>% tally(TMIN <0)
FD = pivot_wider(FD, names_from = YR, values_from = n)
write.csv(FD, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/FD.csv")  

## SU ##
# number of summer days, annual count of days when daily maximum temperature (TMAX) is greater than 25C
SU = MACH_data %>% group_by(SITENO, YR) %>% tally(TMAX >25)
SU = pivot_wider(SU, names_from = YR, values_from = n)
write.csv(SU, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/SU.csv")  

## ID ##
# number of icing days, annual count of days when daily maximum temperature (TMAX) is less than 0C
ID = MACH_data %>% group_by(SITENO, YR) %>% tally(TMAX <0)
ID = pivot_wider(ID, names_from = YR, values_from = n)
write.csv(ID, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/ID.csv") 

## TR ##
# number of tropical nights, annual count of days when daily minimum temperature (TMIN) is greater than 20C
TR = MACH_data %>% group_by(SITENO, YR) %>% tally(TMIN >20)
TR = pivot_wider(TR, names_from = YR, values_from = n)
write.csv(TR, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TR.csv")  

## GSL ##
# growing season length, annual (January 1 to December 31) in Northern Hemisphere count between first span 
# of at least 5 days with daily mean temperature greater than 5C and first span of 5 days with 
# daily mean temperature less than 5C
# TGij is daily mean temperature on day i in year j
# count the number of days between the first occurrence of at least 6 consecutive days
# with TGij > 5C and the first occurrence after July 1 of at least 6 consecutive days with TGij < 5C

# mean temperature using min and max temp and get day of year (1-366)
temp = temp %>% mutate(YR = year(DATE), day_of_year = yday(DATE))

# calculate growing season length for a year
calc_gsl = function(temp, doy) {
  rle_above = rle(temp > 5) # start of growing season is first span of 5 consecutive days > 5C
  starts = which(rle_above$values == TRUE & rle_above$lengths >= 5)
  if (length(starts) == 0) return(NA) # if no start of growing season present
  start_index = sum(rle_above$lengths[1:starts[1] - 1]) + 1
  # end of growing season is first span 5 consecutive days < 5C
  rle_below = rle(temp < 5)
  ends = which(rle_below$values == TRUE & rle_below$lengths >= 5)
  if (length(ends) == 0) return(NA) # no end of growing season present
  end_index = sum(rle_below$lengths[1:ends[1] - 1]) + 1
  
  # calculate gsl
  return(start_index - end_index)
}

gsl = temp %>% group_by(SITENO, YR) %>% summarise(gsl = calc_gsl(TAIR, day_of_year), .groups = "drop")
gsl = pivot_wider(gsl, names_from = YR, values_from = gsl)
write.csv(gsl, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/GSL.csv")  

## TXx ##
# monthly maximum value of daily maximum temperature
TXx9120 = MACH_data %>% filter(YR %in% 1991:2020) %>% group_by(SITENO, MNTH) %>% reframe(TXx = max(TMAX))
TXx9120 = pivot_wider(TXx9120, names_from = MNTH, values_from = TXx)
write.csv(TXx9120, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TXx9120.csv")  

## TNx ##
# monthly maximum value of daily minimum temperature
TNx9120 = MACH_data %>% filter(YR %in% 1991:2020) %>% group_by(SITENO, MNTH) %>% reframe(TNx = max(TMIN))
TNx9120 = pivot_wider(TNx9120, names_from = MNTH, values_from = TNx)
write.csv(TNx9120, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TNx9120.csv")  

## TXn ##
# monthly minimum value of daily maximum temperature
TXn8110 = MACH_data %>% filter(YR %in% 1981:2010) %>% group_by(SITENO, MNTH) %>% reframe(TXn = min(TMAX))
TXn8110 = pivot_wider(TXn8110, names_from = MNTH, values_from = TXn)
write.csv(TXn8110, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TXn8110.csv")  

## TNn ##
# monthly minimum value of daily minimum temperature
TNn8110 = MACH_data %>% filter(YR %in% 1981:2010) %>% group_by(SITENO, MNTH) %>% reframe(TNn = min(TMIN))
TNn8110 = pivot_wider(TNn8110, names_from = MNTH, values_from = TNn)
write.csv(TNn8110, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TNn8110.csv")  

## TN10p ##
# percentage of days when TN (TMIN) < 10th percentile
# TNij be daily minimum temperature on day i in period j and let TNin10 be the calendar day 10th percentile
# centered on a 5 day window for the base period 1981-2010, to avoid inhomogeneity calculation for base period
# requires bootstrap procedure per Zhang et al (2005) 
# https://journals.ametsoc.org/view/journals/clim/18/11/jcli3366.1.xml
base_period = MACH_data %>% filter(YR >= 1981 & YR <=2010) %>% 
  dplyr::select(SITENO, DATE, TMIN, TMAX, TMEAN, YR, MNTH, DY) %>% 
  # add day of year to base period (1-365 or 366 for leap years)
  mutate(day_of_year = as.integer(format(DATE, "%j"))) %>% # j is date format code for day of year as zero padded number 
  group_by(SITENO)

# function to calculate the 10th percentile with a 5-day centered window
calculate_10th_percentile = function(day_of_year, temp_data) {
  days = c(day_of_year - 2, day_of_year - 1, day_of_year, day_of_year + 1, day_of_year + 2)
  days = ((days - 1) %% 365) + 1 # wrap around to handle year boundaries
  temp_values = temp_data[temp_data$day_of_year %in% days, "TMIN"]
  quantile(temp_values, 0.1, na.rm = TRUE)
}

# function to calculate bootstrapped 10th percentiles for each SITENO
bootstrap_10th_percent = function(temp_data, n_bootstrap = 100) {
  replicate(n_bootstrap, {
    resampled_data = temp_data[sample(nrow(temp_data), replace = TRUE), ]
    sapply(1:365, calculate_10th_percentile, temp_data = resampled_data)
  }) %>% rowMeans() # average percentiles over all bootstrap samples
}

# calculate 10th percentiles for each SITENO
percentiles_by_site = base_period %>%
  group_split() %>% # split data by SITENO
  lapply(function(group_data) {
    tibble(
      SITENO = unique(group_data$SITENO),
      day_of_year = 1:365,
      percentile_10th = sapply(1:365, calculate_10th_percentile, temp_data = group_data)
    )
  }) %>%  bind_rows() # combine results into a single dataframe

# add day_of_year to the full dataset (1-365, 366 for leap years)
data = MACH_data %>% dplyr::select(SITENO, DATE, YR, MNTH, DY, TMIN, TMAX, TMEAN) %>% 
  mutate(day_of_year = as.integer(format(DATE, "%j")))

# match each day's temperature with its percentile threshold
data_with_thresholds = data %>%
  left_join(percentiles_by_site, by = c("SITENO", "day_of_year")) %>%
  mutate(below_10th = TMIN < percentile_10th)

# calculate TN10p for each SITENO (percent of days from 1980 to 2023 where TMIN is below 10th percentile)
tn10p_by_site = data_with_thresholds %>% group_by(SITENO) %>%
  summarize(TN10p = sum(below_10th, na.rm = TRUE) / n() * 100, .groups = "drop")

write.csv(tn10p_by_site, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TN10p.csv")  

## TX10p ##
# percentage of days when TX (TMAX) <10th percentile, daily maximum temperature 
# use above code, change TMIN to TMAX
# calculate TN10p for each SITENO (percent of days from 1980 to 2023 where TMIN is below 10th percentile)
tx10p_by_site = data_with_thresholds %>% group_by(SITENO) %>%
  summarize(TX10p = sum(below_10th, na.rm = TRUE) / n() * 100, .groups = "drop")
write.csv(tx10p_by_site, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TX10p.csv")  

## TN90p ##
# percentage of days when TN (TMIN) >90th percentile
# function to calculate the 90th percentile with a 5-day centered window
calculate_90th_percentile = function(day_of_year, temp_data) {
  days = c(day_of_year - 2, day_of_year - 1, day_of_year, day_of_year + 1, day_of_year + 2)
  days = ((days - 1) %% 365) + 1 # wrap around to handle year boundaries
  temp_values = temp_data[temp_data$day_of_year %in% days, "TMIN"]  # or TMAX
  quantile(temp_values, 0.9, na.rm = TRUE)
}

# function to calculate bootstrapped 90th percentiles for each SITENO
bootstrap_90th_percent = function(temp_data, n_bootstrap = 100) {
  replicate(n_bootstrap, {
    resampled_data = temp_data[sample(nrow(temp_data), replace = TRUE), ]
    sapply(1:365, calculate_90th_percentile, temp_data = resampled_data)
  }) %>% rowMeans() # average percentiles over all bootstrap samples
}

# calculate 90th percentiles for each SITENO
percentiles_by_site = base_period %>%
  group_split() %>% # split data by SITENO
  lapply(function(group_data) {
    tibble(
      SITENO = unique(group_data$SITENO),
      day_of_year = 1:365,
      percentile_90th = sapply(1:365, calculate_90th_percentile, temp_data = group_data)
    )
  }) %>%  bind_rows() # combine results into a single dataframe

# match each day's temperature with its percentile threshold
data_with_thresholds = data %>%
  left_join(percentiles_by_site, by = c("SITENO", "day_of_year")) %>%
  mutate(above_90th = TMIN > percentile_90th)  # or TMAX

# calculate TN90p for each SITENO (percent of days from 1980 to 2023 where TMIN is above 90th percentile)
tn90p_by_site = data_with_thresholds %>% group_by(SITENO) %>%
  summarize(TN90p = sum(above_90th, na.rm = TRUE) / n() * 100, .groups = "drop")

write.csv(tn90p_by_site, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TN90p.csv") 

## TX90p ##
# percentage of days when TX (TMAX) is above 90th percentile
# use above code, replace TMIN with TMAX
# calculate TN90p for each SITENO (percent of days from 1980 to 2023 where TMAX is above 90th percentile)
tx90p_by_site = data_with_thresholds %>% group_by(SITENO) %>%
  summarize(TX90p = sum(above_90th, na.rm = TRUE) / n() * 100, .groups = "drop")

write.csv(tx90p_by_site, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/TX90p.csv") 

## WSDI ##
# warm spell duration index, annual count of days with at least 6 consecutive days when daily maximum 
# temperature TX (TMAX) is above the 90th percentile (>90p)
# US National Centers for Environmental Information (NCEI) new WMO climatological standard normals (CLINO)
baseline = MACH_data %>% filter(YR >=1981 & YR <=2010) %>% 
  dplyr::select(SITENO, DATE, YR, MNTH, DY, TMIN, TMAX, TMEAN)

# determine the 90th percentile of daily maximum temperature over a 30 yr baseline of 1981 to 2010
# find threshold based on baseline above 90th percentile
threshold = quantile(baseline$TMAX, probs = 0.9, na.rm = TRUE)

# function to determine number of consecutive days with TMAX above threshold
calc_wsdi = function(tmax, threshold) {
  rle_result = rle(tmax > threshold)
  warm_spells = rle_result$lengths[rle_result$values == TRUE]
  # filter warm spells of 6 or more consecutive days 
  sum(warm_spells[warm_spells >= 6]) # sum duration of all qualifying spells within a year
}

# select temperature data
temp = MACH_data %>% dplyr::select(c(SITENO, DATE, TMIN, TMAX, YR))
# logical flag for TMAX if greater than threshold value 
temp = temp %>% mutate(warm = TMAX > threshold)

wsdi = temp %>% group_by(SITENO, YR) %>% summarise(wsdi = calc_wsdi(TMAX, threshold), .groups = "drop")
wsdi = pivot_wider(wsdi, names_from = YR, values_from = wsdi)
write.csv(wsdi, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/WSDI.csv") 

## CSDI ##
# cold spell duration index, annual count of days with at least 6 consecutive days when the daily
# minimum T falls below the 10th percentile 
threshold = quantile(baseline$TMIN, probs = 0.1, na.rm = TRUE)

# determine number of consecutive days with TMIN below threshold
calc_csdi = function(tmin, threshold) {
  rle_result = rle(tmin < threshold)
  cold_spells = rle_result$lengths[rle_result$values == TRUE]
  # filter cold spells of 6 or more consecutive days 
  sum(cold_spells[cold_spells >= 6]) # sum duration of all qualifying spells within a year
}

# select temperature data
temp = MACH_data %>% dplyr::select(SITENO, DATE, TMIN, TMAX, YR)
# logical flag for TMIN if less than threshold value 
temp = temp %>% mutate(cold = TMIN < threshold)

csdi = temp %>% group_by(SITENO, YR) %>% summarise(csdi = calc_csdi(TMIN, threshold), .groups = "drop")
csdi = pivot_wider(csdi, names_from = YR, values_from = csdi)
write.csv(csdi, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/CSDI.csv")  

## DTR ##
# daily temperature range, monthly mean difference between TX (TMAX) and TN (TMIN)
# determine daily difference, then find monthly mean for each year, then overall for each month
temp = MACH_data %>% dplyr::select(SITENO, DATE, TMIN, TMAX, YR, MNTH) %>% 
  mutate(DTR = TMAX - TMIN)
temp_mnth = temp %>% group_by(SITENO, MNTH) %>% reframe(DTR = mean(DTR))
temp_mnth = pivot_wider(temp_mnth, names_from = MNTH, values_from = DTR)
write.csv(temp_mnth, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/DTR.csv") 

### Combine into single csv ###
# indices with value for each year

files = list.files("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Annual", 
                   pattern = "\\.csv$", full.names = TRUE)

read_format = function(file) {
  df = read_csv(file) %>% 
    pivot_longer(-SITENO, names_to = "MNTH", values_to = "Value") %>% 
    mutate(MNTH = as.numeric(MNTH)) 
  file_name = tools::file_path_sans_ext(basename(file))
  df[[file_name]] = df$Value
  df = df %>% select(-Value)
}

df_list = lapply(files, read_format)
final_df = reduce(df_list, full_join, by = c("SITENO", "MNTH"))
write.csv(final_df, "D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/month_climate.csv")

### Combine into single csv ###
# indices with value for each month
# repeat above with MNTH instead of YR
files = list.files("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/Monthly", 
                   pattern = "\\.csv$", full.names = TRUE)

#######################################################################
 ## PDSI ##
#######################################################################
library(pdsi)


#######################################################################
 ## NOAA INDICES ##
#######################################################################
# GISS Surface temperature analysis version 4 (GISTEMPv4) estimate of global surface temp change
# combined land surface air and sea surface water temp anomalies (LOTI) 
# https://data.giss.nasa.gov/gistemp/

loti = read_csv("D:/University of Texas at Dallas/Climate/LOTI.csv")

# us temperature annual mean change relative to 1951-1980 mean
ustemp = read_csv("D:/University of Texas at Dallas/Climate/Ustemp.csv")

ustemp = ustemp %>% filter(Year >= 1980 & Year <= 2023)
ggplot(data = ustemp, aes(x = Year, y = Annual_Mean)) + geom_line() +
  theme_bw() + labs(y = "Temperature Anomaly relative to 1951-1980", title = "US Surface Air Temperature", 
  caption = "Source: NASA/GISS/GISTEMP v4") + theme(axis.title.x = element_blank())

#######################################################################
 ## CLIMATE DATA TRENDS ##
#######################################################################
annual = mach_data %>% mutate(WYR = wYear(DATE), YR = year(DATE))
annual = annual %>% select(-c("DAYL", "SRAD", "SWE", "VP"))
year = annual %>% group_by(SITENO, WYR) %>% reframe(AET = sum(AET), PET = sum(PET), 
          OBSQ = sum(OBSQ), PRCP = sum(PRCP), TAIR = mean(TAIR), TMIN = mean(TMIN), TMAX = mean(TMAX)) 

year_avg = year %>% group_by(WYR) %>% reframe(AET = mean(AET), PET = mean(PET), OBSQ = mean(OBSQ), 
              PRCP = mean(PRCP), TAIR = mean(TAIR)) %>% filter(WYR != 2024)

year_avg = year_avg %>% pivot_longer(cols = c(PRCP, TAIR), names_to = "Variable", 
                                     values_to = "Values") 

prcp = ggplot(data = year_avg, aes(x = WYR, y = PRCP)) + geom_line() +
  geom_smooth(method = lm, se=FALSE) + theme_bw() + labs(title = "Total Annual Precipitation (mm/yr)") +
  theme(axis.title = element_blank())

tair = ggplot(data = year_avg, aes(x = WYR, y = TAIR)) + geom_line() +
  geom_smooth(method = lm, se=FALSE) + theme_bw() + labs(title = "Average Annual Temperature (\u00b0C)") +
  theme(axis.title = element_blank())

trend = prcp + tair + plot_layout(nrow = 1) + plot_annotation(tag_levels = list(c("a)", "b)")))
ggsave("D:/University of Texas at Dallas/Dissertation/trends.png", 
       plot = trend, device = "png", dpi = 300)

#######################################################################
 ## RUNOFF EFFICIENCY ##
#######################################################################
annual = mach_data %>% select(SITENO, DATE, OBSQ, PRCP)
annual = annual %>% mutate(WYR = compute_water_year(DATE))

# get annual totals of precip and runoff
annual = annual %>% group_by(SITENO, WYR) %>% reframe(PRCP = sum(PRCP), OBSQ = sum(OBSQ), QvP = OBSQ/PRCP) 
annual = annual %>% drop_na()
overall = annual %>% group_by(SITENO) %>% reframe(QvP = mean(QvP))

site_info = read_csv("D:/University of Texas at Dallas/CombinedDataset/coordinates.csv")
overall = overall %>% mutate(QvP = if_else(QvP > 1, 1, QvP)) %>% mutate(across(where(is.numeric),~round(.x,2)))
# us state boundaries
us = map_data("state")

overall = overall %>% left_join(site_info, by = "SITENO")

  qvp =  ggplot(QvP, aes(x = longitude, y = latitude, color = QvP)) +
    geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +  
    geom_point(size = 2) + scale_color_viridis(option = "viridis") +
    theme_minimal() + coord_fixed(1.3) + labs(title = "Runoff Efficiency") +
      theme(axis.title = element_blank())
  
  ggsave(filename = "D:/University of Texas at Dallas/Dissertation/QvP.png", 
       plot = qvp, device = "png", width = 8, height = 6, units = "in", dpi = 300)
#######################################################################
 ## POTENTIAL EVAPOTRANSPIRATION ##
#######################################################################
## PRIESTLY TAYLOR ##

# library(FAO56)
# https://www.fao.org/4/x0490E/x0490e00.htm
# calculate Priestly-Taylor PET
# PET = alpha/lambda*(Rn-G)*(delta/delta+gamma)
# alpha is dryness coefficient, use 1.26
# lambda is latent heat of vaporization, 2.45 MJ/kg (Allen, 1998)
# Rn is daily total incoming net radiation (MJ/m2/day), obtained using daylength and solar radiation 
# G is soil heat flux (MJ/m2/day) assumed to be zero
# delta is slope of saturation vapor pressure curve (kPa/°C)
# gamma is psychrometric constant (kPa/°C)

# shortwave radiation (srad) and day length (dayl) obtained from Daymet V2 data
# s is (4098*[0.6108exp((17.27*T)/(T + 237.3))])/((T + 237.3)^2) where T is daily mean air temp (°C)
# gamma is 0.000665*P (see FAO Penman-Monteith reference, chapter 3, equation 8) where P is atmospheric pressure (kPa)
# P is 101.3*exp((-0.00342*z)/(T + 273.15)) and z is elevation in meters

# path to data folder
data_folder = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/MACH/test"
  
# output folder 
output = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/MACH/PT_PET"

# list files in data folders 
data_files = list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# get attributes
attributes = read_csv("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/site_info.csv")
# get altitude information from each gauge as z in meters 
elev_data = select(attributes, c("SITENO", "alt_meters"))
# get latitude data and convert from decimal degrees to radians
lat_data = select(attributes, c("SITENO", "dec_lat_va")) %>% mutate(lat_rad = dec_lat_va * pi / 180)

# create named vector with numeric elevation values 
elev_site = setNames(elev_data$alt_meters, elev_data$SITENO)
# create named vector with radian latitude
lat_site = setNames(lat_data$lat_rad, lat_data$SITENO)

# loop to calculate PET 
for (data_file in data_files) {
  
  # extract file name and site number 
  file_name = basename(data_file)
  site_no = str_extract(file_name, "\\d{8}")  # extracts 8-digit number
  
  # read in data 
  all_data = read_csv(data_file)
  
  # get day of year from date
  all_data$DOY = yday(all_data$DATE)

  # convert units 
  # shortwave radiation flux density in w/m2 to daily total radiation in MJ/m2/day by dividing by 1e6 
    srad = all_data$SRAD * all_data$DAYL / 1000000
  # vapor pressure in Pa to kPa
    vp = all_data$VP / 1000
  # min, max temp from celcius to kelvin for Rnl calculation 
    tmin_k = all_data$TMIN + 273.15
    tmax_k = all_data$TMAX + 273.15
  
  # get temp data in celcius
    tmean = all_data$TAIR
    tmin = all_data$TMIN
    tmax = all_data$TMAX
    
  # saturation vapor pressure (es, kPa) with temp in celcius
  # tetens formula for temps above and below 0 celcius
  if (tair > 0) {
    es = 0.61078 * exp((17.27 * tair) / (tair + 237.3)) 
  } else {
    es = 0.61078 * exp((21.875 * tair) / (tair + 265.5))
  }

    
  # slope of vapor pressure curve (delta, kPa/C)
  delta = (4098 * es) / ((tmean + 237.3)^2)
  
  # atmospheric pressure (P, kPa) with temp in celcius and elevation in meters  
  z = elev_site[site_no] # retrieve elevation value for site 
  P = 101.3 * exp((-0.00342 * z) / (tmean + 273.15))
  
  # psychrometric constant (gamma, kPa/C) using atmospheric pressure 
  gamma = 0.000665 * P
  
  # calculate extraterrestrial radiation (Ra, MJ/m2/day)
  doy = all_data$DOY
  lat_rad = lat_site[site_no]
  # inverse relative distance Earth-sun (closer in Jan ~1.033, further in Jul ~0.967)
  dr = 1 + 0.033 * cos(2 * pi * doy / 365) 
  # solar declination angle (radians), angle between sun's rays and plane of equator, ranges from -0.409 (winter) to 0.409 (summer solstice)
  delta_solar = 0.409 * sin(2 * pi * doy / 365 - 1.39) 
  # sunset hour angle (radians), angular time between solar noon and sunset, ranges from 0 (no daylight) to pi (24 hours of light)
  ws = acos(-tan(lat_rad) * tan(delta_solar)) 
  Ra = (24 * 60 / pi) * 0.0820 * dr * (ws * sin(lat_rad) * sin(delta_solar) + cos(lat_rad) * cos(delta_solar) * sin(ws))
  
  # clear sky solar radiation (Rso, MJ/m2/day)
  Rso = (0.75 + 2e-5 * z) * Ra
  
  # net shortwave radiation (Rns)
  albedo = 0.23
  Rns = (1 - albedo) * srad
  
  # net longwave radiation (Rnl)
  sigma = 4.903e-9 # Stefan-Boltzmann constant (MJ/K4/m2/day)
  Rnl = sigma * ((tmax_k^4 + tmin_k^4) / 2) * (0.34 - 0.14 * sqrt(vp)) * (1.35 * (srad / Rso) - 0.35)
  
  # net radiation (Rn)
  Rn = Rns - Rnl

  # Yang and Roderick (2019) Radiation, surface temperature and evaporation over wet surfaces formulation of PT
  # LE = delta /(delta + 0.24 * gamma) * Rn - G, delta and gamma in Pa/K and Rn in W/m2, LE in W/m2
  # G = 0 # ground heat flux assumed to be zero daily
  # convert lamba and gamma to Pa/K from kPa/C
  delta_pa = delta * 1000
  gamma_pa = gamma * 1000
  # convert Rn from MJ/m2/day to W/m2
  rn_wm2 = Rn * 1e6 / 86400 # MJ to joules and days to seconds (J/m2/s = W/m2)
  
  # compute PET in W/m2
  pet_wm2 = (delta_pa / (delta_pa + (0.24 * gamma_pa))) * rn_wm2
  
  # convert PET from W/m2 to mm/day
  # latent heat of vaporization of water (lambda, MJ/kg)
  lambda = 2.45
  PET = (pet_wm2 * 86400) / (lambda * 1e6) 
  
  output_df = tibble(DATE = all_data$DATE, PET_PT = PET)
  write_csv(output_df, file.path(output, paste0("basin_", site_no, "_PET_PT.csv")))

}

## THORNTHWAITE ##
# PET = 16(L/12)*(N/30)*(10Td/I)^alpha, results in mm/month
# L is average day length in hours of month
# N is number of days in month
# Td is average daily temperature in Celcius for given month
# I is annual heat index (sum of monthly heat indices, calculated with (Tmean/5)^1.514)
# alpha is constant using annual heat index

# folder for temperature data, list files, same as above
# daylight hours
# folder for day length
dayl_folder = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/DAYL_SRAD"

# output folder
output_thorn = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/TH_PET"

for (temp_file in temp_files) {
  
   # extract file name and site number 
  file_name = basename(temp_file)
  site_no = str_extract(file_name, "\\d{8}")  # Extracts only the 8-digit number
  
  # get path to corresponding daylength file for same site as temperature
  dayl_file = file.path(dayl_folder, paste0("basin_", site_no, "_dayl_srad.csv")) 
  
  # read temperature and day length
  temp_data = read_csv(temp_file)
  # if negative temperature, replace with 0 
  temp_data$TAIR = replace(temp_data$TAIR, temp_data$TAIR < 0, 0)
  
  dayl_data = read_csv(dayl_file)
  dayl_data = select(dayl_data, c("SITENO", "DATE", "DAYL"))
  
  # join temperature and daylength data
  combined_data = left_join(temp_data, dayl_data, by = c("SITENO", "DATE"))
  
  # convert day length in seconds to hours
  combined_data = combined_data %>% mutate(DAYL_HRS = DAYL/3600)
  
  # extract month and calculate number of days in current month
  combined_data = combined_data %>% mutate(YR = year(DATE), MNTH = month(DATE), DY_MNTH = days_in_month(DATE))
  
   # get monthly average temperature 
  monthly_temp = combined_data %>% group_by(YR, MNTH) %>% reframe(TAIR = mean(TAIR))
 
  # calculate monthly heat index
  monthly_temp = monthly_temp %>% mutate(index = (TAIR/5)^1.514)
  # calculate annual heat index
  annual_I = monthly_temp %>% group_by(YR) %>% reframe(I = sum(index))
  
  # calculate alpha exponent
  annual_I = annual_I %>% mutate(a = 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239)
  
  # calculate monthly, then daily PET
  combined_data = left_join(combined_data, annual_I, by = "YR") %>% 
   mutate(L = DAYL_HRS/12, N = DY_MNTH/30, PT = 16*L*N*((10*TAIR / I)^a), 
          PET = PT/DY_MNTH)
    
  # save results to output folder
  output_file = file.path(output_thorn, paste0("basin_", site_no, "_TH_PET.csv"))
  write.csv(combined_data, output_file)
  
}

day_test = read_csv("D:/University of Texas at Dallas/CombinedDataset/Formatted_data/DAYL_SRAD/basin_01013500_dayl_srad.csv")
temp_test = read_csv("D:/University of Texas at Dallas/CombinedDataset/Formatted_data/TAIR_M/basin_01013500_tair.csv")
combined_data = left_join(temp_test, day_test, by = c("SITENO", "DATE"))
