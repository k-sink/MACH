# MACH dataset 
# Katharine Sink

# load libraries
library(tidyverse)
library(sf)
library(terra)
library(dataRetrieval)
library(nhdplusTools)
library(FedData)
library(ncdf4)
library(here)

########################################################################
## USGS DISCHARGE DATA RETRIEVAL ##
########################################################################

# obtain hydrologic data from USGS NWIS using gage number 
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html

# get list of gauges as csv file (single column)
# relative path based on project root folder 
gages = read.csv(here("data", "final_gages.csv"))

# imports as integer, convert to character
gages = as.character(gages$SITENO)

# add leading zero to gauges with 7 characters to make it an 8 digit value
gages = str_pad(gages, width = 8, side = "left", pad = "0")

# obtain USGS NWIS site metadata (i.e. station name, drainage area, latitude, longitude)
siteinfo = readNWISsite(gages)

# get available data for daily values (service), discharge (parameterCd), mean value (statCd), date range
available = whatNWISdata(siteNumber = gages, service = "dv", parameterCd = "00060", statCd = "00003")

# get individual discharge files (cfs) for gages, save as csv
# create output directory 
out_dir = here("data", "discharge_cfs")

  if(!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE) 
  }

# set the start and end date
startDate = as.Date("1980-01-01")
endDate = as.Date("2023-12-31")

# loop through each gage 
for (i in 1:length(gages)){

  # mean daily discharge parameter and stat codes
  siteID = gages[i] 
  discharge = readNWISdata(sites = siteID, service = "dv", parameterCd = c("00060"), 
                         statCd = "00003", startDate = startDate, endDate = endDate)

    if (nrow(discharge) > 0){  # make sure data is available

      out_file = file.path(out_dir, paste(siteID, ".csv"))
      
  # save each gage as separate file (columns will include agency, site_no, dateTime, mean value, qualifier, time zone)
  write.csv(discharge, out_file, row.names = FALSE)
}
}

########################################################################
# get gage status (inactive or active)

info = readNWISsite(gages$SITENO)

avail = whatNWISdata(siteNumber = gages$SITENO)

status = avail %>%
  mutate(
    active = data_type_cd %in% c("dv", "uv")
  ) %>%
  group_by(site_no) %>%
  summarize(active = any(active)) %>%
  left_join(readNWISsite(gages), by = "site_no") %>%
  dplyr::select(site_no, station_nm, site_tp_cd, active)

tally = sum(status$active)

#######################################################################
 ## DISCHARGE CFS TO MM ##
#######################################################################
# discharge values in ft3/sec in csv file, convert to mm/day using area of each basin

# get metadata for all sites
attribute_csv = read.csv(here("data", "site_info.csv"), colClasses = c(SITENO = "character")) %>% 
  mutate(SITENO = str_pad(SITENO, width = 8, side = "left", pad = "0"))

discharge_path = here("data", "discharge_cfs")

# create output directory for runoff depth files
out_folder = here("data", "discharge_mm")

  if(!dir.exists(out_folder)) {
    dir.create(out_folder, recursive = TRUE) 
  }

streamflow_files = list.files(path = discharge_path, pattern = "\\.csv$", full.names = TRUE)

# loop through each cfs streamflow file
for (streamflow_file in streamflow_files) {
  
  # Read the streamflow data
  streamflow_data <- read_csv(streamflow_file, col_types = cols(
    agency_cd = col_character(), 
    site_no = col_character(),  # ensure site_no is read as a character
    dateTime = col_datetime(), 
    X_00060_00003 = col_double(),
    X_00060_00003_cd = col_character(),
    tz_cd = col_character()
    ))

  # extract the SITENO from the streamflow data
  gauge_id = unique(streamflow_data$site_no)
  
  # ensure there's only one unique SITENO
  if (length(gauge_id) != 1) {
    stop(paste("Multiple or no SITENO found in file:", streamflow_file))
  }
  
  # find the corresponding area value from the attributes data frame
  area_value = attribute_csv %>% dplyr::filter(SITENO == gauge_id) %>% dplyr::select(area_sqkm) %>% pull()
  
  # ensure the area value was found
  if (length(area_value) != 1) {
    stop(paste("Area value not found for SITENO:", gauge_id))
  }
  
  # area in sq km, add to streamflow data
  streamflow_data$Area_sqkm = area_value
  
  # add area in sq mm to streamflow data
  streamflow_data$Area_sqmm = area_value*(1000000 * 1000000)
  
  # convert ft3/sec to mm3/sec to mm/day
  streamflow_data$OBSQ = ((streamflow_data$X_00060_00003*(304.8^3))/streamflow_data$Area_sqmm)*86400
  
  # create output file path
  output_file = file.path(out_folder, basename(streamflow_file))
  
  # Save the updated streamflow data
  write_csv(streamflow_data, output_file)
  
  # Print message indicating completion for the current streamflow file
  cat("Processed streamflow file:", streamflow_file, "\n")
}

########################################################################
# reformat discharge data files 
discharge_mm_path = here("data", "discharge_mm")

# create output directory for runoff depth files
obsq_files = here("data", "MACH/OBSQ")

  if(!dir.exists(obsq_files)) {
    dir.create(obsq_files, recursive = TRUE) 
  }

streamflow_files = list.files(path = discharge_mm_path, pattern = "\\.csv$", full.names = TRUE)

# loop through each streamflow file
for (streamflow_file in streamflow_files) {
  
  # Read the streamflow data
  streamflow_data = read_csv(streamflow_file, col_types = cols(
    agency_cd = col_character(), 
    site_no = col_character(),  # ensure site_no is read as a character
    dateTime = col_datetime(), 
    X_00060_00003 = col_double(),
    X_00060_00003_cd = col_character(),
    tz_cd = col_character(), 
    Area_sqkm = col_double(), 
    Area_sqmm = col_character(), 
    OBSQ = col_character()))
  
  streamflow_data = streamflow_data %>% dplyr::select(site_no, dateTime, OBSQ) %>% 
    rename(SITENO = site_no, DATE = dateTime) %>% mutate(OBSQ = parse_number(OBSQ))

  # extract the GaugeID (SITENO) from the streamflow data
  gauge_id = unique(streamflow_data$SITENO)
   
  # define new file name
  new_file_name = paste0("basin_", gauge_id, "_obsq.csv")
  new_file_path = file.path(obsq_files, new_file_name)
  
  # write the modified data to a new CSV file
   write.csv(streamflow_data, new_file_path, row.names = FALSE)
  
  # print message indicating completion for the current streamflow file
  cat("Processed streamflow file:", streamflow_file, "\n")
}
  
########################################################################
# discharge files are not complete for all years and/or basins
# create a complete time series with NA values for missing dates

# function to create the full date sequence for each file
create_complete_dates = function() {
  # Create a complete sequence of dates from 01/01/1980 to 12/31/2023 (16071 days)
  seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "day")
}

# function to process each CSV file
process_csv_file = function(file_path) {
  # create the full date sequence
  complete_dates = create_complete_dates()
  
  # Read the CSV file
  df = read_csv(file_path, col_types = list(col_character(), col_date(), col_double()))
  
  # ensure the 'DATE' column is in Date format
  df$DATE= as.Date(df$DATE, format = "%m/%d/%Y")
  
  # merge the CSV file data with the complete date sequence
  # use full_join to preserve all the dates, even those missing in the CSV file
  df_complete = full_join(data.frame(DATE = complete_dates), df, by = "DATE")
  
  # fill missing OBSQ values with NaN
  df_complete$OBSQ[is.na(df_complete$OBSQ)] = NaN
  
  # make sure the SITENO column is correctly populated (use the SITENO from the first row)
  # all rows will have the same SITENO, so just fill it in with the correct value
  df_complete$SITENO = str_extract(file_path, "(?<=basin_)(\\d+)(?=_obsq.csv)")
  
 # order the columns 
  df_complete = df_complete %>% dplyr::select(SITENO, DATE, OBSQ)
  
   # return the processed data frame in desired order
  return(df_complete)
}

# directory where the CSV files are stored
input_dir = here("data", "MACH", "OBSQ")

# get the list of all CSV files in the directory
csv_files = list.files(input_dir, pattern = "basin_\\d{8}_obsq.csv", full.names = TRUE)

# loop through each file, process it, and save the updated file
for (file_path in csv_files) {
  # process the CSV file
  df_processed = process_csv_file(file_path)
  
  # write the updated data frame back to a CSV file (overwriting the original)
  write.csv(df_processed, file_path, row.names = FALSE)
}


########################################################################
## NHD BASIN DELINEATIONS ##
########################################################################
# get basin delineations consistent with NWIS 
# https://doi-usgs.github.io/nhdplusTools/

# add prefix to site numbers for nldi query
nwis_site_ids = paste0("USGS_", gages)

# create base directory to save shapefiles
base_dir = here("data", "nldi_basins")

if (!dir.exists(base_dir)) {
  dir.create(base_dir, recursive = TRUE)
}

# loop through each NWIS site ID
for (site_id in nwis_site_ids) {
  
  # create the NLDI query for the NWIS site
  nldi_query = list(featureSource = "nwissite", featureID = site_id)

  # get NLDI basin for the NWIS site as sf dataframe, CRS is WGS 84
  basin = get_nldi_basin(nldi_feature = nldi_query)

  # create the file path for saving the shapefile
  shapefile_path = paste0(base_dir, "basin_", site_id, ".shp")
  
  # save the basin boundary as a shapefile
  st_write(basin, shapefile_path, append = FALSE)
  
  # print a message indicating the processing is complete for the current site
  cat("Processed NWIS site ID:", site_id, "\n")
}


