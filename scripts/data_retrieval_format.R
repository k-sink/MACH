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

# obtain hydrologic data from USGS NWIS
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


