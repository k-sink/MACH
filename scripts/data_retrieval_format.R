# MACH dataset 
# Katharine Sink

# filepaths are relative 

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

info = readNWISsite(gages)

avail = whatNWISdata(siteNumber = gages)

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

# path to cfs data 
discharge_path = here("data", "discharge_cfs")

# create output directory for runoff depth files
out_folder = here("data", "discharge_mm")

  if(!dir.exists(out_folder)) {
    dir.create(out_folder, recursive = TRUE) 
  }

# list all cfs files 
streamflow_files = list.files(path = discharge_path, pattern = "\\.csv$", full.names = TRUE)

# loop through each cfs streamflow file
for (streamflow_file in streamflow_files) {
  
  # Read the streamflow data
  streamflow_data = read_csv(streamflow_file, col_types = cols(
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
obsq_folder = here("data", "MACH", "OBSQ")

  if(!dir.exists(obsq_folder)) {
    dir.create(obsq_folder, recursive = TRUE) 
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
  new_file_path = file.path(obsq_folder, new_file_name)
  
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

########################################################################################
# date range using dataRetrieval shows as complete for 1/1/1980 - 12/31/2023 but does not identify gaps in between
# count number of rows in discharge files (rows = days)
# correctly list the files with the full path

csv_files = list.files(here("data", "discharge_cfs"), full.names = TRUE)

# create empty dataframe for number of columns, rows per gauge
csv_details = data.frame(cols = integer(), rows = integer(), name = character(), stringsAsFactors = FALSE)

# create function to count number of columns and rows (days) per discharge file
Qdays = function(file_path) {
  
  csv_read_file = data.table::fread(file_path)
  csv_read_file = csv_read_file %>% drop_na()
  
  number_of_cols = ncol(csv_read_file)
  number_of_rows = nrow(csv_read_file)
  
  tibble(
    cols = number_of_cols,
    rows = number_of_rows,
    name = str_remove_all(basename(file_path), ".csv"))
}

# map function to each discharge file 
csv_details = map_df(csv_files, Qdays)

########################################################################
## NHD BASIN DELINEATIONS ##
########################################################################
# get basin delineations consistent with NWIS 
# queries the nldi using site number 
# https://doi-usgs.github.io/nhdplusTools/

# add prefix to site numbers for nldi query
nwis_site_ids = paste0("USGS-", gages)

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
  shapefile_path = file.path(base_dir, paste0("basin_", site_id, ".shp"))
  
  # save the basin boundary as a shapefile (.dbf, .prj, .shp, .shx files for each basin)
  # WGS 1984 
  sf::st_write(basin, shapefile_path, append = FALSE)
  
  # print a message indicating the processing is complete for the current site
  cat("Processed NWIS site ID:", site_id, "\n")
}


########################################################################
## DAYMET DATA RETRIEVAL ##
########################################################################
# download Daymet using FedData package 
# https://cran.r-project.org/web/packages/FedData/index.html
# daymet data consists of "tiles" which are 1kmx1km raster cells 

# use for one variable at a time due to large volume
# dayl (during of daylight period in seconds per day), 
# prcp (daily total precip in mm per day, converted to water equivalent), 
# srad (incident shortwave radiation flux density in watts per square meter as average over daylight period of day)
# swe (snow water equivalent in kg per square meter, amount contained in snowpack)
# tmax (daily max 2-meter air temp in celcius), tmin
# vp (water vapor pressure in pascals, daily average partial pressure of water vapor)

# folder containing shapefiles 
shp_path = here("data", "nldi_basins")

# output folder
# change for each different variable
prcp_folder = here("data", "MACH", "PRCP")

  if(!dir.exists(prcp_folder)) {
    dir.create(prcp_folder, recursive = TRUE) 
  }


# list shapefiles
shapefiles = list.files(
  path = shp_path,
  pattern = "\\.shp$",
  full.names = TRUE
)


# loop through shapefiles

for (shapefile in shapefiles) {
  
  # read shapefile (native WGS84)
  basin_shp = sf::st_read(shapefile, quiet = TRUE)

  # download Daymet (SpatRaster)
  # change element (dayl, prcp, srad, swe, tmax, tmin, vp)
  day_rast = FedData::get_daymet(
    template = basin_shp,
    label = "PRCP",
    elements = "prcp",
    years = 1980:2023,
    tempo = "day"
  )
  
 day_rast = terra::rast(day_rast)
  
 basin_vect = terra::vect(basin_shp)
  
   # area-weighted extraction
  extracted = terra::extract(day_rast, basin_vect, weights = TRUE)
  
  weights_vec = extracted$weight
  
  value_matrix = extracted[, !(names(extracted) %in% c("ID", "weight"))]
  
  weighted_means = colSums(value_matrix * weights_vec, na.rm = TRUE) /
                    colSums(weights_vec, na.rm = TRUE)
  
  # build date sequence
  start_date = as.Date("1980-01-01")
  dates = seq(start_date, by = "day", length.out = length(weighted_means))
  
  prcp = data.frame(
    DATE = dates,
    PRCP = round(weighted_means, 2)
  )
  
  # write CSV (relative path)
  shapefile_id = tools::file_path_sans_ext(basename(shapefile))
  csv_filename = file.path(prcp_folder, paste0(shapefile_id, "_Daymet_prcp.csv"))
  write.csv(prcp, file = csv_filename, row.names = FALSE)
  
  cat("Processed shapefile:", shapefile_id, "\n")
  
  rm(basin_shp, basin_vect, day_rast, extracted,
     value_matrix, weights_vec, weighted_means, prcp)
  gc()
}

########################################################################
## DAYMET DATA LEAP YEAR ##
########################################################################
# https://daymet.ornl.gov/overview (under calendar section)
# The Daymet calendar is based on a standard calendar year. 
# All Daymet years have 1 - 365 days, including leap years. For leap years, 
# the Daymet database includes leap day. Values for December 31 are discarded 
# from leap years to maintain a 365-day year.

# values exist for 2/29 but omit 12/31 values. leap year days need to be added 
# in as 12/31 and value interpolated for missing day

# repeat this for each variable in daymet 

# function to add leap year rows
add_leap_year_rows = function(df) {
  
  # format the date column 
  df$DATE = as_date(df$DATE, format = "%m/%d/%Y")
  
  # extract the years from the date column
  years = unique(year(df$DATE))
  
  # identify leap years with lubridate function
  leap_years = years[leap_year(years)]
  
  # create new rows for each leap year on December 31 with variable value of NA (placeholder)
  new_rows = data.frame(
    DATE = as.Date(paste0(leap_years, "-12-31")),
    PRCP = NA
  )
  
  # ensure the new rows are only added if they do not already exist in the dataframe
  new_rows = new_rows[!new_rows$DATE %in% df$DATE, ]
  
  # bind the new rows to the original dataframe
  df = bind_rows(df, new_rows)
  
  # sort the dataframe by date
  df = df %>% arrange(DATE)
  
  return(df)
}

# directory containing the CSV files
filepath = here("data", "MACH", "PRCP")

# create new folder, filepath for leap year files 
output_filepath = here("data", "MACH", "PRCP_LY")

# Get the list of all CSV files in the directory
allfiles = dir_ls(filepath, regexp = "\\_Daymet_prcp.csv$")

# process each file
for (file in allfiles) {
  # read the csv file
  datatable = read_csv(file)
  
  # add leap year rows
  datatable = add_leap_year_rows(datatable)
  
  # interpolate missing variable values for leap years using linear imputation
  datatable$PRCP = na_interpolation(datatable$PRCP, option = "linear")
  
  # generate the new filename by adding "_LY" before the file extension
  new_filename = sub("_Daymet_prcp.csv$", "_Daymet_prcp_LY.csv", basename(file))
  new_filepath = file.path(output_filepath, new_filename)
  
  # save the processed data to the new CSV file
  write_csv(datatable, new_filepath)
}

#######################################################################
 ## DAYMET UNIFORM FILE FORMATTING AND NAMING ##
#######################################################################
# format all csv files to ensure the same columns and type (DATE, VARIABLE)
# define the folder containing the CSV files
folder_path = here("data", "MACH", "PRCP_LY")
output_filepath = here("data", "Formatted_MACH", "PRCP")

# list all CSV files in the folder that match the pattern
#file_list = list.files(path = folder_path, pattern = "basin_USGS-\\d{8}_GLEAM_PET_1980_2023.csv", full.names = TRUE)
# file_list = list.files(path = folder_path, pattern = "basin_USGS-\\d{8}_Daymet_vp_LY.csv", full.names = TRUE)
file_list = list.files(path = folder_path, pattern = "^basin_\\d{8}_.+\\.csv$", full.names = TRUE)

# function to process each file
process_file = function(file) {
  # extract the 8-digit number from the file name
  file_name = basename(file)
  usgs_number = str_extract(file_name, "\\d{8}")  # extracts only the 8-digit number
# usgs_number = tools::file_path_sans_ext(file_name)
  
  # read the CSV file
  data = read_csv(file)
  
  # columns to include in dataframe
  data = data %>%
    mutate(
      DATE = as.Date(Date, format = "%m/%d/%Y"),
      # SITENO = usgs_number,
      PRCP = round(mean, 2) 
      ) %>%
    dplyr::select(DATE, PRCP)
  
  # define new file name
  new_file_name = paste0("basin_", usgs_number, "_prcp.csv")
  new_file_path = file.path(output_filepath, new_file_name)
  
  # write the modified data to a new CSV file
  write.csv(data, new_file_path, row.names = FALSE)
}

# apply the function to each file in the list
lapply(file_list, process_file)

#######################################################################
 ## GLEAM EVAPOTRANSPIRATION AND POTENTIAL EVAPOTRANSPIRATION ##
#######################################################################
# extract daily ET and ETp from GLEAM netcdf files 
# downloaded from GLEAM ftp https://www.gleam.eu/
# resample to match 1km resolution for daymet

# folder path containing netcdf files
gleam_path = "F:/Evapotranspiration"
# folder path containing shapefiles of basin boundaries downloaded from NLDI
shapefile_path = here("data", "nldi_basins")

# directory folder to save output csv files
et_folder = here("data", "MACH", "GLEAM_ET")

# list files
nc_files = list.files(gleam_path, pattern = "\\.nc$", full.names = TRUE)
shapefiles = list.files(shapefile_path, pattern = "\\.shp$", full.names = TRUE)

# target resolution approximately 1 km (0.01 degrees)
target_res = 0.01  

# loop through shapefiles
for (shapefile in shapefiles) {
  basin_shp = sf::st_read(shapefile)
  basin_vect = terra::vect(basin_shp)
  
  # create list to store daily averages
  daily_list = list()
  
  for (nc_file in nc_files) {
    # extract year from filename (adjust regex as needed)
    year = as.numeric(gsub("E_|_GLEAM_v4.1a.nc", "", basename(nc_file)))
    
    # load NetCDF as raster
    gleam_rast = rast(nc_file)
    
    # crop raster to basin extent (shapefile)
    gleam_crop = crop(gleam_rast, basin_vect)
    
    # build target raster template for resampling
    ext_crop = ext(gleam_crop)
    ncol_target = ceiling((ext_crop[2] - ext_crop[1]) / target_res)
    nrow_target = ceiling((ext_crop[4] - ext_crop[3]) / target_res)
    target_raster = rast(ncol = ncol_target, nrow = nrow_target,
                          ext = ext_crop, resolution = target_res, nlyr = nlyr(gleam_crop))
    
    # resample GLEAM raster to 1 km template
    gleam_resamp = resample(gleam_crop, target_raster, method = "cubic")
    
    # area-weighted extraction
    ext_data = terra::extract(gleam_resamp, basin_vect, weights = TRUE)
    weights_vec = ext_data$weight
    value_matrix = ext_data[, !(names(ext_data) %in% c("ID", "weight"))]
    weighted_means = colSums(value_matrix * weights_vec, na.rm = TRUE) /
                      colSums(weights_vec, na.rm = TRUE)
    
    # build dates for this year (layers)
    start_date = as.Date(paste0(year, "-01-01"))
    dates = seq(start_date, by = "day", length.out = length(weighted_means))
    
    daily_list[[as.character(year)]] = data.frame(Date = dates, DailyAverage = round(weighted_means, 3))
    
    # remove files after loop
    rm(gleam_rast, gleam_crop, gleam_resamp, ext_data, value_matrix, weights_vec, weighted_means)
    gc()
  }
  
  # combine all years
  daily_averages_all_years = do.call(rbind, daily_list)
  
  # write CSV
  shapefile_id = tools::file_path_sans_ext(basename(shapefile))
  csv_filename = file.path(et_folder, paste0(shapefile_id, "_GLEAM_ET_1980_2023_1km.csv"))
  write.csv(daily_averages_all_years, csv_filename, row.names = FALSE)
  
  cat("Processed shapefile:", shapefile_id, "\n")
}

###########
# save resampled aet and et with new filename in formatted data
files = list.files(folder, pattern = "\\.csv$", full.names = TRUE)

# loop through each file and rename
for (file in files) {
  # Extract 8-digit site number from filename
  site_no = str_extract(file, "\\d{8}")
  
  # Construct new filename
  new_name = paste0("basin_", site_no, "_pet.csv")
  
  # Define full path to save renamed file
  new_path = file.path(pet_folder, new_name)
  
  # Copy and rename the file
  file_copy(file, new_path, overwrite = TRUE)
}

#######################################################################
 ## COMBINED DAYMET and GLEAM DATA PER BASIN ##
#######################################################################
# get parent directory with all file folders 
parent_dir = here("data", "MACH")

# folder path for merged csv files 
merged = here("data", "MACH", "ALLVAR")

# get list of all subdirectories
subfolders = list.dirs(parent_dir, recursive = FALSE)

# extract all files from each subfolder
all_files = map(subfolders, list.files, full.names = TRUE) %>% unlist()

# extract unique SITENO values from file names
siteloop = unique(str_extract(all_files, "\\d{8}"))

# ensure complete date sequence (due to missing discharge days)
full_date = tibble(DATE = seq(as.Date("1980-01-01"), as.Date("2023-12-31"), by = "day"))

# function to process a single basin and its corresponding data
process_basin = function(siteno) {
 
   # filter files for SITENO
  basin_files = all_files[str_detect(all_files, siteno)]
 
   # read and process each file
  data_list = lapply(basin_files, function(file) {
    df = read_csv(file, col_types = cols())
   
     # ensure SITENO and DATE columns are present in each csv file
  #  if(!all(c("SITENO", "DATE") %in% names(df))) { 
  #    stop(paste("Missing SITENO or DATE in file:", file))
  #  }
    
    # ensure DATE column is present 
    if (!"DATE" %in% names(df)) {
      stop(paste("Missing DATE column in file:", file))
    }
    
    # convert date column to date format in case of character column type
    df = df %>% mutate(DATE = as.Date(DATE))
    
    # remove SITENO column if present
    df = df %>% dplyr::select(-any_of("SITENO"))
    
    return(df)
  })

# merge all data by DATE
combined_df = reduce(data_list, full_join, by = "DATE")

 # ensure complete date range
#combined_df = full_date %>% 
 # left_join(combined_df, by = "DATE") %>% 
  #mutate(SITENO = first(na.omit(combined_df$SITENO))) # fill in siteno if missing 

# reorder columns to have SITENO and then DATE
final_df = full_date %>% left_join(combined_df, by = "DATE")
 # combined_df %>% dplyr::select(SITENO, DATE, everything())

# save final csv file
output_file = file.path(merged, paste0("basin_", siteno, "_MACH.csv"))
write_csv(final_df, output_file)
}

walk(siteloop, process_basin)

#######################################################################
 ## ORIGINAL MOPEX ##
#######################################################################
# list of MOPEX (395 gages)
mopex_list = read_csv("MOPEX_gauges.csv")
mopex_list$site_no = str_pad(mopex_list$site_no, max(nchar(mopex_list$site_no)), side = "left", pad = "0")

# read in all files in folder
all_files = list.files(path = "/MOPEX/Mojave MOPEX/MOPEX/US_Data/Us_438_Daily/", 
                     pattern = "\\.dly$", full.names = TRUE)

# get id numbers from file name, remove extensions
m_files = gsub("\\..*$","", basename(all_files))

# mopex_list2 = mopex_list[76:395,]
# match ids with list to get filepaths for selected gauges
mopex_filter = all_files[m_files %in% mopex_list2$site_no]

# define output folder path for modified MOPEX files
output_folder = here("data", "MOPEX")

# function to process each file

process_file = function(file_path) {
  # read the raw file as text
  raw_data = readLines(file_path)
  
  # initialize a list to store processed lines
  processed_lines = list()
  
  # process each line
  for (line in raw_data) {
    # split the line into components based on whitespace
    parts = str_split(line, "\\s+")[[1]]
    
    # initialize variables for year, month, and day
    year = NA
    month = NA
    day = NA
    remaining_parts = NA
    
    # determine the format based on the number of columns
    # mopex file formats are not consistent based on the date and tabs
    if (length(parts) == 8) {
      
      # case 1: YYYY M D format
      year = parts[1]
      month = str_pad(as.character(parts[2]), width = 2, side = "left", pad = "0")
      day = str_pad(as.character(parts[3]), width = 2, side = "left", pad = "0")
      remaining_parts = parts[4:8]
    } else if (length(parts) == 7) {
      if (nchar(parts[2]) == 3) {
        
        # case 2: YYYY MDD format
        year = parts[1]
        month = str_pad(as.character(substr(parts[2], 1, 1)), width = 2, side = "left", pad = "0")
        day = str_pad(as.character(substr(parts[2], 2, 3)), width = 2, side = "left", pad = "0")
        remaining_parts = parts[3:7]
      } else if (nchar(parts[1]) == 6 && nchar(parts[2]) == 1) {
        
        # case 3: YYYYMM D format
        year = substr(parts[1], 1, 4)
        month = substr(parts[1], 5, 6)
        day = str_pad(as.character(parts[2]), width = 2, side = "left", pad = "0")
        remaining_parts = parts[3:7]
      } else {
        stop("Unexpected format for 7 columns.")
      }
    } else if (length(parts) == 6) {
      
      # case 4: YYYYMMDD format
      date_str = str_trim(parts[1])
      if (nchar(date_str) == 8) {
        year = substr(date_str, 1, 4)
        month = substr(date_str, 5, 6)
        day = substr(date_str, 7, 8)
      } else {
        stop("Unexpected length for YYYYMMDD format.")
      }
      remaining_parts = parts[2:6]
    } else {
      stop("Unexpected number of columns.")
    }
    
    # create the YYYYMMDD date string
    date_str = paste0(year, month, day)
    
    # reconstruct the line with formatted date components
    new_line = c(date_str, remaining_parts)
    
    # append the processed line to the list
    processed_lines = append(processed_lines, list(new_line))
  }
  
  # convert the list to a data frame
  df = as_tibble(do.call(rbind, processed_lines))
  colnames(df) = c("DATE", "PRCP", "PET", "OBS_RUN", "TMAX", "TMIN")
  
  # convert columns to appropriate types
  df = df %>%
    mutate(across(c(DATE), as.character),
           across(c(PRCP, PET, OBS_RUN, TMAX, TMIN), as.numeric))
  
  # define the output file path
  output_file_path = file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file_path)), "_MOPEX.csv"))
  
  # write the dataframe to a CSV file
  write_csv(df, output_file_path)
  
  # return the file path of the saved CSV
  return(output_file_path)
}

# process all files and combine into one dataframe
saved_files = map(mopex_filter, process_file)

##########################################
# reformat MOPEX to match MACH and filter for days up to 12/31/1979

# Define the folder containing the CSV files
folder_path = here("data", "MOPEX") 
output_filepath = here("data", "formatted", "MOPEX")

# list all CSV files in the folder that match the pattern
file_list = list.files(path = folder_path, pattern = "\\d{8}_MOPEX.csv", full.names = TRUE)

# function to process each file
process_file = function(file) {
  # extract the 8-digit number from the file name
  file_name = basename(file)
 usgs_number = str_extract(file_name, "\\d{8}")  # extracts only the 8-digit number
# usgs_number = tools::file_path_sans_ext(file_name)
  
  # read the CSV file
  data = read_csv(file)
  
  # replace -99 in all columns with NA to match MACH formatting
  data = data %>% mutate(across(where(is.numeric), ~na_if(.,-99)))
  
  # columns to include in dataframe
  data = data %>%
    mutate(
      DATE = as.character(DATE), 
      DATE = as.Date(DATE, format = "%Y%m%d"),
      DATE = as.Date(DATE, format = "%m/%d/%Y"),
      SITENO = usgs_number,
      OBSQ = round(OBS_RUN, 2), 
      PET = round(PET, 2), 
      PRCP = round(PRCP, 2), 
      TMAX = round(TMAX, 2), 
      TMIN = round(TMIN, 2)
      ) %>%
    dplyr::select(SITENO, DATE, OBSQ, PRCP, TMAX, TMIN)
  
  data = data %>% 
    filter(DATE < as.Date("1980-01-01"))
  
  # define new file name
  new_file_name = paste0("basin_", usgs_number, "_MOPEX.csv")
  new_file_path = file.path(output_filepath, new_file_name)
  
  # write the modified data to a new CSV file
  write.csv(data, new_file_path, row.names = FALSE)
}

# apply the function to each file in the list
lapply(file_list, process_file)



