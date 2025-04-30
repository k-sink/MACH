# packages and data for app
# automatically reads if in app folder

################################################
### LIBRARIES ###
################################################
library(plyr)
library(leaflet) # Mapping
library(tidyverse) # 
library(DT)
library(sf) # read shapefiles
library(shiny)
library(shinythemes) # bootstrap themes
library(shinyjs) #shiny java script with R language
library(shinycssloaders)
library(shinyalert)
library(bslib)
library(lubridate)
library(raster)
library(stats)
library(here) 

################################################
### DATA ###
################################################
# site attribute file with name, HUC, coordinates, area
site_attributes = read_csv(here("MACH_explorer", "data", "attributes", "site_info.csv"))
discharge_count = read_csv(here("MACH_explorer", "data", "attributes", "discharge_mach.csv"))

# polygon shapefiles
shapefile_dir = here("MACH_explorer", "data", "shapefile", "MACH_basins.shp")
basins_shp = st_read(shapefile_dir) %>% st_transform(4326) # convert to lat/lon

# format is "basin_00000000_MACH.csv" where 0 is 8 digit site no 
# files have first column as "SITENO", second "DATE"

# mach time series files
mach_dir = here("MACH_explorer", "data", "timeseries", "MACH")
# list all files 
mach_files = list.files(mach_dir, pattern = "basin_\\d{8}_MACH.csv", full.names = TRUE)                     
# 8 digit site number from filename
mach_ids = mach_files %>% basename() %>% str_extract("(?<=basin_)\\d{8}(?=_MACH.csv)")    

# mopex time series files
mopex_dir = here("MACH_explorer", "data", "timeseries", "MOPEX")
mopex_files = list.files(mopex_dir, pattern = "basin_\\d{8}_MOPEX.csv", full.names = TRUE)  
mopex_ids = mopex_files %>% basename() %>% str_extract("(?<=basin_)\\d{8}(?=_MOPEX.csv)")  

# site attributes
site = site_attributes  
site_names = colnames(site)[-(1:2)] 

# overall climate attributes
overall_climate = read_csv(here("MACH_explorer", "data", "attributes", "overall_climate.csv"))
# removes columns that are completely filled with NA or blank
overall_climate = overall_climate[,colSums(is.na(overall_climate)|overall_climate == "") !=nrow(overall_climate)]
overall_clim_names = colnames(overall_climate)[-1] # remove the site name from selection options

# hydrology attributes
hydrology = read_csv(here("MACH_explorer", "data", "attributes", "hydrology.csv"))
hydrology = hydrology[,colSums(is.na(hydrology)|hydrology == "") !=nrow(hydrology)]
hydro_names = colnames(hydrology)[-1]

# soil attributes
soil = read_csv(here("MACH_explorer", "data", "attributes", "soil.csv"))
soil = soil[,colSums(is.na(soil)|soil == "") !=nrow(soil)]
soil_names = colnames(soil)[-1]

# geology attributes
geology = read_csv(here("MACH_explorer", "data", "attributes", "geology.csv"))
geology = geology[,colSums(is.na(geology)|geology == "") !=nrow(geology)]
geology_names = colnames(geology)[-1]

# regional attributes
regional = read_csv(here("MACH_explorer", "data", "attributes", "regional.csv"))
regional = regional[,colSums(is.na(regional)|regional == "") !=nrow(regional)]
regional_names = colnames(regional)[-1]

# anthropogenic attributes
anthropogenic = read_csv(here("MACH_explorer", "data", "attributes", "anthropogenic.csv"))
anthropogenic = anthropogenic[,colSums(is.na(anthropogenic)|anthropogenic == "") !=nrow(anthropogenic)]
anthropogenic_names = colnames(anthropogenic)[-1]

# monthly climate attributes
monthly_climate = read_csv(here("MACH_explorer", "data", "attributes", "monthly_climate.csv"))
# removes columns that are completely filled with NA or blank
monthly_climate = monthly_climate[,colSums(is.na(monthly_climate)|monthly_climate == "") !=nrow(monthly_climate)]
monthly_clim_names = colnames(monthly_climate)[-(1:2)] # remove site number and month from selection options

# annual climate attributes
annual_climate = read_csv(here("MACH_explorer", "data", "attributes", "annual_climate.csv"))
# removes columns that are completely filled with NA or blank
annual_climate = annual_climate[,colSums(is.na(annual_climate)|annual_climate == "") !=nrow(annual_climate)]
annual_clim_names = colnames(annual_climate)[-(1:2)] # remove site number and year from selection options

# land cover attributes
lc_dir = here("MACH_explorer", "data", "attributes", "land_cover")
lc_files = list.files(lc_dir, pattern = "^ANLCD\\d{4}\\.csv", full.names = TRUE)

################################################
### VARIABLES ###
################################################
# numeric vector for calendar years to filter daily data
years = seq(from = 1980, to = 2023, by = 1)
# numeric vector for water years 
wateryears = seq(from = 1981, to = 2023, by = 1)
# month abbreviations 
months = c("JAN"= 1, "FEB"= 2, "MAR"= 3, "APR"= 4, "MAY"= 5, "JUN"= 6, 
           "JUL"= 7, "AUG"= 8, "SEP"= 9, "OCT"= 10, "NOV"= 11, "DEC"= 12)

# land cover years 
lc_years = gsub(".*ANLCD(\\d{4})\\.csv", "\\1", lc_files)
names(lc_files) = lc_years # name vector for easy lookup

# land cover classes 
lc_class_names = c("Water" = 11, "Perennial Ice/Snow" = 12, "Developed, Open Space" = 21, 
                   "Developed, Low Intensity" = 22, "Developed, Medium Intensity" = 23, "Developed, High Intensity" = 24, 
                   "Barren Land" = 31, "Deciduous Forest" = 41, "Evergreen Forest" = 42, "Mixed Forest" = 43, 
                   "Shrub" = 52, "Grassland/Herbaceous" = 71, "Pasture/Hay" = 81, "Cultivated Crops" = 82, 
                   "Woody Wetlands" = 90, "Herbaceous Wetlands" = 95)
################################################
### FUNCTIONS ###
################################################
# convert dates to water year 
  wYear = function(date) {
    ifelse(month(date) < 10, year(date), year(date)+1)}

  # function to create a complete date sequence   
  create_complete_dates = function(gauge_id, frequency = "daily") {
    complete_dates = switch(
      frequency,
      "daily" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "day"),
      "monthly" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "month"),
      "yearly" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "year"), 
      "wyearly" = seq.Date(from = as.Date("1980-10-01"), to = as.Date("2023-09-30"), by = "year")
    )
    data.frame(SITENO = gauge_id, DATE = complete_dates)
  }  


# function that will read and format data with a complete date sequence, handles missing days
 # create a function that will read and format data with a complete date sequence, handles missing days
  read_and_format = function(file_path, variable_names, gauge_id, frequency = "daily") {
    complete_dates = create_complete_dates(gauge_id, frequency)
    
    if (file.exists(file_path)) {
      df = read_csv(file_path, col_types = cols(
        SITENO = col_character(), 
        DATE = col_date(), 
        .default = col_double()))

      # make sure each csv file has the required columns
      required_columns = c("SITENO", "DATE", variable_names)
      if (!all(required_columns %in% colnames(df))) {
        # if there are columns missing, fill with NA 
        df_complete = complete_dates
        for (variable in variable_names) {
          df_complete[[variable]] = NA
        }
        return(df_complete)
      }
      
      # select only the required columns
      df = df %>% 
        dplyr::select(SITENO, DATE, dplyr::all_of(variable_names))
      
      # merge with complete date sequence to fill in missing rows with NA
      df_complete = dplyr::full_join(complete_dates, df, by = c("SITENO", "DATE"))
      
      # sort the data
    df_complete = df_complete %>% arrange(DATE)

    return(df_complete) 
       
    } else {
      # if the file does not exist, return the complete date sequence with NA for the variables
      df_complete = complete_dates
      for (variable in variable_names) {
        df_complete[[variable]] = NA
      }
      return(df_complete)
    }
  }
  
# create function to conditionally apply filters based on the input values in the sliders    
apply_filters = function(df, gauge_numbers, var_name, input_check, range_input) {
  if(input_check) {
    df = df %>% 
      dplyr::filter(SITENO %in% gauge_numbers) %>% 
      dplyr::filter(is.na(.data[[var_name]]) |
          (.data[[var_name]] >= range_input[1] & .data[[var_name]] <= range_input[2])
      )
  }
  df
}    