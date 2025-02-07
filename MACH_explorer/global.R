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
library(sp) # spatial data, create objects of spatial classes 
library(plotly)
library(lubridate)
library(ggpubr)
library(rstatix)
library(raster)
library(stats)
library(moments)
library(here) 

################################################
### DATA ###
################################################
# site attribute file with name, HUC, coordinates, area
site_attributes = read_csv(here("/MACH_explorer/data/site_info.csv")
discharge_count = read_csv("F:/MACH/data/discharge_count.csv")

# shapefiles
#shapefile_dir = here("data", "shapefiles")

# climate variables (organized by folder with individual csv file per site no)
# format is "basin_00000000_XXXX.csv" where 0 is 8 digit site no and XXXX is variable abbreviation
# files have first column as "SITENO", second "DATE"

prcp_filepath = "F:/MACH/data/PRCP"
prcp_files = list.files(path = prcp_filepath, pattern = "basin_\\d{8}_prcp.csv", full.names = TRUE)

#tair_filepath = "F:/MACH/data/TAIR"
tair_filepath = "F:/MACH/data/TAIR_MEAN"
tair_files = list.files(path = tair_filepath, pattern = "basin_\\d{8}_tair.csv", full.names = TRUE)

pet_filepath = "F:/MACH/data/PET"
pet_files = list.files(path = pet_filepath, pattern = "basin_\\d{8}_pet.csv", full.names = TRUE)

aet_filepath = "F:/MACH/data/AET"
aet_files = list.files(path = aet_filepath, pattern = "basin_\\d{8}_aet.csv", full.names = TRUE)

disch_filepath = "F:/MACH/data/OBSQ"
disch_files = list.files(path = disch_filepath, pattern = "basin_\\d{8}_obsq.csv", full.names = TRUE)

swe_filepath = "F:/MACH/data/SWE"
swe_files = list.files(path = swe_filepath, pattern = "basin_\\d{8}_swe.csv", full.names = TRUE)

# site attributes
site = site_attributes  
site_names = colnames(site)[-(1:2)]  
# climate attributes
climate = read_csv("F:/MACH/data/climate.csv")
# removes columns that are completely filled with NA or blank
climate = climate[,colSums(is.na(climate)|climate == "") !=nrow(climate)]
clim_names = colnames(climate)[-(1:2)]

# hydrology attributes
hydrology = read_csv("F:/MACH/data/hydrology.csv")
hydrology = hydrology[,colSums(is.na(hydrology)|hydrology == "") !=nrow(hydrology)]
hydro_names = colnames(hydrology)[-(1:2)]

################################################
### VARIABLES ###
################################################
# numeric vector for calendar years to filter daily data
years = seq(from = 1980, to = 2023, by = 1)
wateryears = seq(from = 1981, to = 2023, by = 1)
months = c("JAN"= 1, "FEB"= 2, "MAR"= 3, "APR"= 4, "MAY"= 5, "JUN"= 6, 
           "JUL"= 7, "AUG"= 8, "SEP"= 9, "OCT"= 10, "NOV"= 11, "DEC"= 12)


################################################
### FUNCTIONS ###
################################################
# convert dates to water year or calendar year
wYear = function(date) ifelse(month(date) < 10, year(date), year(date) + 1)

  # function to create a complete date sequence   
  create_complete_dates = function(gauge_id, frequency = "day") {
    complete_dates = switch(
      frequency,
      "day" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "day"),
      "monthly" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "month"),
      "yearly" = seq.Date(from = as.Date("1980-01-01"), to = as.Date("2023-12-31"), by = "year")
    )
    data.frame(SITENO = gauge_id, DATE = complete_dates)
  }  


# function that will read and format data with a complete date sequence, handles missing days
 # create a function that will read and format data with a complete date sequence, handles missing days
  read_and_format = function(file_path, variable_names, gauge_id, frequency = "day") {
    complete_dates = create_complete_dates(gauge_id, frequency)
    
    if (file.exists(file_path)) {
      df = read_csv(file_path, col_types = list(col_character(), col_date(), col_double()))

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
  
  
# variable selection and values   
# create function to check if variable is selected, read file, format, merge with gauge data
  #merge_data = function(gauge_df, gauge_id, var_name, file_path_template, input_check) {
   # if(input_check) {
    #  filepath = sprintf(file_path_template, gauge_id)
     # data_df = read_and_format(filepath, var_name, gauge_id)
      #if(!is.null(data_df)) {
       # gauge_df = dplyr::full_join(gauge_df, data_df, by = c("SITENO", "DATE"))
    #  }
    #}
    #gauge_df
  #}

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