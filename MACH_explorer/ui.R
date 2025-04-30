# Katharine Sink
# MACH (Mopex and Camels Hydro Explorer)

### USER INTERFACE ###
ui = fluidPage(
  
    # Add custom CSS for resizing
  tags$head(tags$style(HTML("
    .well { 
      min-height: 100px; 
      height: auto !important; 
      overflow: hidden; 
    }

    .dataTables_scroll {
     max-height: 70vh !important;
     overflow-y: auto !important;
    }
    .dataTables_paginate { 
      margin-top: 10px !important; 
    }
    .dataTables_info { 
      margin-top: 10px !important; 
    }
  "))),
  
  # bootstrap CSS frameworks 
  theme = bslib::bs_theme(bootswatch = "lumen"), 
  
  # implement shiny js features
  useShinyjs(), 
  
  titlePanel("MACH Explorer"), 
  
  # create tabs   
  tabsetPanel(

##############################
#### TAB 1 SITE SELECTION ####
##############################
# filter data by site, location, area, elevation, slope

# spatial data filters for gauges with map (HUC, state, domain, lat, long, elev, slope, area)    
 tabPanel(title = "Site Selection",
             
   # first row           
     br(), 
     fluidRow(
             
       # column for map of site locations 
         column(width = 7, 
            wellPanel(
            h6(strong("USGS Stream Gauging Site Locations")),
            br(),
       
   # leaflet map output, gauges that have been filtered will be displayed, new column      
          leafletOutput(outputId = "my_leaflet"), 
                  )), # wellPanel, column close 
               
       # column for filters 
         column(width = 3,
            wellPanel(
            h6(strong("Filter Sites")),
            br(), 
                        
       selectizeInput(inputId = "state1", label = "State",
                    choices = sort(unique(site_attributes$state)),
                    multiple = TRUE, 
                    options = list(placeholder = "Select one or more states")),
                        
       checkboxInput(inputId = "latitude", label = HTML("Latitude (&deg;N)"), value = FALSE), 
                conditionalPanel(
                   condition = "input.latitude",
                   sliderInput(inputId = "latitude1", label = NULL,
                   min = 25, max = 50, value = c(25,50), ticks = FALSE)),
  
       checkboxInput(inputId = "longitude", label = HTML("Longitude (&deg;W)"), value = FALSE), 
               conditionalPanel(
                   condition = "input.longitude", 
                   sliderInput(inputId = "longitude1", label = NULL,
                   min = -125, max = -65, value = c(-125,-65), ticks = FALSE)),
                        
       checkboxInput(inputId = "elevation", label = "Mean Elevation (m)", value = FALSE), 
               conditionalPanel(
                   condition = "input.elevation", 
                   sliderInput(inputId = "elevation1", label = NULL,
                   min = 5, max = 3605, value = c(5, 3605), ticks = FALSE)),
                        
      checkboxInput(inputId = "area", label = HTML("Drainage Area (km<sup>2</sup>)"), value = FALSE), 
                conditionalPanel(
                  condition = "input.area",
                  sliderInput(inputId = "area1", label = NULL,
                  min = 2, max = 26000, value = c(2, 26000),  ticks = FALSE)), 
                        
     checkboxInput(inputId = "slope", label = "Mean Slope (percent)", value = FALSE), 
                conditionalPanel(
                   condition = "input.slope",
                   sliderInput(inputId = "slope1", label = NULL, 
                   min = 0, max = 70, value = c(0, 70), ticks = FALSE)),  
                        
              br(), 
                        
  # action button that will reset all selections when pressed  
      actionButton(inputId = "reset", label = "Reset Filters")
                      )), # wellPanel, column close
               
  # column for manually adding and removing sites by gauge id number       
  column(width = 2,
             wellPanel(
             h6(strong("Edit Site Selections")), 
             br(),
    
     # text input to add a site by its number
        textInput(inputId = "add_site_no", label = "Manually Add Site", 
                  placeholder = "Enter 8-digit SITENO"),
                        
     # action button to trigger the addition of the site
      actionButton(inputId = "add_site_btn", label = "Add Site"),
            br(), br(),
        
     # text input to remove a site from the table
         textInput(inputId = "remove_site_no", label = "Manually Remove Site", 
                   placeholder = "Enter 8-digit SITENO"),
                        
     # action button to trigger removal of a site
      actionButton(inputId = "remove_site_btn", label = "Remove Site"), 
     br(), br(), 
     
     # action button to remove all sites and clear table to no results
     actionButton(inputId = "remove_site_all", label = "Remove All Sites"),
                      )), # wellPanel, column close
               
      ), # fluidRow close
             
             
 # create table number of discharge days per year for selected sites
 fluidRow(
      # create column for first table 
               column(width = 5,
                      br(),
                      wellPanel(
                        h6(strong("Stream Discharge Record")),
                        br(), 
                        DTOutput(outputId = "discharge_days")
                      )), # wellPanel, column close 
               
 # table with selected (filtered sites) and area, slope, name, state, lat/lon   
               column(width = 7,
                      br(),  
                      wellPanel(
                        h6(strong("Selected Stream Gauging Sites")),
                        br(),
                        DTOutput(outputId = "gauges")
                      )), # wellPanel, column close
             ), #fluidRow close
             
  ), #site selection tabPanel close
##############################    
#### TAB 2 DAILY DATA ####
##############################  
# based on selected sites from spatial tab, choose variables 
# data tables for hydroclimatic variables (precip, temp, et, pet, runoff)
# daily values shown 
    tabPanel(title = "Daily Data",
       br(), 
       fluidRow(
         ## create a column with selection boxes for variable ##    
           column(width = 4, 
             wellPanel(
              style = "overflow: visible; height: auto;",
               h6(strong("Select Variable(s)")),
          
      # select climate variables      
         checkboxInput(inputId = "select_prcp", label = "Precipitation, PRCP (mm)"), 
                conditionalPanel(
                   condition = "input.select_prcp",
                   sliderInput(inputId = "prcp1", label = NULL, 
                   min = 0, max = 300, value = c(0,300), ticks = FALSE)),
          
         checkboxInput(inputId = "select_tair", label = HTML("Mean Temperature, TAIR (&deg;C)")),  
                 conditionalPanel(
                   condition = "input.select_tair",
                   sliderInput(inputId = "tair1", label = NULL, 
                   min = -50, max = 50, value = c(-50,50), ticks = FALSE)),

         checkboxInput(inputId = "select_tmin", label = HTML("Minimum Temperature, TMIN (&deg;C)")), 
                 conditionalPanel(
                   condition = "input.select_tmin",
                   sliderInput(inputId = "tmin1", label = NULL, 
                   min = -50, max = 50, value = c(-50,50), ticks = FALSE)),

         checkboxInput(inputId = "select_tmax", label = HTML("Maximum Temperature, TMAX (&deg;C)")), 
                 conditionalPanel(
                   condition = "input.select_tmax",
                   sliderInput(inputId = "tmax1", label = NULL, 
                   min = -50, max = 50, value = c(-50,50), ticks = FALSE)),

         checkboxInput(inputId = "select_pet", label = "Potential Evapotranspiration, PET (mm)"), 
              conditionalPanel(
                   condition = "input.select_pet",
                   sliderInput(inputId = "pet1", label = NULL, 
                   min = -1, max = 40, value = c(-1,40), ticks = FALSE)),
      
         checkboxInput(inputId = "select_aet", label = "Actual Evapotranspiration, AET (mm)"),
              conditionalPanel(
                   condition = "input.select_aet",
                   sliderInput(inputId = "aet1", label = NULL, 
                   min = -1, max = 40, value = c(-1,40), ticks = FALSE)),
  
         checkboxInput(inputId = "select_disch", label = "Observed Discharge, OBSQ (mm)"),
             conditionalPanel(
                   condition = "input.select_disch",
                   sliderInput(inputId = "disch1", label = NULL, 
                   min = -1, max = 400, value = c(-1,400), ticks = FALSE)),

         checkboxInput(inputId = "select_swe", label = "Snow Water Equivalent, SWE (mm)"), 
                conditionalPanel(
                   condition = "input.select_swe",
                   sliderInput(inputId = "swe1", label = NULL, 
                   min = 0, max = 1850, value = c(0,1850), ticks = FALSE)),
      
         checkboxInput(inputId = "select_srad", label = HTML("Shortwave Radiation, SRAD (W/m<sup>2</sup>)")), 
                conditionalPanel(
                   condition = "input.select_srad",
                   sliderInput(inputId = "srad1", label = NULL, 
                   min = 10, max = 900, value = c(10,900), ticks = FALSE)),
      
         checkboxInput(inputId = "select_vp", label = "Water Vapor Pressure, VP (Pa)"), 
                conditionalPanel(
                   condition = "input.select_vp",
                   sliderInput(inputId = "vp1", label = NULL, 
                   min = 5, max = 4000, value = c(5,4000), ticks = FALSE)),

         checkboxInput(inputId = "select_dayl", label = "Day Length, DAYL (sec)"), 
                conditionalPanel(
                   condition = "input.select_dayl",
                   sliderInput(inputId = "dayl1", label = NULL, 
                   min = 30000, max = 60000, value = c(30000,60000), ticks = FALSE)),
          br(),
 
  # temporal filter for daily data    
      h6(strong("Select Time Period(s)")), 
        checkboxInput(inputId = "select_date", label = "Date Range"), 
                  conditionalPanel(
                    condition = "input.select_date", 
                    dateRangeInput(inputId = "date_range1", label = NULL, 
                   start = "1980-01-01", end = "2023-12-31", 
                   format = "mm/dd/yyyy", separator = "to")),
      
       checkboxInput(inputId = "select_year", label = "Calendar Year"), 
            conditionalPanel(
              condition = "input.select_year", 
      selectizeInput(inputId = "year1", label = NULL, 
                  choices = years, multiple = TRUE, 
                  options = list(placeholder = "Select one or more"))),      
      
      checkboxInput(inputId = "select_month", label = "Month"), 
          conditionalPanel(
            condition = "input.select_month", 
            selectizeInput(inputId = "month1", label = NULL, 
                        choices = months, multiple = TRUE, 
                        options = list(placeholder = "Select one or more"))),
          
      br(),
      
  # button to get selected data and combine into single data table
      actionButton(inputId = "retrieve_data", label = "Retrieve and View Data"), 
                      ), # wellPanel close for variable selection
            br(),

  # new panel for downloading data           
        wellPanel(
            h6(strong("Download Daily Data")), 
            br(), 
           # button to download data table as single csv file
           downloadButton(outputId = "download_csv", label = "Export as csv"), 
            br(), br(),
           # button to download data table as separate csv files 
           downloadButton(outputId = "download_separate", label = "Export as separate csv files")
              )  # wellPanel close
               ), # column close
               
   # new column to display data table for selected variables          
  column(width = 8, 
            wellPanel(
              h6(strong("Filtered Daily Data")),
            br(), 
            # output data table for selected variables
            shinycssloaders::withSpinner(
              DTOutput(outputId = "merged_data_table"))
            ) # wellPanel close
            ) # column close
             ) # fluidRow close
    ), # tabPanel 2 data retrieval close

##########################    
#### TAB 3 MONTHLY   ####
########################## 
# tab for monthly aggregation 
# based on selected sites from spatial tab, choose variables 
# data tables for hydroclimatic variables (precip, temp, et, pet, runoff)
# and values shown monthly

    tabPanel(title = "Monthly Data",
             
       br(), 
       # first row 
       fluidRow(
         ## create a column with selection boxes for variable ##    
           column(width = 4, 
            wellPanel(
              style = "overflow: visible; height: auto;",
              h6(strong("Select Statistic")), 
              selectInput(inputId = "month_agg", label = NULL, multiple = FALSE, 
                          choices = c("Minimum", "Maximum", "Median", "Mean", "Total"), 
                          selected = "Mean"), 
            ), # wellPanel close
            
            br(),
                  
            wellPanel(
              style = "overflow: visible; height: auto;",
              h6(strong("Select Variable(s)")),
          
      # select climate variables      
         checkboxInput(inputId = "select_prcp_m", label = "Precipitation, PRCP (mm)"), 
         checkboxInput(inputId = "select_tair_m", label = HTML("Mean Temperature, TAIR (&deg;C)")), 
         checkboxInput(inputId = "select_tmin_m", label = HTML("Minimum Temperature, TMIN (&deg;C)")), 
         checkboxInput(inputId = "select_tmax_m", label = HTML("Maximum Temperature, TMAX (&deg;C)")), 
         checkboxInput(inputId = "select_pet_m", label = "Potential Evapotranspiration, PET (mm)"), 
         checkboxInput(inputId = "select_aet_m", label = "Actual Evapotranspiration, AET (mm)"),
         checkboxInput(inputId = "select_disch_m", label = "Observed Discharge, OBSQ (mm)"),
         checkboxInput(inputId = "select_swe_m", label = "Snow Water Equivalent, SWE (mm)"), 
         checkboxInput(inputId = "select_srad_m", label = HTML("Shortwave Radiation, SRAD (W/m<sup>2</sup>)")), 
         checkboxInput(inputId = "select_vp_m", label = "Water Vapor Pressure, VP (Pa)"), 
         checkboxInput(inputId = "select_dayl_m", label = "Day Length, DAYL (sec)"), 
         
          br(),
 
  # temporal filter for daily data    
      h6(strong("Select Time Period(s)")), 
       checkboxInput(inputId = "select_year_m", label = "Calendar Year"), 
            conditionalPanel(
              condition = "input.select_year_m", 
      selectizeInput(inputId = "year2", label = NULL, 
                  choices = years, multiple = TRUE, 
                  options = list(placeholder = "Select one or more"))),      
      
      checkboxInput(inputId = "select_month_m", label = "Month"), 
          conditionalPanel(
            condition = "input.select_month_m", 
            selectizeInput(inputId = "month2", label = NULL, 
                        choices = months, multiple = TRUE, 
                        options = list(placeholder = "Select one or more"))),
          
      br(),
      
  # button to get selected data and combine into single data table
      actionButton(inputId = "retrieve_month", label = "Retrieve and View Data"), 
                      ), # wellPanel close for variable selection
            br(),

  # new panel for downloading data           
        wellPanel(
            h6(strong("Download Monthly Data")), 
            br(), 
           # button to download data table as single csv file
            downloadButton(outputId = "download_csv_m", label = "Export as csv"), 
            br(), br(),
           # button to download data table as separate csv files 
            downloadButton(outputId = "download_separate_m", label = "Export as separate csv files")
              )  # wellPanel close
               ), # column close
   # new column to display data table for selected variables          
  column(width = 8, 
            wellPanel(
            h6(strong("Filtered Monthly Data")),
            br(), 
            # output data table for selected variables
            shinycssloaders::withSpinner(
            DTOutput(outputId = "merged_data_table_m")))
            ) # column close
             ) # fluidRow close
    ), # tabPanel 2 data retrieval close

##########################    
#### TAB 4 ANNUAL ####
########################## 
# tab for annual aggregation 

# based on selected sites from spatial tab, choose variables 
# data tables for hydroclimatic variables (precip, temp, et, pet, runoff)
# and values shown annual
    tabPanel(title = "Annual Data",
             
       br(), 
       # first row 
       fluidRow(
         column(width = 4,
          wellPanel(
             style = "overflow: visible; height: auto;",  
             
             h6(strong("Annual Aggregation")), 
             radioButtons(inputId = "year_type", label = NULL, inline = FALSE, 
                          choices = c("Water Year" = "water", "Calendar Year" = "calendar"), 
                          selected = "water"),
            
             h6(strong("Select Statistic")), 
              selectInput(inputId = "year_agg", label = NULL, multiple = FALSE, 
                          choices = c("Minimum", "Maximum", "Median", "Mean", "Total"), 
                          selected = "Mean"), 
            ), # wellPanel close
            
            br(),
                  
            wellPanel(
             h6(strong("Select Variable(s)")),
                        
      # select climate variables      
         checkboxInput(inputId = "select_prcp_y", label = "Precipitation, PRCP (mm)"), 
         checkboxInput(inputId = "select_tair_y", label = HTML("Mean Temperature, TAIR (&deg;C)")), 
         checkboxInput(inputId = "select_tmin_y", label = HTML("Minimum Temperature, TMIN (&deg;C)")), 
         checkboxInput(inputId = "select_tmax_y", label = HTML("Maximum Temperature, TMAX (&deg;C)")), 
         checkboxInput(inputId = "select_pet_y", label = "Potential Evapotranspiration, PET (mm)"), 
         checkboxInput(inputId = "select_aet_y", label = "Actual Evapotranspiration, AET (mm)"),
         checkboxInput(inputId = "select_disch_y", label = "Observed Discharge, OBSQ (mm)"),
         checkboxInput(inputId = "select_swe_y", label = "Snow Water Equivalent, SWE (mm)"), 
         checkboxInput(inputId = "select_srad_y", label = HTML("Shortwave Radiation, SRAD (W/m<sup>2</sup>)")), 
         checkboxInput(inputId = "select_vp_y", label = "Water Vapor Pressure, VP (Pa)"), 
         checkboxInput(inputId = "select_dayl_y", label = "Day Length, DAYL (sec)"), 
         
          br(),
 
  # temporal filter for annual data    
      h6(strong("Select Time Period(s)")), 
  style = "overflow: visible; height: auto;",
  
  conditionalPanel(
    condition = "input.year_type == 'water'",
   checkboxInput(inputId = "select_year_wy", label = "Water Year"), 
            conditionalPanel(
              condition = "input.select_year_wy", 
      selectizeInput(inputId = "wateryear1", label = NULL, 
                  choices = wateryears, multiple = TRUE, 
                  options = list(placeholder = "Select one or more")))),  
  
  conditionalPanel(
    condition = "input.year_type == 'calendar'", 
    checkboxInput(inputId = "select_year_cal", label = "Calendar Year"), 
    conditionalPanel(
    condition = "input.select_year_cal", 
    selectizeInput(inputId = "calyear1", label = NULL, 
                choices = years, multiple = TRUE, 
                options = list(placeholder = "Select one or more")))),
  
      br(),
      
  # button to get selected data and combine into single data table
      actionButton(inputId = "retrieve_year", label = "Retrieve and View Data"), 
                      ), # wellPanel close for variable selection
            br(),

  # new panel for downloading data           
        wellPanel(
            h6(strong("Download Annual Data")), 
            br(), 
           # button to download data table as single csv file
            downloadButton(outputId = "download_csv_y", label = "Export as csv"), 
            br(), br(),
           # button to download data table as separate csv files 
            downloadButton(outputId = "download_separate_y", label = "Export as separate csv files")
              )  # wellPanel close
               ), # column close

   # new column to display data table for selected variables          
  column(width = 8, 
            wellPanel(
            h6(strong("Filtered Annual Data")),
            br(), 
            
            # output data table for selected variables
            shinycssloaders::withSpinner(
            DTOutput(outputId = "merged_data_table_y")))
            ) # column close
             ) # fluidRow close
    ), # tabPanel 2 data retrieval close

##########################    
#### TAB 5 MOPEX ####
########################## 
# tab for mopex data and appending data to mach
 tabPanel(title = "MOPEX", 
          br(), 
          fluidRow(
            column(width = 4,
                   wellPanel(
                    style = "overflow: visible; height: auto;",
                      h6(strong("Select Export Option")), 
                     br(), 
                     selectInput(inputId = "mopex_data", label = NULL, multiple = FALSE, 
                                 choices = c("MOPEX only" = "mopex", 
                                 "MOPEX & MACH" = "combined")),
                    br(), 
                    actionButton(inputId = "retrieve_mopex", label = "Retrieve and View Data"), 
                    ), # wellPanel close
                   br(), 
                   
                   wellPanel(
                     h6(strong("Download MOPEX")), 
                     br(), 
                     downloadButton(outputId = "download_mopex", label = "Export as csv"), 
                     br(), br(), 
                     downloadButton(outputId = "download_separate_mopex", label = "Export as separate csv files")
                   ) # wellPanel close
                   ), # column close
          
              column(width = 8, 
                   wellPanel(
                       style = "overflow: visible; height: auto;",
                      h6(strong("MOPEX Basins Data")), 
                br(), 
                shinycssloaders::withSpinner(      
                DTOutput(outputId = "mopex_table"))
                     ) # wellPanel close
                   )) # column, fluidRow close
          ), #tabPanel close

##########################    
#### TAB 6 ATTRIBUTES ####
########################## 
# get selected characteristics for selected sites 
    tabPanel(title = "Attributes", 
             
       br(), 
         fluidRow(
             column(5, 
                wellPanel(
                 style = "overflow: visible; height: auto;",
                   style = "width: 100%;", 
                  h6(strong("Select Attribute Type")),
      radioButtons(inputId = "att_data_type", label = NULL, 
                 choices = c("Single Value per Site" = "single", 
                             "Monthly Value per Site" = "monthly", 
                             "Annual Value per Site" = "annual"), 
                 selected = "single", inline = FALSE),
                br(),
                
    conditionalPanel(
      condition = "input.att_data_type == 'single'", 
      h6(strong("Select Overall Site Attribute(s)")), 
    
    # select column(s) from site attributes csv file
        selectizeInput(inputId = "site_att", label = "Catchment", 
                    choices = site_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
                
    # select column(s) from climate attributes csv file
        selectizeInput(inputId = "overall_climate_att", label = "Climate", 
                    choices = overall_clim_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
                        
    # select column(s) from hydrology attributes csv file
        selectizeInput(inputId = "hydro_att", label = "Hydrology", 
                    choices = hydro_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
    
    # select column(s) from soil attributes csv file
        selectizeInput(inputId = "soil_att", label = "Soil", 
                    choices = soil_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
    
     # select column(s) from geology attributes csv file
        selectizeInput(inputId = "geology_att", label = "Geology", 
                    choices = geology_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
    
     # select column(s) from regional attributes csv file
        selectizeInput(inputId = "regional_att", label = "Regional", 
                    choices = regional_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
    
     # select column(s) from anthropogenic attributes csv file
        selectizeInput(inputId = "anthro_att", label = "Anthropogenic", 
                    choices = anthropogenic_names, multiple = TRUE, 
                    options = list(placeholder = "Select one or more")), 
    
    ), # conditionalPanel close
    
    conditionalPanel(
      condition = "input.att_data_type == 'monthly'", 
      h6(strong("Select Monthly Site Attribute(s)")),
      
      selectizeInput(inputId = "monthly_climate_att", label = "Monthly Climate", 
                  choices = monthly_clim_names, multiple = TRUE, 
                  options = list(placeholder = "Select one or more"))
    ), # conditionalPanel close
    
      conditionalPanel(
      condition = "input.att_data_type == 'annual'", 
      h6(strong("Select Annual Site Attribute(s)")),
      
      selectizeInput(inputId = "annual_climate_att", label = "Annual Climate", 
                  choices = annual_clim_names, multiple = TRUE, 
                  options = list(placeholder = "Select one or more"))
    ), # conditionalPanel close
    
      br(), 
      actionButton(inputId = "get_attributes", label = "Retrieve Attributes"),
  
                ), # wellPanel close
    br(), 
    wellPanel(
    h6(strong("Download Attributes")), 
    br(), 
    
    conditionalPanel(
      condition = "input.att_data_type == 'single'", 
      downloadButton(outputId = "download_single_att", label = "Export Overall Data")
    ),  # conditionalPanel close
    
    conditionalPanel(
      condition = "input.att_data_type == 'monthly'", 
      downloadButton(outputId = "download_monthly_att", label = "Export Monthly Data")
    ), # conditionalPanel close
    
     conditionalPanel(
      condition = "input.att_data_type == 'annual'", 
      downloadButton(outputId = "download_annual_att", label = "Export Annual Data")
    ) # conditionalPanel close
    ), # wellPanel close
             ), # column close
    
# new column to display data table for selected attributes
                   column(7, 
                      wellPanel(
                        h6(strong("Selected Attributes")), 
                        br(),
                        shinycssloaders::withSpinner(
                        DTOutput(outputId = "catch_attributes"))
                      )) # wellPanel, column close
             ) # fluidRow close
             
    ), # tab panel 5 attributes close

##############################
#### LAND COVER TAB  ####
############################## 
 tabPanel(title = "Land Cover", 
       br(), 
         fluidRow(
             column(5, 
                wellPanel(
                 style = "overflow: visible; height: auto;",
                   style = "width: 100%;", 
                  h6(strong("Select Calendar Year(s)")),
                selectizeInput(inputId = "lc_year", label = NULL, multiple = TRUE, 
                            choices = lc_years, options = list(placeholder = "Select one or more")), 
                br(), 
          h6(strong("Select Land Cover Class(es)")), 
          style = "overflow: visible; height: auto;",
          selectizeInput(inputId = "lc_class", label = NULL, multiple = TRUE, 
                      choices = lc_class_names, options = list(placeholder = "Select one or more")),
          br(), 
          actionButton(inputId = "get_landcover", label = "Retrieve Attributes"), 
                ), # wellPanel close
          
          br(), 
          wellPanel(
            h6(strong("Download Attributes")),
            br(),
            downloadButton(outputId = "download_lc_att", label = "Export LC Data")
          ) # wellPanel close
                 ), # column close
          
          column(7, 
                 wellPanel(
                   h6(strong("Selected Land Cover Data")),
                   br(), 
                   shinycssloaders::withSpinner(
                   DTOutput(outputId = "lc_attributes"))
                 )) # wellPanel, column close
        ) #fluidRow close
        
         ), # tabPanel close

##############################
#### ABOUT TAB  ####
##############################  
# user information 
 tabPanel(title = "About", 
  br(),   
  fluidRow(
    column(width = 12, 
      wellPanel(
        h6(strong("WELCOME TO MACH EXPLORER")), 
        p("This app allows users to navigate and manipulate the MACH dataset, which contains daily climate and streamflow
        data along with catchment attributes for 1,014 watersheds within the contiguous United States. For all tables, columns
        can be sorted using the diamond button beside each header. The Search box can be used to locate numerical or character data  
        in a table, which will refresh automatically. All tabs (Daily Data, Monthly Data, Annual Data, Historical, and Attributes), 
        retrieve data based on the sites selected on the 'Site Selection' tab.") 
      ) # wellPanel close
    ) # column close
  ), # fluidRow close
  br(), 
  fluidRow(
    column(width = 12, 
      wellPanel(
        h6(strong("SITE SELECTION")),
        p("This tab filters and selects the watershed(s) from the MACH dataset."),
        p(strong("USGS Stream Gauging Site Locations")), 
        tags$ul(
          tags$li("The map displays the sites listed in the 'Selected Stream Gauging Sites' table. 
          Basemap options include the OpenStreetMap and EsriTopo. Basin Delineations can be toggled on and off ")
        ), 
        p(strong("Filter Sites")), 
        tags$ul( 
          tags$li("State: Select site(s) by state. Allows for multiple selections. Use backspace to remove a selection."),
          tags$li("Latitude (N): Select site(s) by latitude range in decimal degrees."), 
          tags$li("Longitude (W): Select site(s) by longitude range in decimal degrees."), 
          tags$li("Mean Elevation: Select site(s) by mean elevation in meters above sea level."),
          tags$li("Drainage area: Select site(s) by drainage area in square kilometers."),
          tags$li("Mean Slope: Select site(s) by mean slope in percent."),
          tags$li("RESET FILTERS button will clear all selected filters.")
        ),
        p(strong("Edit Site Selections")), 
        tags$ul(
          tags$li("Enter 8-digit Site No: Manually ADD SITE or REMOVE SITE by entering the 8-digit site number including leading zeroes."), 
          tags$li("REMOVE ALL SITES button will clear all sites, resulting in a blank 'USGS Stream Gauging Site Locations' map and 'Selected Stream Gauging Sites' table."), 
          tags$li("The RESET FILTERS button can be pressed at any time to display all 1,014 sites.")
        ),
        p(strong("Stream Discharge Record")),
        tags$ul(
          tags$li("Table displays selected sites and the streamflow record information, which includes the number of records 
          (count), the first and last available dates, and the number of records for each calendar year.")
        ), 
        p(strong("Selected Stream Gauging Sites")), 
        tags$ul(
          tags$li("Table displays sites based on 'Filter Sites' and 'Edit Site Selections' information."),
          tags$li("Table includes the USGS site number, Name, State, Latitude, Longitude, Elevation, Area, and Slope.")
        )
      ) # wellPanel close
    ) # column close
  ), # fluidRow close
  br(), 
  fluidRow(
    column(width = 12, 
      wellPanel(
        h6(strong("DAILY DATA, MONTHLY DATA, ANNUAL DATA")), 
        p("Each tab returns daily data values on a scale that corresponds with the name. Annual is by water year."), 
        p(strong("Select Variable(s)")), 
        tags$ul(
          tags$li("Select one or more variables for the filtered sites chosen in the 'Site Selection' tab."), 
          tags$li("Variable abbreviations and units are: Precipitation (PRCP) in millimeters per day, mean air temperature (TAIR), minimum air temperature (TMIN),      
          and maximum air temperature (TMAX) in degrees Celcius, potential evapotranspiration (PET) in millimeters per day, actual evapotranspiration (AET) in 
          millimeters per day, stream discharge (OBSQ) in millimeters per day, snow water equivalent (SWE) in  millimeters per day, shortwave radiation (SRAD) in 
          watts per square meter, water vapor pressure (VP) Pascals, and day length (DAYL) in seconds per day."),
          tags$li("DAILY DATA tab only - selected variables can be filtered by range using the slider bars. Defaults to all available data for selected variable.")
        ), 
        p(strong("Select Statistic")), 
        tags$ul(
          tags$li("MONTHLY DATA and ANNUAL DATA tabs only - determines the data value returned for the corresponding time period. 
          Options include Minimum, Maximum, Mean, Median, or Total. Only one option can be selected. Each statistic is 
          calculated over the corresponding month or year. Mean is the default. If total is selected, all temperature variables will return the mean.")
        ),
        p(strong("Select Time Period(s)")),
        tags$ul(
          tags$li("Variables can be filtered temporally with options including Date Range, Calendar Year(s), Month(s), and Water Year(s), 
          depending on the data tab selected. Multiple selections can be made for month and year options."), 
          tags$li("The RETRIEVE AND VIEW button must be pressed each time selection criteria are modified.")
        ),
        p(strong("Filtered Data")), 
        tags$ul(
          tags$li("Tables display the site number, date (DAILY DATA only), calendar year and month (MONTHLY DATA only), 
          water year (ANNUAL DATA only), and selected variable(s)"), 
          tags$li("If filtering criteria does not match any of the available data, the table will display that no data is available.")
        ),
        p(strong("Download Data")), 
        tags$ul(
          tags$li("EXPORT AS CSV will download the data displayed in the 'Filtered Data' table as one csv file."), 
          tags$li("EXPORT AS SEPARATE CSV FILES will download the data displayed in the 'Filtered Data' table as separate csv files
          for each site into a single zip file.")
        )
      ) # wellPanel close
    ) # column close
  ), # fluidRow close
   br(), 
  fluidRow(
    column(width = 12, 
      wellPanel(
        h6(strong("MOPEX")), 
        p("This tab returns daily values (January 1, 1948 to December 31, 1979) for watersheds 
        derived from MOPEX. The sites returned are based on the 'Site Selection' tab. Any filtered sites
        with site numbers also present in the original MOPEX 395 watersheds will be retrieved. No data will be returned
        if none of the 'Selected Stream Gauging Sites' appear in MOPEX. Variables are PRCP, OBSQ, TMIN, and TMAX."),
        p(strong("Select Export Option")), 
        tags$ul(
          tags$li("If 'MOPEX only' is selected, daily values for 1948-1979 will be returned."),
          tags$li("If 'MOPEX & MACH' is selected, daily values for 1980-2023 from MACH will be appended to MOPEX data for 1948-1979."), 
          tags$li("The RETRIEVE AND VIEW button must be pressed if the export option is changed.")
                )
      ) # wellPanel close
    ) # column close
  ), # fluidRow close
 
br(), 
fluidRow(
  column(width  = 12, 
         wellPanel(
           h6(strong("ATTRIBUTES")), 
           p("This tab returns catchment attributes for 'Site Selection' watersheds."),
           p(strong("Select Attribute Type")),
           tags$ul(
             tags$li("Attributes can be retrieved depending on the type. Single Value per site refers to overall
                     attributes. Monthly Value per site will return attributes determined on a monthly scale. Annual
                     Value per site will return attributes determined on an annual scale. Only one type option can be 
                     selected at a time.")
           ),
      p(strong("Select Site Attribute(s)")), 
        tags$ul(
          tags$li("Available attributes will change depending on the 'Select Attribute Type' option chosen. Multiple
                  attribute selections can be made. The RETRIEVE ATTRIBUTES button must be pressed if selections are 
                  changed."),
          tags$li("Please refer to the READme file for detailed attribute information including descriptions of the names
                  displayed in the pull down menu.")
           )
         ) # wellPanel close
         ) # column close
), #fluidRow close

br(), 
fluidRow(
  column(width = 12, 
         wellPanel(
           h6(strong("Documentation")),
           p(strong("Datasets")),
           p("This app uses data downloaded and processed from the following sources"),
           tags$ul(
             tags$li("Daily climate variables (PRCP, TMIN, TMAX, SWE, VP, SRAD, DAYL) from Daymet V4 -  https://daymet.ornl.gov/"),
             tags$li("Daily streamflow data (OBSQ) from USGS National Water Information System - https://waterdata.usgs.gov/nwis?"), 
             tags$li("Daily potential and actual evapotranspiration (PET, AET) from GLEAM4 - https://www.gleam.eu/"), 
             tags$li("Various catchment attributes from USGS NHDPlus Version 2.1 - https://doi.org/10.5066/F7765D7V"), 
             tags$li("Land cover catchment attributes from Multi-Resolution Land Characteristics (MRLC) - https://www.mrlc.gov/"), 
             tags$li("Dam attributes from the US Army Corps of Engineers (USACE) National Inventory of Dams - https://nid.sec.usace.army.mil/#/")
           )
         ) # wellPanel close
         ) # column close
) # fluidRow close
  

) # tabPanel close

    
  ) #tabsetPanel close (all tabs)
  
) # ui fluidpage close