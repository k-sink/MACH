# Katharine Sink
# MACH (Mopex and Camels Hydro Explorer)

# user interface
ui = fluidPage(
  
  # bootstrap CSS frameworks 
  theme = bslib::bs_theme(bootswatch = "cosmo"), 
  
  # implement shiny js features
  useShinyjs(), 
  
  titlePanel("MACH Explorer"), 
  
  # create tabs   
  tabsetPanel(
    
    
##############################
#### HOME TAB  ####
##############################  
# user information 

    tabPanel(title = "Home", 
       h5(strong("Welcome to the MACH Explorer")), 
        br(),
       
         fluidRow(
          column(width = 12, 
                 wellPanel(
                   h6(strong("SITE SELECTION")),
                   br(),
                   p(strong("USGS Stream Gauging Site Locations")), 
                   tags$ul(
                     tags$li("The map displays the sites listed in the Selected Stream Gauging Sites table. 
                       Basemap options include the OpenStreetMap and EsriTopo.")), 
                   
                   p(strong("Filter Sites")), 
                   tags$ul( 
                    tags$li("Regional HUC: Select site by regional hydrologic unit code (huc). Allows for multiple selections. 
                       Use backspace to remove a selection."), 
                    tags$li("State: Select site by state. Allows for multiple selections. Use backspace to remove a selection."),
                    tags$li("Latitude (N): Select site by latitude range in decimal degrees."), 
                    tags$li("Longitude (W): Select site by longitude range in decimal degrees."), 
                    tags$li("Mean Elevation: Select site by mean elevation in meters above sea level."),
                    tags$li("Drainage area: Select site by drainage area in square kilometers."),
                    tags$li("Mean Slope: Select site by mean slope in percent."),
                    tags$li("Reset Filters button will clear all selected filters.")),
                   
                   p(strong("Edit Site Selections")), 
                   tags$ul(
                     tags$li("Enter 8-digit Site No: Manually add or remove a site by entering the 8-digit site number including leading zeroes."), 
                     tags$li("Remove all Sites button will clear all sites, resulting in a blank Site Locations Map and Selected Stream Gauging Sites table."), 
                     tags$li("The Reset Filters button can be pressed at any time to display all 1,014 sites.")),
                   
                   p(strong("Stream Discharge Record")),
                   tags$ul(
                     tags$li("Table displays selected sites and the streamflow record information, which includes
                             the number of records (count), the first and last available dates, and the number of 
                             records for each calendar year.")), 
                   
                   p(strong("Selected Stream Gauging Sites")), 
                   tags$ul(
                     tags$li("Table displays sites based on Filter Sites and Edit Site Selections information."),
                     tags$li("Table includes the USGS site number, HUC, State, Latitude, Longitude, Elevation, Area, and Slope.")),
                   ) # wellPanel close
                ) #column close
                 ), #fluidRow close
      br(), 
      fluidRow(
        column(width = 12, 
               wellPanel(
                 h6(strong("DAILY DATA")), 
                 br(), 
                 p(strong("Select Variable(s)")), 
                 tags$ul(
                   tags$li("Select one or more variables for the filtered sites chosen in the Site Selection tab."), 
                   tags$li("Variables can be filtered by a range of values using the slider bars. Defaults to all available data for selected variable."), 
                   tags$li("Variables can be filtered by date, calendar year, and/or by month."), 
                   tags$li("The Retrieve and View Data button must be pressed if selection criteria are modified.")),
                 
                 p(strong("Filtered Daily Data")), 
                 tags$ul(
                   tags$li("Table displays the site number, date, and selected variable(s)"), 
                   tags$li("Variable abbreviations and units are: Precipitation (PRCP) mm/day, Mean (TAIR), minimum (TMIN), max (TMAX) air temperature degrees Celcius, 
                           Potential Evapotranspiration (PET) mm/day, Actual Evapotranspiration (AET) mm/day, 
                           Stream discharge (OBSQ) mm/day, Snow water equivalent (SWE) mm/day."),
                   tags$li("If filtering criteria does not match any of the available
                           data, the table will display that no data is available.")), 
                 
                 p(strong("Download Daily Data")), 
                 tags$ul(
                   tags$li("Export as csv button will download the data displayed in the Filtered Daily Data table as one csv file."), 
                   tags$li("Export as separate csv files will download the data displayed in the Filtered Daily Data table as separate csv files
                           for each site into a single zip file."))
                
                 )
               )
      ) #fluidRow close
                 ), #tabPanel close
    
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
          leafletOutput(outputId = "my_leaflet")
            )), # wellPanel, column close 
               
       # column for filters 
         column(width = 3,
            wellPanel(
            h6(strong("Filter Sites")),
            br(), 
                    
   # filter by HUC (2 digit USGS), column huc_02
        selectInput(inputId = "huc1", label = "Regional HUC", 
                    choices = sort(unique(site_attributes$huc_02)),
                    multiple = TRUE),
                        
  # filter by state (contiguous only), column state
        selectInput(inputId = "state1", label = "State",
                    choices = sort(unique(site_attributes$state)),
                    multiple = TRUE),
                        
  # filter by latitude
        checkboxInput(inputId = "latitude", label = "Latitude (N)", value = FALSE), 
                conditionalPanel(
                   condition = "input.latitude",
                   sliderInput(inputId = "latitude1", label = NULL,
                   min = 25, max = 50, value = c(25,50), ticks = FALSE)),
                        
  # filter by longitude
       checkboxInput(inputId = "longitude", label = "Longitude (W)", value = FALSE), 
               conditionalPanel(
                   condition = "input.longitude", 
                   sliderInput(inputId = "longitude1", label = NULL,
                   min = -125, max = -65, value = c(-125,-65), ticks = FALSE)),
                        
  # filter by mean elevation   
       checkboxInput(inputId = "elevation", label = "Mean Elevation (m)", value = FALSE), 
               conditionalPanel(
                   condition = "input.elevation", 
                   sliderInput(inputId = "elevation1", label = NULL,
                   min = 5, max = 3605, value = c(5, 3605), ticks = FALSE)),
                        
  # filter by total catchment area     
      checkboxInput(inputId = "area", label = "Drainage Area (km2)", value = FALSE), 
                conditionalPanel(
                  condition = "input.area",
                  sliderInput(inputId = "area1", label = NULL,
                  min = 2, max = 26000, value = c(2, 26000),  ticks = FALSE)), 
                        
  # filter by mean slope
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
        textInput("add_site_no", "Enter 8-digit Site No:", ""),
                        
     # action button to trigger the addition of the site
      actionButton("add_site_btn", "Add Site"),
            br(),
            br(),
        
     # text input to remove a site from the table
         textInput("remove_site_no", "Enter 8-digit Site No:", ""),
                        
     # action button to trigger removal of a site
      actionButton("remove_site_btn", "Remove Site"), 
     br(), br(), 
     
     # action button to remove all sites and clear table to no results
     actionButton("remove_site_all", "Remove All Sites"),
                      )), # wellPanel, column close
               
      ), # fluidRow close
             
             
 # create table number of discharge days per year for selected sites
 # new row            
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
       # first row 
       fluidRow(
         ## create a column with selection boxes for variable ##    
           column(width = 4, 
             wellPanel(
             h6(strong("Select Variable(s)")),
          
                        
      # select climate variables      
        # precipitation     
         checkboxInput(inputId = "select_prcp", label = "Precipitation (mm)"), 
                conditionalPanel(
                   condition = "input.select_prcp",
                   sliderInput(inputId = "prcp1", label = NULL, 
                   min = 0, max = 300, value = c(0,300), ticks = FALSE)),
          
         # temperature
         checkboxInput(inputId = "select_tair", label = "Mean Temperature (C)"), 
                 conditionalPanel(
                   condition = "input.select_tair",
                   sliderInput(inputId = "tair1", label = NULL, 
                   min = -50, max = 50, value = c(-50,50), ticks = FALSE)),
          
        # potential evapotranspiration 
         checkboxInput(inputId = "select_pet", label = "Potential Evapotranspiration (mm)"), 
              conditionalPanel(
                   condition = "input.select_pet",
                   sliderInput(inputId = "pet1", label = NULL, 
                   min = -1, max = 40, value = c(-1,40), ticks = FALSE)),
      
           # actual evapotranspiration
         checkboxInput(inputId = "select_aet", label = "Actual Evapotranspiration (mm)"),
              conditionalPanel(
                   condition = "input.select_aet",
                   sliderInput(inputId = "aet1", label = NULL, 
                   min = -1, max = 40, value = c(-1,40), ticks = FALSE)),
      
          # observed discharge      
         checkboxInput(inputId = "select_disch", label = "Stream Discharge (mm)"),
             conditionalPanel(
                   condition = "input.select_disch",
                   sliderInput(inputId = "disch1", label = NULL, 
                   min = -1, max = 400, value = c(-1,400), ticks = FALSE)),
      
       # snow water equivalent
         checkboxInput(inputId = "select_swe", label = "Snow Water Equivalent (mm)"), 
                conditionalPanel(
                   condition = "input.select_swe",
                   sliderInput(inputId = "swe1", label = NULL, 
                   min = 0, max = 1850, value = c(0,1850), ticks = FALSE)),
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
      selectInput(inputId = "year1", label = NULL, 
                  choices = years, multiple = TRUE)),      
      
      checkboxInput(inputId = "select_month", label = "Month"), 
          conditionalPanel(
            condition = "input.select_month", 
            selectInput(inputId = "month1", label = NULL, 
                        choices = months, multiple = TRUE)),
          
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
           shinycssloaders::withSpinner( 
           downloadButton(outputId = "download_csv", label = "Export as csv")), 
            br(), br(),
           # button to download data table as separate csv files 
           shinycssloaders::withSpinner(  
           downloadButton(outputId = "download_separate", label = "Export as separate csv files"))
              )  # wellPanel close
               ), # column close
               
            br(), 
 
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
              h6(strong("Select Aggregation")), 
              selectInput(inputId = "month_agg", label = NULL, multiple = FALSE, 
                          choices = c("Minimum", "Maximum", "Median", "Mean", "Total")), 
            ), # wellPanel close
            
            br(),
                  
            wellPanel(
             h6(strong("Select Variable(s)")),
          
                        
      # select climate variables      
        # precipitation     
         checkboxInput(inputId = "select_prcp_m", label = "Precipitation (mm)"), 
       
         # temperature
         checkboxInput(inputId = "select_tair_m", label = "Mean Temperature (C)"), 
            
        # potential evapotranspiration 
         checkboxInput(inputId = "select_pet_m", label = "Potential Evapotranspiration (mm)"), 
    
        # actual evapotranspiration
         checkboxInput(inputId = "select_aet_m", label = "Actual Evapotranspiration (mm)"),
          
        # observed discharge      
         checkboxInput(inputId = "select_disch_m", label = "Stream Discharge (mm)"),
         
         # snow water equivalent
         checkboxInput(inputId = "select_swe_m", label = "Snow Water Equivalent (mm)"), 
         
          br(),
 
  # temporal filter for daily data    
      h6(strong("Select Time Period(s)")), 
 
       checkboxInput(inputId = "select_year_m", label = "Calendar Year"), 
            conditionalPanel(
              condition = "input.select_year_m", 
      selectInput(inputId = "year2", label = NULL, 
                  choices = years, multiple = TRUE)),      
      
      checkboxInput(inputId = "select_month_m", label = "Month"), 
          conditionalPanel(
            condition = "input.select_month_m", 
            selectInput(inputId = "month2", label = NULL, 
                        choices = months, multiple = TRUE)),
          
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
               
            br(), 
 
   # new column to display data table for selected variables          
  column(width = 8, 
            wellPanel(
            h6(strong("Filtered Monthly Data")),
            br(), 
            
            # output data table for selected variables
            DTOutput(outputId = "merged_data_table_m"))
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
              h6(strong("Select Aggregation")), 
              selectInput(inputId = "year_agg", label = NULL, multiple = FALSE, 
                          choices = c("Minimum", "Maximum", "Median", "Mean", "Total")), 
            ), # wellPanel close
            
            br(),
                  
            wellPanel(
             h6(strong("Select Variable(s)")),
          
                        
      # select climate variables      
        # precipitation     
         checkboxInput(inputId = "select_prcp_y", label = "Precipitation (mm)"), 
       
         # temperature
         checkboxInput(inputId = "select_tair_y", label = "Mean Temperature (C)"), 
            
        # potential evapotranspiration 
         checkboxInput(inputId = "select_pet_y", label = "Potential Evapotranspiration (mm)"), 
    
        # actual evapotranspiration
         checkboxInput(inputId = "select_aet_y", label = "Actual Evapotranspiration (mm)"),
          
        # observed discharge      
         checkboxInput(inputId = "select_disch_y", label = "Stream Discharge (mm)"),
         
         # snow water equivalent
         checkboxInput(inputId = "select_swe_y", label = "Snow Water Equivalent (mm)"), 
         
          br(),
 
  # temporal filter for annual data    
      h6(strong("Select Time Period(s)")), 
 
      checkboxInput(inputId = "select_year_wy", label = "Water Year"), 
            conditionalPanel(
              condition = "input.select_year_wy", 
      selectInput(inputId = "wateryear1", label = NULL, 
                  choices = wateryears, multiple = TRUE)),      
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
               
            br(), 
 
   # new column to display data table for selected variables          
  column(width = 8, 
            wellPanel(
            h6(strong("Filtered Annual Data")),
            br(), 
            
            # output data table for selected variables
            DTOutput(outputId = "merged_data_table_y"))
            ) # column close
             ) # fluidRow close
    ), # tabPanel 2 data retrieval close

##########################    
#### TAB 5 ATTRIBUTES ####
########################## 
    
# get selected characteristics for selected sites 
    
    tabPanel(title = "Attributes", 
             
       br(), 
         fluidRow(
             column(5, 
                wellPanel(
                h6(strong("Select Attribute(s)")),
                br(),
    
    # select column(s) from site attributes csv file
        selectInput(inputId = "site_att", label = "Catchment", 
                    choices = site_names, multiple = TRUE), 
                
    # select column(s) from climate attributes csv file
        selectInput(inputId = "climate_att", label = "Climate", 
                    choices = clim_names, multiple = TRUE), 
                        
    # select column(s) from hydrology attributes csv file
        selectInput(inputId = "hydro_att", label = "Hydrology", 
                    choices = hydro_names, multiple = TRUE), 
                        
      br(), 
      actionButton(inputId = "get_attributes", label = "Retrieve Attributes"),
    br(), br(), 
    downloadButton(outputId = "download_att", label = "Export attributes as csv" )
                      )), # wellPanel, column close
               
# new column to display data table for selected attributes
                   column(7, 
                      wellPanel(
                        h6(strong("Selected Attributes")), 
                        br(),
                        DTOutput(outputId = "catch_attributes")
                      )) # wellPanel, column close
             ) # fluidRow close
             
    ) # tab panel 3 attributes close
    
  ) #tabsetPanel close (all tabs)
  
) # ui fluidpage close