# Katharine Sink
# September 2024
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

### SERVER ###

server = function(input, output, session) {
  

##############################
#### TAB 1 SITE SELECTION ####
##############################
  # create a reactive value to hold site attributes for filter by manually entering site no
  manual_edit = reactiveVal(site_attributes)
  
  
  # table for site data based on site_info csv (read in as site_attributes), reactive variable sitefilt
  sitefilt = reactive({
    # start with the complete dataset     
    siteinfo = site_attributes 
    
    # apply filters conditionally based on user inputs
    # table will be filtered based on selections from dropdowns and sliders, initially displays all 1,014
    # updates instantly if any filter is changed
    
    # apply HUC filter if input is not NULL and not empty
    if (!is.null(input$huc1) && length(input$huc1) > 0) {
      siteinfo = siteinfo %>% dplyr::filter(huc_02 %in% input$huc1)
    }
    
    # apply state filter if input is not NULL and not empty
    if (!is.null(input$state1) && length(input$state1) > 0) {
      siteinfo = siteinfo %>% dplyr::filter(state %in% input$state1)
    }
    
    # apply latitude filter if input is not NULL and has a length of 2 (2 ended slider)
    if (!is.null(input$latitude1) && length(input$latitude1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(dec_lat_va >= input$latitude1[1], dec_lat_va <= input$latitude1[2])
    }
    
    # apply longitude filter if input is not NULL and has a length of 2
    if (!is.null(input$longitude1) && length(input$longitude1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(dec_long_va >= input$longitude1[1], dec_long_va <= input$longitude1[2])
    }
    
    # elevation filter - apply only if input is not NULL and has a length of 2
    if (!is.null(input$elevation1) && length(input$elevation1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(elev_mean_m >= input$elevation1[1], elev_mean_m <= input$elevation1[2])
    }
    
    # area filter - apply only if input is not NULL and has a length of 2
    if (!is.null(input$area1) && length(input$area1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(drain_area_sqkm >= input$area1[1], drain_area_sqkm <= input$area1[2])
    }
    
    # slope filter - apply only if input is not NULL and has a length of 2
    if (!is.null(input$slope1) && length(input$slope1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(basin_slope >= input$slope1[1], basin_slope <= input$slope1[2])
    }
    
    # columns to display in table shown at the bottom of the page, based on filtered sites
    siteinfo %>% 
      dplyr::select(
        SITENO = SITENO, 
        HUC = huc_02,
        NAME = station_name, 
        STATE = state, 
        LATITUDE = dec_lat_va, 
        LONGITUDE = dec_long_va, 
        ELEVATION = elev_mean_m, 
        AREA = drain_area_sqkm, 
        SLOPE = basin_slope
      )
    
  }) # close sitefilt reactive table
  
  # update manual_edit based on the filtered data
  observe({
    manual_edit(sitefilt())
  })
  
  # add site by site number, manual entry
  observeEvent(input$add_site_btn, {
    if (input$add_site_no != "") {
      # ensure the site number is treated as a string to preserve leading zeros
      new_site_no = sprintf("%08d", as.numeric(input$add_site_no))
      
      # find the site in the original dataset by site number
      new_site = site_attributes %>% dplyr::filter(SITENO == new_site_no)
      
      if (nrow(new_site) > 0) {
        # select the same columns as the manual_edit() to avoid mismatched columns
        new_site = new_site %>%
          dplyr::select(
            SITENO = SITENO, 
            HUC = huc_02,
            NAME = station_name, 
            STATE = state, 
            LATITUDE = dec_lat_va, 
            LONGITUDE = dec_long_va, 
            ELEVATION = elev_mean_m, 
            AREA = drain_area_sqkm, 
            SLOPE = basin_slope
          )
        
        # add the new site to the current filtered data
        manual_edit(rbind(manual_edit(), new_site))
        showNotification(paste("Site", input$add_site_no, "added successfully!"), type = "message")
      } else {
        showNotification("Invalid site number. Site not found.", type = "error")
      }
    }
  })
  
  # remove site by selected site number, manual entry of 8 digit number
  observeEvent(input$remove_site_btn, {
    if (input$remove_site_no != "") {
      # Ensure the site number is treated as a string to preserve leading zeros
      remove_site_no = sprintf("%08d", as.numeric(input$remove_site_no))
      
      # check if the site exists in the current filtered data
      if (remove_site_no %in% manual_edit()$SITENO) {
        # remove the selected site from the filtered data
        manual_edit(manual_edit() %>% dplyr::filter(SITENO != remove_site_no))
        showNotification(paste("Site", input$remove_site_no, "removed successfully!"), type = "message")
      } else {
        showNotification("Site number not found in the current table.", type = "error")
      }
    }
  })
  
  # observe event for the "Remove All Sites" button, essentially an empty dataframe
  observeEvent(input$remove_site_all, {
    # clear all sites from the filtered data
    manual_edit(manual_edit()[0, ])  #  sets manual_edit to an empty dataframe with the same structure
    showNotification("All sites removed successfully!", type = "message")
  })
  
  # render the data table
  output$gauges = renderDT({
    DT::datatable(manual_edit(), options = list(pageLength = 20, scrollX = '400px'))
  })
  
  # discharge table to show the number of days on record per year for filtered gauges
  discharge_filt = reactive({
    disch_days = discharge_count
    disch_days = disch_days %>% dplyr::filter(SITENO %in% manual_edit()$SITENO)
  })
  
  # render discharge days table
  output$discharge_days = renderDT({
    DT::datatable(discharge_filt(), options = list(pageLength = 20, scrollX = '400px'))
  })
  
  # observe the reset button click and reset all inputs
  observeEvent(input$reset, {
    updateSelectInput(session, "huc1", selected = "")
    updateSelectInput(session, "state1", selected = "")
    updateSliderInput(session, "latitude1", value = c(-90, 90))
    updateSliderInput(session, "longitude1", value = c(-180, 180))
    updateSliderInput(session, "elevation1", value = c(min(site_attributes$elev_mean_m), max(site_attributes$elev_mean_m)))
    updateSliderInput(session, "area1", value = c(min(site_attributes$drain_area_sqkm), max(site_attributes$drain_area_sqkm)))
    updateSliderInput(session, "slope1", value = c(min(site_attributes$basin_slope), max(site_attributes$basin_slope)))
  })
  
  #### MAP ####
  # create leaflet map  
  output$my_leaflet = renderLeaflet({
    
    # display gauge locations based on filtered datatable sitefilt
    # options for different basemaps and overlays (from shapefiles)    
    leaflet() %>% 
      setView(lng = -99, lat = 40, zoom = 3) %>% 
      addTiles(group = "OpenStreetMap") %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "EsriTopo") %>% 
      addCircleMarkers(data = manual_edit(),
                       lng = ~LONGITUDE, lat = ~LATITUDE, 
                       radius = 3, 
                       popup = paste0("Gauge ID: ", manual_edit()$SITENO, "<br>", # names correspond to edited above
                                      "Gauge Name: ", manual_edit()$NAME, "<br>",
                                      "Latitude: ",  manual_edit()$LATITUDE, "<br>",
                                      "Longitude: ", manual_edit()$LONGITUDE))  %>% 
      addLayersControl(
        baseGroups = c("OpenStreetMap", "EsriTopo"),
        options = layersControlOptions(collapsed = FALSE)) 
  })
  
  
##############################
#### TAB 2 DAILY DATA ####
##############################

# get site numbers for filtered site locations from tab 1
  filtered_sites = reactive({
    req(manual_edit()) # make sure table is not empty
    manual_edit()$SITENO # get gauge id from filtered data
  })  
  

  
  # create a reactive table to manage data retrieval and merge data when button (retrieve_data) is clicked
 table = eventReactive(input$retrieve_data, {
   req(filtered_sites()) # make sure filtered_sites is not empty
   gauge_numbers = filtered_sites()
   all_gauges_data = list()

 
  # loop through each gauge ID to create the data frame
  for (gauge_id in gauge_numbers) {
   gauge_df = create_complete_dates(gauge_id, frequency = "day")
    
  # merge data for each variable into single table if selected
  if (input$select_prcp) {
    prcp_filepath = paste0("F:/MACH/data/PRCP/basin_", gauge_id, "_prcp.csv")
    prcp_df = read_and_format(prcp_filepath, c("PRCP"), gauge_id, frequency = "day")
    if (!is.null(prcp_df)) {
      gauge_df = dplyr::full_join(gauge_df, prcp_df, by = c("SITENO", "DATE"))
    }
  }
      # read and merge temperature data if checkbox selected
      if (input$select_tair) {
        tair_filepath = paste0("F:/MACH/data/TAIR_MEAN/basin_", gauge_id, "_tair.csv")
        tair_df = read_and_format(tair_filepath, c("TAIR"), gauge_id, frequency = "day")
        # if the table has values, merge data  
       if (!is.null(tair_df)) {
          gauge_df = dplyr::full_join(gauge_df, tair_df, by = c("SITENO", "DATE"))
        }
      }
      
      # read and merge potential evapotranspiration data if checkbox selected
      if (input$select_pet) {
        pet_filepath = paste0("F:/MACH/data/PET/basin_", gauge_id, "_pet.csv")
        pet_df = read_and_format(pet_filepath, c("PET"), gauge_id, frequency = "day")
        # if the table has values, merge data  
        if (!is.null(pet_df)) {
          gauge_df = dplyr::full_join(gauge_df, pet_df, by = c("SITENO", "DATE"))
       }
      }
      
      # read and merge actual evapotranspiration data if checkbox selected
      if (input$select_aet) {
        aet_filepath = paste0("F:/MACH/data/AET/basin_", gauge_id, "_aet.csv")
        aet_df = read_and_format(aet_filepath, c("AET"), gauge_id, frequency = "day")
      # if the table has values, merge data 
        if (!is.null(aet_df)) {
          gauge_df = dplyr::full_join(gauge_df, aet_df, by = c("SITENO", "DATE"))
        }
      }
      
      # read and merge discharge data if checkbox selected
      if (input$select_disch) {
        disch_filepath = paste0("F:/MACH/data/OBSQ/basin_", gauge_id, "_obsq.csv")
        disch_df = read_and_format(disch_filepath, c("OBSQ"), gauge_id, frequency = "day")
        # if the table has values, merge data
        if (!is.null(disch_df)) {
          gauge_df = dplyr::full_join(gauge_df, disch_df, by = c("SITENO", "DATE"))
        } 
      }
      
      # read and merge swe data if checkbox selected
      if (input$select_swe) {
        swe_filepath = paste0("F:/MACH/data/SWE/basin_", gauge_id, "_swe.csv")
        swe_df = read_and_format(swe_filepath, c("SWE"), gauge_id, frequency = "day")
      # if the table has values, merge data   
        if (!is.null(swe_df)) {
          gauge_df = dplyr::full_join(gauge_df, swe_df, by = c("SITENO", "DATE"))
        }
      }
    
    all_gauges_data[[gauge_id]] = gauge_df
  }
  
  # combine all gauge data frames
  combined_df = dplyr::bind_rows(all_gauges_data)
  
  # apply variable specific filters if selected
  combined_df = apply_filters(combined_df, gauge_numbers, "PRCP", input$select_prcp, input$prcp1)
  combined_df = apply_filters(combined_df, gauge_numbers, "TAIR", input$select_tair, input$tair1)
  combined_df = apply_filters(combined_df, gauge_numbers, "PET", input$select_pet, input$pet1)
  combined_df = apply_filters(combined_df, gauge_numbers, "AET", input$select_aet, input$aet1)
  combined_df = apply_filters(combined_df, gauge_numbers, "OBSQ", input$select_disch, input$disch1)
  combined_df = apply_filters(combined_df, gauge_numbers, "SWE", input$select_swe, input$swe1)

  # apply date filter if selected
  if (input$select_date) {
    combined_df = combined_df %>%
      dplyr::filter(DATE >= as.Date(input$date_range1[1]) &
                    DATE <= as.Date(input$date_range1[2]))
  }
  
  # apply year filter if selected
  if (input$select_year) {
    combined_df = combined_df %>%
      dplyr::filter(lubridate::year(DATE) %in% input$year1)
  }
  
  # apply month filter if selected
  if (input$select_month) {
    month_abbreviations = months # get month abbreviation that corresponds to number
    combined_df = combined_df %>%
      dplyr::filter(month_abbreviations[lubridate::month(DATE)] %in% input$month1)
  }
        combined_df
    
  }) # eventReactive close for table
  
  # display the merged data in a data table
  output$merged_data_table = renderDT({
    table()
  })

  # download the data displayed in the table as csv file if button is clicked 
  output$download_csv = downloadHandler(
    filename = function() {
      paste0("MACH_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(table(), file, row.names = FALSE) 
    }
  )

 # download the data displayed in the table as individual csv files if button is clicked    
  output$download_separate = downloadHandler(
    filename = function() {
      paste0("basin_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir = "basin_files/"
      dir.create(temp_dir, showWarnings = FALSE)  # create the directory if it doesn't exist
      
      # clear temporary directory by removing any existing files
      unlink(paste0(temp_dir, "*"), recursive = TRUE)
      
      # group data by SITENO and write separate CSV files for each site
      table() %>%
        group_by(SITENO) %>%
        group_split() %>%
        walk(.f = function(data) {
          siteno = unique(data$SITENO)  # extract the unique SITENO
          file_name = paste0(temp_dir, "basin_", siteno, ".csv")  # create file path
          write.csv(data, file = file_name, row.names = FALSE)  # write CSV file
        })
    
        # create a ZIP file containing the individual CSV files
      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    }
  )

  
##############################
#### TAB 3 MONTHLY DATA ####
##############################    
# monthly data filter and selection
# uses sites from tab 1  

# function to aggregate monthly data based on selected statistic (minimum, maximum, mean, median or total) 
# sum will not apply to temperature (will use the mean value)  
aggregate_monthly_data = function(df, var_name, agg_type) {
# will use mean value for temperature if total is selected as aggregation type
   agg_func = if(agg_type == "Total" && var_name == "TAIR") {
   function(x) round(mean(x, na.rm = TRUE), 2)
  } else {
   agg_func = switch(
    agg_type, 
    "Minimum" = min, 
    "Maximum" = max, 
    "Median" = median, 
    "Mean" = function(x) round(mean(x, na.rm = TRUE),2), 
    "Total" = sum
  )}
 
# create year and month columns using the DATE
 # apply aggregation function by month and year
  df %>% 
    dplyr::mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE)) %>% 
    dplyr::group_by(SITENO, YEAR, MONTH) %>% 
    dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop")
}  

 # reactive table to retrieve sites and combine data when retrieve button is clicked    
  monthly_table = eventReactive(input$retrieve_month, {
    req(filtered_sites())
    gauge_numbers = filtered_sites()
    all_gauges_data = list()
    
 # loop through each gauge to retrieve monthly data 
    for (gauge_id in gauge_numbers) {
   gauge_df = create_complete_dates(gauge_id, frequency = "monthly") %>% 
     # create year and month columns in gauge_df dataframe
   dplyr::mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE))
         
 # retrieve and aggregate data for each variable if selected
    if (input$select_prcp_m) {
      prcp_data = read_and_format(sprintf("F:/MACH/data/PRCP/basin_%s_prcp.csv", gauge_id), "PRCP", gauge_id, frequency = "monthly")
      if (!is.null(prcp_data)) {
        prcp_agg = aggregate_monthly_data(prcp_data, "PRCP", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, prcp_agg, by = c("SITENO", "YEAR", "MONTH"))}
    }
      
    if (input$select_tair_m) {
      tair_data = read_and_format(sprintf("F:/MACH/data/TAIR_MEAN/basin_%s_tair.csv", gauge_id), "TAIR", gauge_id, frequency = "monthly")
      if (!is.null(tair_data)) {
        tair_agg = aggregate_monthly_data(tair_data, "TAIR", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, tair_agg, by = c("SITENO", "YEAR", "MONTH"))}
    }
   
    if (input$select_pet_m) {
      pet_data = read_and_format(sprintf("F:/MACH/data/PET/basin_%s_pet.csv", gauge_id), "PET", gauge_id, frequency = "monthly")
      if (!is.null(pet_data)) {
        pet_agg = aggregate_monthly_data(pet_data, "PET", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, pet_agg, by = c("SITENO", "YEAR", "MONTH"))}
    }
 
    if (input$select_aet_m) {
      aet_data = read_and_format(sprintf("F:/MACH/data/AET/basin_%s_aet.csv", gauge_id), "AET", gauge_id, frequency = "monthly")
      if (!is.null(aet_data)) {
        aet_agg = aggregate_monthly_data(tair_data, "AET", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, aet_agg, by = c("SITENO", "YEAR", "MONTH"))}
      }

    if (input$select_disch_m) {
      disch_data = read_and_format(sprintf("F:/MACH/data/OBSQ/basin_%s_obsq.csv", gauge_id), "OBSQ", gauge_id, frequency = "monthly")
      if (!is.null(disch_data)) {
        disch_agg = aggregate_monthly_data(disch_data, "OBSQ", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, disch_agg, by = c("SITENO", "YEAR", "MONTH"))}
      }
          
    if (input$select_swe_m) {
      swe_data = read_and_format(sprintf("F:/MACH/data/SWE/basin_%s_swe.csv", gauge_id), "SWE", gauge_id, frequency = "monthly")
      if (!is.null(swe_data)) {
        swe_agg = aggregate_monthly_data(swe_data, "SWE", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, swe_agg, by = c("SITENO", "YEAR", "MONTH"))}
      }
      
    all_gauges_data[[gauge_id]] = gauge_df
  } # close for loop
  
  # combine all gauge data frames
  combined_df = dplyr::bind_rows(all_gauges_data)
  
  # apply year filter if selected
  if (input$select_year_m) {
    combined_df = combined_df %>%
      dplyr::filter(YEAR %in% input$year2)
  }
  
  # apply month filter if selected
  if (input$select_month_m) {
     combined_df = combined_df %>%
      dplyr::filter(MONTH %in% input$month2)
  }
  
  combined_df %>% 
    dplyr::select(-DATE) # remove date column
}) # monthly eventReactive close

# render monthly data table
output$merged_data_table_m = DT::renderDT({
  monthly_table()
}) 

 # download the data displayed in the table as csv file when button is clicked 
  output$download_csv_m = downloadHandler(
    filename = function() {
      paste0("MACH_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(monthly_table(), file, row.names = FALSE) 
    }
  )

 # download the data displayed in the table as individual csv files     
  output$download_separate_m = downloadHandler(
    filename = function() {
      paste0("basin_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir = "basin_files/"
      dir.create(temp_dir, showWarnings = FALSE)  # create the directory if it doesn't exist
      
      # clear temporary directory by removing any existing files
      unlink(paste0(temp_dir, "*"), recursive = TRUE)
      
      # group data by SITENO and write separate CSV files for each site
      monthly_table() %>%
        group_by(SITENO) %>%
        group_split() %>%
        walk(.f = function(data) {
          siteno = unique(data$SITENO)  # extract the unique SITENO
          file_name = paste0(temp_dir, "basin_", siteno, ".csv")  # create file path
          write.csv(data, file = file_name, row.names = FALSE)  # write CSV file
        })
    
        # create a ZIP file containing the CSV files
      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    }
  )
  
##############################
#### TAB 4 ANNUAL DATA ####
##############################    

# aggregate data annually based on year type and selected variables
# add condition if total selected for temperature to use mean 
  aggregate_annual_data = function(df, var_name, agg_type) {
    agg_func = if(agg_type == "Total" && var_name == "TAIR") {
      function(x) round(mean(x, na.rm = TRUE), 2)
    } else {
      switch(
        agg_type, 
        "Minimum" = min, 
        "Maximum" = max, 
        "Median" = median, 
        "Mean" = function(x) round(mean(x, na.rm = TRUE), 2), 
        "Total" = sum
      )
    }

    # add water year using function, apply aggregation 
        df %>% 
            dplyr::mutate(WATERYR = wYear(DATE)) %>% 
            dplyr::group_by(SITENO, WATERYR) %>% 
            dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop") 
    }

  # create eventReactive for retrieving and aggregating annual data when retrieve button is pressed
  annual_table = eventReactive(input$retrieve_year, {
    req(filtered_sites())
    gauge_numbers = filtered_sites()
    all_gauges_data = list()

    for (gauge_id in gauge_numbers) {
      gauge_df = create_complete_dates(gauge_id, frequency = "yearly") %>%
        dplyr::mutate(WATERYR = wYear(DATE))

      # retrieve and aggregate data for selected variables
      if (input$select_prcp_y) {
        prcp_data = read_and_format(sprintf("F:/MACH/data/PRCP/basin_%s_prcp.csv", gauge_id), "PRCP", gauge_id, frequency = "yearly")
        if (!is.null(prcp_data)) {
        prcp_agg = aggregate_annual_data(prcp_data, "PRCP", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, prcp_agg, by = c("SITENO", "WATERYR"))}
        }
      
      if (input$select_tair_y) {
        tair_data = read_and_format(sprintf("F:/MACH/data/TAIR_MEAN/basin_%s_tair.csv", gauge_id), "TAIR", gauge_id, frequency = "yearly")
        if (!is.null(tair_data)) {
        tair_agg = aggregate_annual_data(tair_data, "TAIR", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, tair_agg, by = c("SITENO", "WATERYR"))}
        }
    
       if (input$select_pet_y) {
        pet_data = read_and_format(sprintf("F:/MACH/data/PET/basin_%s_pet.csv", gauge_id), "PET", gauge_id, frequency = "yearly")
        if (!is.null(pet_data)) {
        pet_agg = aggregate_annual_data(pet_data, "PET", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, pet_agg, by = c("SITENO", "WATERYR"))}
        }

       if (input$select_aet_y) {
        aet_data = read_and_format(sprintf("F:/MACH/data/AET/basin_%s_aet.csv", gauge_id), "AET", gauge_id, frequency = "yearly")
        if (!is.null(aet_data)) {
          aet_agg = aggregate_annual_data(aet_data, "AET", input$year_agg)
          gauge_df = dplyr::left_join(gauge_df, aet_agg, by = c("SITENO", "WATERYR"))}
        }
       
      if (input$select_disch_y) {
        disch_data = read_and_format(sprintf("F:/MACH/data/OBSQ/basin_%s_obsq.csv", gauge_id), "OBSQ", gauge_id, frequency = "yearly")
        if (!is.null(disch_data)) {
          disch_agg = aggregate_annual_data(disch_data, "OBSQ", input$year_agg)
          gauge_df = dplyr::left_join(gauge_df, disch_agg, by = c("SITENO", "WATERYR"))}
        }

       if (input$select_swe_y) {
        swe_data = read_and_format(sprintf("F:/MACH/data/SWE/basin_%s_swe.csv", gauge_id), "SWE", gauge_id, frequency = "yearly")
        if (!is.null(swe_data)) {
          swe_agg = aggregate_annual_data(swe_data, "SWE", input$year_agg)
          gauge_df = dplyr::left_join(gauge_df, swe_agg, by = c("SITENO", "WATERYR"))}
        }
      
      all_gauges_data[[gauge_id]] = gauge_df
    }

    # combine all gauge data frames
    combined_df = dplyr::bind_rows(all_gauges_data)

    # filter by selected year (Calendar Year or Water Year)
    if (input$select_year_wy) {
    combined_df = combined_df %>% 
      dplyr::filter(WATERYR %in% input$wateryear1)
    }
    combined_df %>% 
    dplyr::select(-DATE) # remove date column
  })
  

  # display aggregated annual data table
  output$merged_data_table_y = DT::renderDT({
    datatable(annual_table(), options = list(pageLength = 10))
  })
  
  # download the data displayed in the table as csv file when button is clicked 
  output$download_csv_y = downloadHandler(
    filename = function() {
      paste0("MACH_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(annual_table(), file, row.names = FALSE) 
    }
  )

 # download the data displayed in the table as individual csv files     
  output$download_separate_y = downloadHandler(
    filename = function() {
      paste0("basin_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir = "basin_files/"
      dir.create(temp_dir, showWarnings = FALSE)  # create the directory if it doesn't exist
      
      # clear temporary directory by removing any existing files
      unlink(paste0(temp_dir, "*"), recursive = TRUE)
      
      # group data by SITENO and write separate CSV files for each site
      annual_table() %>%
        group_by(SITENO) %>%
        group_split() %>%
        walk(.f = function(data) {
          siteno = unique(data$SITENO)  # extract the unique SITENO
          file_name = paste0(temp_dir, "basin_", siteno, ".csv")  # create file path
          write.csv(data, file = file_name, row.names = FALSE)  # write CSV file
        })
    
        # create a ZIP file containing the CSV files
      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    }
  )
  
  
##############################
#### TAB 5 ATTRIBUTES ####
##############################   
  
  # create a reactive table to get selected attributes for selected gauges from tab 1    
  att_table = eventReactive(input$get_attributes, {
    
    req(filtered_sites()) # make sure filtered_sites tables is not empty
    # get the filtered sites from tab 1
    gauge_numbers = filtered_sites()
    
    # initialize empty dataframes for attributes
   attribute_data = list()
    
  # create a function to filter attributes for a dataset (csv file)
   filter_and_select = function(data, attributes) {
     if (!is.null(attributes) && length(attributes) > 0) {
       return(data %>% 
                dplyr::filter(SITENO %in% gauge_numbers) %>% 
                dplyr::select(SITENO, all_of(attributes)))
     } 
     return(NULL) # if no attributes are selected
   }
   
   # filter attributes for each dataset and add to the list if not NULL
  if (!is.null(input$site_att)) {
    attribute_data[["site"]] = filter_and_select(site, input$site_att)
  }
  
  if (!is.null(input$climate_att)) {
    attribute_data[["climate"]] = filter_and_select(climate, input$climate_att)
  }
  
  if (!is.null(input$hydro_att)) {
    attribute_data[["hydrology"]] = filter_and_select(hydrology, input$hydro_att)
  }
  
  # initialize combined_data_att with SITENO column
  combined_data_att = data.frame(SITENO = gauge_numbers)
  
  # perform the full join for all available attribute data
  combined_data_att = Reduce(function(x, y) {
    if (!is.null(y)) {
      dplyr::full_join(x, y, by = "SITENO")
    } else {
      x
    }
  }, attribute_data, init = combined_data_att)
  
  # return the final merged data
  return(combined_data_att)
}) # close eventReactive  
   

  # get selected attributes as combined data table
  output$catch_attributes = renderDT({
    att_table()
  })  
  
  # download the data displayed in the table as csv file when button is clicked 
  output$download_att = downloadHandler(
    filename = function() {
      paste0("MACH_att_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(att_table(), file, row.names = FALSE) 
    }
  )
  
} # close server