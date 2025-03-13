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
  #  if (!is.null(input$huc1) && length(input$huc1) > 0) {
  #    siteinfo = siteinfo %>% dplyr::filter(huc_cd %in% input$huc1)
  #  }
    
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
      siteinfo = siteinfo %>% dplyr::filter(elev_mean >= input$elevation1[1], elev_mean <= input$elevation1[2])
    }
    
    # area filter - apply only if input is not NULL and has a length of 2
    if (!is.null(input$area1) && length(input$area1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(NHD_drain_area_sqkm >= input$area1[1], NHD_drain_area_sqkm <= input$area1[2])
    }
    
    # slope filter - apply only if input is not NULL and has a length of 2
    if (!is.null(input$slope1) && length(input$slope1) == 2) {
      siteinfo = siteinfo %>% dplyr::filter(basin_slope >= input$slope1[1], basin_slope <= input$slope1[2])
    }
    
    # columns to display in table shown at the bottom of the page, based on filtered sites
    siteinfo %>% 
      dplyr::select(
        SITENO = SITENO, 
      #  HUC = huc_cd,
        NAME = station_name, 
        STATE = state, 
        LAT = dec_lat_va, 
        LONG = dec_long_va, 
        ELEV = elev_mean, 
        AREA = NHD_drain_area_sqkm, 
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
           # HUC = huc_cd,
            NAME = station_name, 
            STATE = state, 
            LAT = dec_lat_va, 
            LONG = dec_long_va, 
            ELEV = elev_mean, 
            AREA = NHD_drain_area_sqkm, 
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
    DT::datatable(manual_edit(), options = list(
      pageLength = 10, scrollX = TRUE, #autoWidth = TRUE, 
      lengthMenu = c(5, 10, 20, 50), dom = "Blfrtip", paging = TRUE), class = "display responsive nowrap")
  })
  
  # discharge table to show the number of days on record per year for filtered gauges
  discharge_filt = reactive({
    disch_days = discharge_count
    disch_days = disch_days %>% dplyr::filter(SITENO %in% manual_edit()$SITENO)
  })
  
  # render discharge days table
  output$discharge_days = renderDT({
    DT::datatable(discharge_filt(), options = list(pageLength = 10, scrollX = TRUE, #autoWidth = TRUE, 
      lengthMenu = c(5, 10, 20, 50), dom = "Blfrtip", paging = TRUE), class = "display responsive nowrap")
  })
  
  # observe the reset button click and reset all inputs
  observeEvent(input$reset, {
   # updateSelectInput(session, "huc1", selected = "")
    updateSelectInput(session, "state1", selected = "")
    updateSliderInput(session, "latitude1", value = c(-90, 90))
    updateSliderInput(session, "longitude1", value = c(-180, 180))
    updateSliderInput(session, "elevation1", value = c(min(site_attributes$elev_mean), max(site_attributes$elev_mean)))
    updateSliderInput(session, "area1", value = c(min(site_attributes$NHD_drain_area_sqkm), max(site_attributes$NHD_drain_area_sqkm)))
    updateSliderInput(session, "slope1", value = c(min(site_attributes$basin_slope), max(site_attributes$basin_slope)))
  })
  
  #### MAP ####
  # create leaflet map  
  output$my_leaflet = renderLeaflet({
    
    # display gauge locations based on filtered datatable sitefilt
    # options for different basemaps and overlays (from shapefiles)    
    leaflet() %>% 
      setView(lng = -99, lat = 40, zoom = 4) %>% 
      addTiles(group = "OpenStreetMap") %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "EsriTopo") %>% 
      addCircleMarkers(data = manual_edit(),
                       lng = ~LONG, lat = ~LAT, 
                       radius = 2, color = "blue", 
                      popup = paste0("Gauge ID: ", manual_edit()$SITENO, "<br>", # names correspond to edited above
                            "Gauge Name: ", manual_edit()$NAME, "<br>",
                            "Latitude: ",  manual_edit()$LAT, "<br>",
                            "Longitude: ", manual_edit()$LONG))  %>% 
      
      addPolygons(data = basins_shp, 
                  color = "black", fillColor = "white", 
                  weight = 1, opacity = 0.7, fillOpacity = 0.2, group = "Basin Delineations") %>% 
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "EsriTopo"),
        overlayGroups = c("Basin Delineations"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Basin Delineations") %>% 
      
      setMaxBounds(lng1 = -125, lat1 = 25, lng2 = -65, lat2 = 50)
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
   
   selected_site_ids = mach_ids[mach_ids %in% filtered_sites()]
   gauge_numbers = mach_files[mach_ids %in% selected_site_ids]

   all_gauges_data = list() # empty list to store site data

  selected_vars = c() # empty vector for selected variables
   
  # check selected variables from user input and add them to selected_vars
      if (input$select_prcp) selected_vars <- c(selected_vars, "PRCP")
      if (input$select_tair) selected_vars <- c(selected_vars, "TAIR")
      if (input$select_tmin) selected_vars <- c(selected_vars, "TMIN")
      if (input$select_tmax) selected_vars <- c(selected_vars, "TMAX")
      if (input$select_pet) selected_vars <- c(selected_vars, "PET")
      if (input$select_aet) selected_vars <- c(selected_vars, "AET")
      if (input$select_disch) selected_vars <- c(selected_vars, "OBSQ")
      if (input$select_swe) selected_vars <- c(selected_vars, "SWE")
      
  # Loop through each file path (gauge_id) and read the corresponding data
  for (file_path in gauge_numbers) {
    # Extract the gauge_id from the file name (this assumes your filenames have the pattern "basin_00000000_MACH.csv")
    gauge_id = str_extract(basename(file_path), "(?<=basin_)\\d{8}(?=_MACH.csv)")
    
    # Read and format the data
    gauge_df = read_and_format(file_path, selected_vars, gauge_id)  # Assuming read_and_format is defined as you have it
    
    # Store the processed data for each gauge
    all_gauges_data[[gauge_id]] = gauge_df
  }

  # Combine all site data into a single dataframe
  if (length(all_gauges_data) > 0) {
    combined_df = dplyr::bind_rows(all_gauges_data)
  } else {
    return(data.frame(SITENO = character(), DATE = as.Date(character()), stringsAsFactors = FALSE))
  }
  
  # Apply numeric filters for the selected variables
  combined_df = apply_filters(combined_df, selected_site_ids, "PRCP", input$select_prcp, input$prcp1)
  combined_df = apply_filters(combined_df, selected_site_ids, "TAIR", input$select_tair, input$tair1)
  combined_df = apply_filters(combined_df, selected_site_ids, "TMIN", input$select_tmin, input$tmin1)
  combined_df = apply_filters(combined_df, selected_site_ids, "TMAX", input$select_tmax, input$tmax1)
  combined_df = apply_filters(combined_df, selected_site_ids, "PET", input$select_pet, input$pet1)
  combined_df = apply_filters(combined_df, selected_site_ids, "AET", input$select_aet, input$aet1)
  combined_df = apply_filters(combined_df, selected_site_ids, "OBSQ", input$select_disch, input$disch1)
  combined_df = apply_filters(combined_df, selected_site_ids, "SWE", input$select_swe, input$swe1)

  # Apply date filter if selected
  if (input$select_date) {
    combined_df = combined_df %>%
      dplyr::filter(DATE >= as.Date(input$date_range1[1]) &
                    DATE <= as.Date(input$date_range1[2]))
  }
  
  # Apply year filter if selected
  if (input$select_year) {
    combined_df = combined_df %>%
      dplyr::filter(lubridate::year(DATE) %in% input$year1)
  }
  
  # Apply month filter if selected
  if (input$select_month) {
    month_abbreviations = months  # You already have month mappings, so use that
    combined_df = combined_df %>%
      dplyr::filter(month_abbreviations[lubridate::month(DATE)] %in% input$month1)
  }

  return(combined_df)  # Final processed dataset
 
  }) # eventReactive close for table
  
  # display the merged data in a data table
  output$merged_data_table = renderDT({
    DT::datatable(table(), options = list(pageLength = 10, scrollX = TRUE, #autoWidth = TRUE,
      lengthMenu = c(5, 10, 20, 50), dom = "Blfrtip", paging = TRUE), class = "display responsive nowrap")
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
    # Check the corresponding file path from mach_files
    file_path = mach_files[mach_ids == gauge_id]
    
    # Create a dataframe for the gauge, ensuring monthly frequency
    gauge_df = create_complete_dates(gauge_id, frequency = "monthly") %>% 
      dplyr::mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE))
    
    # Retrieve and aggregate data for each variable if selected
    if (input$select_prcp_m) {
      prcp_data = read_and_format(file_path, "PRCP", gauge_id, frequency = "monthly")
      if (!is.null(prcp_data)) {
        prcp_agg = aggregate_monthly_data(prcp_data, "PRCP", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, prcp_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }
    
    if (input$select_tair_m) {
      tair_data = read_and_format(file_path, "TAIR", gauge_id, frequency = "monthly")
      if (!is.null(tair_data)) {
        tair_agg = aggregate_monthly_data(tair_data, "TAIR", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, tair_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }
    
     if (input$select_tmin_m) {
      tmin_data = read_and_format(file_path, "TMIN", gauge_id, frequency = "monthly")
      if (!is.null(tmin_data)) {
        tmin_agg = aggregate_monthly_data(tmin_data, "TMIN", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, tmin_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
     }
    
     if (input$select_tmax_m) {
      tmax_data = read_and_format(file_path, "TMAX", gauge_id, frequency = "monthly")
      if (!is.null(tmax_data)) {
        tmax_agg = aggregate_monthly_data(tmax_data, "TMAX", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, tmax_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }
    
    if (input$select_pet_m) {
      pet_data = read_and_format(file_path, "PET", gauge_id, frequency = "monthly")
      if (!is.null(pet_data)) {
        pet_agg = aggregate_monthly_data(pet_data, "PET", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, pet_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }

    if (input$select_aet_m) {
      aet_data = read_and_format(file_path, "AET", gauge_id, frequency = "monthly")
      if (!is.null(aet_data)) {
        aet_agg = aggregate_monthly_data(aet_data, "AET", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, aet_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }

    if (input$select_disch_m) {
      disch_data = read_and_format(file_path, "OBSQ", gauge_id, frequency = "monthly")
      if (!is.null(disch_data)) {
        disch_agg = aggregate_monthly_data(disch_data, "OBSQ", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, disch_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }
    
    if (input$select_swe_m) {
      swe_data = read_and_format(file_path, "SWE", gauge_id, frequency = "monthly")
      if (!is.null(swe_data)) {
        swe_agg = aggregate_monthly_data(swe_data, "SWE", input$month_agg)
        gauge_df = dplyr::left_join(gauge_df, swe_agg, by = c("SITENO", "YEAR", "MONTH"))
      }
    }

    # Store aggregated data for each gauge
    all_gauges_data[[gauge_id]] = gauge_df
  } # Close for loop
  
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
      prcp_data = read_and_format(file_path, "PRCP", gauge_id, frequency = "yearly")
        if (!is.null(prcp_data)) {
        prcp_agg = aggregate_annual_data(prcp_data, "PRCP", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, prcp_agg, by = c("SITENO", "WATERYR"))}
        }
      
      if (input$select_tair_y) {
        tair_data = read_and_format(file_path, "TAIR", gauge_id, frequency = "yearly")
        if (!is.null(tair_data)) {
        tair_agg = aggregate_annual_data(tair_data, "TAIR", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, tair_agg, by = c("SITENO", "WATERYR"))}
      }
      
      if (input$select_tmin_y) {
        tmin_data = read_and_format(file_path, "TMIN", gauge_id, frequency = "yearly")
        if (!is.null(tmin_data)) {
        tmin_agg = aggregate_annual_data(tmin_data, "TMIN", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, tmin_agg, by = c("SITENO", "WATERYR"))}
        }
      
      if (input$select_tmax_y) {
        tmax_data = read_and_format(file_path, "TMAX", gauge_id, frequency = "yearly")
        if (!is.null(tmax_data)) {
        tmax_agg = aggregate_annual_data(tmax_data, "TMAX", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, tmax_agg, by = c("SITENO", "WATERYR"))}
      }
      
       if (input$select_pet_y) {
        pet_data = read_and_format(file_path, "PET", gauge_id, frequency = "yearly")
        if (!is.null(pet_data)) {
        pet_agg = aggregate_annual_data(pet_data, "PET", input$year_agg)
        gauge_df = dplyr::left_join(gauge_df, pet_agg, by = c("SITENO", "WATERYR"))}
        }

       if (input$select_aet_y) {
        aet_data = read_and_format(file_path, "AET", gauge_id, frequency = "yearly")
        if (!is.null(aet_data)) {
          aet_agg = aggregate_annual_data(aet_data, "AET", input$year_agg)
          gauge_df = dplyr::left_join(gauge_df, aet_agg, by = c("SITENO", "WATERYR"))}
        }
       
      if (input$select_disch_y) {
        disch_data = read_and_format(file_path, "OBSQ", gauge_id, frequency = "yearly")
        if (!is.null(disch_data)) {
          disch_agg = aggregate_annual_data(disch_data, "OBSQ", input$year_agg)
          gauge_df = dplyr::left_join(gauge_df, disch_agg, by = c("SITENO", "WATERYR"))}
        }

       if (input$select_swe_y) {
        swe_data = read_and_format(file_path, "SWE", gauge_id, frequency = "yearly")
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