# Katharine Sink
# September 2024

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
      # ensure the site number is treated as a string to preserve leading zeros
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
      lengthMenu = c(5, 10, 20, 50), dom = "Blfrtip", paging = TRUE), class = "display responsive nowrap") %>% 
      DT::formatRound(columns = c("LAT", "LONG"), digits = 2)
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
                      radius = 3, color = "blue", 
                      popup = paste0("Gauge ID: ", manual_edit()$SITENO, "<br>", # names correspond to edited above
                            "Gauge Name: ", manual_edit()$NAME, "<br>",
                            "Latitude: ",  manual_edit()$LAT, "<br>",
                            "Longitude: ", manual_edit()$LONG))  %>% 
      addPolygons(data = basins_shp, color = "black", fillColor = "white", 
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
   # get sites from tab 1
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
      if (input$select_srad) selected_vars <- c(selected_vars, "SRAD")
      if (input$select_vp) selected_vars <- c(selected_vars, "VP")
      if (input$select_dayl) selected_vars <- c(selected_vars, "DAYL")
      
  # loop through each file path (gauge_id) and read the corresponding data
  for (file_path in gauge_numbers) {
    # extract the gauge_id from the file name (this assumes your filenames have the pattern "basin_00000000_MACH.csv")
    gauge_id = str_extract(basename(file_path), "(?<=basin_)\\d{8}(?=_MACH.csv)")
    # read and format the data
    gauge_df = read_and_format(file_path, selected_vars, gauge_id)  
    # store the processed data for each gauge
    all_gauges_data[[gauge_id]] = gauge_df
  }

  # combine all site data into a single dataframe
  if (length(all_gauges_data) > 0) {
    combined_df = dplyr::bind_rows(all_gauges_data)
  } else {
    return(data.frame(SITENO = character(), DATE = as.Date(character()), stringsAsFactors = FALSE))
  }
  
  # apply numeric filters for the selected variables
  combined_df = apply_filters(combined_df, selected_site_ids, "PRCP", input$select_prcp, input$prcp1)
  combined_df = apply_filters(combined_df, selected_site_ids, "TAIR", input$select_tair, input$tair1)
  combined_df = apply_filters(combined_df, selected_site_ids, "TMIN", input$select_tmin, input$tmin1)
  combined_df = apply_filters(combined_df, selected_site_ids, "TMAX", input$select_tmax, input$tmax1)
  combined_df = apply_filters(combined_df, selected_site_ids, "PET", input$select_pet, input$pet1)
  combined_df = apply_filters(combined_df, selected_site_ids, "AET", input$select_aet, input$aet1)
  combined_df = apply_filters(combined_df, selected_site_ids, "OBSQ", input$select_disch, input$disch1)
  combined_df = apply_filters(combined_df, selected_site_ids, "SWE", input$select_swe, input$swe1)
  combined_df = apply_filters(combined_df, selected_site_ids, "SRAD", input$select_srad, input$srad1)
  combined_df = apply_filters(combined_df, selected_site_ids, "VP", input$select_vp, input$vp1)
  combined_df = apply_filters(combined_df, selected_site_ids, "DAYL", input$select_dayl, input$dayl1)

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
    month_abbreviations = months  
    combined_df = combined_df %>%
      dplyr::filter(month_abbreviations[lubridate::month(DATE)] %in% input$month1)
  }

  return(combined_df)  # final processed dataset
 
  }) # eventReactive close for table
  
  # display the merged data in a data table
  output$merged_data_table = renderDT({
    DT::datatable(table(), options = list(pageLength = 10, scrollX = TRUE, #autoWidth = TRUE,
      lengthMenu = c(5, 10, 20, 50), dom = "Blfrtip", paging = TRUE, digits = 2), 
      class = "display responsive nowrap") 
  })

  # download the data displayed in the table as csv file if button is clicked 
  output$download_csv = downloadHandler(
    filename = function() {
      paste0("MACH_daily_", Sys.Date(), ".csv")
    },
    content = function(file) {
      withProgress(message = "Creating CSV file", value = 0, {
        for (i in 1:5) {           
        Sys.sleep(0.2)
                  incProgress(1/5)
                  }
      write.csv(table(), file, row.names = FALSE) 
    })
    }
  )

 # download the data displayed in the table as individual csv files if button is clicked    
output$download_separate = downloadHandler(
  filename = function() {
    paste0("MACH_daily_", Sys.Date(), ".zip")
  },
  content = function(file) {
    temp_dir = "basin_files/"
    dir.create(temp_dir, showWarnings = FALSE)
    unlink(paste0(temp_dir, "*"), recursive = TRUE)

    full_data = table()
    sites = unique(full_data$SITENO)
    n_sites = length(sites)

    withProgress(message = "Creating ZIP file", value = 0, {
      for (i in seq_along(sites)) {
        siteno = sites[i]
        data = full_data[full_data$SITENO == siteno, ]
        write.csv(data, file = paste0(temp_dir, "MACH_daily_", siteno, ".csv"), row.names = FALSE)

        # update progress bar
        incProgress(1 / n_sites)
      }

      # zip after all files are written
      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    })
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
   agg_func = if(agg_type == "Total" && var_name %in% c("TAIR", "TMIN", "TMAX")) {
   function(x) round(mean(x, na.rm = TRUE), 2)
  } else {
   agg_func = switch(
    agg_type, 
    "Minimum" = min, 
    "Maximum" = max, 
    "Median" = median, 
    "Mean" = function(x) round(mean(x, na.rm = TRUE),2), 
    "Total" = function(x) round(sum(x, na.rm = TRUE),2)
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
    
  # list of all possible variables and corresponding input names
  variables = tibble(
    var = c("PRCP", "TAIR", "TMIN", "TMAX", "PET", "AET", "OBSQ", "SWE", "SRAD", "VP", "DAYL"),
    input_name = c(
      "select_prcp_m", "select_tair_m", "select_tmin_m", "select_tmax_m",
      "select_pet_m", "select_aet_m", "select_disch_m", "select_swe_m",
      "select_srad_m", "select_vp_m", "select_dayl_m"
    )
  )

  for (gauge_id in gauge_numbers) {
    file_path = mach_files[mach_ids == gauge_id]
    
    # base dataframe with complete monthly dates
    gauge_df = create_complete_dates(gauge_id, frequency = "monthly") %>%
      mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE))
    
    for (i in seq_len(nrow(variables))) {
      varname = variables$var[i]
      input_flag = variables$input_name[i]
      
      # check if the variable is selected
      if (isTRUE(input[[input_flag]])) {
        var_data = read_and_format(file_path, varname, gauge_id, frequency = "monthly")
        if (!is.null(var_data)) {
          var_agg = aggregate_monthly_data(var_data, varname, input$month_agg)
          gauge_df = left_join(gauge_df, var_agg, by = c("SITENO", "YEAR", "MONTH"))
        }
      }
    }
    
    # store aggregated data for each gauge
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
  DT::datatable(monthly_table(), options = list(pageLength = 10, scrollX = TRUE, #autoWidth = TRUE,
      lengthMenu = c(5, 10, 20, 50), dom = "Blfrtip", paging = TRUE), 
      class = "display responsive nowrap") 
}) 

 # download the data displayed in the table as csv file when button is clicked 
output$download_csv_m = downloadHandler(
  filename = function() {
    paste0("MACH_monthly_", Sys.Date(), ".csv")
  },
  content = function(file) {
    withProgress(message = "Creating CSV file", value = 0, {
      for (i in 1:5) {
        Sys.sleep(0.2)  
        incProgress(1 / 5)
      }
      write.csv(monthly_table(), file, row.names = FALSE)
    })
  }
)

 # download the data displayed in the table as individual csv files     
  output$download_separate_m = downloadHandler(
  filename = function() {
    paste0("MACH_monthly_", Sys.Date(), ".zip")
  },
  content = function(file) {
    temp_dir = "basin_files/"
    dir.create(temp_dir, showWarnings = FALSE)
    unlink(paste0(temp_dir, "*"), recursive = TRUE)

    full_data = monthly_table()
    sites = unique(full_data$SITENO)
    n_sites = length(sites)

    withProgress(message = "Creating ZIP file", value = 0, {
      for (i in seq_along(sites)) {
        siteno = sites[i]
        data = full_data[full_data$SITENO == siteno, ]
        write.csv(data, file = paste0(temp_dir, "MACH_monthly_", siteno, ".csv"), row.names = FALSE)
        incProgress(1 / n_sites)
      }

      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    })
  }
)
  
##############################
#### TAB 4 ANNUAL DATA ####
##############################    
# aggregate data annually based on year type and selected variables
# add condition if total selected for temperature to use mean 
  aggregate_annual_data = function(df, var_name, agg_type) {
      agg_func = if(agg_type == "Total" && var_name %in% c("TAIR", "TMIN", "TMAX")) {
      function(x) round(mean(x, na.rm = TRUE), 2)
    } else {
      switch(
        agg_type, 
        "Minimum" = min, 
        "Maximum" = max, 
        "Median" = median, 
        "Mean" = function(x) round(mean(x, na.rm = TRUE), 2), 
        "Total" = function(x) round(sum(x, na.rm = TRUE),2)
      )
    }
# based on radio button choice of water or calendar year 
    # add water year using function, apply aggregation 
       if (input$year_type == 'water') {
        df %>% 
            dplyr::mutate(WATERYR = wYear(DATE)) %>% 
            dplyr::group_by(SITENO, WATERYR) %>% 
            dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop") 
       } else {
         df %>% 
           dplyr::mutate(YEAR = lubridate::year(DATE)) %>% 
           dplyr::group_by(SITENO, YEAR) %>% 
           dplyr::summarise(!!var_name := agg_func(.data[[var_name]]), .groups = "drop") 
       }
    }

  # define annual variables
  annual_vars = list( 
  "PRCP" = "select_prcp_y",
  "TAIR" = "select_tair_y",
  "TMIN" = "select_tmin_y",
  "TMAX" = "select_tmax_y",
  "PET"  = "select_pet_y",
  "AET"  = "select_aet_y",
  "OBSQ" = "select_disch_y",
  "SWE"  = "select_swe_y",
  "SRAD" = "select_srad_y",
  "VP"   = "select_vp_y",
  "DAYL" = "select_dayl_y"
  )
  
  # create eventReactive for retrieving and aggregating annual data when retrieve button is pressed
  annual_table = eventReactive(input$retrieve_year, {
    req(filtered_sites())
    gauge_numbers = filtered_sites()
    all_gauges_data = list()

    for (gauge_id in gauge_numbers) {
   # Check the corresponding file path from mach_files
    file_path = mach_files[mach_ids == gauge_id]
  # water year selected, use complete water year 
    if (input$year_type == 'water') {
        gauge_df = create_complete_dates(gauge_id, frequency = "wyearly") %>%
        dplyr::mutate(WATERYR = wYear(DATE))

        # retrieve and aggregate data for selected variables
      for (var_name in names(annual_vars)) {
        input_name = annual_vars[[var_name]]
        if (isTRUE(input[[input_name]])) {
          var_data = read_and_format(file_path, var_name, gauge_id, frequency = "wyearly")
          if (!is.null(var_data)) {
            var_agg = aggregate_annual_data(var_data, var_name, input$year_agg)
            gauge_df = dplyr::left_join(gauge_df, var_agg, by = c("SITENO", "WATERYR"))
          }
        }
      }
    } else {  # calendar year 
          gauge_df = create_complete_dates(gauge_id, frequency = "yearly") %>% 
            dplyr::mutate(YEAR = lubridate::year(DATE))
          
          for (var_name in names(annual_vars)) {
            input_name = annual_vars[[var_name]]
            if (isTRUE(input[[input_name]])) {
              var_data = read_and_format(file_path, var_name, gauge_id, frequency = "yearly")
              if (!is.null(var_data)) {
                var_agg = aggregate_annual_data(var_data, var_name, input$year_agg)
                gauge_df = dplyr::left_join(gauge_df, var_agg, by = c("SITENO", "YEAR"))
              }
            }
          }
        }
      all_gauges_data[[gauge_id]] = gauge_df
    }

    # combine all gauge data frames
    combined_df = dplyr::bind_rows(all_gauges_data)

    if (input$year_type == "water") {
    # check if years are selected, if not, don't filter
    if (length(input$wateryear1) > 0) {
      combined_df = combined_df %>% 
        dplyr::filter(WATERYR %in% input$wateryear1)
    }
  } else {
    # check if years are selected, if not, don't filter
    if (length(input$calyear1) > 0) {
      combined_df = combined_df %>% 
        dplyr::filter(YEAR %in% input$calyear1)
    }
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
    paste0("MACH_annual_", Sys.Date(), ".csv")
  },
  content = function(file) {
    withProgress(message = "Creating CSV file", value = 0, {
      for (i in 1:5) {
        Sys.sleep(0.2)  
        incProgress(1 / 5)
      }
      write.csv(annual_table(), file, row.names = FALSE)
    })
  }
)

 # download the data displayed in the table as individual csv files     
 output$download_separate_y = downloadHandler(
  filename = function() {
    paste0("MACH_annual_", Sys.Date(), ".zip")
  },
  content = function(file) {
    temp_dir = "basin_files/"
    dir.create(temp_dir, showWarnings = FALSE)
    unlink(paste0(temp_dir, "*"), recursive = TRUE)

    full_data = annual_table()
    sites = unique(full_data$SITENO)
    n_sites = length(sites)

    withProgress(message = "Creating ZIP file", value = 0, {
      for (i in seq_along(sites)) {
        siteno = sites[i]
        data = full_data[full_data$SITENO == siteno, ]
        write.csv(data, file = paste0(temp_dir, "MACH_annual_", siteno, ".csv"), row.names = FALSE)
        incProgress(1 / n_sites)
      }

      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    })
  }
)
  
##############################
#### TAB 5 MOPEX ####
############################## 
# create table with mopex data for basins on tab 1
  mopex_table = eventReactive(input$retrieve_mopex, {
  req(filtered_sites())  # ensure filtered_sites is available
  gauge_ids = filtered_sites()  # list of selected gauge IDs

  # find matching MOPEX file paths for selected gauges
  matching_files = mopex_files[mopex_ids %in% gauge_ids]  # match file names with selected IDs
  
  # if no matching files, return an empty data frame
  if (length(matching_files) == 0) {
    return(data.frame())  
  }

  # read and combine the selected files
  mopex_data = purrr::map_df(matching_files, ~read_csv(
    .x, col_types = cols(
      SITENO = col_character(), 
      DATE = col_date(), 
      OBSQ = col_double(), 
      PRCP = col_double(), 
      TMIN = col_double(), 
      TMAX = col_double())
    )
  )

  # check if MACH data should be appended based on user input
  if (input$mopex_data == "combined") {
  
  # get the basin ids for mopex files
  matching_basin_ids = purrr::map_chr(matching_files, ~str_extract(basename(.x), "\\d{8}"))
    
  # find MACH files that correspond to the selected basins
  mach_mopex_files = mach_files[basename(mach_files) %in% paste0("basin_", matching_basin_ids, "_MACH.csv")]
    
    # if no MACH files are found, just return MOPEX data
    if (length(mach_mopex_files) == 0) {
      return(mopex_data)
    }
    
    # read MACH data
    mach_mopex_data = purrr::map_df(mach_mopex_files, ~read_csv(
      .x, col_types = cols(
    SITENO = col_character(),
    DATE = col_date(), 
    AET = col_double(),
    DAYL = col_double(),
    SRAD = col_double(),
    OBSQ = col_double(),
    PET = col_double(),
    PRCP = col_double(),
    SWE = col_double(),
    TAIR = col_double(),
    TMIN = col_double(),
    TMAX = col_double(),
    VP = col_double())
      )
    )
    
    # if spec_tbl_df, convert to data.frame
    if (inherits(mopex_data, "spec_tbl_df")) {
      mopex_data <- as.data.frame(mopex_data)
    }
    
    # keep only relevant columns (OBSQ, PRCP, TMIN, TMAX from MACH)
    mach_data_relevant = mach_mopex_data %>% dplyr::select(SITENO, DATE, OBSQ, PRCP, TMAX, TMIN)
    
    # full join MOPEX and MACH data by SITENO and DATE (with relevant columns from MACH)
    combined_data = dplyr::bind_rows(mopex_data, mach_data_relevant)
    
    return(combined_data)
  }

  # if "MOPEX only" is selected, return just MOPEX data
  return(mopex_data)
})

# display table 
output$mopex_table = renderDT({
  mopex_table() 
})

# download MOPEX data
output$download_mopex = downloadHandler(
  filename = function() {
    paste0("MOPEX_daily_", Sys.Date(), ".csv")
  },
  content = function(file) {
    withProgress(message = "Creating MOPEX CSV file", value = 0, {
      for (i in 1:5) {
        Sys.sleep(0.2)
        incProgress(1 / 5)
      }
      write.csv(mopex_table(), file, row.names = FALSE)
    })
  }
)

# download MOPEX + MACH data
output$download_combined = downloadHandler(
  filename = function() {
    paste0("MOPEX_MACH_", Sys.Date(), ".csv")
  },
  content = function(file) {
    withProgress(message = "Creating MOPEX & MACH CSV file", value = 0, {
      for (i in 1:5) {
        Sys.sleep(0.2)
        incProgress(1 / 5)
      }
      write.csv(mopex_table(), file, row.names = FALSE)
    })
  }
)

# download separate MOPEX files as individual csvs
output$download_separate_mopex = downloadHandler(
  filename = function() {
    paste0("MOPEX_", Sys.Date(), ".zip")
  },
  content = function(file) {
    temp_dir = "basin_files/"
    dir.create(temp_dir, showWarnings = FALSE)
    unlink(paste0(temp_dir, "*"), recursive = TRUE)

    full_data = mopex_table()
    sites = unique(full_data$SITENO)
    n_sites = length(sites)

    withProgress(message = "Creating MOPEX ZIP file", value = 0, {
      for (i in seq_along(sites)) {
        siteno = sites[i]
        data = full_data[full_data$SITENO == siteno, ]
        write.csv(data, file = paste0(temp_dir, "mopex_", siteno, ".csv"), row.names = FALSE)
        incProgress(1 / n_sites)
      }

      zip::zip(
        zipfile = file,
        files = list.files(temp_dir, full.names = TRUE)
      )
    })
  }
)

##############################
#### TAB 5 ATTRIBUTES ####
##############################   
# reactive function for attribute selection when button pressed
att_table = eventReactive(input$get_attributes, {
  
  req(filtered_sites())  # ensure filtered_sites table is not empty
  gauge_numbers = filtered_sites()
  
  # initialize list for storing filtered datasets
  attribute_data = list()
  
  # function to filter and select attributes
  filter_and_select = function(data, attributes, include_time_col = NULL) {
    if (!is.null(attributes) && length(attributes) > 0) {
      cols_to_select = c("SITENO", if (!is.null(include_time_col)) include_time_col else NULL, attributes)
      return(data[data$SITENO %in% gauge_numbers, cols_to_select, drop = FALSE])
    } 
    return(NULL)
  }
  
  # select attributes based on user selection type
  if (input$att_data_type == "single") {
    if (!is.null(input$site_att)) {
      attribute_data[["site"]] = filter_and_select(site, input$site_att)
    }
    if (!is.null(input$overall_climate_att)) {
      attribute_data[["overall_climate"]] = filter_and_select(overall_climate, input$overall_climate_att)
    }
    if (!is.null(input$hydro_att)) {
      attribute_data[["hydrology"]] = filter_and_select(hydrology, input$hydro_att)
    }
    if (!is.null(input$soil_att)) {
      attribute_data[["soil"]] = filter_and_select(soil, input$soil_att)
    }
    if (!is.null(input$geology_att)) {
      attribute_data[["geology"]] = filter_and_select(geology, input$geology_att)
    }
    if (!is.null(input$regional_att)) {
      attribute_data[["regional"]] = filter_and_select(regional, input$regional_att)
    }
    if (!is.null(input$anthro_att)) {
      attribute_data[["anthropogenic"]] = filter_and_select(anthropogenic, input$anthro_att)
    }
  } else if (input$att_data_type == "monthly") {
    if (!is.null(input$monthly_climate_att)) {
      attribute_data[["monthly_climate"]] = filter_and_select(monthly_climate, input$monthly_climate_att, include_time_col = "MNTH")
    }
  } else if (input$att_data_type == "annual") {
    if (!is.null(input$annual_climate_att)) {
      attribute_data[["annual_climate"]] = filter_and_select(annual_climate, input$annual_climate_att, include_time_col = "YR")
    }
  }
  
  # merge data efficiently with Reduce() function
  if (length(attribute_data) > 0) {
    return(Reduce(function(x, y) dplyr::full_join(x, y, by = "SITENO"), attribute_data))
  } else {
    return(data.frame(SITENO = gauge_numbers))
  }
})

  # get selected attributes as combined data table
  output$catch_attributes = renderDT({
    att_table()
  })  
  
  # download the data displayed in the table as csv file when button is clicked 
  output$download_single_att = downloadHandler(
    filename = function() {paste0("MACH_att_", Sys.Date(), ".csv")},
    content = function(file) {write.csv(att_table(), file, row.names = FALSE)}
  )
  
  output$download_monthly_att = downloadHandler(
    filename = function() {paste0("MACH_monthly_att", Sys.Date(), ".csv")}, 
    content = function(file) {write.csv(att_table(), file, row.names = FALSE)}
  )
  
  output$download_annual_att = downloadHandler(
  filename = function() { paste0("MACH_annual_att_", Sys.Date(), ".csv") },
  content = function(file) { write.csv(att_table(), file, row.names = FALSE) }
)
  
##############################
#### TAB  LAND COVER ####
##############################     
# reactive function for land cover select when button pressed
  lc_table = eventReactive(input$get_landcover, {
    
    req(filtered_sites()) # ensure filtered_sites table is not empty
    gauge_numbers = filtered_sites()
    
    selected_lcyears = input$lc_year
    selected_classes = input$lc_class
    
    # filter and select land cover years
    selected_lc_files = lc_files[names(lc_files) %in% selected_lcyears]
     
    lc_results = lapply(selected_lc_files, function(file_path) {
      lc_df = read_csv(file_path, show_col_types = FALSE)
      
      lc_df$SITENO = as.character(lc_df$SITENO)
      gauge_numbers = as.character(gauge_numbers)
      
      lc_df %>% 
        dplyr::filter(SITENO %in% gauge_numbers) %>% 
        dplyr::select(SITENO, YR, all_of(selected_classes))
    })
    bind_rows(lc_results)
  })
  
  # output selected years and classes in table 
  output$lc_attributes = renderDT({
   lc_table()
})
    
  # download lc file
  output$download_lc_att = downloadHandler(
    filename = function() {
      paste0("LC_data", Sys.Date(), ".csv")
    }, 
    content = function(file) {
      write.csv(lc_table(), file, row.names = FALSE)
    }
  )
  
} # close server