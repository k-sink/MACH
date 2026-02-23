## MACH dataset attributes
# Katharine Sink

# filepaths are relative 

#######################################################################
 ## NHD CHARACTERISTICS ##
#######################################################################
# NHD Attributes
# downloaded txt files from https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47
# filter by comids 

# get comids and select as vector
comids = read.csv("Comid.csv")
ids_select = comids$COMID

# read in complete attribute file 
# change filename based on attribute
file_path = "/NHDPlusAttributes/Topographic/STREAM_DENSITY_CONUS.txt"
data = read.csv(file_path, sep = ",", header = TRUE)

# filter txt data by comids and save as csv
filtered = data[data$COMID %in% ids_select,]

# now match comids back to gauge id numbers
com_gauge = left_join(filtered, comids, by = "COMID") 
com_gauge$SITENO = str_pad(com_gauge$SITENO, width = 8, pad = "0", side = "left")
write.csv(com_gauge, "/NHDPlusV2/STREAM_DENSITY_CONUS.csv")

# for the 21 separate files for AnningAtor lithologic classes, combine into one file first
# file_list = list.files(path = "/NHDPlusAttributes/Geology", 
#                       pattern = "^AnningAtor_P.*\\.txt$", full.names = TRUE)
# combined_df = file_list %>% lapply(read_csv) %>% bind_rows()
# filtered = combined_df[combined_df$COMID %in% ids_select,]


#######################################################################
 ## NATIONAL INVENTORY OF DAMS ##
#######################################################################
# locations taken from points within each polygon (QGIS)
dams = read_csv("selected_dam_locations.csv")

volume = dams %>% group_by(site_no) %>% reframe(min_NID = min(NID_storage_acre_ft), max_NID = max(NID_storage_acre_ft))
volume$site_no = str_pad(volume$site_no, max(nchar(volume$site_no)), side = "left", pad = "0")

dam_sum = dams %>% count(site_no, name = "count")
dam_sum$site_no = as.character(dam_sum$site_no)
dam_sum$site_no = str_pad(dam_sum$site_no, max(nchar(dam_sum$site_no)), side = "left", pad = "0")

dams_count = left_join(comids, dam_sum, by = "site_no")
dam_info = left_join(dams_count, volume, by = "site_no")
write.csv(dam_info, "/dam_info.csv")


#######################################################################
 ## STATSGO SOIL ATTRIBUTES ##
#######################################################################
# exported attribute table from QGIS for attributes from STATSGO

soil = read_csv("/soil_attributes.csv")

# drop columns that are all NA 
soil = soil[, colSums(!is.na(soil)) > 0]

# group by SITENO and MUKEY 
summarised = soil %>% group_by(SITENO, MUKEY) %>% 
  summarise(total_area = sum(poly_area, na.rm = TRUE), 
            across(-poly_area, first), # keep first value of all other columns 
            .groups = "drop")

summarised = summarised %>% mutate(percent_area = (total_area/area_sqm) *100)

write.csv(summarised, "AttributesMACH/soil_statsgo.csv")


########################################################################
## AREA COMPARISONS
########################################################################
area = area %>% 
  mutate(NWIS_NHD = (abs(NWIS_sqkm - NHDPlusV2_sqkm))/((NWIS_sqkm + NHDPlusV2_sqkm)/2) * 100, 
         NWIS_QGIS = (abs(NWIS_sqkm - QGIS_area_sqkm))/((NWIS_sqkm + QGIS_area_sqkm)/2) * 100,
         NHD_QGIS = (abs(NHDPlusV2_sqkm - QGIS_area_sqkm))/((NHDPlusV2_sqkm + QGIS_area_sqkm)/2) * 100)

area_diff = area %>% dplyr::select(SITENO, NWIS_NHD, NWIS_QGIS, NHD_QGIS) %>% 
  mutate(across(where(is.numeric), ~round(., digits = 2)))

area_long = area_diff %>% pivot_longer(cols = -SITENO, names_to = "Comparison", 
                                       values_to = "Perc_diff")

ggplot(data = area_long, aes(x = Perc_diff)) +
  geom_histogram(bins = 40) +
  facet_wrap(~Comparison, ncol = 1, scales = "free_y")


ggplot(area, aes(x = QGIS_area_sqkm, y = NHDPlusV2_sqkm)) + 
  geom_point(alpha = 0.6, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_log10(labels = label_log(base = 10, digits = 2)) + 
  scale_y_log10(labels = label_log(base = 10, digits = 2)) +
  xlab(bquote("QGIS-calculated basin area " (km^2))) +
  ylab(bquote("NHDPlusV2 basin area  " (km^2))) +
  theme_bw()

 ggplot(area, aes(x = NWIS_sqkm, y = NHDPlusV2_sqkm)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_log10(labels = label_log(base = 10, digits = 2)) + 
  scale_y_log10(labels = label_log(base = 10, digits = 2)) +
  xlab(bquote("NWIS reported basin area " (km^2))) +
  ylab(bquote("NHDPlusV2 basin area  " (km^2))) +
  theme_bw() 
 

plot_stat = area %>% dplyr::select(NWIS_NHD, NHD_QGIS)
plot_stat = pivot_longer(data = plot_stat, cols = everything(),  names_to = "comparison", values_to = "pct_diff") %>% 
  mutate(comparison = factor(comparison, 
                             levels = c("NHD_QGIS", "NWIS_NHD"), 
                             labels = c("QGIS vs NHDPlusV2", 
                                        "NWIS vs NHDPlusV2")))

cdf_diff_plot = ggplot(plot_stat, aes(x = pct_diff, color = comparison)) + 
  stat_ecdf(linewidth = 1) +
  coord_cartesian(xlim = c(0, 10)) +
  geom_vline(xintercept = c(0.1, 1, 5, 10), 
             linetype = "dotted", linewidth = 1, color = "grey40") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(
    values = c("QGIS vs NHDPlusV2" = "#0072B2", 
               "NWIS vs NHDPlusV2" = "#D55E00")) + 
  labs(x = "Absolute percent difference (%)", y = "Cumulative fraction of basins", 
       color = "Comparison") +   theme_classic()

ggsave("cdf_plot.png", dpi = 320, width = 200, height = 160, unit = "mm")


outliers = area %>% dplyr::select(SITENO, NWIS_NHD, NWIS_QGIS, NHD_QGIS) %>% 
  pivot_longer(cols = -SITENO, names_to = "comparison", values_to = "pct_diff") %>% 
  filter(pct_diff > 50) %>% 
  mutate(comparison = factor(
    comparison, 
      levels = c("NHD_QGIS", "NWIS_QGIS", "NWIS_NHD"),
      labels = c("QGIS vs NHDPlusV2", "NWIS vs QGIS", "NWIS vs NHDPlusV2")))
  

ggplot(outliers,
              aes(x = pct_diff,
                  y = reorder(SITENO, pct_diff),
                  color = comparison)) +
  geom_point(size = 3) +
  labs(
    x = "Absolute percent difference (%)",
    y = "USGS station ID",
    color = "Comparison"
  ) +
  theme_bw()
library(scales)