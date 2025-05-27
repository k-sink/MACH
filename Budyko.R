## Katharine Sink
# January 2025 

# libraries
library(tidyverse)
library(lubridate)
library(sf)
library(nleqslv)
library(forestmangr)  # nls_table
library(ggplot2)
library(maps)
library(mapdata)
library(usmap)
library(viridis)
library(signal) # FIR filtering
library(ggpubr)

# csv files for 1,014 basins with complete PRCP, TMIN, TMAX, TAIR, SWE, DAYL, SRAD, VP, OBSQ, PET, AET
# streamflow contain at least ten years of data but may have gaps indicated by NA
# basin_10336740 does not have PET or AET data from GLEAM because there is a missing pixel at that location

filepath = "D:/University of Texas at Dallas/CombinedDataset/Formatted_data/ALLVAR"

# read in each csv file 
data = list.files(filepath, pattern = "*.csv", full.names = TRUE) 

# extract 8-digit number from filename and add as column into dataframe 
MACH = purrr::map_dfr(data, function(file) {
  site_id = str_extract(basename(file), "\\d{8}")
  read_csv(file) %>%
    mutate(SITENO = site_id)
})

# get complete record basins
MACH = MACH %>% dplyr::filter(SITENO %in% basin_id)

MACH = MACH %>% mutate(YR = year(DATE), MNTH = month(DATE), DY = day(DATE))

# create function to convert the dates to water year based on month and year
# organizes by water year (shows as year and represents Oct to Sept)
  wYear = function(date) {
    ifelse(month(date) < 10, year(date), year(date)+1)}

# add column (WATERYR) to table using wYear function 
MACH = MACH %>% mutate(WYR = wYear(DATE))    
# add column (WATERYR) to table using wYear function 

# create a function that will make Jan and Feb of a given year associate with the 
# previous year since winter is December, January and February
# i.e. December 1980, January 1981, February 1981 will all have season year of 1980
# the remainder of the months, March - November will have the same year as the date
sYear = function(date) {
  ifelse(month(date) == 1 | month(date) == 2, year(date)-1, year(date))
       }
# add column called SEASONYR which uses the function 
MACH = MACH %>% mutate(SYR = sYear(DATE))

# add column called SEASON which will assign the season based on the month
MACH = MACH %>% 
  mutate(SEASON = case_when(
    MNTH %in% 9:11 ~ "Fall",
    MNTH %in% c(12, 1, 2) ~ "Winter",
    MNTH %in% 3:5 ~ "Spring",
      TRUE ~ "Summer"))

###############################################################################
#### MAPS ####
###############################################################################
# annual plots of variables at each site 
# using water years, 1981 to 2023 since 1980 and 2024 incomplete

site_info = read_csv("D:/University of Texas at Dallas/CombinedDataset/coordinates.csv")
csv_files = list.files("D:/University of Texas at Dallas/CombinedDataset/Formatted_data/MACH/TMIN_TMAX", full.names = TRUE)

output_dir = "F:/Maps/TMAX"

# us state boundaries
us_map = map_data("state")

# Function to get annual value
compute_annual = function(file) {
  data = read_csv(file, col_types = cols(SITENO = col_character(), DATE = col_character(), 
                                         TMIN = col_double(), TMAX = col_double()))
  
  # Convert DATE to Date type
  data = data %>%
    mutate(DATE = lubridate::ymd(DATE),  # Convert date format
          # YR = lubridate::year(DATE), 
           WYR = wYear(DATE)) %>%
    dplyr::select(SITENO, WYR, TMAX) %>% 
    group_by(SITENO, WYR) %>% 
    summarise(YR_TMAX = mean(TMAX, na.rm = TRUE), .groups = "drop")  
  
  return(data)
}

# Process all files in folder
plot_data = map_dfr(csv_files, compute_annual)

# Merge with site coordinates
merged_data = left_join(plot_data, site_info, by = "SITENO")

# Get unique years
years = unique(merged_data$WYR)

# Create and save a map for each year
for (yr in years) {
  yearly_data = merged_data %>% filter(WYR == yr)
  
  p = ggplot(yearly_data, aes(x = longitude, y = latitude, color = YR_TMAX)) +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +  
    geom_point(size = 2) +
  #  scale_color_viridis(option = "turbo", direction = -1, limits = c(0, 5200)) + # reverses color scale with blue high values, red low
    scale_color_viridis(option = "turbo", limits = c(0, 35)) +
    theme_minimal() +
    coord_fixed(1.3) +
    labs(title = paste("Annual Mean TMAX -", yr), color = "TMAX (C)")
  
  # Save as TIFF in the output folder
  png_filename <- file.path(output_dir, paste0("annual_tmax_", yr, ".png"))
  ggsave(filename = png_filename, plot = p, width = 8, height = 6, units = "in", dpi = 300)
  
  message("Saved: ", png_filename)  # Print a message confirming the save
}

###############################
# single climate attributes
attributes = read_csv("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/annual_climate.csv")
output_dir = "F:/Maps/SPEI"

spei = attributes %>% select(SITENO, YR, SPEI)
merged = left_join(spei, site_info, by = "SITENO")

# Create and save a map for each year
for (yr in years) {
  yearly_data = merged %>% filter(YR == yr)
  
  p = ggplot(yearly_data, aes(x = longitude, y = latitude, color = SPEI)) +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +  
    geom_point(size = 2) +
   scale_color_gradient(low = "red", high = "blue", limits = c(-1.30, 1.30)) +
    theme_minimal() +
    coord_fixed(1.3) +
    labs(title = paste("Annual SPEI -", yr), color = "SPEI")
  
  # Save as TIFF in the output folder
  tiff_filename <- file.path(output_dir, paste0("annual_SPEI_", yr, ".tiff"))
  ggsave(filename = tiff_filename, plot = p, device = "tiff", width = 8, height = 6, units = "in", dpi = 300)
  
  message("Saved: ", tiff_filename)  # Print a message confirming the save
}

###############################################################################
#### BUDYKO ####
###############################################################################
# Budyko formula (1974)
# evapotranspiration related to the aridity index (PET/P)
# Et/P = [PET/P*tanh(P/PET)(1-exp(-PEt/P))]^0.5
# PEt is potential evapotranspiration (available energy) and P is precipitation (available water)

#### Budyko curve ####

# blank Budyko curve boundaries

budyko_function = function(PET_P) {
  ifelse(PET_P == 0,0, 
         (PET_P * tanh(1/PET_P) * (1 - exp(-PET_P)))^0.5)
}

BudykoCurve = ggplot(data.frame(PETvP = c(0.01, 2)), aes(x = PETvP)) +
  stat_function(fun = budyko_function, color = "black", linewidth = 1.2, linetype = 2) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "red", linewidth = 1.2) +
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 1), color = "blue", linewidth = 1.2) +
  coord_cartesian(xlim = c(0,2)) + geom_vline(xintercept = 1, lty = 2) + theme_bw() +
  labs(x = "PET/P", y = "ET/P") +
  geom_text(data = data.frame(
    labels = c("Energy Limited", "Water Limited"), 
    x = c(0.7, 1.3), y = c(0.25, 0.25)), aes(x = x, y = y, label = labels)) 

#### FU EQUATION ####
# calculate w parameter using Fu equation (1981)
# E/P = 1 + PET/P - [1 + (PET/P)^w]^(1/w)

# get annual totals
annual = MACH %>% group_by(SITENO, WYR) %>% 
  reframe(AET = sum(AET), OBSQ = sum(OBSQ), PET = sum(PET), PRCP = sum(PRCP))

# drop water years 1980 and 2024 since incomplete 
annual = annual %>% filter(WYR != 1980 & WYR != 2024)

#### overall ####

# indices for all years on record using annual totals of precip, aet, pet, and obsq
indices = annual %>% mutate(ETvP = AET/PRCP, PETvP = PET/PRCP, QvP = OBSQ/PRCP) %>% 
     filter(if_all(everything(), ~!is.infinite(.) & !is.nan(.)))

# budyko calculate ET/P for curve using mean of indices per site
budyko = indices %>% group_by(SITENO) %>% reframe(ETvP = mean(ETvP), PETvP = mean(PETvP), QvP = mean(QvP))
# calculate evaporative index using mean aridity index 
budyko = budyko %>% mutate(EI = sqrt((1 - exp(-PETvP))*PETvP*tanh(1/PETvP)))

# plot budyko curve for data 
ggplot(data = budyko, aes(x = PETvP, y = EI)) + geom_line() + theme_bw()

# calculate w parameter value for each site using observed ETvP and aridity indices for all years  
Fu = indices %>% group_by(SITENO) %>% nls_table(ETvP ~ I(1 + PETvP - (1 + PETvP^(w))^(1/w)), mod_start = c(w=0.5))
Fu = Fu %>% dplyr::rename(w = b0) 

# join w parameter with all annual indices 
indices = indices %>% left_join(Fu, by = "SITENO")  
# calculate predicted ETvP using equation and w
indices = indices %>% mutate(pred_ETvP = 1 + PETvP - (1 + PETvP^w)^(1/w))

# calculate w parameter with observed runoff efficiency and aridity index 
# Fu equation rearranged based on Q/P = 1 - E/P
# Q/P = 1 - [1 + PET/P - (1 + (PET/P)^w)^1/w]
re = indices %>% group_by(SITENO) %>% nls_table(QvP ~ I(1-(1 + PETvP - (1 + PETvP^(w))^(1/w))), mod_start = c(w=0.5))
# join w (b0) parameter with all annual indices
indices = indices %>% left_join(re, by = "SITENO") 
# calculate predicted QvP using equation and w 
indices = indices %>% mutate(pred_QvP = 1 - (1 + PETvP - (1 + PETvP^b0)^(1/b0)))

# determine residuals from observed and predicted ETvP and QvP (observed - predicted)
indices = indices %>% mutate(res_QvP = QvP - pred_QvP, res_ETvP = ETvP - pred_ETvP)


# join mean indices with w parameter for each site 
Fu_w = Fu %>% left_join(budyko, by = "SITENO")
# plot evaporative vs aridity indices for each site, use Budyko curve with calculate EI
# color w parameter by range
p =ggplot() + geom_point(data = Fu_w, aes(x = PETvP, y = ETvP, 
                                     color = cut(w, 
      breaks = c(0.5,1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 10.0), 
        labels = c("0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-10")))) + 
  xlim(0,5) + ylim(0, 1) + 
  geom_line(data = Fu_w, aes(x = PETvP, y = EI)) + theme_bw() +
  labs(x = "Aridity Index (PET/P)", y = "Evaporative Index (ET/P)", 
       title = "Catchment Parameter ETvP (w)") + scale_color_manual(name = "w", 
                                               values = c("0.5-1" = "red",
                                                          "1-1.5" = "darkorange", 
                                                          "1.5-2" = "gold", 
                                                          "2-2.5" = "darkgreen", 
                                                          "2.5-3" = "green", 
                                                          "3-3.5" = "cyan2", 
                                                          "3.5-4" = "blue", 
                                                          "4-10" = "purple"))  

ggsave("D:/University of Texas at Dallas/Dissertation/Parameter.png", plot = p, device = "png", 
       width = 8, height = 6, units = "in", dpi = 300)

coords = read_csv("D:/University of Texas at Dallas/CombinedDataset/coordinates.csv")
# join lat lon for each site 
Fu_w = Fu_w %>% left_join(coords, by = "SITENO")

# map of w parameter values 
m = ggplot() + geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = Fu_w, aes(x = longitude, y = latitude,  
              color = cut(w, breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, Inf), 
                      labels = c("0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-10")))) + 
  scale_color_manual(name = "w", 
                     values = c("0.5-1" = "red",
                                "1-1.5" = "darkorange", 
                                "1.5-2" = "goldenrod1", 
                                "2-2.5" = "darkgreen", 
                                "2.5-3" = "green", 
                                "3-3.5" = "cyan2", 
                                "3.5-4" = "blue", 
                                "4-10" = "purple")) + 
  coord_fixed(1.3) + theme_minimal() + theme(axis.title = element_blank()) + 
  labs(x = "Longitude", y = "Latitude", title = "Distribution of parameter (ETvP)") 

ggsave(filename = "D:/University of Texas at Dallas/Dissertation/map_parameter.png", 
       plot = m, device = "png", width = 8, height = 6, units = "in", dpi = 300)

re_w = left_join(Fu_w, re, by = "SITENO")

budyko = budyko %>% mutate(QP = 1-sqrt((1 - exp(-PETvP))*PETvP*tanh(1/PETvP)))

r = ggplot() + geom_point(data = re_w, aes(x = PETvP, y = QvP, 
                                     color = cut(b0, 
      breaks = c(0.5,1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 10.0), 
        labels = c("0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-10")))) + 
  xlim(0,5) + ylim(0, 1) + 
  geom_line(data = budyko, aes(x = PETvP, y = QP)) + theme_bw() +
  labs(x = "Aridity Index (PET/P)", y = "Runoff Efficiency (Q/P)", 
       title = "Catchment Parameter QvP (w)") + scale_color_manual(name = "w", 
                                               values = c("0.5-1" = "red",
                                                          "1-1.5" = "darkorange", 
                                                          "1.5-2" = "gold", 
                                                          "2-2.5" = "darkgreen", 
                                                          "2.5-3" = "green", 
                                                          "3-3.5" = "cyan2", 
                                                          "3.5-4" = "blue", 
                                                          "4-10" = "purple"))  

ggsave("D:/University of Texas at Dallas/Dissertation/ParameterQP.png", plot = r, device = "png", 
       width = 8, height = 6, units = "in", dpi = 300)

# map of w parameter values 
m_r = ggplot() + geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = re_w, aes(x = longitude, y = latitude,  
              color = cut(b0, breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, Inf), 
                      labels = c("0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-10")))) + 
  scale_color_manual(name = "w", 
                     values = c("0.5-1" = "red",
                                "1-1.5" = "darkorange", 
                                "1.5-2" = "goldenrod1", 
                                "2-2.5" = "darkgreen", 
                                "2.5-3" = "green", 
                                "3-3.5" = "cyan2", 
                                "3.5-4" = "blue", 
                                "4-10" = "purple")) + 
  coord_fixed(1.3) + theme_minimal() + theme(axis.title = element_blank()) + 
  labs(x = "Longitude", y = "Latitude", title = "Distribution of parameter (QvP)") 

ggsave(filename = "D:/University of Texas at Dallas/Dissertation/map_parameterQP.png", 
       plot = m_r, device = "png", width = 8, height = 6, units = "in", dpi = 300)

# find difference in calculated w parameter using QvP compared to ETvP
catch_param = catch_param %>% mutate(diff = w - b0)
# get coordinates for each site 
catch_param = left_join(catch_param, coords, by = "SITENO")

w_diff = ggplot() + geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = catch_param, aes(x = longitude, y = latitude,  
              color = cut(diff, breaks = c(-10, -1, 0, 1, 10), 
                      labels = c("-10 to -1", "-1 to 0", "0 to 1", "1 to 10")))) + 
  scale_color_manual(name = "w", 
                     values = c("-10 to -1" = "red",
                                "-1 to 0" = "white", 
                                "0 to 1" = "darkgray", 
                                "1 to 10" = "orange")) + 
  coord_fixed(1.3) + theme_minimal() + theme(axis.title = element_blank()) + 
  labs(x = "Longitude", y = "Latitude", title = "Difference in parameter (ETvP - QvP)") 

ggsave(filename = "D:/University of Texas at Dallas/Dissertation/map_paramdiff.png", 
       plot = w_diff, device = "png", width = 8, height = 6, units = "in", dpi = 300)



ggplot(data = re_w, aes(x = w, y = b0)) + geom_point() +  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # 1:1 line
  stat_cor(method = "pearson", aes(label = paste(..rr.label..)), label.x = 0.1, label.y = 0.9) +
  theme_minimal()

# runoff ratio vs parameter
# find best fit line for QvP vs w parameter plot
# get w parameter for each site, keeping all years 
indices = indices %>% left_join(Fu, by = "SITENO")
# nonlinear least squares regression create fit curve and minimizing least squares between estimated and
# observed runoff ratio yielded goodness of fit 
line = nls(QvP ~ k*w^a, data = indices, start = list(k = 0.25, a = 0.25)) 
summary(line)
# k = 0.635661, a = -0.668209
indices$predicted = 0.635661*indices$w^(-0.668209)
# determine the difference of value from best fit line (actual to predicted)
# vertical distance 
indices$difference = indices$QvP - indices$predicted

# get mean difference for each site between observed and predicted QvP
diff = indices %>% group_by(SITENO) %>% reframe(difference = mean(difference), predicted = mean(predicted))
diff = diff %>% left_join(overall, by = "SITENO")

ggplot(data = diff, aes(x = w, y = QvP)) + geom_point(aes(color = cut(w, 
                                  c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, Inf), right = TRUE))) + 
  theme_bw() + labs(y = "Q/P", title = "Parameter w vs. Runoff Ratio") +
                                  xlim(1, 5) + ylim(0, 1) + scale_color_manual(name = "w", 
                                           values = c("(1,1.5]" = "red", 
                                                         "(1.5,2]" = "orange", 
                                                         "(2,2.5]" = "gold",
                                                         "(2.5,3]" = "green", 
                                                         "(3,3.5]" = "blue",
                                                         "(3.5,4]" = "purple", 
                                                         "(4,Inf]" = "black")) +
                                                  geom_line(aes(y = predicted))


# difference overall regardless of vertical direction
# Fu$percent_diff = abs(Fu$difference/Fu$predicted) *100

# get negative vertical differences, indicates a low runoff efficiency 
#anomQvP = subset(Fu, difference <=-0.1 & difference >=-0.4)

ggplot(data = diff, aes(x = w, y = QvP)) + geom_point(aes(color = diff)) +
  xlim(1, 5) + ylim(0, 1) + geom_line(aes(x = w, y = predicted)) + 
  scale_color_gradient2(limits = c(-0.40, -0.10)) + theme_bw() +
  labs(y = "Q/P", title = "Anomalous Gauges") 

budyko_map = left_join(diff, coords, by = "SITENO")

us_map = map_data("state")
ggplot() + geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = budyko_map, aes(x = longitude, y = latitude, shape = dataset, 
  color = ifelse(difference >=-0.4 & difference <= -0.1, "Anomalous", "Normal"))) +
  scale_color_manual(name = "Basin Classification", values = c("Normal" = "black", "Anomalous" = "red")) +
  scale_shape_manual(name = "Dataset", values = c("MOPEX" = 2, "CAMELS" = 17, "SAME" = 4)) +
  coord_fixed(1.3) + theme_minimal() + labs(x = "Longitude", y = "Latitude", 
   title = "Characterization of Runoff Efficiency 1981-2023") +
   theme(legend.title = element_blank()) 

ggsave(filename = "D:/University of Texas at Dallas/Dissertation/anomalousbasins.png", 
       plot = p, device = "png", width = 8, height = 6, units = "in", dpi = 300)


##### decadal plots ####
us_map = map_data("state")
# 1981-1991, 1991-2001, 2001-2011, 2011-2021

# filter by decade, calculate index for each year, remove any years with inf or nan 
decade = annual %>% filter(WYR %in% 2011:2021) %>% mutate(ETvP = AET/PRCP, PETvP = PET/PRCP, QvP = OBSQ/PRCP) %>% 
     filter(if_all(everything(), ~!is.infinite(.) & !is.nan(.)))

# calculate w parameter for each decade, use all water years so nls can fit data (more than one value per site)
Fu = decade %>% group_by(SITENO) %>% 
  nls_table(ETvP ~ I(1 + PETvP - (1 + PETvP^(w))^(1/w)), mod_start = c(w=0.5))

Fu = Fu %>% rename(w = b0)

# combine annual indices by decade with w parameter 
decade = decade %>% left_join(Fu, by = "SITENO")

# determine equation for best fit curve using w parameter for each water year in decade (get k and a values)
line = nls(QvP ~ k*w^a, data = decade, start = list(k = 1, a = 1)) 
summary(line)

# calculate predicted QvP for each year using equation and w parameter 

# 1981-1991
# k = 0.78551, a = -0.87653
decade$predicted = 0.78551*decade$w^(-0.87653)

# 1991-2001
# k = 0.67569, a = -0.69841
decade$predicted = 0.67569*decade$w^(-0.69841)

# 2001-2011
# k = 0.62511, a = -0.67392
decade$predicted = 0.62511*decade$w^(-0.67392)

# 2011-2021
# k = 0.45085, a = -0.27179
decade$predicted = 0.45085*decade$w^(-0.27179)

# get the difference between observed and predicted QvP for each year 
decade$difference = decade$QvP - decade$predicted 
decade = decade %>% drop_na()
# get mean difference between observed and predicted for each site 
mean_diff = decade %>% group_by(SITENO) %>% reframe(difference = mean(difference))
# join with site coordinates for plotting 
budyko_map = left_join(mean_diff, coords, by = "SITENO")

# create map of each site and identify locations with QvP 10% or greater difference from curve 
p = ggplot() + geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = budyko_map, aes(x = longitude, y = latitude, shape = dataset, 
  color = ifelse(difference >=-0.4 & difference <= -0.1, "Anomalous", "Normal"))) +
  scale_color_manual(name = "Basin Classification", values = c("Normal" = "black", "Anomalous" = "red")) +
  scale_shape_manual(name = "Dataset", values = c("MOPEX" = 2, "CAMELS" = 17, "SAME" = 4)) +
  coord_fixed(1.3) + theme_minimal() + labs(x = "Longitude", y = "Latitude", 
          title = "Characterization of Runoff Efficiency 2011-2021") + 
  theme(legend.title = element_blank()) 

ggsave(filename = "D:/University of Texas at Dallas/Dissertation/decade1121.png", 
       plot = p, device = "png", width = 8, height = 6, units = "in", dpi = 300)

###########################
library(ggpubr)
# plot observed vs predicted to get r squared, linear 
ggplot(data = Fu, aes(x = predicted, y = QvP)) + geom_point() + theme_bw() +
  labs(x = "Predicted Q/P", y = "Actual Q/P", title = "Predicted vs Actual Q/P") + 
  geom_smooth(method = lm, se = FALSE) + stat_regline_equation(aes(label = ..rr.label..))

library(nleqslv)
# express as a function of w, rearrange equation so it is equal to zero
# defines equation with w as input and returns the difference between left hand (E/P) and right hand sides
# tries to find value of w that makes f(w, E, P, PET) = 0
solve_w = function(E, P, PET) {
  f = function(w) {
  lhs = E/P
  rhs = 1 + PET/P - (1 + (PET/P)^w)^(1/w)
  return(lhs - rhs)
}

initial_w = 1
result = nleqslv(x = initial_w, fn = f)

# get w values
return(result$x)
}

result = annual %>% group_by(SITENO, YR) %>% 
  mutate(w = solve_w(AET, PRCP, PET))
