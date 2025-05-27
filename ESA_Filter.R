library(tidyverse)
library(sf)
library(mapdata)
library(rstatix)
library(SPEI)

##################################################
# read in matlab esa .dat file as .txt file
lines = readLines("F:/Matlab/esaoutput.txt")

# create empty list
site_data = list()

# set origin date for custom day count used in runQEPanal.m
origin_date = as.Date("1980-01-01")
origin_value = 723181.5

i = 1

while(i <= length(lines)) {
  header = strsplit(lines[i], "\\s+")[[1]] # read header line (siteno, lat, lon, elev, area, samples, dataset)
  siteno = header[1] # get site number
  
  data_lines = lines[(i+1):(i+88)] # get 88 lines of ESA values
  df = read.table(text = data_lines, header = FALSE, fill = TRUE)
  
 if (ncol(df) >= 2) {
    names(df)[1:2] <- c("DATE", "ESA")  # Rename first two columns
    df <- df[, 1:2]  # Keep only DATE and ESA
    df$SITENO <- siteno  # Add site number
    df$DATE = origin_date + (as.numeric(df$DATE) - origin_value)
    
    site_data[[length(site_data) + 1]] <- df
  }
  i = i + 89 #loop through blocks (sites)
}

final_df = bind_rows(site_data)

##################
## zero crossings 
##################
count_zero = function(df) {
  df %>% arrange(DATE) %>% 
    mutate(
    prev_ESA = lag(ESA), 
    transition = case_when(
    prev_ESA > 0 & ESA < 0 ~ "arid_to_humid", 
    prev_ESA < 0 & ESA > 0 ~ "humid_to_arid", 
    TRUE ~ NA_character_
  )) %>% 
  summarise(
    arid_to_humid = sum(transition == "arid_to_humid", na.rm = TRUE), 
    humid_to_arid = sum(transition == "humid_to_arid", na.rm = TRUE)
  )
}

transition = final_df %>% 
  group_by(SITENO) %>% 
  group_modify(~ count_zero(.x)) %>% 
  ungroup()

site_info = read_csv("D:/University of Texas at Dallas/CombinedDataset/coordinates.csv")
us_map = map_data("state")

transition = left_join(transition, site_info, by = "SITENO")

esa_map = transition %>% 
  mutate(arid_humid = cut(arid_to_humid, 
                        breaks = c(-1, 0, 2, 4, 6, 8, 10, 12, 14, 16), 
                        labels = c("0", "1–2", "3–4", "5–6", "7–8", "9–10", "11–12", "13–14", "15–16")))    

ggplot(data = esa_map, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(aes(color = arid_humid), size = 3) +  theme_minimal() + coord_fixed(1.3) +
  scale_color_brewer(palette = "YlGnBu", name = "Arid to Humid\nTransitions")

esa_map = esa_map %>% 
  mutate(humid_arid = cut(humid_to_arid, 
                        breaks = c(-1, 0, 2, 4, 6, 8, 10, 12, 14, 16), 
                        labels = c("0", "1–2", "3–4", "5–6", "7–8", "9–10", "11–12", "13–14", "15–16")))  

ggplot(data = esa_map, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(aes(color = humid_arid), size = 3) +  theme_minimal() + coord_fixed(1.3) +
  scale_color_brewer(palette = "RdYlGn", direction = -1, name = "Humid to Arid\nTransitions")

transition = transition %>% mutate(total = arid_to_humid + humid_to_arid)

esa_map = transition %>% 
  mutate(zero_cross = cut(total, 
                        breaks = c(-1, 0, 4, 8, 12, 16, 20, 24, 28, 32), 
                        labels = c("0", "1–4", "5–8", "9–12", "13–16", "17–20", "21–24", "25–28", "29–32")))  

ggplot(data = esa_map, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(aes(color = zero_cross), size = 3) +  theme_minimal() + coord_fixed(1.3) +
  scale_color_brewer(palette = "Spectral", name = "Zero Crossings")





################################################
### ESA ###
################################################
dir_path = "F:/Matlab/MACH_daily"

# Read MACH file
  rdMACH <- function(filename) {
    data <- read_csv(filename, col_types = cols(
      SITENO = col_character(),
      DATE = col_date(),
      PRCP = col_double(),
      PET = col_double(),
      AET = col_double()
    ))
    
    Y <- as.matrix(data[, c("PRCP", "PET", "AET")])
    DT <- as.Date(data$DATE)
    SN <- unique(data$SITENO)
    
    if (length(SN) > 1) stop("Multiple site numbers detected")
    if (length(SN) == 0) stop("No valid site number found")
    
    return(list(Y = Y, DT = DT, SN = SN[1]))
  }


#########################################
# QEP function
QEP <- function(A) {
  n <- nrow(A)
  m <- ncol(A)

# Budyko analysis
# define indices 
  EP <- A[,3] / A[,1]
  EpP <- A[,2] / A[,1]

# evaporation state angle    
  delta <- sin(135/2 * pi/180) / sqrt(2)
  esa <- numeric(n)
  
 for (i in 1:n) {
    nhat <- c(EpP[i] - 1, EP[i] - 1) / sqrt(sum((c(EpP[i] - 1, EP[i] - 1))^2))
    esa[i] <- (atan2(nhat[2], nhat[1]) * 180/pi + 67.5) / 67.5
  }
  
  return(list(esa = esa, delta = delta, EP = EP, EpP = EpP, Ynew = A))
}

#########################################
# Lowpass FIR filter
firsmth <- function(y, dt, fc, dftb) {
  y <- as.matrix(y)
  nsam <- nrow(y)
  nchn <- ncol(y)
  
  # Determine filter length
  nfir <- 4 / (dt * dftb)
  nfir <- min(nfir, nsam)
  nfir <- floor(nfir)
  if (nfir %% 2 == 0) nfir <- nfir - 1  # Make it odd
  
  n <- 0:(nfir - 1)
  nfiro2 <- (nfir - 1) / 2
  n_centered <- n - nfiro2
  
  # Define Hamming window
  hamming_window <- 0.53 - 0.46 * cos(2 * pi * n / (nfir - 1))
  
  # Define sinc filter
  sinc_func <- function(x) {
    y <- rep(0, length(x))
    y[x == 0] <- 1
    y[x != 0] <- sin(pi * x[x != 0]) / (pi * x[x != 0])
    return(y)
  }
  
  flp <- 2 * fc * dt * sinc_func(2 * fc * dt * n_centered) * hamming_window
  
  # Initialize output
  s <- matrix(0, nrow = nsam, ncol = nchn)
  
  # Convolve each channel and apply zero-phase shift
  for (i in 1:nchn) {
    stmp <- convolve(y[, i], flp, type = "open")  # full convolution
    start_idx <- nfiro2 + 1
    end_idx <- length(stmp) - nfiro2
    s[, i] <- stmp[start_idx:end_idx]
  }
  
  return(list(s = s, flp = flp))
}

  
#########################################    
# Equivalent of QEPanal.m

QEPanal <- function(stacod, Y, DT, head, fc) {
  tyr <- 365.242   # tropical year (days)
  dt <- 1   # sample interval (days)
  fNfac <- 2 # Nyquist frequency factor for decimation
  
  nd <- nrow(Y)
  T <- 0:(nd-1)
  
# Set headers
  head <- c("P", "Ep", "E")
  
  # Lowpass filter and decimate

  # Transition bandwidth
  df <- 1/(nd*dt)
  dftb <- 4*df
  
  # Remove trends and make zero mean
  Ytmp <- matrix(0, nd, 3)
  plin <- matrix(0, 3, 2)
  for (j in 1:3) {
    plin[j,] <- lm(Y[,j] ~ T)$coefficients
    Ytmp[,j] <- Y[,j] - plin[j,1] - plin[j,2]*T
  }
  
  filt <- firsmth(Ytmp, dt, fc/tyr, dftb)
  Ytmp <- filt$s
  flp <- filt$flp
  
  # Equivalent width of filter
  EW <- sum(flp)/max(flp)
  
  # Set new Nyquist frequency
  fNnew <- fNfac*fc/tyr
  dtnew <- 1/(2*fNnew)
  
#ndec <- max(1, round(dtnew/dt)) # Ensure ndec >= 1
ndec = max(1, ceiling(dtnew/dt))
#  ndd <- max(1, floor(nd/ndec))   # Ensure at least one output sample
ndd = max(1, floor((nd - 1)/ndec) + 1)
Ynew <- matrix(0, ndd, 3)
Tnew <- numeric(ndd)
DTnew <- numeric(ndd)

j <- 1
for (i in seq(1, nd, by=ndec)) {
  if (i > nd || j > ndd) break
  Tnew[j] <- T[i]
  DTnew[j] <- DT[i] + 719529 # Convert Unix epoch (seconds) to MATLAB datenum (days)
  Ynew[j,] <- Ytmp[i,]
  j <- j + 1
}
# Ensure Ynew has valid data
if (j == 1) {
  Ynew[1,] <- Ytmp[1,]
  Tnew[1] <- T[1]
  DTnew[1] <- DT[1]  + 719529
}
  
  # Restore trends and mean
  for (j in 1:3) {
    Ynew[,j] <- Ynew[,j] + plin[j,1] + plin[j,2]*Tnew
  }
  
  # Convert mean values to window sums
  Ynew <- EW * Ynew
  
  A <- Ynew
  DTa <- DTnew
  
  # Budyko analysis
  result <- QEP(A)
  
  return(list(esaa=result$esa, DTa=DTa, delta=result$delta, 
              EPa=result$EP, EpPa=result$EpP, Ynew = A))
}

#########################################
# Equivalent of runQEPanal.m
runQEPanal <- function(fc, dir_path, output_file = "qep_output.csv") {
  library(readr)
  
  # Validate directory
  path <- dir_path
  if (!dir.exists(path)) stop("Directory does not exist")
  
  # Compile list of *.csv files
  filelst <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  nfile <- length(filelst)
  
  if (nfile == 0) stop("No *.csv files were found")
  
  # Initialize results data frame
  results <- data.frame()
  
  for (i in 1:nfile) {
    filename <- basename(filelst[i])
    stacod <- sub("\\.csv$", "", filename)
    
    # Read data
    dat <- rdMACH(filelst[i])
    X <- dat$Y
    DT <- dat$DT + 719529 # Convert R days to MATLAB datenum
    head <- c("P", "Ep", "E")
    
    # Run QEP analysis
    result <- QEPanal(stacod, X, DT, head, fc)
    
    # Combine results
    file_results <- data.frame(
      SITENO = rep(stacod, length(result$esaa)),
      ESA = result$esaa,
      P = result$Ynew[,1],  # Filtered/decimated P
      Ep = result$Ynew[,2],
      E = result$Ynew[,3],
      EP = result$EPa,
      EpP = result$EpPa
    )
    results <- rbind(results, file_results)
  }
  
  # Write to CSV
  write_csv(results, output_file)
  
  return(results)
}


runQEPanal(fc = 0.5, dir_path)

# Example usage
# dir_path <- "path/to/your/csv/files"
# runQEPanal(fc = 0.5, dir_path)  # fc = 0.5 for semiannual sampling


# Equivalent of mkyearly.m (not used for fc = 0.5, included for completeness)
mkyearly <- function(t, X) {
  if (!is.vector(X)) stop("X must be a vector")
  ndy <- length(X)
  tv <- as.Date(t)
  yrlst <- year(tv[1])
  Xyr <- numeric()
  tyr <- as.Date(character())
  ntyr <- numeric()
  yrsum <- 0
  nyr <- 0
  iyr <- 1
  
  for (i in 2:ndy) {
    yr <- year(tv[i])
    if (yr != yrlst) {
      if (nyr >= 364) {
        Xyr <- c(Xyr, yrsum)
        tyr <- c(tyr, as.Date(sprintf("%d-03-15", yrlst)))
        ntyr <- c(ntyr, nyr)
      }
      iyr <- iyr + 1
      nyr <- 1
      yrsum <- X[i]
      yrlst <- yr
    } else {
      yrsum <- yrsum + X[i]
      nyr <- nyr + 1
      yrlst <- yr
    }
  }
  
  if (nyr >= 364) {
    Xyr <- c(Xyr, yrsum)
    tyr <- c(tyr, as.Date(sprintf("%d-03-15", yrlst)))
    ntyr <- c(ntyr, nyr)
  }
  
  return(list(tyr = tyr, Xyr = Xyr, ntyr = ntyr))
}


################################################
### SPEI ###
################################################
# read esa spreadsheet 
esa_data = read_csv("F:/qep_output.csv")

esa_data = esa_data %>% mutate(SITENO = str_extract(SITENO, "\\d{8}")) %>% 
  rename(PRCP = P, PET = Ep, AET = E)

esa_data = esa_data %>% group_by(SITENO) %>% 
  mutate(DATE = as.Date("1980-01-01") + (row_number() - 1) * 183) %>% ungroup()

## SPEI (standardized precipitation evapotranspiration index) ##
# ran test on single site with vector of wb data and scale of 1, same results as function
# each point with wb already represents 6 months
esa_data = esa_data %>% mutate(WB = PRCP - PET)

calc_spei = function(wb) {
  spei_result = spei(wb, scale = 1)
  return(spei_result$fitted)
}

spei_data = esa_data %>% group_by(SITENO) %>% mutate(spei_6 = calc_spei(WB)) %>% ungroup()

esa_spei = spei_data %>% group_by(SITENO) %>% 
  reframe(esa_spei = cor(ESA, spei_6, use = "complete.obs"))

cor_lag = spei_data %>% group_by(SITENO) %>% 
  reframe(ccf_result = list(data.frame(
        lag = ccf(ESA, spei_6, lag.max = 2, type = "correlation", plot = FALSE)$lag,
        cor_value = ccf(ESA, spei_6, lag.max = 2, type = "correlation", plot = FALSE)$acf))) %>% 
  tidyr::unnest(ccf_result) %>%
  filter(!is.na(cor_value))


# Summarize correlations by lag
cor_summary_spei <- cor_lag %>%
   group_by(lag) %>%
   summarise(
     mean_cor = mean(cor_value, na.rm = TRUE),
     median_cor = median(cor_value, na.rm = TRUE),
     sd_cor = sd(cor_value, na.rm = TRUE), 
     prop_significant = mean(abs(cor_value) > 0.3)) # proportion of basins with correlation values greater

# look at correlation at each lag 
ggplot(cor_lag, aes(x = lag, y = cor_value)) +
  geom_boxplot(aes(group = lag)) +
  labs(title = "ESA-SPEI Cross-Correlation by Lag Across 737 Sites",
       x = "Lag (Periods, 6 months each)", y = "Correlation") + theme_minimal()
##############################################
### PDSI ###
##############################################
input_folder = "D:/University of Texas at Dallas/CombinedDataset/gridMET"
# List all CSV files
file_list <- list.files(input_folder, pattern = "^basin_\\d{8}_pdsi\\.csv$", full.names = TRUE)

# Function to read and summarize a single file
process_pdsi_file <- function(file) {

  # Read file
  df <- read_csv(file, col_types = cols(
    SITENO = col_character(),
    DATE = col_date(),
    PDSI = col_double()
    ))
    
    # Add year and half-year period
  df <- df %>%
    mutate(
      Year = year(DATE),
      Period = if_else(month(DATE) <= 6, "H1", "H2")
    ) %>%
    group_by(Year, Period) %>%
    slice(which.min(abs(DATE - ymd(paste0(Year, if_else(Period == "H1", "-01-01", "-07-01")))))) %>%
    ungroup()
  
  # Create a standard DATE column (Jan 1 and Jul 1)
  df <- df %>%
    mutate(DATE = ymd(paste0(Year, if_else(Period == "H1", "-01-01", "-07-01")))) %>%
    select(SITENO, DATE, PDSI) 
  
  return(df)
}

# Process all files and bind results
all_pdsi_summary <- map_dfr(file_list, process_pdsi_file)

# replace date column to match the esa data column exactly 
all_pdsi_summary = all_pdsi_summary %>% group_by(SITENO) %>% 
  mutate(DATE = as.Date("1980-01-01") + (row_number() - 1) * 183) %>% ungroup()


pdsi_esa = esa_data %>% left_join(all_pdsi_summary, by = c("SITENO", "DATE"))

# determine correlations using lag 
cor_lag = pdsi_esa %>% group_by(SITENO) %>% 
  reframe(ccf_result = list(data.frame(
        lag = ccf(ESA, PDSI, lag.max = 2, type = "correlation", plot = FALSE)$lag,
        cor_value = ccf(ESA, PDSI, lag.max = 2, type = "correlation", plot = FALSE)$acf))) %>% 
  tidyr::unnest(ccf_result) %>%
  filter(!is.na(cor_value))

# Summarize correlations by lag
cor_summary_pdsi <- cor_lag %>%
   group_by(lag) %>%
   summarise(
     mean_cor = mean(cor_value, na.rm = TRUE),
     median_cor = median(cor_value, na.rm = TRUE),
     sd_cor = sd(cor_value, na.rm = TRUE), 
     prop_significant = mean(abs(cor_value) > 0.3)) # proportion of basins with correlation values greater

# look at correlation at each lag 
ggplot(cor_lag, aes(x = lag, y = cor_value)) +
  geom_boxplot(aes(group = lag)) +
  labs(title = "ESA-PDSI Cross-Correlation by Lag Across 737 Sites",
       x = "Lag (Periods, 6 months each)", y = "Correlation") + theme_minimal()

combined = bind_rows(cor_summary_pdsi, cor_summary_spei)

ggplot(combined, aes(x = lag, y = mean_cor, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_cor - sd_cor, ymax = mean_cor + sd_cor), 
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Mean Correlation of ESA with SPEI and PDSI by Lag", 
       x = "Lag (6-month steps)", y = "Mean Correlation") +
  theme_minimal()

####################################################
### BUDYKO ###
####################################################
# assess how well ESA reflects hydrologic partitioning of P into E and Q

# correlation with budyko variables
cor_results <- esa_data %>%
  group_by(SITENO) %>%
  reframe(cor_ESA_EP = cor(ESA, EP, use = "complete.obs"),
    cor_ESA_EpP = cor(ESA, EpP, use = "complete.obs"))

# compute budyko evaporative index
esa_data = esa_data %>% 
  mutate(EI = sqrt((1 - exp(-EpP))*EpP*tanh(1/EpP)))
esa_data = esa_data %>% mutate(Budyko_dev = abs(EP - EI))

dev_summary = esa_data %>% group_by(SITENO) %>% 
  reframe(mean_dev = mean(Budyko_dev), mean_ESA = mean(ESA))

cor(dev_summary$mean_dev, dev_summary$mean_ESA, use = "complete.obs")


cv_results = esa_data %>% group_by(SITENO) %>% 
  reframe(CV_ESA = sd(ESA) / mean(ESA), 
          CV_PRCP = sd(PRCP) / mean(PRCP), 
          CV_AET = sd(AET) / mean(AET), 
          CV_PET = sd(PET) / mean(PET))

long_term = esa_data %>% group_by(SITENO) %>% 
  reframe(ESA = mean(ESA), EP = mean(EP), EpP = mean(EpP))

long_term = long_term %>% filter(EP <= 1.0)

ggplot() +
  geom_line(data = budyko_df, aes(x = PET_P, y = E_P), color = "black", linetype = "dashed") +
  geom_point(data = long_term, aes(x = EpP, y = EP, color = ESA), size = 2) +
  scale_color_viridis_c(option = "plasma", name = "Mean ESA") +
  labs(title = "Mean E/P vs. PET/P by Watershed",
       x = "PET/P (Aridity Index)", y = "E/P (Evaporative Index)") +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") + # Energy limit
  geom_hline(yintercept = 1, linetype = "dotted") # Water limit

# example analyses
sample_site = esa_data %>% filter(SITENO == "07340300")
ggplot(sample_site, aes(x = EpP, y = EP, color = ESA)) +
  geom_point() + geom_path(alpha = 0.5) +
  scale_color_viridis_c(option = "plasma", name = "ESA") + xlim(0,2) + ylim(0,1) +
#  geom_line(data = budyko_df, aes(x = PET_P, y = E_P), color = "black", linetype = "dashed") +
  labs(title = "Biannual E/P vs. PET/P for 07340300", x = "PET/P", y = "E/P") +
  theme_minimal()

########################
# define switches 
# count number of switches between each sampling point 
# identify the 5 year periods beginning with 1980 
# 9 periods (1980-1985, 1985-1990, etc)
switch = esa_data %>% group_by(SITENO) %>% 
   mutate(
    Switch = as.integer((ESA > 0 & lag(ESA) < 0) | (ESA < 0 & lag(ESA) > 0)),
    Switch = replace_na(Switch, 0),
    Year = lubridate::year(DATE), Period = floor((Year - 1980) / 5) + 1) %>%  ungroup()

# summarise number of switches in each period 
switch_trend <- switch %>%
  group_by(SITENO, Period) %>%
  summarise(Switches = sum(Switch), .groups = "drop")

# test for trend using Poisson regression for count data 
library(lme4)
per_site_models <- switch_trend %>%
  group_by(SITENO) %>%
  do(tidy(glmer(Switches ~ Period + (1|SITENO), family = poisson, data = .))) %>%
  ungroup()





# Identify switches
df[, Switch := as.integer((ESA > 0 & shift(ESA, n = 1, type = "lag") < 0) |
                         (ESA < 0 & shift(ESA, n = 1, type = "lag") > 0)), by = SITENO]
df[is.na(Switch), Switch := 0] # First period has no lag

df[, Year := year(DATE)]
df[, Period := floor((Year - 1980) / 5) + 1] # 5-year periods: 1980-1984, 1985-1989, ...
switch_trend <- df[, .(Switches = sum(Switch)), by = .(SITENO, Period)]


# over all basins 
# 1|SITENO random intercept for each of the 737 watersheds, capturing site specific variation 
model_switch <- glmer(Switches ~ Period + (1|SITENO), family = poisson, data = switch_trend)
summary(model_switch)


# analyze ESA value trends , fit linear models for trends
# compute slope of ESA vs time with positive slopes indicating aridification and negative humidification
df = esa_data %>% select(SITENO, ESA, DATE)
# use data table
setDT(df)
df[, Period_Index := seq_len(.N), by = SITENO] # 1 to 88

trends <- df[, {
  mod <- lm(ESA ~ Period_Index)
  slope <- coef(mod)[2]
  p_value <- summary(mod)$coefficients[2, 4]
  mean_esa <- mean(ESA, na.rm = TRUE)
  .(Slope = slope, P_value = p_value, Mean_ESA = mean_esa)
}, by = SITENO]

# classify trends by slope, trend direction, flag significant trends with p <0.05
trends[, Trend := fifelse(Slope > 0, "Aridification", "Humidification")]
trends[, Significant := P_value < 0.05]

# Summary statistics
summary(trends$Slope) # Distribution of slopes
prop_arid <- mean(trends$Slope > 0) # Proportion aridifying
prop_humid <- mean(trends$Slope < 0) # Proportion humidifying
prop_sig_arid <- mean(trends$Slope > 0 & trends$Significant) # Significant aridification
prop_sig_humid <- mean(trends$Slope < 0 & trends$Significant) # Significant humidification
cat("Proportion of sites aridifying:", prop_arid, "\n")
cat("Proportion of sites humidifying:", prop_humid, "\n")
cat("Proportion with significant aridification (p<0.05):", prop_sig_arid, "\n")
cat("Proportion with significant humidification (p<0.05):", prop_sig_humid, "\n")

# Mean slope magnitude by trend direction
trends[, .(Mean_Slope_Magnitude = mean(abs(Slope))), by = Trend]

switch_summary <- switch_trend[, .(Mean_Switches_Per_Period = mean(Switches)), by = SITENO]
output <- merge(switch_summary, trends, by = "SITENO")

output = merge(output, site_info, by = "SITENO")
cor.test(output$longitude, output$Slope) # Test if trends correlate with longitude


library(cluster)
# Pivot to wide format (one row per SITENO, 88 ESA columns)
df_wide <- dcast(df, SITENO ~ Period_Index, value.var = "ESA")

# Remove SITENO for clustering and handle NAs (if any)
esa_matrix <- as.matrix(df_wide[, -1])
esa_matrix[is.na(esa_matrix)] <- mean(esa_matrix, na.rm = TRUE) # Impute NAs with mean

# Standardize
esa_scaled <- scale(esa_matrix)

# K-means with 12 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(esa_scaled, centers = 12, nstart = 25)
df_wide$Cluster <- kmeans_result$cluster

# combine clusters and trends 
trends <- merge(trends, df_wide[, .(SITENO, Cluster)], by = "SITENO")

# analyze trends by cluster, summarise slopes, trend directions, and significance within each cluster
cluster_summary <- trends[, .(
  N_Sites = .N,
  Mean_Slope = mean(Slope),
  Prop_Arid = mean(Slope > 0),
  Prop_Humid = mean(Slope < 0),
  Prop_Sig_Arid = mean(Slope > 0 & Significant),
  Prop_Sig_Humid = mean(Slope < 0 & Significant),
  Mean_ESA = mean(Mean_ESA)
), by = Cluster]

df <- merge(df, df_wide[, .(SITENO, Cluster)], by = "SITENO")

# Loop through clusters
for (cl in 1:12) {
  # Subset data for cluster
  cluster_data <- df[Cluster == cl]
  cluster_data[, Date := as.Date("1980-01-01") + (Period_Index - 1) * 183]

  cluster_mean <- cluster_data[, .(Mean_ESA = mean(ESA, na.rm = TRUE)), by = Period_Index]
  cluster_mean[, Date := as.Date("1980-01-01") + (Period_Index - 1) * 183]
  
  # Plot A: All basins' ESA time series
plot_a <- ggplot(cluster_data, aes(x = Date, y = ESA, group = SITENO)) +
  geom_line(alpha = 0.3, linewidth = 0.5) +
  ylim(-1, 1.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2025-01-01"), by = "5 years"),
    labels = scales::date_format("%Y"),
    limits = as.Date(c("1980-01-01", "2025-01-01"))) +
  labs(title = paste("Cluster", cl, ": ESA Time Series for All Basins"), y = "ESA") +
  theme(axis.title.x = element_blank()) + theme_minimal() + theme(legend.position = "none")
  
  # Plot B: Mean ESA time series
  plot_b <- ggplot(cluster_mean, aes(x = Date, y = Mean_ESA)) +
    geom_line(color = "blue", linewidth = 1) + ylim(-1, 1.25) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2025-01-01"), by = "5 years"),
    labels = scales::date_format("%Y"),
    limits = as.Date(c("1980-01-01", "2025-01-01"))) +
    labs(title = paste("Cluster", cl, ": Mean ESA Time Series"), y = "Mean ESA") +
  theme(axis.title.x = element_blank()) + theme_minimal() + theme(legend.position = "none")
  
  # Combine plots
  library(patchwork)
  combined_plot <- plot_a / plot_b
  
  # Save as PNG
  ggsave(filename = sprintf("cluster_plots/cluster_%02d_plots.png", cl),
         plot = combined_plot, width = 8, height = 8, dpi = 300)
}


sites_sf <- st_as_sf(site_info, coords = c("longitude", "latitude"), crs = 4326)
sites_sf <- merge(sites_sf, df_wide[, .(SITENO, Cluster)], by = "SITENO")
sites_sf$Cluster <- factor(sites_sf$Cluster, levels = 1:12)

# Create map
map_plot <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_sf(data = sites_sf, aes(color = Cluster), size = 2) +
    scale_color_viridis_d(option = "turbo", name = "Cluster") +
  labs(title = "Watershed Sites Colored by Cluster (1–12)") +
  theme_minimal() +
  theme(legend.position = "right")


climate = merge(esa_data, group, by = "SITENO")
setDT(climate)
climate_trends = climate[, .(
  Mean_PRCP = mean(PRCP, na.rm = TRUE),
  Mean_PET = mean(PET, na.rm = TRUE),
  Mean_EpP = mean(EpP, na.rm = TRUE)
), by = .(Cluster, DATE)]

climate_trends_long = melt(climate_trends, 
                            id.vars = c("Cluster", "DATE"), 
                            measure.vars = c("Mean_PRCP", "Mean_PET", "Mean_EpP"),
                            variable.name = "Variable", 
                            value.name = "Value")

ggplot(climate_trends_long, aes(x = DATE, y = Value, color = factor(Cluster))) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, 
             labeller = as_labeller(c(Mean_PRCP = "Precipitation (mm)", 
                                      Mean_PET = "PET (mm)", 
                                      Mean_EpP = "Aridity (PET/P)"))) +
  labs(title = "Trends in PRCP, PET, and Aridity by Cluster (1981-2025)",
       x = "Date", y = "Value", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Compute slopes for each variable by cluster
slopes <- climate_trends[, .(
  Slope_PRCP = lm(Mean_PRCP ~ DATE)$coefficients[2],
  Slope_PET = lm(Mean_PET ~ DATE)$coefficients[2],
  Slope_EpP = lm(Mean_EpP ~ DATE)$coefficients[2],
  Pvalue_PRCP = summary(lm(Mean_PRCP ~ DATE))$coefficients[2, 4],
  Pvalue_PET = summary(lm(Mean_PET ~ DATE))$coefficients[2, 4],
  Pvalue_EpP = summary(lm(Mean_EpP ~ DATE))$coefficients[2, 4]
), by = Cluster]

# Annualize slopes (since biannual data, multiply by 2 to get per-year change)
slopes = slopes %>% mutate(
  Slope_PRCP_Annual = Slope_PRCP * 2,
  Slope_PET_Annual = Slope_PET * 2,
  Slope_EpP_Annual = Slope_EpP * 2
)
#############################################################
### Land cover, soil, climate change
############################################################
group = df_wide %>% select(SITENO, Cluster)

soil = read_csv("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/soil_statsgo.csv")
soil$SITENO = str_pad(as.character(soil$SITENO), width = 8, side = "left", pad = "0")

gauge = unique(group$SITENO)

soil1 = soil %>% filter(SITENO %in% gauge)
soil1 = merge(soil1, group[, .(SITENO, Cluster)], by = "SITENO")

setDT(soil1)

attributes = c("wtdepannmin", "aws025wta", "aws050wta", "aws0100wta", "aws0150wta", 
          "drainagecl", "sandtotal", "silttotal", "claytotal", "om", "ksat", "awc", "texdesc")

# Split attributes into numeric and categorical
numeric_attrs <- attributes[sapply(soil1[, ..attributes], is.numeric)]
cat_attrs <- attributes[sapply(soil1[, ..attributes], is.character) | sapply(soil1[, ..attributes], is.factor)]

# Area-weighted mean for numeric attributes
weighted_summary_numeric <- soil1[, 
  lapply(.SD, function(x) weighted.mean(x, percent_area, na.rm = TRUE)), 
  by = .(Cluster, SITENO), 
  .SDcols = numeric_attrs
]

# Area-weighted mode for categorical attributes
mode_weighted <- function(x, w) {
  w <- w[!is.na(x)]
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  t <- table(x)
  w_sum <- tapply(w, x, sum, na.rm = TRUE)
  names(w_sum)[which.max(w_sum)]
}

weighted_summary_cat <- soil1[, 
  lapply(.SD, function(x) mode_weighted(x, percent_area)), 
  by = .(Cluster, SITENO), 
  .SDcols = cat_attrs
]

# Combine summaries
weighted_summary <- merge(weighted_summary_numeric, weighted_summary_cat, by = c("Cluster", "SITENO"))

cluster_summary <- weighted_summary[, 
  lapply(.SD, function(x) list(
    Min = min(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )), 
  by = Cluster, 
  .SDcols = numeric_attrs
]

# Unnest the list columns into separate Min, Mean, Max columns
cluster_summary <- cluster_summary[, 
  lapply(.SD, function(x) unlist(x)), 
  by = Cluster
]

#################
land_cover = read_csv("D:/University of Texas at Dallas/CombinedDataset/AttributesMACH/LC_change/landcover_allperiods.csv")
land_cover = merge(land_cover, group, by = "SITENO")
setDT(land_cover)

setDT(esa_data)
esa_data[, Year := year(DATE)]
# create 5 year increments for dates
esa_data[, Period := cut(Year, breaks = seq(1980, 2025, by = 5), include.lowest = TRUE, 
                        labels = paste0(seq(1980, 2020, by = 5), "-", seq(1985, 2025, by = 5)))]

# Compute ESA trends per SITENO (slope over time) for each 5 year period
esa_trends <- esa_data[, {
  mod <- lm(ESA ~ as.numeric(DATE))
  list(Slope_ESA = coef(mod)[2] * 365.25 * 2, # Annualize (days to years, biannual steps)
       Pvalue_ESA = summary(mod)$coefficients[2, 4])
}, by = .(SITENO, Period)]


# Reshape land cover data to long format
land_long <- melt(land_cover, 
                  id.vars = c("SITENO", "Cluster", "category"),
                  measure.vars = patterns("^percent_"),
                  variable.name = "Year",
                  value.name = "Percent_Coverage") %>%
  mutate(Year = as.numeric(gsub("percent_", "", Year)),
         Period = cut(Year, breaks = seq(1985, 2025, by = 5), include.lowest = TRUE,
                      labels = paste0(seq(1985, 2020, by = 5), "-", seq(1990, 2025, by = 5))))


# Calculate land cover change for each 5-year period
land_change <- land_long[, .(Change = Percent_Coverage - shift(Percent_Coverage, type = "lag")),
                        by = .(SITENO, Cluster, category)][!is.na(Change)]

# Create a helper data.table with Period assignments for the second observation
period_map <- land_long[, .(Period = Period[2:.N]), by = .(SITENO, Cluster, category)]

# Merge Period with land_change (same number of rows after shift)
land_change <- land_change[, Period := period_map$Period]

# Merge ESA trends with land cover changes
combined_data <- merge(esa_trends, land_change, by = c("SITENO", "Period"), all.x = TRUE)
combined_data = drop_na(combined_data)


# Summarize overall trends (1985-2020)
overall_land_change <- land_long[, .(
  Overall_Change = Percent_Coverage[.N] - Percent_Coverage[1]
), by = .(SITENO, Cluster, category)]
overall_esa_trends <- esa_trends[, .(Overall_Slope_ESA = mean(Slope_ESA, na.rm = TRUE),
                                    Overall_Pvalue_ESA = mean(Pvalue_ESA, na.rm = TRUE)), 
                                by = SITENO]

overall_combined <- merge(overall_esa_trends, overall_land_change, by = "SITENO")

# Correlate ESA slope with land cover change by period and cluster
cor_results <- combined_data[, 
  .(Correlation = cor(Slope_ESA, Change, use = "complete.obs"),
    Pvalue_Cor = cor.test(Slope_ESA, Change)$p.value), 
  by = .(Cluster, category, Period)]

ggplot(combined_data, aes(x = Period, y = Slope_ESA, color = category, group = SITENO)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(size = abs(Change))) +
  facet_wrap(~ Cluster, scales = "free_y") +
  labs(title = "ESA Slope vs. Land Cover Change by 5-Year Period and Cluster",
       x = "5-Year Period", y = "ESA Slope (per year)", color = "Land Cover", size = "Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Summarize significant correlations and direction changes
sig_cor <- cor_results[Pvalue_Cor < 0.05]

direction_change <- combined_data[, .(
  Direction_Shift = sum(sign(Change) != shift(sign(Change), type = "lag"), na.rm = TRUE) > 0
), by = .(SITENO, Cluster, category)]