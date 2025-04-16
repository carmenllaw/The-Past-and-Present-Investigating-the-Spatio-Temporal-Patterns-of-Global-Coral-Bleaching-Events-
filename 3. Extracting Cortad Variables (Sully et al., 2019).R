#this R script follows Sully et al. (2019), missing SSTA data, still downloading 

#extract cortad variables (Sully et al., 2019) ----
rm(list=ls())

#open packages: 
library(ncdf4)
library(raster)
library(dplyr)
library(ggplot2)
library(string)

#redo (last)----
#read data
rc_new <- read.csv("rc_new2017.csv")
number_of_surveys <- nrow(rc_new)

#open FilledSST for cortad cell grid info 
ncFilledSST <- nc_open("cortadv6_FilledSST.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
FilledSST_time_bounds <- ncvar_get(ncFilledSST, varid = "time_bounds")
FilledSST_lat_bounds<- ncvar_get(ncFilledSST, varid = "lat_bounds")
FilledSST_lon_bounds<- ncvar_get(ncFilledSST, varid = "lon_bounds")
FilledSST_land<- ncvar_get(ncFilledSST, varid = "land")

#check grid cell size (should be)
difference <- numeric(ncol(FilledSST_lon_bounds) - 1)
for (j in 1:(ncol(FilledSST_lon_bounds) - 1)) {
  difference[j] <- FilledSST_lon_bounds[1, j + 1] - FilledSST_lon_bounds[1, j]
}


#map reefcheck coordinates with cortad coordinates
#latitude grid cell mapping
Bleaching_cortad_lat_cell <- numeric(number_of_surveys)
lat_step <- -1 * (FilledSST_lat_bounds[2, ncol(FilledSST_lat_bounds)] - FilledSST_lat_bounds[1, 1]) / (ncol(FilledSST_lat_bounds) + 1)
for (i in 1:number_of_surveys) {
  lat_grid_cell <- NA
  if (is.na(rc_new$Latitude.Degrees[i])) {
    lat_grid_cell <- NA 
  } else {
    n_lat_steps <- floor((FilledSST_lat_bounds[1, 1] - rc_new$Latitude.Degrees[i]) / lat_step + 1)
    if (FilledSST_lat_bounds[1, n_lat_steps] >= rc_new$Latitude.Degrees[i]) {
      if (FilledSST_lat_bounds[2, n_lat_steps] <= rc_new$Latitude.Degrees[i]) {
        lat_grid_cell <- n_lat_steps
      } else {
        repeat {
          n_lat_steps <- n_lat_steps + 1
          if (FilledSST_lat_bounds[1, n_lat_steps] > rc_new$Latitude.Degrees[i]) {
            if (FilledSST_lat_bounds[2, n_lat_steps] <= rc_new$Latitude.Degrees[i]) {
              break
            }
          }
        }
        lat_grid_cell <- n_lat_steps
      }
    }
    if (FilledSST_lat_bounds[1, n_lat_steps] < rc_new$Latitude.Degrees[i]) {
      repeat {
        n_lat_steps <- n_lat_steps - 1
        if (FilledSST_lat_bounds[1, n_lat_steps] >= rc_new$Latitude.Degrees[i]) {
          if (FilledSST_lat_bounds[2, n_lat_steps] <= rc_new$Latitude.Degrees[i]) {
            break
          }
        }
      }
      lat_grid_cell <- n_lat_steps
    }
  }
  Bleaching_cortad_lat_cell[i] <- lat_grid_cell
}

#longitude grid cell mapping
Bleaching_cortad_lon_cell <- numeric(number_of_surveys)
lon_step <- (FilledSST_lon_bounds[1, ncol(FilledSST_lon_bounds)] - FilledSST_lon_bounds[1, 1]) / (ncol(FilledSST_lon_bounds) + 1)
for (i in 1:length(rc_new$Longitude.Degrees)) {
  lon_grid_cell <- NA
  if (is.na(rc_new$Longitude.Degrees[i])) {
    lon_grid_cell <- NA 
  } else {
    n_lon_steps <- floor(-1 * (FilledSST_lon_bounds[1, 1] - rc_new$Longitude.Degrees[i]) / lon_step + 1)
    if (n_lon_steps > ncol(FilledSST_lon_bounds)) { n_lon_steps <- ncol(FilledSST_lon_bounds) }
    if (n_lon_steps < 1) { n_lon_steps <- 1 }
    if (FilledSST_lon_bounds[1, n_lon_steps] <= rc_new$Longitude.Degrees[i]) {
      if (FilledSST_lon_bounds[2, n_lon_steps] > rc_new$Longitude.Degrees[i]) {
        lon_grid_cell <- n_lon_steps
      } else {
        repeat {
          n_lon_steps <- n_lon_steps + 1
          if (n_lon_steps > ncol(FilledSST_lon_bounds)) { break }
          if (FilledSST_lon_bounds[1, n_lon_steps] <= rc_new$Longitude.Degrees[i]) {
            if (FilledSST_lon_bounds[2, n_lon_steps] > rc_new$Longitude.Degrees[i]) {
              break
            }
          }
        }
        lon_grid_cell <- n_lon_steps
      }
    }
    if (FilledSST_lon_bounds[1, n_lon_steps] > rc_new$Longitude.Degrees[i]) {
      repeat {
        n_lon_steps <- n_lon_steps - 1
        if (n_lon_steps == 0) { break }
        if (FilledSST_lon_bounds[1, n_lon_steps] <= rc_new$Longitude.Degrees[i]) {
          if (FilledSST_lon_bounds[2, n_lon_steps] > rc_new$Longitude.Degrees[i]) {
            break
          }
        }
      }
      lon_grid_cell <- n_lon_steps
    }
  }
  Bleaching_cortad_lon_cell[i] <- lon_grid_cell
}

#format dates and calculate time indices
Bleaching_days_since_19811231 <- numeric(number_of_surveys)
for (i in 1:number_of_surveys) {
  date_string <- str_split(rc_new$Date[i], "-")
  day_numeric <- as.numeric(date_string[[1]][1])
  month_string <- date_string[[1]][2]
  if (month_string == "Jan") { days_since_19811231_due_to_month_number <- 0 }
  if (month_string == "Feb") { days_since_19811231_due_to_month_number <- 31 }
  if (month_string == "Mar") { days_since_19811231_due_to_month_number <- 59 }
  if (month_string == "Apr") { days_since_19811231_due_to_month_number <- 90 }
  if (month_string == "May") { days_since_19811231_due_to_month_number <- 120 }
  if (month_string == "Jun") { days_since_19811231_due_to_month_number <- 151 }
  if (month_string == "Jul") { days_since_19811231_due_to_month_number <- 181 }
  if (month_string == "Aug") { days_since_19811231_due_to_month_number <- 212 }
  if (month_string == "Sep") { days_since_19811231_due_to_month_number <- 243 }
  if (month_string == "Oct") { days_since_19811231_due_to_month_number <- 273 }
  if (month_string == "Nov") { days_since_19811231_due_to_month_number <- 304 }
  if (month_string == "Dec") { days_since_19811231_due_to_month_number <- 334 }
  
  year_numeric <- as.numeric(date_string[[1]][3])
  century <- 1900
  if (year_numeric < 25) { century <- 2000 }
  full_year <- century + year_numeric
#adjusting leap years here
  leap_year_days <- 0
  if ((full_year > 1984) | (full_year == 1984 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 1 }
  if ((full_year > 1988) | (full_year == 1988 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 2 }
  if ((full_year > 1992) | (full_year == 1992 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 3 }
  if ((full_year > 1996) | (full_year == 1996 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 4 }
  if ((full_year > 2000) | (full_year == 2000 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 5 }
  if ((full_year > 2004) | (full_year == 2004 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 6 }
  if ((full_year > 2008) | (full_year == 2008 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 7 }
  if ((full_year > 2012) | (full_year == 2012 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 8 }
  if ((full_year > 2016) | (full_year == 2016 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 9 }
  if ((full_year > 2020) | (full_year == 2020 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 10 }
  if ((full_year > 2024) | (full_year == 2024 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 11 }
  days_since_19811231 <- ((full_year - 1982) * 365) + days_since_19811231_due_to_month_number + day_numeric + leap_year_days
  Bleaching_days_since_19811231[i] <- days_since_19811231
}

Bleaching_cortad_day_index <- numeric(number_of_surveys)
max_index_of_CoRTAD <- ncol(FilledSST_time_bounds) + 1
for (i in 1:number_of_surveys) {
  Bleaching_cortad_day_index[i] <- floor((Bleaching_days_since_19811231[i] + FilledSST_time_bounds[1, 1]) / 7) + 1
  if (Bleaching_cortad_day_index[i] > max_index_of_CoRTAD) {
    Bleaching_cortad_day_index[i] <- NA
  }
}

#empty arrays to extract environmental variables
ClimSST<- rep(NA, number_of_surveys)
Temperature_Kelvin<- rep(NA, number_of_surveys)
Temperature_Mean<- rep(NA, number_of_surveys)
Temperature_Minimum<- rep(NA, number_of_surveys)
Temperature_Maximum<- rep(NA, number_of_surveys)
Temperature_Kelvin_Standard_Deviation <- rep(NA, number_of_surveys)
Windspeed<- rep(NA, number_of_surveys)
SSTA_data<- rep(NA, number_of_surveys)
SSTA_Standard_Deviation<- rep(NA, number_of_surveys)
SSTA_Mean<- rep(NA, number_of_surveys)
SSTA_Minimum<- rep(NA, number_of_surveys)
SSTA_Maximum<- rep(NA, number_of_surveys)
SSTA_Freq<- rep(NA, number_of_surveys)
SSTA_Freq_Standard_Deviation<- rep(NA, number_of_surveys)
SSTA_Freq_Max<- rep(NA, number_of_surveys)
SSTA_Freq_Mean<- rep(NA, number_of_surveys)
SSTA_DHW<- rep(NA, number_of_surveys)
SSTA_DHW_Standard_Deviation<- rep(NA, number_of_surveys)
SSTA_DHW_Max<- rep(NA, number_of_surveys)
SSTA_DHW_Mean<- rep(NA, number_of_surveys)
TSA_data<- rep(NA, number_of_surveys)
TSA_Standard_Deviation<- rep(NA, number_of_surveys)
TSA_Minimum<- rep(NA, number_of_surveys)
TSA_Maximum<- rep(NA, number_of_surveys)
TSA_Mean<- rep(NA, number_of_surveys)
TSA_Freq<- rep(NA, number_of_surveys)
TSA_Freq_Standard_Deviation<- rep(NA, number_of_surveys)
TSA_Freq_Max<- rep(NA, number_of_surveys)
TSA_Freq_Mean<- rep(NA, number_of_surveys)
TSA_DHW<- rep(NA, number_of_surveys)
TSA_DHW_Standard_Deviation<- rep(NA, number_of_surveys)
TSA_DHW_Max<- rep(NA, number_of_surveys)
TSA_DHW_Mean<- rep(NA, number_of_surveys)

#nc files needed
ncSSTA<- nc_open("cortadv6_SSTA.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
ncTSA<- nc_open("cortadv6_TSA.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
ncWind<- nc_open("cortadv6_WindSpeed.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
ncHarmonics<- nc_open("cortadv6_HarmonicsClimatology.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)

#defining extraction pass function
first_pass_function_Harmonics <- function(nc_handle, variable_id) {
  try(ncvar_get(nc_handle, varid = variable_id, 
                start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], (Bleaching_cortad_day_index[i] %% 52 + 1)), 
                count = c(1, 1, 1)), silent = TRUE)
}
first_pass_function_2d <- function(nc_handle, variable_id) {
  try(ncvar_get(nc_handle, varid = variable_id, 
                start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                count = c(1, 1)), silent = TRUE)
}
first_pass_function_3d <- function(nc_handle, variable_id) {
  try(ncvar_get(nc_handle, varid = variable_id, 
                start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                count = c(1, 1, 1)), silent = TRUE)
}
second_pass_2d <- function(nc_handle, variable_id) {
  expand <- 1
  result <- NA
  repeat {
    expanded_grid <- try(ncvar_get(nc_handle, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) ^ 2)) {
      expand <- expand + 1
      if (expand >= 3) break
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  result
}
second_pass_3d <- function(nc_handle, variable_id) {
  expand <- 1
  result <- NA
  repeat {
    expanded_grid <- try(ncvar_get(nc_handle, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand, Bleaching_cortad_day_index[i]), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand, 1)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) ^ 2)) {
      expand <- expand + 1
      if (expand >= 3) break
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  result
}
second_pass_Harmonics <- function(nc_handle, variable_id) {
  expand <- 1
  result <- NA
  repeat {
    expanded_grid <- try(ncvar_get(nc_handle, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand, (Bleaching_cortad_day_index[i] %% 52) + 1), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand, 1)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) ^ 2)) {
      expand <- expand + 1
      if (expand >= 3) break
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  result
}

#pass for SST, wind, ClimSST
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    ClimSST[i] <- first_pass_function_Harmonics(ncHarmonics, "ClimSST")
    Temperature_Kelvin[i] <- first_pass_function_3d(ncFilledSST, "FilledSST")
    Temperature_Minimum[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTminimum")
    Temperature_Maximum[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTmaximum")
    Temperature_Mean[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTmean")
    Temperature_Kelvin_Standard_Deviation[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTstandardDeviation")
    Windspeed[i] <- first_pass_function_3d(ncWind, "wind_speed")
  }
  print(i)
}

#pass for SSTA variables
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    SSTA_data[i] <- first_pass_function_3d(ncSSTA, "SSTA")
    SSTA_Standard_Deviation[i] <- first_pass_function_2d(ncSSTA, "SSTA_StandardDeviation")
    SSTA_Mean[i] <- first_pass_function_2d(ncSSTA, "SSTA_Mean")
    SSTA_Maximum[i] <- first_pass_function_2d(ncSSTA, "SSTA_Maximum")
    SSTA_Minimum[i] <- first_pass_function_2d(ncSSTA, "SSTA_Minimum")
    SSTA_Freq[i] <- first_pass_function_3d(ncSSTA, "SSTA_Frequency")
    SSTA_Freq_Standard_Deviation[i] <- first_pass_function_2d(ncSSTA, "SSTA_FrequencyStandardDeviation")
    SSTA_Freq_Max[i] <- first_pass_function_2d(ncSSTA, "SSTA_FrequencyMax")
    SSTA_Freq_Mean[i] <- first_pass_function_2d(ncSSTA, "SSTA_FrequencyMean")
    SSTA_DHW[i] <- first_pass_function_3d(ncSSTA, "SSTA_DHW")
    SSTA_DHW_Standard_Deviation[i] <- first_pass_function_2d(ncSSTA, "SSTA_DHWStandardDeviation")
    SSTA_DHW_Max[i] <- first_pass_function_2d(ncSSTA, "SSTA_DHWMax")
    SSTA_DHW_Mean[i] <- first_pass_function_2d(ncSSTA, "SSTA_DHWMean")
  }
  print(i)
}

#pass for TSA variables
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    TSA_data[i] <- first_pass_function_3d(ncTSA, "TSA")
    TSA_Standard_Deviation[i] <- first_pass_function_2d(ncTSA, "TSA_StandardDeviation")
    TSA_Maximum[i] <- first_pass_function_2d(ncTSA, "TSA_Maximum")
    TSA_Minimum[i] <- first_pass_function_2d(ncTSA, "TSA_Minimum")
    TSA_Mean[i] <- first_pass_function_2d(ncTSA, "TSA_Mean")
    TSA_Freq[i] <- first_pass_function_3d(ncTSA, "TSA_Frequency")
    TSA_Freq_Standard_Deviation[i] <- first_pass_function_2d(ncTSA, "TSA_FrequencyStandardDeviation")
    TSA_Freq_Max[i] <- first_pass_function_2d(ncTSA, "TSA_FrequencyMax")
    TSA_Freq_Mean[i] <- first_pass_function_2d(ncTSA, "TSA_FrequencyMean")
    TSA_DHW[i] <- first_pass_function_3d(ncTSA, "TSA_DHW")
    TSA_DHW_Standard_Deviation[i] <- first_pass_function_2d(ncTSA, "TSA_DHWStandardDeviation")
    TSA_DHW_Max[i] <- first_pass_function_2d(ncTSA, "TSA_DHWMax")
    TSA_DHW_Mean[i] <- first_pass_function_2d(ncTSA, "TSA_DHWMean")
  }
  print(i)
}

#second pass for missing values
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    if (is.na(Temperature_Kelvin[i])) {
      Temperature_Kelvin[i] <- second_pass_3d(ncFilledSST, "FilledSST")
    }
    if (is.na(Temperature_Mean[i])) {
      Temperature_Mean[i] <- second_pass_2d(ncFilledSST, "FilledSSTmean")
    }
    if (is.na(Temperature_Maximum[i])) {
      Temperature_Maximum[i] <- second_pass_2d(ncFilledSST, "FilledSSTmaximum")
    }
    if (is.na(Temperature_Minimum[i])) {
      Temperature_Minimum[i] <- second_pass_2d(ncFilledSST, "FilledSSTminimum")
    }
    if (is.na(Temperature_Kelvin_Standard_Deviation[i])) {
      Temperature_Kelvin_Standard_Deviation[i] <- second_pass_2d(ncFilledSST, "FilledSSTstandardDeviation")
    }
    if (is.na(Windspeed[i])) {
      Windspeed[i] <- second_pass_3d(ncWind, "wind_speed")
    }
    if (is.na(SSTA_data[i])) {
      SSTA_data[i] <- second_pass_3d(ncSSTA, "SSTA")
    }
    if (is.na(SSTA_Standard_Deviation[i])) {
      SSTA_Standard_Deviation[i] <- second_pass_2d(ncSSTA, "SSTA_StandardDeviation")
    }
    if (is.na(SSTA_Mean[i])) {
      SSTA_Mean[i] <- second_pass_2d(ncSSTA, "SSTA_Mean")
    }
    if (is.na(SSTA_Maximum[i])) {
      SSTA_Maximum[i] <- second_pass_2d(ncSSTA, "SSTA_Maximum")
    }
    if (is.na(SSTA_Minimum[i])) {
      SSTA_Minimum[i] <- second_pass_2d(ncSSTA, "SSTA_Minimum")
    }
    if (is.na(SSTA_Freq[i])) {
      SSTA_Freq[i] <- second_pass_3d(ncSSTA, "SSTA_Frequency")
    }
    if (is.na(SSTA_Freq_Standard_Deviation[i])) {
      SSTA_Freq_Standard_Deviation[i] <- second_pass_2d(ncSSTA, "SSTA_FrequencyStandardDeviation")
    }
    if (is.na(SSTA_Freq_Max[i])) {
      SSTA_Freq_Max[i] <- second_pass_2d(ncSSTA, "SSTA_FrequencyMax")
    }
    if (is.na(SSTA_Freq_Mean[i])) {
      SSTA_Freq_Mean[i] <- second_pass_2d(ncSSTA, "SSTA_FrequencyMean")
    }
    if (is.na(SSTA_DHW[i])) {
      SSTA_DHW[i] <- second_pass_3d(ncSSTA, "SSTA_DHW")
    }
    if (is.na(SSTA_DHW_Standard_Deviation[i])) {
      SSTA_DHW_Standard_Deviation[i] <- second_pass_2d(ncSSTA, "SSTA_DHWStandardDeviation")
    }
    if (is.na(SSTA_DHW_Max[i])) {
      SSTA_DHW_Max[i] <- second_pass_2d(ncSSTA, "SSTA_DHWMax")
    }
    if (is.na(SSTA_DHW_Mean[i])) {
      SSTA_DHW_Mean[i] <- second_pass_2d(ncSSTA, "SSTA_DHWMean")
    }
    if (is.na(TSA_data[i])) {
      TSA_data[i] <- second_pass_3d(ncTSA, "TSA")
    }
    if (is.na(TSA_Standard_Deviation[i])) {
      TSA_Standard_Deviation[i] <- second_pass_2d(ncTSA, "TSA_StandardDeviation")
    }
    if (is.na(TSA_Maximum[i])) {
      TSA_Maximum[i] <- second_pass_2d(ncTSA, "TSA_Maximum")
    }
    if (is.na(TSA_Mean[i])) {
      TSA_Mean[i] <- second_pass_2d(ncTSA, "TSA_Mean")
    }
    if (is.na(TSA_Minimum[i])) {
      TSA_Minimum[i] <- second_pass_2d(ncTSA, "TSA_Minimum")
    }
    if (is.na(TSA_Freq[i])) {
      TSA_Freq[i] <- second_pass_3d(ncTSA, "TSA_Frequency")
    }
    if (is.na(TSA_Freq_Standard_Deviation[i])) {
      TSA_Freq_Standard_Deviation[i] <- second_pass_2d(ncTSA, "TSA_FrequencyStandardDeviation")
    }
    if (is.na(TSA_Freq_Max[i])) {
      TSA_Freq_Max[i] <- second_pass_2d(ncTSA, "TSA_FrequencyMax")
    }
    if (is.na(TSA_Freq_Mean[i])) {
      TSA_Freq_Mean[i] <- second_pass_2d(ncTSA, "TSA_FrequencyMean")
    }
    if (is.na(TSA_DHW[i])) {
      TSA_DHW[i] <- second_pass_3d(ncTSA, "TSA_DHW")
    }
    if (is.na(TSA_DHW_Standard_Deviation[i])) {
      TSA_DHW_Standard_Deviation[i] <- second_pass_2d(ncTSA, "TSA_DHWStandardDeviation")
    }
    if (is.na(TSA_DHW_Max[i])) {
      TSA_DHW_Max[i] <- second_pass_2d(ncTSA, "TSA_DHWMax")
    }
    if (is.na(TSA_DHW_Mean[i])) {
      TSA_DHW_Mean[i] <- second_pass_2d(ncTSA, "TSA_DHWMean")
    }
    if (is.na(ClimSST[i])) {
      ClimSST[i] <- second_pass_Harmonics(ncHarmonics, "ClimSST")
    }
    print(i)
  }
}

ClimSST<- as.numeric(ClimSST)
Temperature_Kelvin<- as.numeric(Temperature_Kelvin)
Temperature_Minimum<- as.numeric(Temperature_Minimum)
Temperature_Maximum<- as.numeric(Temperature_Maximum)
Temperature_Mean<- as.numeric(Temperature_Mean)
Temperature_Kelvin_Standard_Deviation <- as.numeric(Temperature_Kelvin_Standard_Deviation)
Windspeed<- as.numeric(Windspeed)
SSTA_data<- as.numeric(SSTA_data)
SSTA_Standard_Deviation<- as.numeric(SSTA_Standard_Deviation)
SSTA_Mean<- as.numeric(SSTA_Mean)
SSTA_Minimum<- as.numeric(SSTA_Minimum)
SSTA_Maximum<- as.numeric(SSTA_Maximum)
SSTA_Freq<- as.numeric(SSTA_Freq)
SSTA_Freq_Standard_Deviation<- as.numeric(SSTA_Freq_Standard_Deviation)
SSTA_Freq_Max<- as.numeric(SSTA_Freq_Max)
SSTA_Freq_Mean<- as.numeric(SSTA_Freq_Mean)
SSTA_DHW<- as.numeric(SSTA_DHW)
SSTA_DHW_Standard_Deviation<- as.numeric(SSTA_DHW_Standard_Deviation)
SSTA_DHW_Max<- as.numeric(SSTA_DHW_Max)
SSTA_DHW_Mean<- as.numeric(SSTA_DHW_Mean)
TSA_data<- as.numeric(TSA_data)
TSA_Standard_Deviation<- as.numeric(TSA_Standard_Deviation)
TSA_Minimum<- as.numeric(TSA_Minimum)
TSA_Maximum<- as.numeric(TSA_Maximum)
TSA_Mean<- as.numeric(TSA_Mean)
TSA_Freq<- as.numeric(TSA_Freq)
TSA_Freq_Standard_Deviation<- as.numeric(TSA_Freq_Standard_Deviation)
TSA_Freq_Max<- as.numeric(TSA_Freq_Max)
TSA_Freq_Mean<- as.numeric(TSA_Freq_Mean)
TSA_DHW<- as.numeric(TSA_DHW)
TSA_DHW_Standard_Deviation<- as.numeric(TSA_DHW_Standard_Deviation)
TSA_DHW_Max<- as.numeric(TSA_DHW_Max)
TSA_DHW_Mean<- as.numeric(TSA_DHW_Mean)

#combine all data with rc_new
bleaching_with_cortad_variables <- cbind(rc_new, 
                                         ClimSST, Temperature_Kelvin, Temperature_Mean, Temperature_Minimum, Temperature_Maximum, 
                                         Temperature_Kelvin_Standard_Deviation, Windspeed,
                                         SSTA = SSTA_data, SSTA_Standard_Deviation, SSTA_Mean, SSTA_Minimum, SSTA_Maximum, 
                                         SSTA_Freq, SSTA_Freq_Standard_Deviation, SSTA_Freq_Max, SSTA_Freq_Mean, SSTA_DHW, 
                                         SSTA_DHW_Standard_Deviation, SSTA_DHW_Max, SSTA_DHW_Mean, 
                                         TSA = TSA_data, TSA_Standard_Deviation, TSA_Mean, TSA_Minimum, TSA_Maximum, 
                                         TSA_Freq, TSA_Freq_Standard_Deviation, TSA_Freq_Mean, TSA_Freq_Max, TSA_DHW, 
                                         TSA_DHW_Standard_Deviation, TSA_DHW_Max, TSA_DHW_Mean)

sum (is.na(Temperature_Kelvin))
sum(is.na(Temperature_Kelvin_Standard_Deviation)) 
sum(is.na(Windspeed))
sum(is.na(SSTA_data))
sum(is.na(SSTA_Standard_Deviation)) 
sum(is.na(SSTA_Mean))
sum(is.na(SSTA_Maximum))
sum(is.na(SSTA_Freq)) 
sum(is.na(SSTA_Freq_Standard_Deviation)) 
sum(is.na(SSTA_Freq_Max))
sum(is.na(SSTA_Freq_Mean)) 
sum(is.na(SSTA_DHW))
sum(is.na(SSTA_DHW_Standard_Deviation)) 
sum(is.na(SSTA_DHW_Max)) 
sum(is.na(SSTA_DHW_Mean)) 
sum(is.na(TSA_data)) 
sum(is.na(TSA_Standard_Deviation))
sum(is.na(TSA_Maximum)) 
sum(is.na(TSA_Mean))
sum(is.na(TSA_Freq)) 
sum(is.na(TSA_Freq_Standard_Deviation)) 
sum(is.na(TSA_Freq_Max))
sum(is.na(TSA_Freq_Mean))
sum(is.na(TSA_DHW))
sum(is.na(TSA_DHW_Standard_Deviation))
sum(is.na(TSA_DHW_Max))
sum(is.na(TSA_DHW_Mean))

#remove 2023-2024 in rc dataset
bleaching_with_cortad_variables <- bleaching_with_cortad_variables %>%
  mutate(Date2 = as.Date(Date, format = "%d-%b-%y")) %>%
  filter(!(format(Date2, "%Y") %in% c("2023", "2024")))

write.csv(bleaching_with_cortad_variables, "rc_cortad_2022.csv", row.names = FALSE)


#cw_cleaned extraction:----
cw<-read.csv("cw_cleaned.csv")
number_of_surveys <- nrow(cw)

# Open FilledSST for cortad cell grid info 
ncFilledSST <- nc_open("cortadv6_FilledSST.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
FilledSST_time_bounds <- ncvar_get(ncFilledSST, varid = "time_bounds")
FilledSST_lat_bounds <- ncvar_get(ncFilledSST, varid = "lat_bounds")
FilledSST_lon_bounds <- ncvar_get(ncFilledSST, varid = "lon_bounds")
FilledSST_land <- ncvar_get(ncFilledSST, varid = "land")

# Check grid cell size (should be constant)
difference <- numeric(ncol(FilledSST_lon_bounds) - 1)
for (j in 1:(ncol(FilledSST_lon_bounds) - 1)) {
  difference[j] <- FilledSST_lon_bounds[1, j + 1] - FilledSST_lon_bounds[1, j]
}

# Map bleaching coordinates (from cw) to CoRTAD grid cells
# Latitude grid cell mapping
Bleaching_cortad_lat_cell <- numeric(number_of_surveys)
lat_step <- -1 * (FilledSST_lat_bounds[2, ncol(FilledSST_lat_bounds)] - FilledSST_lat_bounds[1, 1]) / (ncol(FilledSST_lat_bounds) + 1)
for (i in 1:number_of_surveys) {
  lat_grid_cell <- NA
  if (is.na(cw$Latitude.Degrees[i])) {
    lat_grid_cell <- NA 
  } else {
    n_lat_steps <- floor((FilledSST_lat_bounds[1, 1] - cw$Latitude.Degrees[i]) / lat_step + 1)
    if (FilledSST_lat_bounds[1, n_lat_steps] >= cw$Latitude.Degrees[i]) {
      if (FilledSST_lat_bounds[2, n_lat_steps] <= cw$Latitude.Degrees[i]) {
        lat_grid_cell <- n_lat_steps
      } else {
        repeat {
          n_lat_steps <- n_lat_steps + 1
          if (FilledSST_lat_bounds[1, n_lat_steps] > cw$Latitude.Degrees[i]) {
            if (FilledSST_lat_bounds[2, n_lat_steps] <= cw$Latitude.Degrees[i]) {
              break
            }
          }
        }
        lat_grid_cell <- n_lat_steps
      }
    }
    if (FilledSST_lat_bounds[1, n_lat_steps] < cw$Latitude.Degrees[i]) {
      repeat {
        n_lat_steps <- n_lat_steps - 1
        if (FilledSST_lat_bounds[1, n_lat_steps] >= cw$Latitude.Degrees[i]) {
          if (FilledSST_lat_bounds[2, n_lat_steps] <= cw$Latitude.Degrees[i]) {
            break
          }
        }
      }
      lat_grid_cell <- n_lat_steps
    }
  }
  Bleaching_cortad_lat_cell[i] <- lat_grid_cell
}

# Longitude grid cell mapping
Bleaching_cortad_lon_cell <- numeric(number_of_surveys)
lon_step <- (FilledSST_lon_bounds[1, ncol(FilledSST_lon_bounds)] - FilledSST_lon_bounds[1, 1]) / (ncol(FilledSST_lon_bounds) + 1)
for (i in 1:length(cw$Longitude.Degrees)) {
  lon_grid_cell <- NA
  if (is.na(cw$Longitude.Degrees[i])) {
    lon_grid_cell <- NA 
  } else {
    n_lon_steps <- floor(-1 * (FilledSST_lon_bounds[1, 1] - cw$Longitude.Degrees[i]) / lon_step + 1)
    if (n_lon_steps > ncol(FilledSST_lon_bounds)) { n_lon_steps <- ncol(FilledSST_lon_bounds) }
    if (n_lon_steps < 1) { n_lon_steps <- 1 }
    if (FilledSST_lon_bounds[1, n_lon_steps] <= cw$Longitude.Degrees[i]) {
      if (FilledSST_lon_bounds[2, n_lon_steps] > cw$Longitude.Degrees[i]) {
        lon_grid_cell <- n_lon_steps
      } else {
        repeat {
          n_lon_steps <- n_lon_steps + 1
          if (n_lon_steps > ncol(FilledSST_lon_bounds)) { break }
          if (FilledSST_lon_bounds[1, n_lon_steps] <= cw$Longitude.Degrees[i]) {
            if (FilledSST_lon_bounds[2, n_lon_steps] > cw$Longitude.Degrees[i]) {
              break
            }
          }
        }
        lon_grid_cell <- n_lon_steps
      }
    }
    if (FilledSST_lon_bounds[1, n_lon_steps] > cw$Longitude.Degrees[i]) {
      repeat {
        n_lon_steps <- n_lon_steps - 1
        if (n_lon_steps == 0) { break }
        if (FilledSST_lon_bounds[1, n_lon_steps] <= cw$Longitude.Degrees[i]) {
          if (FilledSST_lon_bounds[2, n_lon_steps] > cw$Longitude.Degrees[i]) {
            break
          }
        }
      }
      lon_grid_cell <- n_lon_steps
    }
  }
  Bleaching_cortad_lon_cell[i] <- lon_grid_cell
}

# Format dates and calculate time indices
Bleaching_days_since_19811231 <- numeric(number_of_surveys)
for (i in 1:number_of_surveys) {
  date_string <- str_split(cw$Date[i], "-")
  day_numeric <- as.numeric(date_string[[1]][1])
  month_string <- date_string[[1]][2]
  if (month_string == "Jan") { days_since_19811231_due_to_month_number <- 0 }
  if (month_string == "Feb") { days_since_19811231_due_to_month_number <- 31 }
  if (month_string == "Mar") { days_since_19811231_due_to_month_number <- 59 }
  if (month_string == "Apr") { days_since_19811231_due_to_month_number <- 90 }
  if (month_string == "May") { days_since_19811231_due_to_month_number <- 120 }
  if (month_string == "Jun") { days_since_19811231_due_to_month_number <- 151 }
  if (month_string == "Jul") { days_since_19811231_due_to_month_number <- 181 }
  if (month_string == "Aug") { days_since_19811231_due_to_month_number <- 212 }
  if (month_string == "Sep") { days_since_19811231_due_to_month_number <- 243 }
  if (month_string == "Oct") { days_since_19811231_due_to_month_number <- 273 }
  if (month_string == "Nov") { days_since_19811231_due_to_month_number <- 304 }
  if (month_string == "Dec") { days_since_19811231_due_to_month_number <- 334 }
  
  year_numeric <- as.numeric(date_string[[1]][3])
  century <- 1900
  if (year_numeric < 25) { century <- 2000 }
  full_year <- century + year_numeric
  
  # Adjust leap years here:
  leap_year_days <- 0
  if ((full_year > 1984) | (full_year == 1984 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 1 }
  if ((full_year > 1988) | (full_year == 1988 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 2 }
  if ((full_year > 1992) | (full_year == 1992 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 3 }
  if ((full_year > 1996) | (full_year == 1996 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 4 }
  if ((full_year > 2000) | (full_year == 2000 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 5 }
  if ((full_year > 2004) | (full_year == 2004 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 6 }
  if ((full_year > 2008) | (full_year == 2008 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 7 }
  if ((full_year > 2012) | (full_year == 2012 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 8 }
  if ((full_year > 2016) | (full_year == 2016 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 9 }
  if ((full_year > 2020) | (full_year == 2020 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 10 }
  if ((full_year > 2024) | (full_year == 2024 & !(month_string %in% c("Jan", "Feb")))) { leap_year_days <- 11 }
  
  days_since_19811231 <- ((full_year - 1982) * 365) + days_since_19811231_due_to_month_number + day_numeric + leap_year_days
  Bleaching_days_since_19811231[i] <- days_since_19811231
}

Bleaching_cortad_day_index <- numeric(number_of_surveys)
max_index_of_CoRTAD <- ncol(FilledSST_time_bounds) + 1
for (i in 1:number_of_surveys) {
  Bleaching_cortad_day_index[i] <- floor((Bleaching_days_since_19811231[i] + FilledSST_time_bounds[1, 1]) / 7) + 1
  if (Bleaching_cortad_day_index[i] > max_index_of_CoRTAD) {
    Bleaching_cortad_day_index[i] <- NA
  }
}

# Create empty arrays for environmental variables (same as before)
ClimSST <- rep(NA, number_of_surveys)
Temperature_Kelvin <- rep(NA, number_of_surveys)
Temperature_Mean <- rep(NA, number_of_surveys)
Temperature_Minimum <- rep(NA, number_of_surveys)
Temperature_Maximum <- rep(NA, number_of_surveys)
Temperature_Kelvin_Standard_Deviation <- rep(NA, number_of_surveys)
Windspeed <- rep(NA, number_of_surveys)
SSTA_data <- rep(NA, number_of_surveys)
SSTA_Standard_Deviation <- rep(NA, number_of_surveys)
SSTA_Mean <- rep(NA, number_of_surveys)
SSTA_Minimum <- rep(NA, number_of_surveys)
SSTA_Maximum <- rep(NA, number_of_surveys)
SSTA_Freq <- rep(NA, number_of_surveys)
SSTA_Freq_Standard_Deviation <- rep(NA, number_of_surveys)
SSTA_Freq_Max <- rep(NA, number_of_surveys)
SSTA_Freq_Mean <- rep(NA, number_of_surveys)
SSTA_DHW <- rep(NA, number_of_surveys)
SSTA_DHW_Standard_Deviation <- rep(NA, number_of_surveys)
SSTA_DHW_Max <- rep(NA, number_of_surveys)
SSTA_DHW_Mean <- rep(NA, number_of_surveys)
TSA_data <- rep(NA, number_of_surveys)
TSA_Standard_Deviation <- rep(NA, number_of_surveys)
TSA_Minimum <- rep(NA, number_of_surveys)
TSA_Maximum <- rep(NA, number_of_surveys)
TSA_Mean <- rep(NA, number_of_surveys)
TSA_Freq <- rep(NA, number_of_surveys)
TSA_Freq_Standard_Deviation <- rep(NA, number_of_surveys)
TSA_Freq_Max <- rep(NA, number_of_surveys)
TSA_Freq_Mean <- rep(NA, number_of_surveys)
TSA_DHW <- rep(NA, number_of_surveys)
TSA_DHW_Standard_Deviation <- rep(NA, number_of_surveys)
TSA_DHW_Max <- rep(NA, number_of_surveys)
TSA_DHW_Mean <- rep(NA, number_of_surveys)

# Open nc files for SSTA, TSA, wind, and harmonics
ncSSTA <- nc_open("cortadv6_SSTA.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
ncTSA <- nc_open("cortadv6_TSA.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
ncWind <- nc_open("cortadv6_WindSpeed.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)
ncHarmonics <- nc_open("cortadv6_HarmonicsClimatology.nc", write = FALSE, readunlim = TRUE, verbose = FALSE)

# Define extraction functions (same as before)
first_pass_function_Harmonics <- function(nc_handle, variable_id) {
  try(ncvar_get(nc_handle, varid = variable_id, 
                start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], (Bleaching_cortad_day_index[i] %% 52 + 1)), 
                count = c(1, 1, 1)), silent = TRUE)
}
first_pass_function_2d <- function(nc_handle, variable_id) {
  try(ncvar_get(nc_handle, varid = variable_id, 
                start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                count = c(1, 1)), silent = TRUE)
}
first_pass_function_3d <- function(nc_handle, variable_id) {
  try(ncvar_get(nc_handle, varid = variable_id, 
                start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                count = c(1, 1, 1)), silent = TRUE)
}
second_pass_2d <- function(nc_handle, variable_id) {
  expand <- 1
  result <- NA
  repeat {
    expanded_grid <- try(ncvar_get(nc_handle, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) ^ 2)) {
      expand <- expand + 1
      if (expand >= 3) break
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  result
}
second_pass_3d <- function(nc_handle, variable_id) {
  expand <- 1
  result <- NA
  repeat {
    expanded_grid <- try(ncvar_get(nc_handle, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand, Bleaching_cortad_day_index[i]), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand, 1)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) ^ 2)) {
      expand <- expand + 1
      if (expand >= 3) break
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  result
}
second_pass_Harmonics <- function(nc_handle, variable_id) {
  expand <- 1
  result <- NA
  repeat {
    expanded_grid <- try(ncvar_get(nc_handle, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand, (Bleaching_cortad_day_index[i] %% 52) + 1), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand, 1)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) ^ 2)) {
      expand <- expand + 1
      if (expand >= 3) break
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  result
}

# First pass for SST, wind, ClimSST
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    ClimSST[i] <- first_pass_function_Harmonics(ncHarmonics, "ClimSST")
    Temperature_Kelvin[i] <- first_pass_function_3d(ncFilledSST, "FilledSST")
    Temperature_Minimum[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTminimum")
    Temperature_Maximum[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTmaximum")
    Temperature_Mean[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTmean")
    Temperature_Kelvin_Standard_Deviation[i] <- first_pass_function_2d(ncFilledSST, "FilledSSTstandardDeviation")
    Windspeed[i] <- first_pass_function_3d(ncWind, "wind_speed")
  }
  print(i)
}

# First pass for SSTA variables
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    SSTA_data[i] <- first_pass_function_3d(ncSSTA, "SSTA")
    SSTA_Standard_Deviation[i] <- first_pass_function_2d(ncSSTA, "SSTA_StandardDeviation")
    SSTA_Mean[i] <- first_pass_function_2d(ncSSTA, "SSTA_Mean")
    SSTA_Maximum[i] <- first_pass_function_2d(ncSSTA, "SSTA_Maximum")
    SSTA_Minimum[i] <- first_pass_function_2d(ncSSTA, "SSTA_Minimum")
    SSTA_Freq[i] <- first_pass_function_3d(ncSSTA, "SSTA_Frequency")
    SSTA_Freq_Standard_Deviation[i] <- first_pass_function_2d(ncSSTA, "SSTA_FrequencyStandardDeviation")
    SSTA_Freq_Max[i] <- first_pass_function_2d(ncSSTA, "SSTA_FrequencyMax")
    SSTA_Freq_Mean[i] <- first_pass_function_2d(ncSSTA, "SSTA_FrequencyMean")
    SSTA_DHW[i] <- first_pass_function_3d(ncSSTA, "SSTA_DHW")
    SSTA_DHW_Standard_Deviation[i] <- first_pass_function_2d(ncSSTA, "SSTA_DHWStandardDeviation")
    SSTA_DHW_Max[i] <- first_pass_function_2d(ncSSTA, "SSTA_DHWMax")
    SSTA_DHW_Mean[i] <- first_pass_function_2d(ncSSTA, "SSTA_DHWMean")
  }
  print(i)
}

# First pass for TSA variables
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    TSA_data[i] <- first_pass_function_3d(ncTSA, "TSA")
    TSA_Standard_Deviation[i] <- first_pass_function_2d(ncTSA, "TSA_StandardDeviation")
    TSA_Maximum[i] <- first_pass_function_2d(ncTSA, "TSA_Maximum")
    TSA_Minimum[i] <- first_pass_function_2d(ncTSA, "TSA_Minimum")
    TSA_Mean[i] <- first_pass_function_2d(ncTSA, "TSA_Mean")
    TSA_Freq[i] <- first_pass_function_3d(ncTSA, "TSA_Frequency")
    TSA_Freq_Standard_Deviation[i] <- first_pass_function_2d(ncTSA, "TSA_FrequencyStandardDeviation")
    TSA_Freq_Max[i] <- first_pass_function_2d(ncTSA, "TSA_FrequencyMax")
    TSA_Freq_Mean[i] <- first_pass_function_2d(ncTSA, "TSA_FrequencyMean")
    TSA_DHW[i] <- first_pass_function_3d(ncTSA, "TSA_DHW")
    TSA_DHW_Standard_Deviation[i] <- first_pass_function_2d(ncTSA, "TSA_DHWStandardDeviation")
    TSA_DHW_Max[i] <- first_pass_function_2d(ncTSA, "TSA_DHWMax")
    TSA_DHW_Mean[i] <- first_pass_function_2d(ncTSA, "TSA_DHWMean")
  }
  print(i)
}

# Second pass for missing values
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i]) &&
      !is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i])) {
    
    if (is.na(Temperature_Kelvin[i])) {
      Temperature_Kelvin[i] <- second_pass_3d(ncFilledSST, "FilledSST")
    }
    if (is.na(Temperature_Mean[i])) {
      Temperature_Mean[i] <- second_pass_2d(ncFilledSST, "FilledSSTmean")
    }
    if (is.na(Temperature_Maximum[i])) {
      Temperature_Maximum[i] <- second_pass_2d(ncFilledSST, "FilledSSTmaximum")
    }
    if (is.na(Temperature_Minimum[i])) {
      Temperature_Minimum[i] <- second_pass_2d(ncFilledSST, "FilledSSTminimum")
    }
    if (is.na(Temperature_Kelvin_Standard_Deviation[i])) {
      Temperature_Kelvin_Standard_Deviation[i] <- second_pass_2d(ncFilledSST, "FilledSSTstandardDeviation")
    }
    if (is.na(Windspeed[i])) {
      Windspeed[i] <- second_pass_3d(ncWind, "wind_speed")
    }
    if (is.na(SSTA_data[i])) {
      SSTA_data[i] <- second_pass_3d(ncSSTA, "SSTA")
    }
    if (is.na(SSTA_Standard_Deviation[i])) {
      SSTA_Standard_Deviation[i] <- second_pass_2d(ncSSTA, "SSTA_StandardDeviation")
    }
    if (is.na(SSTA_Mean[i])) {
      SSTA_Mean[i] <- second_pass_2d(ncSSTA, "SSTA_Mean")
    }
    if (is.na(SSTA_Maximum[i])) {
      SSTA_Maximum[i] <- second_pass_2d(ncSSTA, "SSTA_Maximum")
    }
    if (is.na(SSTA_Minimum[i])) {
      SSTA_Minimum[i] <- second_pass_2d(ncSSTA, "SSTA_Minimum")
    }
    if (is.na(SSTA_Freq[i])) {
      SSTA_Freq[i] <- second_pass_3d(ncSSTA, "SSTA_Frequency")
    }
    if (is.na(SSTA_Freq_Standard_Deviation[i])) {
      SSTA_Freq_Standard_Deviation[i] <- second_pass_2d(ncSSTA, "SSTA_FrequencyStandardDeviation")
    }
    if (is.na(SSTA_Freq_Max[i])) {
      SSTA_Freq_Max[i] <- second_pass_2d(ncSSTA, "SSTA_FrequencyMax")
    }
    if (is.na(SSTA_Freq_Mean[i])) {
      SSTA_Freq_Mean[i] <- second_pass_2d(ncSSTA, "SSTA_FrequencyMean")
    }
    if (is.na(SSTA_DHW[i])) {
      SSTA_DHW[i] <- second_pass_3d(ncSSTA, "SSTA_DHW")
    }
    if (is.na(SSTA_DHW_Standard_Deviation[i])) {
      SSTA_DHW_Standard_Deviation[i] <- second_pass_2d(ncSSTA, "SSTA_DHWStandardDeviation")
    }
    if (is.na(SSTA_DHW_Max[i])) {
      SSTA_DHW_Max[i] <- second_pass_2d(ncSSTA, "SSTA_DHWMax")
    }
    if (is.na(SSTA_DHW_Mean[i])) {
      SSTA_DHW_Mean[i] <- second_pass_2d(ncSSTA, "SSTA_DHWMean")
    }
    if (is.na(TSA_data[i])) {
      TSA_data[i] <- second_pass_3d(ncTSA, "TSA")
    }
    if (is.na(TSA_Standard_Deviation[i])) {
      TSA_Standard_Deviation[i] <- second_pass_2d(ncTSA, "TSA_StandardDeviation")
    }
    if (is.na(TSA_Maximum[i])) {
      TSA_Maximum[i] <- second_pass_2d(ncTSA, "TSA_Maximum")
    }
    if (is.na(TSA_Mean[i])) {
      TSA_Mean[i] <- second_pass_2d(ncTSA, "TSA_Mean")
    }
    if (is.na(TSA_Minimum[i])) {
      TSA_Minimum[i] <- second_pass_2d(ncTSA, "TSA_Minimum")
    }
    if (is.na(TSA_Freq[i])) {
      TSA_Freq[i] <- second_pass_3d(ncTSA, "TSA_Frequency")
    }
    if (is.na(TSA_Freq_Standard_Deviation[i])) {
      TSA_Freq_Standard_Deviation[i] <- second_pass_2d(ncTSA, "TSA_FrequencyStandardDeviation")
    }
    if (is.na(TSA_Freq_Max[i])) {
      TSA_Freq_Max[i] <- second_pass_2d(ncTSA, "TSA_FrequencyMax")
    }
    if (is.na(TSA_Freq_Mean[i])) {
      TSA_Freq_Mean[i] <- second_pass_2d(ncTSA, "TSA_FrequencyMean")
    }
    if (is.na(TSA_DHW[i])) {
      TSA_DHW[i] <- second_pass_3d(ncTSA, "TSA_DHW")
    }
    if (is.na(TSA_DHW_Standard_Deviation[i])) {
      TSA_DHW_Standard_Deviation[i] <- second_pass_2d(ncTSA, "TSA_DHWStandardDeviation")
    }
    if (is.na(TSA_DHW_Max[i])) {
      TSA_DHW_Max[i] <- second_pass_2d(ncTSA, "TSA_DHWMax")
    }
    if (is.na(TSA_DHW_Mean[i])) {
      TSA_DHW_Mean[i] <- second_pass_2d(ncTSA, "TSA_DHWMean")
    }
    if (is.na(ClimSST[i])) {
      ClimSST[i] <- second_pass_Harmonics(ncHarmonics, "ClimSST")
    }
    print(i)
  }
}
#convert all extracted variables to numeric
ClimSST <- as.numeric(ClimSST)
Temperature_Kelvin <- as.numeric(Temperature_Kelvin)
Temperature_Minimum <- as.numeric(Temperature_Minimum)
Temperature_Maximum <- as.numeric(Temperature_Maximum)
Temperature_Mean <- as.numeric(Temperature_Mean)
Temperature_Kelvin_Standard_Deviation <- as.numeric(Temperature_Kelvin_Standard_Deviation)
Windspeed <- as.numeric(Windspeed)
SSTA_data <- as.numeric(SSTA_data)
SSTA_Standard_Deviation <- as.numeric(SSTA_Standard_Deviation)
SSTA_Mean <- as.numeric(SSTA_Mean)
SSTA_Minimum <- as.numeric(SSTA_Minimum)
SSTA_Maximum <- as.numeric(SSTA_Maximum)
SSTA_Freq <- as.numeric(SSTA_Freq)
SSTA_Freq_Standard_Deviation <- as.numeric(SSTA_Freq_Standard_Deviation)
SSTA_Freq_Max <- as.numeric(SSTA_Freq_Max)
SSTA_Freq_Mean <- as.numeric(SSTA_Freq_Mean)
SSTA_DHW <- as.numeric(SSTA_DHW)
SSTA_DHW_Standard_Deviation <- as.numeric(SSTA_DHW_Standard_Deviation)
SSTA_DHW_Max <- as.numeric(SSTA_DHW_Max)
SSTA_DHW_Mean <- as.numeric(SSTA_DHW_Mean)
TSA_data <- as.numeric(TSA_data)
TSA_Standard_Deviation <- as.numeric(TSA_Standard_Deviation)
TSA_Minimum <- as.numeric(TSA_Minimum)
TSA_Maximum <- as.numeric(TSA_Maximum)
TSA_Mean <- as.numeric(TSA_Mean)
TSA_Freq <- as.numeric(TSA_Freq)
TSA_Freq_Standard_Deviation <- as.numeric(TSA_Freq_Standard_Deviation)
TSA_Freq_Max <- as.numeric(TSA_Freq_Max)
TSA_Freq_Mean <- as.numeric(TSA_Freq_Mean)
TSA_DHW <- as.numeric(TSA_DHW)
TSA_DHW_Standard_Deviation <- as.numeric(TSA_DHW_Standard_Deviation)
TSA_DHW_Max <- as.numeric(TSA_DHW_Max)
TSA_DHW_Mean <- as.numeric(TSA_DHW_Mean)

# Combine all data with cw (instead of rc_new)
bleaching_with_cortad_variables <- cbind(cw, 
                                         ClimSST, Temperature_Kelvin, Temperature_Mean, Temperature_Minimum, Temperature_Maximum, 
                                         Temperature_Kelvin_Standard_Deviation, Windspeed,
                                         SSTA = SSTA_data, SSTA_Standard_Deviation, SSTA_Mean, SSTA_Minimum, SSTA_Maximum, 
                                         SSTA_Freq, SSTA_Freq_Standard_Deviation, SSTA_Freq_Max, SSTA_Freq_Mean, SSTA_DHW, 
                                         SSTA_DHW_Standard_Deviation, SSTA_DHW_Max, SSTA_DHW_Mean, 
                                         TSA = TSA_data, TSA_Standard_Deviation, TSA_Mean, TSA_Minimum, TSA_Maximum, 
                                         TSA_Freq, TSA_Freq_Standard_Deviation, TSA_Freq_Mean, TSA_Freq_Max, TSA_DHW, 
                                         TSA_DHW_Standard_Deviation, TSA_DHW_Max, TSA_DHW_Mean)

write.csv(bleaching_with_cortad_variables, "cw_cortad_2022.csv", row.names = FALSE)



