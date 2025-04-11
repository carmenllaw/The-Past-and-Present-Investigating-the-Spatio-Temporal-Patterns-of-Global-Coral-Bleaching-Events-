#this R script follows Github paper, missing SSTA data, still downloading 
#need to remove NAs and replot distribution map 

#extract cortad variables----
rm(list=ls())

library("ncdf4")
library("raster")
library("dplyr")
library(ggplot2)
library(stringr)

setwd("~/Desktop/Reef Check Diss/Diss Directory")
rc_new<-read.csv("rc_new2017.csv")
cw<-read.csv("cw_cleaned.csv")
pp<-read.csv("pp_data.csv")

number_of_surveys=dim(rc_new)[1] #shows how many surveys stored


FilledSST<- nc_open("cortadv6_FilledSST.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(FilledSST$var)

FilledSST_time_bounds <- ncvar_get(FilledSST, varid="time_bounds")
dim(FilledSST_time_bounds) #  2 x 2139

FilledSST_lat_bounds <- ncvar_get(FilledSST, varid="lat_bounds")
dim(FilledSST_lat_bounds)  # 2 x 4320

FilledSST_lon_bounds <- ncvar_get(FilledSST, varid="lon_bounds")
dim(FilledSST_lon_bounds)  # 2 x 8640

FilledSST_land <- ncvar_get(FilledSST, varid="land")
dim(FilledSST_land) # 8640 x 4320 

#check grid cell size
difference <- array(0, dim=(length(FilledSST_lon_bounds[1,]) - 1))
for (i in 1:(dim(FilledSST_lon_bounds)[2] - 1)) {
  difference[i] <- FilledSST_lon_bounds[1, (i+1)] - FilledSST_lon_bounds[1, i]
} #constant value of 0.0417
 
#mapping reef check coordinates to Cortad grid cells 
#determines which latitude cell in the CoRTAD grid corresponds to the Reef Check survey’s latitude
# lat_step - computes the size of one grid cell in latitude.
Bleaching_cortad_lat_cell <- array(0, dim=number_of_surveys)
lat_step <- -1 * (FilledSST_lat_bounds[2, dim(FilledSST_lat_bounds)[2]] - FilledSST_lat_bounds[1,1]) / (dim(FilledSST_lat_bounds)[2] + 1)

for (i in 1:number_of_surveys) {
  lat_grid_cell <- NA
  
  if (is.na(rc_new$Latitude.Degrees[i])) {
    lat_grid_cell <- NA 
  } else {
    n_lat_steps <- floor((FilledSST_lat_bounds[1,1] - rc_new$Latitude.Degrees[i]) / lat_step + 1)
    
    if (FilledSST_lat_bounds[1, n_lat_steps] >= rc_new$Latitude.Degrees[i]) {
      if (FilledSST_lat_bounds[2, n_lat_steps] <= rc_new$Latitude.Degrees[i]) {
        lat_grid_cell <- n_lat_steps
      } else {
        repeat {
          n_lat_steps = n_lat_steps + 1
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
        n_lat_steps = n_lat_steps - 1
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

#does the same for longitude cells
Bleaching_cortad_lon_cell <- array(0, dim=number_of_surveys)
lon_step <- (FilledSST_lon_bounds[1, dim(FilledSST_lon_bounds)[2]] - FilledSST_lon_bounds[1,1]) / (dim(FilledSST_lon_bounds)[2] + 1)

for (i in 1:length(rc_new$Longitude.Degrees)) {
  lon_grid_cell <- NA
  
  if (is.na(rc_new$Longitude.Degrees[i])) {
    lon_grid_cell <- NA 
  } else {
    n_lon_steps <- floor(-1 * (FilledSST_lon_bounds[1,1] - rc_new$Longitude.Degrees[i]) / lon_step + 1)
    
    if (n_lon_steps > (dim(FilledSST_lon_bounds)[2])) { n_lon_steps <- (dim(FilledSST_lon_bounds)[2]) }
    if (n_lon_steps < 1) { n_lon_steps <- 1 }
    
    if (FilledSST_lon_bounds[1, n_lon_steps] <= rc_new$Longitude.Degrees[i]) {
      if (FilledSST_lon_bounds[2, n_lon_steps] > rc_new$Longitude.Degrees[i]) {
        lon_grid_cell <- n_lon_steps
      } else {
        repeat {
          n_lon_steps = n_lon_steps + 1
          if (n_lon_steps > (dim(FilledSST_lon_bounds)[2])) { break }
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
        n_lon_steps = n_lon_steps - 1
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

#formatting dates and accounting for leap years 
#For each survey date: the date string is split into day, month, and year.
#The month is converted to a “day offset” (e.g. Feb starts after 31 days, Mar after 59 days, etc.).
# The two-digit year is converted to a full year by deciding whether it belongs to the 1900s or 2000s.
#Leap days are added if the year is a leap year and the date is past February.
#Finally, the total number of days since December 31, 1981 is computed. This matches the time coordinate used in the CoRTAD NetCDF files, which is in “days since 19811231.”

Bleaching_days_since_19811231 <- array(0, dim=number_of_surveys)
for (i in 1:number_of_surveys) {
  date_string <- str_split(rc_new$Date[i], "-")
  day_string <- date_string[[1]][1]
  day_numeric <- as.numeric(day_string)
  month_string <- date_string[[1]][2]
  if (month_string=="Jan"){ days_since_19811231_due_to_month_number <- 0 }
  if (month_string=="Feb"){ days_since_19811231_due_to_month_number <- 31 }
  if (month_string=="Mar"){ days_since_19811231_due_to_month_number <- 59 }
  if (month_string=="Apr"){ days_since_19811231_due_to_month_number <- 90 }
  if (month_string=="May"){ days_since_19811231_due_to_month_number <- 120 }
  if (month_string=="Jun"){ days_since_19811231_due_to_month_number <- 151 }
  if (month_string=="Jul"){ days_since_19811231_due_to_month_number <- 181 }
  if (month_string=="Aug"){ days_since_19811231_due_to_month_number <- 212 }
  if (month_string=="Sep"){ days_since_19811231_due_to_month_number <- 243 }
  if (month_string=="Oct"){ days_since_19811231_due_to_month_number <- 273 }
  if (month_string=="Nov"){ days_since_19811231_due_to_month_number <- 304 }
  if (month_string=="Dec"){ days_since_19811231_due_to_month_number <- 334 }}
  
  year_string <- date_string[[1]][3]
  year_numeric <- as.numeric(year_string)
  century <- 1900
  if (year_numeric < 25) {  # e.g., 95 means 1995; 25 means 2025
    century <- 2000
  }
  full_year <- century + year_numeric
  
  #adjust for leap years 
  leap_year_days <- 0
  if ((full_year > 1984) | (full_year == 1984 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 1 }
  if ((full_year > 1988) | (full_year == 1988 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 2 }
  if ((full_year > 1992) | (full_year == 1992 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 3 }
  if ((full_year > 1996) | (full_year == 1996 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 4 }
  if ((full_year > 2000) | (full_year == 2000 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 5 }
  if ((full_year > 2004) | (full_year == 2004 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 6 }
  if ((full_year > 2008) | (full_year == 2008 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 7 }
  if ((full_year > 2012) | (full_year == 2012 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 8 }
  if ((full_year > 2016) | (full_year == 2016 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 9 }
  if ((full_year > 2020) | (full_year == 2020 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 10 }
  if ((full_year > 2024) | (full_year == 2024 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 11 }
  
  days_since_19811231 <- ((full_year - 1982) * 365) + days_since_19811231_due_to_month_number + day_numeric + leap_year_days
  Bleaching_days_since_19811231[i] <- days_since_19811231
#calculate day index:
#converts the “days since” value into an index for the CoRTAD time dimension. CoRTAD data is weekly thus divide by 7).
#if the computed index exceeds the number of time steps available, it is set to NA

Bleaching_cortad_day_index <- array(0, dim=number_of_surveys)
max_index_of_CoRTAD <- dim(FilledSST_time_bounds)[2] + 1
for (i in 1:number_of_surveys) {
  Bleaching_cortad_day_index[i] <- floor((Bleaching_days_since_19811231[i] + FilledSST_time_bounds[1,1]) / 7) + 1
  
  if (Bleaching_cortad_day_index[i] > max_index_of_CoRTAD) {
    Bleaching_cortad_day_index[i] <- NA
  }
}

nc_close(FilledSST)

#create arrays to hold other extracted Cortad variables:----
ClimSST<-array(NA, dim=number_of_surveys)

Temperature_Kelvin<-array(NA, dim=number_of_surveys)
Temperature_Mean<-array(NA, dim=number_of_surveys)
Temperature_Minimum<-array(NA, dim=number_of_surveys)
Temperature_Maximum<-array(NA, dim=number_of_surveys)
Temperature_Kelvin_Standard_Deviation<-array(NA, dim=number_of_surveys)

Windspeed<-array(NA, dim=number_of_surveys)

SSTA<-array(NA, dim=number_of_surveys)
SSTA_Standard_Deviation<-array(NA, dim=number_of_surveys)
SSTA_Mean<-array(NA, dim=number_of_surveys)
SSTA_Minimum<-array(NA, dim=number_of_surveys)
SSTA_Maximum<-array(NA, dim=number_of_surveys)
SSTA_Freq<-array(NA, dim=number_of_surveys)
SSTA_Freq_Standard_Deviation<-array(NA, dim=number_of_surveys)
SSTA_Freq_Max<-array(NA, dim=number_of_surveys)
SSTA_Freq_Mean<-array(NA, dim=number_of_surveys)
SSTA_DHW<-array(NA, dim=number_of_surveys)
SSTA_DHW_Standard_Deviation<-array(NA, dim=number_of_surveys)
SSTA_DHW_Max<-array(NA, dim=number_of_surveys)
SSTA_DHW_Mean<-array(NA, dim=number_of_surveys)

TSA<-array(NA, dim=number_of_surveys)
TSA_Standard_Deviation<-array(NA, dim=number_of_surveys)
TSA_Minimum<-array(NA, dim=number_of_surveys)
TSA_Maximum<-array(NA, dim=number_of_surveys)
TSA_Mean<-array(NA, dim=number_of_surveys)
TSA_Freq<-array(NA, dim=number_of_surveys)
TSA_Freq_Standard_Deviation<-array(NA, dim=number_of_surveys)
TSA_Freq_Max<-array(NA, dim=number_of_surveys)
TSA_Freq_Mean<-array(NA, dim=number_of_surveys)
TSA_DHW<-array(NA, dim=number_of_surveys)
TSA_DHW_Standard_Deviation<-array(NA, dim=number_of_surveys)
TSA_DHW_Max<-array(NA, dim=number_of_surveys)
TSA_DHW_Mean<-array(NA, dim=number_of_surveys)

#open required NetCDF files but open and close files individually and run code, there were error messages, possibly due to too many large files being opened
FilledSST <- nc_open("cortadv6_FilledSST.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(FilledSST$var) #require FilledSST, FilledSSTmin, FilledSSTmax, FilledSSTstandardDeviation, FilledSSTmean

SSTA<-nc_open("cortadv6_SSTA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(SSTA$var) #SSTA_Minimum, SSTA_Maximum, SSTA_StandardDeviation, SSTA_Mean, SSTA_AbsoluteValueMean, SSTA_Frequency, SSTA_FrequencyMax, SSTA_FrequencyStandardDeviation, SSTA_FrequencyMean, SSTA_DHW,  SSTA_DHWMax, SSTA_DHWStandardDeviation, SSTA_DHWMean


TSA<-nc_open("cortadv6_TSA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(TSA$var) #TSA, TSA_Minimum, TSA_Maximum, TSA_StandardDeviation, TSA_Mean, TSA_Frequency, TSA_FrequencyMax, TSA_FrequencyStandardDeviation, TSA_FrequencyMean, TSA_DHW, TSA_DHWMax, TSA_DHWStandardDeviation, TSA_DHWMean

Wind<-nc_open("cortadv6_WindSpeed.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(Wind$var) #wind_speed

HarmonicsClimatology<-nc_open("cortadv6_HarmonicsClimatology.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(HarmonicsClimatology$var) #ClimSST, AnnualAmplitudeCoefficient, AnnualPhaseCoefficient, SemiAnnualAmplitudeCoefficient, SemiAnnualPhaseCoefficient, MeanCoefficient, LSqFitFlag

setwd("~/Desktop/Reef Check Diss/Diss Directory")
list.files(pattern = "*.nc")

#defining functions to extract data from CorTAD grid:
first_pass_function_Harmonics <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], (Bleaching_cortad_day_index[i] %% 52 + 1)), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}

first_pass_function_2d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                          count=c(1,1)), silent=TRUE)
  return(result)
}

first_pass_function_3d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}



#second pass
second_pass_2d <- function(netcdf_variable_name, variable_id) {
  expand = 1
  result = NA
  repeat {
    expanded_grid <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                                   start=c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand), 
                                   count=c(1 + 2 * expand, 1 + 2 * expand)), silent=TRUE)
    
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) * (1 + 2 * expand))) {
      expand = expand + 1
      if (expand >= 3) { break }
    } else {
      result <- mean(expanded_grid, na.rm=TRUE)
      break
    }
  }
  return(result)
}

second_pass_3d<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand),Bleaching_cortad_day_index[i]), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}

second_pass_Harmonics<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand), (Bleaching_cortad_day_index[i]%%52)+1), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}


for (i in 1:number_of_surveys)
{
  if(!is.na(Bleaching_cortad_day_index[i]))
  {
    if(!is.na(Bleaching_cortad_lon_cell[i]))
    {
      if(!is.na(Bleaching_cortad_lat_cell[i]))
      {
        ClimSST[i]<-first_pass_function_Harmonics(HarmonicsClimatology, "ClimSST")
        Temperature_Kelvin[i]<-first_pass_function_3d(FilledSST, "FilledSST")
        Temperature_Minimum[i]<-first_pass_function_2d(FilledSST, "FilledSSTminimum")
        Temperature_Maximum[i]<-first_pass_function_2d(FilledSST, "FilledSSTmaximum")
        Temperature_Mean[i]<-first_pass_function_2d(FilledSST, "FilledSSTmean")
        Temperature_Kelvin_Standard_Deviation[i]<-first_pass_function_2d(FilledSST, "FilledSSTstandardDeviation")
        Windspeed[i]<-first_pass_function_3d(Wind, "wind_speed")
      }
    }
  }
  print(i)
}

for (i in 1:number_of_surveys)
{
  if(!is.na(Bleaching_cortad_day_index[i]))
  {
    if(!is.na(Bleaching_cortad_lon_cell[i]))
    {
      if(!is.na(Bleaching_cortad_lat_cell[i]))
      {
        SSTA[i]<-first_pass_function_3d(SSTA, "SSTA")
        SSTA_Standard_Deviation[i]<-first_pass_function_2d(SSTA, "SSTA_StandardDeviation")
        SSTA_Mean[i]<-first_pass_function_2d(SSTA, "SSTA_Mean")
        SSTA_Maximum[i]<-first_pass_function_2d(SSTA,"SSTA_Maximum")
        SSTA_Minimum[i]<-first_pass_function_2d(SSTA, "SSTA_Minimum")
        SSTA_Freq[i]<-first_pass_function_3d(SSTA, "SSTA_Frequency")
        SSTA_Freq_Standard_Deviation[i]<-first_pass_function_2d(SSTA, "SSTA_FrequencyStandardDeviation")
        SSTA_Freq_Max[i]<-first_pass_function_2d(SSTA, "SSTA_FrequencyMax")
        SSTA_Freq_Mean[i]<-first_pass_function_2d(SSTA, "SSTA_FrequencyMean")
        SSTA_DHW[i]<-first_pass_function_3d(SSTA, "SSTA_DHW")
        SSTA_DHW_Standard_Deviation[i]<-first_pass_function_2d(SSTA, "SSTA_DHWStandardDeviation")
        SSTA_DHW_Max[i]<-first_pass_function_2d(SSTA, "SSTA_DHWMax")
        SSTA_DHW_Mean[i]<-first_pass_function_2d(SSTA, "SSTA_DHWMean")
      }
    }
  }
  print(i)
}

for (i in 1:number_of_surveys)
{
  if(!is.na(Bleaching_cortad_day_index[i]))
  {
    if(!is.na(Bleaching_cortad_lon_cell[i]))
    {
      if(!is.na(Bleaching_cortad_lat_cell[i]))
      {
        TSA[i]<-first_pass_function_3d(TSA, "TSA")
        TSA_Standard_Deviation[i]<-first_pass_function_2d(TSA, "TSA_StandardDeviation")
        TSA_Maximum[i]<-first_pass_function_2d(TSA, "TSA_Maximum")
        TSA_Minimum[i]<-first_pass_function_2d(TSA, "TSA_Minimum")
        TSA_Mean[i]<-first_pass_function_2d(TSA, "TSA_Mean")
        TSA_Freq[i]<-first_pass_function_3d(TSA, "TSA_Frequency")
        TSA_Freq_Standard_Deviation[i]<-first_pass_function_2d(TSA, "TSA_FrequencyStandardDeviation")
        TSA_Freq_Max[i]<-first_pass_function_2d(TSA, "TSA_FrequencyMax")
        TSA_Freq_Mean[i]<-first_pass_function_2d(TSA, "TSA_FrequencyMean")
        TSA_DHW[i]<-first_pass_function_3d(TSA, "TSA_DHW")
        TSA_DHW_Standard_Deviation[i]<-first_pass_function_2d(TSA, "TSA_DHWStandardDeviation")
        TSA_DHW_Max[i]<-first_pass_function_2d(TSA, "TSA_DHWMax")
        TSA_DHW_Mean[i]<-first_pass_function_2d(TSA, "TSA_DHWMean")
      }
    }
  }
  print(i)
}

sum (is.na(Temperature_Kelvin))  #1744
sum(is.na(Temperature_Kelvin_Standard_Deviation)) #1744
sum(is.na(Windspeed))  #0
sum(is.na(SSTA)) #2
sum(is.na(SSTA_Standard_Deviation)) #2
sum(is.na(SSTA_Mean)) #2
sum(is.na(SSTA_Maximum)) #2
sum(is.na(SSTA_Freq)) #2
sum(is.na(SSTA_Freq_Standard_Deviation)) #2
sum(is.na(SSTA_Freq_Max)) #2
sum(is.na(SSTA_Freq_Mean)) #2
sum(is.na(SSTA_DHW)) #2
sum(is.na(SSTA_DHW_Standard_Deviation)) #2
sum(is.na(SSTA_DHW_Max)) #2
sum(is.na(SSTA_DHW_Mean)) #2
sum(is.na(TSA)) #2
sum(is.na(TSA_Standard_Deviation)) #2
sum(is.na(TSA_Maximum)) #2
sum(is.na(TSA_Mean)) #2
sum(is.na(TSA_Freq)) #2
sum(is.na(TSA_Freq_Standard_Deviation)) #2
sum(is.na(TSA_Freq_Max)) #2
sum(is.na(TSA_Freq_Mean)) #2
sum(is.na(TSA_DHW)) #2
sum(is.na(TSA_DHW_Standard_Deviation)) #2
sum(is.na(TSA_DHW_Max)) #2
sum(is.na(TSA_DHW_Mean)) #2

#numeric:
ClimSST<-as.numeric(ClimSST)
Temperature_Kelvin<-as.numeric(Temperature_Kelvin)
Temperature_Minimum<-as.numeric(Temperature_Minimum)
Temperature_Maximum<-as.numeric(Temperature_Maximum)
Temperature_Mean<-as.numeric(Temperature_Mean)
Temperature_Kelvin_Standard_Deviation<-as.numeric(Temperature_Kelvin_Standard_Deviation)
Windspeed<-as.numeric(Windspeed)
SSTA<-as.numeric(SSTA)
SSTA_Standard_Deviation<-as.numeric(SSTA_Standard_Deviation)
SSTA_Mean<-as.numeric(SSTA_Mean)
SSTA_Minimum<-as.numeric(SSTA_Minimum)
SSTA_Maximum<-as.numeric(SSTA_Maximum)
SSTA_Freq<-as.numeric(SSTA_Freq)
SSTA_Freq_Standard_Deviation<-as.numeric(SSTA_Freq_Standard_Deviation)
SSTA_Freq_Max<-as.numeric(SSTA_Freq_Max)
SSTA_Freq_Mean<-as.numeric(SSTA_Freq_Mean)
SSTA_DHW<-as.numeric(SSTA_DHW)
SSTA_DHW_Standard_Deviation<-as.numeric(SSTA_DHW_Standard_Deviation)
SSTA_DHW_Max<-as.numeric(SSTA_DHW_Max)
SSTA_DHW_Mean<-as.numeric(SSTA_DHW_Mean)
TSA<-as.numeric(TSA)
TSA_Standard_Deviation<-as.numeric(TSA_Standard_Deviation)
TSA_Minimum<-as.numeric(TSA_Minimum)
TSA_Maximum<-as.numeric(TSA_Maximum)
TSA_Mean<-as.numeric(TSA_Mean)
TSA_Freq<-as.numeric(TSA_Freq)
TSA_Freq_Standard_Deviation<-as.numeric(TSA_Freq_Standard_Deviation)
TSA_Freq_Max<-as.numeric(TSA_Freq_Max)
TSA_Freq_Mean<-as.numeric(TSA_Freq_Mean)
TSA_DHW<-as.numeric(TSA_DHW)
TSA_DHW_Standard_Deviation<-as.numeric(TSA_DHW_Standard_Deviation)
TSA_DHW_Max<-as.numeric(TSA_DHW_Max)
TSA_DHW_Mean<-as.numeric(TSA_DHW_Mean)

for (i in 1:number_of_surveys)
{
  if(!is.na(Bleaching_cortad_lon_cell[i]))
  {
    if(!is.na(Bleaching_cortad_lat_cell[i]))
    {
      if(!is.na(Bleaching_cortad_day_index[i]))
      {
        
        if(is.na(Temperature_Kelvin[i]))
        {Temperature_Kelvin[i]<-second_pass_3d(FilledSST, "FilledSST")}
        
        if(is.na(Temperature_Mean[i]))
        {Temperature_Mean[i]<-second_pass_2d(FilledSST, "FilledSSTmean")}
        
        if(is.na(Temperature_Maximum[i]))
        {Temperature_Maximum[i]<-second_pass_2d(FilledSST, "FilledSSTmaximum")}
        
        if(is.na(Temperature_Minimum[i]))
        {Temperature_Minimum[i]<-second_pass_2d(FilledSST, "FilledSSTminimum")}
        
        if(is.na(Temperature_Kelvin_Standard_Deviation[i]))
        {Temperature_Kelvin_Standard_Deviation[i]<-second_pass_2d(FilledSST, "FilledSSTstandardDeviation")}
        
        if(is.na(Windspeed[i]))
        {Windspeed[i]<-second_pass_3d(Wind, "wind_speed")}
        
        if(is.na(SSTA[i]))
        {SSTA[i]<-second_pass_3d(SSTA, "SSTA")}
        
        if(is.na(SSTA_Standard_Deviation[i]))
        {SSTA_Standard_Deviation[i]<-second_pass_2d(SSTA, "SSTA_StandardDeviation")}
        
        if(is.na(SSTA_Mean[i]))
        {SSTA_Mean[i]<-second_pass_2d(SSTA, "SSTA_Mean")}
        
        if(is.na(SSTA_Maximum[i]))
        {SSTA_Maximum[i]<-second_pass_2d(SSTA, "SSTA_Maximum")}
        
        if(is.na(SSTA_Minimum[i]))
        {SSTA_Minimum[i]<-second_pass_2d(SSTA, "SSTA_Minimum")}  
        
        if(is.na(SSTA_Freq[i]))
        {SSTA_Freq[i]<-second_pass_3d(SSTA, "SSTA_Frequency")}
        
        if(is.na(SSTA_Freq_Standard_Deviation[i]))
        {SSTA_Freq_Standard_Deviation[i]<-second_pass_2d(SSTA, "SSTA_FrequencyStandardDeviation")}
        
        if(is.na(SSTA_Freq_Max[i]))
        {SSTA_Freq_Max[i]<-second_pass_2d(SSTA, "SSTA_FrequencyMax")}
        
        if(is.na(SSTA_Freq_Mean[i]))
        {SSTA_Freq_Mean[i]<-second_pass_2d(SSTA, "SSTA_FrequencyMean")}
        
        if(is.na(SSTA_DHW[i]))
        {SSTA_DHW[i]<-second_pass_3d(SSTA, "SSTA_DHW")}
        
        if(is.na(SSTA_DHW_Standard_Deviation[i]))
        {SSTA_DHW_Standard_Deviation[i]<-second_pass_2d(SSTA, "SSTA_DHWStandardDeviation")}
        
        if(is.na(SSTA_DHW_Max[i]))
        {SSTA_DHW_Max[i]<-second_pass_2d(SSTA, "SSTA_DHWMax")}
        
        if(is.na(SSTA_DHW_Mean[i]))
        {SSTA_DHW_Mean[i]<-second_pass_2d(SSTA, "SSTA_DHWMean")}
        
        if(is.na(TSA[i]))
        {TSA[i]<-second_pass_3d(TSA, "TSA")}
        
        if(is.na(TSA_Standard_Deviation[i]))
        {TSA_Standard_Deviation[i]<-second_pass_2d(TSA, "TSA_StandardDeviation")}
        
        if(is.na(TSA_Maximum[i]))
        {TSA_Maximum[i]<-second_pass_2d(TSA, "TSA_Maximum")}
        
        if(is.na(TSA_Mean[i]))
        {TSA_Mean[i]<-second_pass_2d(TSA, "TSA_Mean")}
        
        if(is.na(TSA_Minimum[i]))
        {TSA_Minimum[i]<-second_pass_2d(TSA, "TSA_Minimum")}
        
        if(is.na(TSA_Freq[i]))
        {TSA_Freq[i]<-second_pass_3d(TSA, "TSA_Frequency")}
        
        if(is.na(TSA_Freq_Standard_Deviation[i]))
        {TSA_Freq_Standard_Deviation[i]<-second_pass_2d(TSA, "TSA_FrequencyStandardDeviation")}
        
        if(is.na(TSA_Freq_Max[i]))
        {TSA_Freq_Max[i]<-second_pass_2d(TSA, "TSA_FrequencyMax")}
        
        if(is.na(TSA_Freq_Mean[i]))
        {TSA_Freq_Mean[i]<-second_pass_2d(TSA, "TSA_FrequencyMean")}
        
        if(is.na(TSA_DHW[i]))
        {TSA_DHW[i]<-second_pass_3d(TSA, "TSA_DHW")}
        
        if(is.na(TSA_DHW_Standard_Deviation[i]))
        {TSA_DHW_Standard_Deviation[i]<-second_pass_2d(TSA, "TSA_DHWStandardDeviation")}
        
        if(is.na(TSA_DHW_Max[i]))
        {TSA_DHW_Max[i]<-second_pass_2d(TSA, "TSA_DHWMax")}
        
        if(is.na(TSA_DHW_Mean[i]))
        {TSA_DHW_Mean[i]<-second_pass_2d(TSA, "TSA_DHWMean")}
        
        if(is.na(ClimSST[i]))
        {ClimSST[i]<-second_pass_Harmonics(HarmonicsClimatology, "ClimSST")}
        
        print(i)
      } #close lon
    } #close lat
  } #close day index
} #close number_of_surveys

bleaching_with_cortad_variables<-cbind(rc_new, 
                                        ClimSST, Temperature_Kelvin, Temperature_Mean, 
                                        Temperature_Minimum, Temperature_Maximum, 
                                        Temperature_Kelvin_Standard_Deviation,Windspeed,
                                        SSTA, SSTA_Standard_Deviation, SSTA_Mean, 
                                        SSTA_Minimum, SSTA_Maximum, SSTA_Freq, 
                                        SSTA_Freq_Standard_Deviation, SSTA_Freq_Max, 
                                        SSTA_Freq_Mean, SSTA_DHW, SSTA_DHW_Standard_Deviation, 
                                        SSTA_DHW_Max, SSTA_DHW_Mean, TSA, TSA_Standard_Deviation, TSA_Mean,
                                        TSA_Minimum, TSA_Maximum, TSA_Freq, TSA_Freq_Standard_Deviation,
                                        TSA_Freq_Mean, TSA_Freq_Max, TSA_DHW,
                                        TSA_DHW_Standard_Deviation, TSA_DHW_Max, TSA_DHW_Mean)

library(dplyr)
combined_data <- bind_cols(rc_new,
                           data.frame(ClimSST,
                                      Temperature_Kelvin,
                                      Temperature_Mean,
                                      Temperature_Minimum,
                                      Temperature_Maximum,
                                      Temperature_Kelvin_Standard_Deviation,
                                      Windspeed,
                                      SSTA,
                                      SSTA_Standard_Deviation,
                                      SSTA_Mean,
                                      SSTA_Minimum,
                                      SSTA_Maximum,
                                      SSTA_Freq,
                                      SSTA_Freq_Standard_Deviation,
                                      SSTA_Freq_Max,
                                      SSTA_Freq_Mean,
                                      SSTA_DHW,
                                      SSTA_DHW_Standard_Deviation,
                                      SSTA_DHW_Max,
                                      SSTA_DHW_Mean,
                                      TSA,
                                      TSA_Standard_Deviation,
                                      TSA_Mean,
                                      TSA_Minimum,
                                      TSA_Maximum,
                                      TSA_Freq,
                                      TSA_Freq_Standard_Deviation,
                                      TSA_Freq_Mean,
                                      TSA_Freq_Max,
                                      TSA_DHW,
                                      TSA_DHW_Standard_Deviation,
                                      TSA_DHW_Max,
                                      TSA_DHW_Mean))


#defining functions to extract data from CorTAD grid:----
first_pass_function_Harmonics <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], (Bleaching_cortad_day_index[i] %% 52 + 1)), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}

first_pass_function_2d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                          count=c(1,1)), silent=TRUE)
  return(result)
}

first_pass_function_3d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}



#second pass
second_pass_2d <- function(netcdf_variable_name, variable_id) {
  expand = 1
  result = NA
  repeat {
    expanded_grid <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                                   start=c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand), 
                                   count=c(1 + 2 * expand, 1 + 2 * expand)), silent=TRUE)
    
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) * (1 + 2 * expand))) {
      expand = expand + 1
      if (expand >= 3) { break }
    } else {
      result <- mean(expanded_grid, na.rm=TRUE)
      break
    }
  }
  return(result)
}

second_pass_3d<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand),Bleaching_cortad_day_index[i]), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}

second_pass_Harmonics<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand), (Bleaching_cortad_day_index[i]%%52)+1), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}

#loop reef check survey to grab cortad data
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i])) {
    if (!is.na(Bleaching_cortad_lon_cell[i])) {
      if (!is.na(Bleaching_cortad_lat_cell[i])) {
        ClimSST[i] <- first_pass_function_Harmonics(HarmonicsClimatology, "ClimSST")
        SST[i] <- first_pass_function_3d(FilledSST, "FilledSST")
        SST_Minimum[i] <- first_pass_function_2d(FilledSST, "FilledSSTminimum")
        SST_Maximum[i] <- first_pass_function_2d(FilledSST, "FilledSSTmaximum")
        SST_Mean[i] <- first_pass_function_2d(FilledSST, "FilledSSTmean")
        SST_StandardDeviation[i] <- first_pass_function_2d(FilledSST, "FilledSSTstandardDeviation")
        SSTA[i] <- first_pass_function_3d(SSTA, "SSTA")
        SSTA_StandardDeviation[i] <- first_pass_function_2d(SSTA, "SSTA_StandardDeviation")
        SSTA_Mean[i] <- first_pass_function_2d(SSTA, "SSTA_Mean")
        SSTA_Maximum[i] <- first_pass_function_2d(SSTA,"SSTA_Maximum")
        SSTA_Minimum[i] <- first_pass_function_2d(SSTA, "SSTA_Minimum")
        SSTA_Frequency[i] <- first_pass_function_3d(SSTA, "SSTA_Frequency")
        SSTA_FrequencyStandardDeviation[i] <- first_pass_function_2d(SSTA, "SSTA_FrequencyStandardDeviation")
        SSTA_FrequencyMax[i] <- first_pass_function_2d(SSTA, "SSTA_FrequencyMax")
        SSTA_FrequencyMean[i] <- first_pass_function_2d(SSTA, "SSTA_FrequencyMean")
        SSTA_DHW[i] <- first_pass_function_3d(SSTA, "SSTA_DHW")
        SSTA_DHWStandardDeviation[i] <- first_pass_function_2d(SSTA, "SSTA_DHWStandardDeviation")
        SSTA_DHWMax[i] <- first_pass_function_2d(SSTA, "SSTA_DHWMax")
        SSTA_DHWMean[i] <- first_pass_function_2d(SSTA, "SSTA_DHWMean")
      }
    }
  }
  print(i)
}

#numeric conversion:
SST<-as.numeric(SST)
SST_StandardDeviation<-as.numeric(SST_StandardDeviation)

SSTA<-as.numeric(SSTA)
SSTA_StandardDeviation<-as.numeric(SSTA_StandardDeviation)
SSTA_Mean<-as.numeric(SSTA_Mean)
SSTA_Maximum<-as.numeric(SSTA_Maximum)
SSTA_Frequency<-as.numeric(SSTA_Frequency)
SSTA_FrequencyStandardDeviation<-as.numeric(SSTA_FrequencyStandardDeviation)
SSTA_FrequencyMax<-as.numeric(SSTA_FrequencyMax)
SSTA_FrequencyMean<-as.numeric(SSTA_FrequencyMean)
SSTA_DHW<-as.numeric(SSTA_DHW)
SSTA_DHWStandardDeviation<-as.numeric(SSTA_DHWStandardDeviation)
SSTA_DHWMax<-as.numeric(SSTA_DHWMax)
SSTA_DHWMean<-as.numeric(SSTA_DHWMean)
ClimSST<-as.numeric(ClimSST)
SSTA_Minimum<-as.numeric(SSTA_Minimum)
SST_Minimum<-as.numeric(SST_Minimum)
SST_Maximum<-as.numeric(SST_Maximum)
SST_Mean<-as.numeric(SST_Mean)

#second passing:
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_lon_cell[i])) {
    if (!is.na(Bleaching_cortad_lat_cell[i])) {
      if (!is.na(Bleaching_cortad_day_index[i])) {
        
        if (is.na(SST[i])) {
          SST[i] <- second_pass_3d(FilledSST, "FilledSST")
        }
        if (is.na(SST_Mean[i])) {
          SST_Mean[i] <- second_pass_2d(FilledSST, "FilledSSTmean")
        }
        if (is.na(Bleaching_cortad_temperature_Maximum[i])) {
          SST_Maximum[i] <- second_pass_2d(FilledSST, "FilledSSTmaximum")
        }
        if (is.na(Bleaching_cortad_temperature_Minimum[i])) {
          SST_Minimum[i] <- second_pass_2d(FilledSST, "FilledSSTminimum")
        }
        if (is.na(Bleaching_cortad_temperature_standardDeviation[i])) {
          SST_StandardDeviation[i] <- second_pass_2d(FilledSST, "FilledSSTstandardDeviation")
        }
        if (is.na(Bleaching_cortad_ClimSST[i])) {
          ClimSST[i] <- second_pass_Harmonics(HarmonicsClimatology, "ClimSST")
        }
        print(i)
      }
    }
  }
}

Bleaching_Data_with_cortad_variables <- cbind(rc_new, 
                                              ClimSST, SST, SST_Mean, 
                                              SST_Minimum, SST_Maximum, 
                                              SST_StandardDeviation,
                                              SSTA, SSTA_StandardDeviation, SSTA_Mean, 
                                              SSTA_Minimum, SSTA_Maximum, SSTA_Frequency, 
                                              SSTA_FrequencyStandardDeviation, SSTA_FrequencyMax, 
                                              SSTA_FrequencyMean, SSTA_DHW, SSTA_DHWStandardDeviation, 
                                              SSTA_DHWMax, SSTA_DHWMean)

write.csv(Bleaching_Data_with_cortad_variables, file = "rc_with_cortad_variables.csv")

#separating bleaching event years----
rc <- read.csv("rc_with_cortad_variables.csv")

keep <- c("Reef.ID", "Reef.Name", "Longitude.Degrees", "Latitude.Degrees", 
          "Ocean", "Country", "State.Province.Island", "City.Town", 
          "Year", "Date", "Depth", "Organism.Code", "S1", "S2", 
          "S3", "S4", "Errors.", "What.errors.", "Average_bleaching", 
          "Bleaching_cortad_temperature", "Bleaching_cortad_SSTA")

rc_2010 <- subset(rc, Year == 2010, select = keep)
rc_2010$Bleaching_cortad_temperature <- rc_2010$Bleaching_cortad_temperature - 273.15
rc_2010$Bleaching_cortad_temperature <- round(rc_2010$Bleaching_cortad_temperature, 2)
rc_2010$Bleaching_cortad_SSTA <- round(rc_2010$Bleaching_cortad_SSTA, 2)

rc_2014_2017 <- subset(rc, Year %in% c(2014, 2015, 2016, 2017), select = keep)
rc_2014_2017$Bleaching_cortad_temperature <- rc_2014_2017$Bleaching_cortad_temperature - 273.15
rc_2014_2017$Bleaching_cortad_temperature <- round(rc_2014_2017$Bleaching_cortad_temperature, 2)
rc_2014_2017$Bleaching_cortad_SSTA <- round(rc_2014_2017$Bleaching_cortad_SSTA, 2)

write.csv(rc_2010,"rc_cortad_2010.csv", row.names=FALSE)
write.csv(rc_2014_2017,"rc_cortad_2014_2017.csv", row.names=FALSE)


#post combining other environmental variables because not enough storage on laptop----
#I will be using a cleaned dataset with different environmental variable labels as I changed them to keep them consistent in another r-script:
rm(list=ls()) 

library("ncdf4")
library("raster")
library("dplyr")
library(ggplot2)
library(stringr)

setwd("~/Desktop/Reef Check Diss/Diss Directory")

rc<-read.csv("rc_with_cortad_variables.csv") 
number_of_surveys=dim(rc)[1] #shows how many surveys stored

TSA<- nc_open("cortadv6_TSA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(TSA$var)

TSA_lat_bounds <- ncvar_get(TSA, varid = "lat_bounds")  # dimensions: 2 x N_lat
TSA_lon_bounds <- ncvar_get(TSA, varid = "lon_bounds")  # dimensions: 2 x N_lon
TSA_time_bounds <- ncvar_get(TSA, varid = "time_bounds")

n_lat <- dim(TSA_lat_bounds)[2]
n_lon <- dim(TSA_lon_bounds)[2]

lat_step <- -1 * (TSA_lat_bounds[2, n_lat] - TSA_lat_bounds[1, 1]) / (n_lat + 1)
lon_step <- (TSA_lon_bounds[1, n_lon] - TSA_lon_bounds[1, 1]) / (n_lon + 1)

Bleaching_cortad_lat_cell <- rep(NA, number_of_surveys)
Bleaching_cortad_lon_cell <- rep(NA, number_of_surveys)

for (i in 1:number_of_surveys) {
  if (is.na(rc$Latitude.Degrees[i])) {
    Bleaching_cortad_lat_cell[i] <- NA
  } else {
    found <- FALSE
    for (j in 1:n_lat) {
      # Assuming TSA_lat_bounds[1,j] is the upper bound and TSA_lat_bounds[2,j] is the lower bound
      if (TSA_lat_bounds[1, j] >= rc$Latitude.Degrees[i] && TSA_lat_bounds[2, j] <= rc$Latitude.Degrees[i]) {
        Bleaching_cortad_lat_cell[i] <- j
        found <- TRUE
        break
      }
    }
    if (!found) Bleaching_cortad_lat_cell[i] <- NA
  }
}

  
for (i in 1:number_of_surveys) {
    if (is.na(rc$Longitude.Degrees[i])) {
      Bleaching_cortad_lon_cell[i] <- NA
    } else {
      found <- FALSE
      for (j in 1:n_lon) {
        # Assuming TSA_lon_bounds[1,j] is the lower bound and TSA_lon_bounds[2,j] is the upper bound
        if (TSA_lon_bounds[1, j] <= rc$Longitude.Degrees[i] && TSA_lon_bounds[2, j] > rc$Longitude.Degrees[i]) {
          Bleaching_cortad_lon_cell[i] <- j
          found <- TRUE
          break
        }
      }
      if (!found) Bleaching_cortad_lon_cell[i] <- NA
    }
  }


Bleaching_days_since_19811231 <- rep(NA, number_of_surveys)
  for (i in 1:number_of_surveys) {
    if (!is.na(rc$Date[i])) {
      survey_date <- as.Date(rc$Date[i], format = "%d-%b-%y")
      ref_date <- as.Date("1981-12-31")
      Bleaching_days_since_19811231[i] <- as.numeric(survey_date - ref_date)
    } else {
      Bleaching_days_since_19811231[i] <- NA
    }
  }
  
  # Convert days to a weekly index (assuming Cortad data are weekly)
Bleaching_cortad_day_index <- rep(NA, number_of_surveys)
  for (i in 1:number_of_surveys) {
    if (!is.na(Bleaching_days_since_19811231[i])) {
      Bleaching_cortad_day_index[i] <- floor(Bleaching_days_since_19811231[i] / 7) + 1
    } else {
      Bleaching_cortad_day_index[i] <- NA
    }
  }
  
first_pass_function_3d <- function(nc_file, var_name, i, lon_cells, lat_cells, day_indices) {
    result <- try(ncvar_get(nc_file, varid = var_name,
                            start = c(lon_cells[i], lat_cells[i], day_indices[i]),
                            count = c(1, 1, 1)), silent = TRUE)
    if (inherits(result, "try-error")) {
      return(NA)
    } else {
      return(result)
    }
  }
  
WindSpeed<-nc_open("cortadv6_WindSpeed.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(WindSpeed$var)

first_pass_function_3d <- function(netcdf_variable_name, variable_id, i, lon_cell, lat_cell, day_index) {
  result <- try(ncvar_get(nc_file, varid = var_name, 
                          start = c(lon_cell[i], lat_cell[i], day_index[i]), 
                          count = c(1, 1, 1)), silent = TRUE)
  return(result)
}

first_pass_function_2d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                          count=c(1,1)), silent=TRUE)
  return(result)
}

first_pass_function_3d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}

#set array to put coratd data into
TSA<-array(NA, dim=number_of_surveys)
TSA.Stand.Dev<-array(NA, dim=number_of_surveys)
TSA.Min<-array(NA, dim=number_of_surveys)
TSA.Max<-array(NA, dim=number_of_surveys)
TSA.Mean<-array(NA, dim=number_of_surveys)
TSA.Freq<-array(NA, dim=number_of_surveys)
TSA.Freq.Max<-array(NA, dim=number_of_surveys)
TSA.Freq.Mean<-array(NA, dim=number_of_surveys)
TSA.Freq.Stand.Dev<-array(NA, dim=number_of_surveys)
TSA.DHW<-array(NA, dim=number_of_surveys)
TSA.DHW.Max<-array(NA, dim=number_of_surveys)
TSA.DHW.Mean<-array(NA, dim=number_of_surveys)
TSA.DHW.Stand.Dev<-array(NA, dim=number_of_surveys)
Wind.Speed<-array(NA, dim=number_of_surveys)

for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_lon_cell[i]) &&
      !is.na(Bleaching_cortad_lat_cell[i]) &&
      !is.na(Bleaching_cortad_day_index[i])) {
    
    # For TSA-related variables:
    TSA[i] <- first_pass_function_3d(TSA_nc, "TSA", i,
                                     Bleaching_cortad_lon_cell, 
                                     Bleaching_cortad_lat_cell, 
                                     Bleaching_cortad_day_index)
    TSA.Stand.Dev[i] <- first_pass_function_2d(TSA, "TSA_StandardDeviation", i,
                                               Bleaching_cortad_lon_cell, 
                                               Bleaching_cortad_lat_cell)
    TSA.Min[i] <- first_pass_function_2d(TSA, "TSA_Minimum", i,
                                         Bleaching_cortad_lon_cell, 
                                         Bleaching_cortad_lat_cell)
    TSA.Max[i] <- first_pass_function_2d(TSA, "TSA_Maximum", i,
                                         Bleaching_cortad_lon_cell, 
                                         Bleaching_cortad_lat_cell)
    TSA.Mean[i] <- first_pass_function_2d(TSA, "TSA_Mean", i,
                                          Bleaching_cortad_lon_cell, 
                                          Bleaching_cortad_lat_cell)
    TSA.Freq[i] <- first_pass_function_3d(TSA, "TSA_Frequency", i,
                                          Bleaching_cortad_lon_cell, 
                                          Bleaching_cortad_lat_cell, 
                                          Bleaching_cortad_day_index)
    TSA.Freq.Max[i] <- first_pass_function_2d(TSA, "TSA_FrequencyMax", i,
                                              Bleaching_cortad_lon_cell, 
                                              Bleaching_cortad_lat_cell)
    TSA.Freq.Mean[i] <- first_pass_function_2d(TSA, "TSA_FrequencyMean", i,
                                               Bleaching_cortad_lon_cell, 
                                               Bleaching_cortad_lat_cell)
    TSA.Freq.Stand.Dev[i] <- first_pass_function_2d(TSA, "TSA_FrequencyStandardDeviation", i,
                                                    Bleaching_cortad_lon_cell, 
                                                    Bleaching_cortad_lat_cell)
    TSA.DHW[i] <- first_pass_function_3d(TSA, "TSA_DHW", i,
                                         Bleaching_cortad_lon_cell, 
                                         Bleaching_cortad_lat_cell, 
                                         Bleaching_cortad_day_index)
    TSA.DHW.Max[i] <- first_pass_function_2d(TSA, "TSA_DHWMax", i,
                                             Bleaching_cortad_lon_cell, 
                                             Bleaching_cortad_lat_cell)
    TSA.DHW.Mean[i] <- first_pass_function_2d(TSA, "TSA_DHWMean", i,
                                              Bleaching_cortad_lon_cell, 
                                              Bleaching_cortad_lat_cell)
    TSA.DHW.Stand.Dev[i] <- first_pass_function_2d(TSA, "TSA_DHWStandardDeviation", i,
                                                   Bleaching_cortad_lon_cell, 
                                                   Bleaching_cortad_lat_cell)
    
    # For WindSpeed (assumed to be a 3D variable)
    Wind.Speed[i] <- first_pass_function_3d(WindSpeed, "WindSpeed", i,
                                            Bleaching_cortad_lon_cell, 
                                            Bleaching_cortad_lat_cell, 
                                            Bleaching_cortad_day_index)
  }
  print(i)  # progress indicator
}

# Convert extracted values to numeric (if not already numeric)
TSA <- as.numeric(TSA)
TSA.Stand.Dev <- as.numeric(TSA.Stand.Dev)
TSA.Min <- as.numeric(TSA.Min)
TSA.Max <- as.numeric(TSA.Max)
TSA.Mean <- as.numeric(TSA.Mean)
TSA.Freq <- as.numeric(TSA.Freq)
TSA.Freq.Max <- as.numeric(TSA.Freq.Max)
TSA.Freq.Mean <- as.numeric(TSA.Freq.Mean)
TSA.Freq.Stand.Dev <- as.numeric(TSA.Freq.Stand.Dev)
TSA.DHW <- as.numeric(TSA.DHW)
TSA.DHW.Max <- as.numeric(TSA.DHW.Max)
TSA.DHW.Mean <- as.numeric(TSA.DHW.Mean)
TSA.DHW.Stand.Dev <- as.numeric(TSA.DHW.Stand.Dev)
Wind.Speed <- as.numeric(Wind.Speed)

rc_final <- cbind(rc, TSA, TSA.Stand.Dev, TSA.Min, TSA.Max, TSA.Mean,
                                  TSA.Freq, TSA.Freq.Max, TSA.Freq.Mean, TSA.Freq.Stand.Dev,
                                  TSA.DHW, TSA.DHW.Max, TSA.DHW.Mean, TSA.DHW.Stand.Dev,
                                  Wind.Speed)

#redo---- 
rm(list=ls())

library("ncdf4")
library("raster")
library("dplyr")
library(ggplot2)
library(stringr)

rc <- read.csv("rc_with_cortad_variables.csv") 
number_of_surveys <- dim(rc)[1]  # shows how many surveys are stored

# Open the TSA and WindSpeed NetCDF files
TSA_nc<- nc_open("cortadv6_TSA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
WindSpeed_nc <- nc_open("cortadv6_WindSpeed.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)

# Extract grid bounds and time bounds from TSA_nc
TSA_time_bounds <- ncvar_get(TSA_nc, varid="time_bounds")
dim(TSA_time_bounds)  # e.g., 2 x (n_time)

TSA_lat_bounds <- ncvar_get(TSA_nc, varid="lat_bounds")
dim(TSA_lat_bounds)  # 2 x (n_lat)

TSA_lon_bounds <- ncvar_get(TSA_nc, varid="lon_bounds")
dim(TSA_lon_bounds)  # 2 x (n_lon)

# Optionally, check grid cell size for longitude
difference <- array(0, dim=(length(TSA_lon_bounds[1,]) - 1))
for (i in 1:(dim(TSA_lon_bounds)[2] - 1)) {
  difference[i] <- TSA_lon_bounds[1, (i+1)] - TSA_lon_bounds[1, i]
} 
# Expect a constant value (e.g., 0.0417)

# Mapping reef check coordinates to Cortad grid cells
# Determine which latitude cell in the Cortad grid corresponds to the survey’s latitude.
Bleaching_cortad_lat_cell <- array(0, dim=number_of_surveys)
lat_step <- -1 * (TSA_lat_bounds[2, dim(TSA_lat_bounds)[2]] - TSA_lat_bounds[1,1]) / (dim(TSA_lat_bounds)[2] + 1)

for (i in 1:number_of_surveys) {
  lat_grid_cell <- NA
  if (is.na(rc$Latitude.Degrees[i])) {
    lat_grid_cell <- NA 
  } else {
    n_lat_steps <- floor((TSA_lat_bounds[1,1] - rc$Latitude.Degrees[i]) / lat_step + 1)
    
    if (TSA_lat_bounds[1, n_lat_steps] >= rc$Latitude.Degrees[i]) {
      if (TSA_lat_bounds[2, n_lat_steps] <= rc$Latitude.Degrees[i]) {
        lat_grid_cell <- n_lat_steps
      } else {
        repeat {
          n_lat_steps = n_lat_steps + 1
          if (TSA_lat_bounds[1, n_lat_steps] > rc$Latitude.Degrees[i]) {
            if (TSA_lat_bounds[2, n_lat_steps] <= rc$Latitude.Degrees[i]) {
              break
            }
          }
        }
        lat_grid_cell <- n_lat_steps
      }
    }
    
    if (TSA_lat_bounds[1, n_lat_steps] < rc$Latitude.Degrees[i]) {
      repeat {
        n_lat_steps = n_lat_steps - 1
        if (TSA_lat_bounds[1, n_lat_steps] >= rc$Latitude.Degrees[i]) {
          if (TSA_lat_bounds[2, n_lat_steps] <= rc$Latitude.Degrees[i]) {
            break
          }
        }
      }
      lat_grid_cell <- n_lat_steps
    }
  }
  Bleaching_cortad_lat_cell[i] <- lat_grid_cell
}

# Do the same for longitude cells
Bleaching_cortad_lon_cell <- array(0, dim=number_of_surveys)
lon_step <- (TSA_lon_bounds[1, dim(TSA_lon_bounds)[2]] - TSA_lon_bounds[1,1]) / (dim(TSA_lon_bounds)[2] + 1)

for (i in 1:length(rc$Longitude.Degrees)) {
  lon_grid_cell <- NA
  if (is.na(rc$Longitude.Degrees[i])) {
    lon_grid_cell <- NA 
  } else {
    n_lon_steps <- floor(-1 * (TSA_lon_bounds[1,1] - rc$Longitude.Degrees[i]) / lon_step + 1)
    if (n_lon_steps > (dim(TSA_lon_bounds)[2])) { n_lon_steps <- (dim(TSA_lon_bounds)[2]) }
    if (n_lon_steps < 1) { n_lon_steps <- 1 }
    if (TSA_lon_bounds[1, n_lon_steps] <= rc$Longitude.Degrees[i]) {
      if (TSA_lon_bounds[2, n_lon_steps] > rc$Longitude.Degrees[i]) {
        lon_grid_cell <- n_lon_steps
      } else {
        repeat {
          n_lon_steps = n_lon_steps + 1
          if (n_lon_steps > (dim(TSA_lon_bounds)[2])) { break }
          if (TSA_lon_bounds[1, n_lon_steps] <= rc$Longitude.Degrees[i]) {
            if (TSA_lon_bounds[2, n_lon_steps] > rc$Longitude.Degrees[i]) {
              break
            }
          }
        }
        lon_grid_cell <- n_lon_steps
      }
    }
    if (TSA_lon_bounds[1, n_lon_steps] > rc$Longitude.Degrees[i]) {
      repeat {
        n_lon_steps = n_lon_steps - 1
        if (n_lon_steps == 0) { break }
        if (TSA_lon_bounds[1, n_lon_steps] <= rc$Longitude.Degrees[i]) {
          if (TSA_lon_bounds[2, n_lon_steps] > rc$Longitude.Degrees[i]) {
            break
          }
        }
      }
      lon_grid_cell <- n_lon_steps
    }
  }
  Bleaching_cortad_lon_cell[i] <- lon_grid_cell
}

# Formatting dates and accounting for leap years
# The survey date string is split into day, month, and year.
# The month is converted to a “day offset” (e.g., Feb starts after 31 days, Mar after 59 days, etc.)
# The two-digit year is converted to a full year by deciding whether it belongs to the 1900s or 2000s.
# Leap days are added if the year is a leap year and the date is past February.
# Finally, the total number of days since December 31, 1981 is computed.
Bleaching_days_since_19811231 <- array(0, dim=number_of_surveys)
for (i in 1:number_of_surveys) {
  date_string <- str_split(rc$Date[i], "-")
  day_string <- date_string[[1]][1]
  day_numeric <- as.numeric(day_string)
  month_string <- date_string[[1]][2]
  if (month_string=="Jan"){ days_since_19811231_due_to_month_number <- 0 }
  if (month_string=="Feb"){ days_since_19811231_due_to_month_number <- 31 }
  if (month_string=="Mar"){ days_since_19811231_due_to_month_number <- 59 }
  if (month_string=="Apr"){ days_since_19811231_due_to_month_number <- 90 }
  if (month_string=="May"){ days_since_19811231_due_to_month_number <- 120 }
  if (month_string=="Jun"){ days_since_19811231_due_to_month_number <- 151 }
  if (month_string=="Jul"){ days_since_19811231_due_to_month_number <- 181 }
  if (month_string=="Aug"){ days_since_19811231_due_to_month_number <- 212 }
  if (month_string=="Sep"){ days_since_19811231_due_to_month_number <- 243 }
  if (month_string=="Oct"){ days_since_19811231_due_to_month_number <- 273 }
  if (month_string=="Nov"){ days_since_19811231_due_to_month_number <- 304 }
  if (month_string=="Dec"){ days_since_19811231_due_to_month_number <- 334 }
  
  year_string <- date_string[[1]][3]
  year_numeric <- as.numeric(year_string)
  century <- 1900
  if (year_numeric < 25)
    century <- 2000
  }
  full_year <- century + year_numeric
  
  # Adjust for leap years (adding extra days for February)
  leap_year_days <- 0
  if ((full_year > 1984) | (full_year == 1984 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 1 }
  if ((full_year > 1988) | (full_year == 1988 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 2 }
  if ((full_year > 1992) | (full_year == 1992 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 3 }
  if ((full_year > 1996) | (full_year == 1996 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 4 }
  if ((full_year > 2000) | (full_year == 2000 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 5 }
  if ((full_year > 2004) | (full_year == 2004 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 6 }
  if ((full_year > 2008) | (full_year == 2008 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 7 }
  if ((full_year > 2012) | (full_year == 2012 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 8 }
  if ((full_year > 2016) | (full_year == 2016 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 9 }
  
  days_since_19811231 <- ((full_year - 1982) * 365) + days_since_19811231_due_to_month_number + day_numeric + leap_year_days
  Bleaching_days_since_19811231[i] <- days_since_19811231


# Calculate day index:
# Converts the “days since” value into an index for the Cortad time dimension (assumed weekly).
Bleaching_cortad_day_index <- array(0, dim=number_of_surveys)
max_index_of_Cortad <- dim(TSA_time_bounds)[2] + 1
for (i in 1:number_of_surveys) {
  Bleaching_cortad_day_index[i] <- floor((Bleaching_days_since_19811231[i] + TSA_time_bounds[1,1]) / 7) + 1
  if (Bleaching_cortad_day_index[i] > max_index_of_Cortad) {
    Bleaching_cortad_day_index[i] <- NA
  }
}

#hold extracted Cortad variables
TSA<- array(NA, dim=number_of_surveys)
TSA.Stand.Dev<- array(NA, dim=number_of_surveys)
TSA.Min<- array(NA, dim=number_of_surveys)
TSA.Max <- array(NA, dim=number_of_surveys)
TSA.Mean<- array(NA, dim=number_of_surveys)
TSA.Freq<- array(NA, dim=number_of_surveys)
TSA.Freq.Max<- array(NA, dim=number_of_surveys)
TSA.Freq.Mean<- array(NA, dim=number_of_surveys)
TSA.Freq.Stand.Dev <- array(NA, dim=number_of_surveys)
TSA.DHW<- array(NA, dim=number_of_surveys)
TSA.DHW.Max<- array(NA, dim=number_of_surveys)
TSA.DHW.Mean<- array(NA, dim=number_of_surveys)
TSA.DHW.Stand.Dev <- array(NA, dim=number_of_surveys)
Wind.Speed<- array(NA, dim=number_of_surveys)

#defining functions to extract data from the Cortad grid:
first_pass_function_2d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid = variable_id, 
                          start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                          count = c(1,1)), silent = TRUE)
  return(result)
}

first_pass_function_3d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid = variable_id, 
                          start = c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                          count = c(1,1,1)), silent = TRUE)
  return(result)
}

second_pass_2d <- function(netcdf_variable_name, variable_id) {
  expand = 1
  result = NA
  repeat {
    expanded_grid <- try(ncvar_get(netcdf_variable_name, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) * (1 + 2 * expand))) {
      expand = expand + 1
      if (expand >= 3) { break }
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  return(result)
}

second_pass_3d <- function(netcdf_variable_name, variable_id) {
  expand = 1
  result = NA
  repeat {
    expanded_grid <- try(ncvar_get(netcdf_variable_name, varid = variable_id, 
                                   start = c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand, Bleaching_cortad_day_index[i]), 
                                   count = c(1 + 2 * expand, 1 + 2 * expand, 1)), silent = TRUE)
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) * (1 + 2 * expand))) {
      expand = expand + 1
      if (expand >= 3) { break }
    } else {
      result <- mean(expanded_grid, na.rm = TRUE)
      break
    }
  }
  return(result)
}

#loop through each survey to grab Cortad data for TSA and WindSpeed
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i])) {
    if (!is.na(Bleaching_cortad_lon_cell[i])) {
      if (!is.na(Bleaching_cortad_lat_cell[i])) {
        Wind.Speed[i] <- first_pass_function_3d(WindSpeed_nc, "wind_speed")
        TSA[i]<- first_pass_function_3d(TSA_nc,"TSA")
        TSA.Stand.Dev[i]<- first_pass_function_2d(TSA_nc,"TSA_StandardDeviation")
        TSA.Min[i]<- first_pass_function_2d(TSA_nc,"TSA_Minimum")
        TSA.Max[i]<- first_pass_function_2d(TSA_nc,"TSA_Maximum")
        TSA.Mean[i]<- first_pass_function_2d(TSA_nc,"TSA_Mean")
        TSA.Freq[i]<- first_pass_function_3d(TSA_nc,"TSA_Frequency")
        TSA.Freq.Max[i]<- first_pass_function_2d(TSA_nc,"TSA_FrequencyMax")
        TSA.Freq.Mean[i]<- first_pass_function_2d(TSA_nc,"TSA_FrequencyMean")
        TSA.Freq.Stand.Dev[i]<- first_pass_function_2d(TSA_nc,"TSA_FrequencyStandardDeviation")
        TSA.DHW[i]<- first_pass_function_3d(TSA_nc,"TSA_DHW")
        TSA.DHW.Max[i]<- first_pass_function_2d(TSA_nc,"TSA_DHWMax")
        TSA.DHW.Mean[i]<- first_pass_function_2d(TSA_nc,"TSA_DHWMean")
        TSA.DHW.Stand.Dev[i]<- first_pass_function_2d(TSA_nc,"TSA_DHWStandardDeviation")
      }
    }
  }
  print(i)
}

#problem: windspeed, TSA SD and mean columns not present:
#windspeed:


#make sure that variables are all numeric
TSA<- as.numeric(TSA)
TSA.Stand.Dev<- as.numeric(TSA.Stand.Dev)
TSA.Min<- as.numeric(TSA.Min)
TSA.Max<- as.numeric(TSA.Max)
TSA.Mean<- as.numeric(TSA.Mean)
TSA.Freq<- as.numeric(TSA.Freq)
TSA.Freq.Max<- as.numeric(TSA.Freq.Max)
TSA.Freq.Mean<- as.numeric(TSA.Freq.Mean)
TSA.Freq.Stand.Dev <- as.numeric(TSA.Freq.Stand.Dev)
TSA.DHW<- as.numeric(TSA.DHW)
TSA.DHW.Max<- as.numeric(TSA.DHW.Max)
TSA.DHW.Mean<- as.numeric(TSA.DHW.Mean)
TSA.DHW.Stand.Dev<- as.numeric(TSA.DHW.Stand.Dev)
Wind.Speed<- as.numeric(Wind.Speed)

#combine the new TSA and WindSpeed variables with the rc data
rc_with_cortad_variables <- cbind(rc, 
                                  TSA, TSA.Stand.Dev, TSA.Min, TSA.Max, TSA.Mean,
                                  TSA.Freq, TSA.Freq.Max, TSA.Freq.Mean, TSA.Freq.Stand.Dev,
                                  TSA.DHW, TSA.DHW.Max, TSA.DHW.Mean, TSA.DHW.Stand.Dev,
                                  Wind.Speed)

write.csv(rc_with_cortad_variables, file = "rc_with_cortad_variables_new.csv", row.names = FALSE)

print(paste("Survey:", i, "WindSpeed:", Wind.Speed[i]))
print(WindSpeed_nc)
names(WindSpeed_nc$var)
WindSpeed_nc$dim


# Close the NetCDF files
nc_close(TSA_nc)
nc_close(WindSpeed_nc)




#redo2----
rm(list=ls())

library("ncdf4")
library("raster")
library("dplyr")
library(ggplot2)
library(stringr)

setwd("~/Desktop/Reef Check Diss/Diss Directory")

rc<-read.csv("rc_with_cortad_variables.csv") 
number_of_surveys=dim(rc)[1] #shows how many surveys stored

TSA_nc<- nc_open("cortadv6_TSA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(TSA_nc$var)

TSA_time_bounds <- ncvar_get(TSA_nc, varid="time_bounds")
dim(TSA_time_bounds) #2 2139

TSA_lat_bounds <- ncvar_get(TSA_nc, varid="lat_bounds")
dim(TSA_lat_bounds) #2 4320

TSA_lon_bounds <- ncvar_get(TSA_nc, varid="lon_bounds")
dim(TSA_lon_bounds)  #2 8640

TSA_land <- ncvar_get(TSA_nc, varid="land")
dim(TSA_land) # 8640 x 4320 

#check grid cell size
difference <- array(0, dim=(length(TSA_lon_bounds[1,]) - 1))
for (i in 1:(dim(TSA_lon_bounds)[2] - 1)) {
  difference[i] <- TSA_lon_bounds[1, (i+1)] - TSA_lon_bounds[1, i]
} #constant value of 0.0417

#mapping reef check coordinates to Cortad grid cells 
#determines which latitude cell in the CoRTAD grid corresponds to the Reef Check survey’s latitude
# lat_step - computes the size of one grid cell in latitude.
Bleaching_cortad_lat_cell <- array(0, dim=number_of_surveys)
lat_step <- -1 * (TSA_lat_bounds[2, dim(TSA_lat_bounds)[2]] - TSA_lat_bounds[1,1]) / (dim(TSA_lat_bounds)[2] + 1)

for (i in 1:number_of_surveys) {
  lat_grid_cell <- NA
  
  if (is.na(rc$Latitude.Degrees[i])) {
    lat_grid_cell <- NA 
  } else {
    n_lat_steps <- floor((TSA_lat_bounds[1,1] - rc$Latitude.Degrees[i]) / lat_step + 1)
    
    if (TSA_lat_bounds[1, n_lat_steps] >= rc$Latitude.Degrees[i]) {
      if (TSA_lat_bounds[2, n_lat_steps] <= rc$Latitude.Degrees[i]) {
        lat_grid_cell <- n_lat_steps
      } else {
        repeat {
          n_lat_steps = n_lat_steps + 1
          if (TSA_lat_bounds[1, n_lat_steps] > rc$Latitude.Degrees[i]) {
            if (TSA_lat_bounds[2, n_lat_steps] <= rc$Latitude.Degrees[i]) {
              break
            }
          }
        }
        lat_grid_cell <- n_lat_steps
      }
    }
    
    if (TSA_lat_bounds[1, n_lat_steps] < rc$Latitude.Degrees[i]) {
      repeat {
        n_lat_steps = n_lat_steps - 1
        if (TSA_lat_bounds[1, n_lat_steps] >= rc$Latitude.Degrees[i]) {
          if (TSA_lat_bounds[2, n_lat_steps] <= rc$Latitude.Degrees[i]) {
            break
          }
        }
      }
      lat_grid_cell <- n_lat_steps
    }
  }
  Bleaching_cortad_lat_cell[i] <- lat_grid_cell
}

#does the same for longitude cells
Bleaching_cortad_lon_cell <- array(0, dim=number_of_surveys)
lon_step <- (TSA_lon_bounds[1, dim(TSA_lon_bounds)[2]] - TSA_lon_bounds[1,1]) / (dim(TSA_lon_bounds)[2] + 1)

for (i in 1:length(rc$Longitude.Degrees)) {
  lon_grid_cell <- NA
  
  if (is.na(rc$Longitude.Degrees[i])) {
    lon_grid_cell <- NA 
  } else {
    n_lon_steps <- floor(-1 * (TSA_lon_bounds[1,1] - rc$Longitude.Degrees[i]) / lon_step + 1)
    
    if (n_lon_steps > (dim(TSA_lon_bounds)[2])) { n_lon_steps <- (dim(TSA_lon_bounds)[2]) }
    if (n_lon_steps < 1) { n_lon_steps <- 1 }
    
    if (TSA_lon_bounds[1, n_lon_steps] <= rc$Longitude.Degrees[i]) {
      if (TSA_lon_bounds[2, n_lon_steps] > rc$Longitude.Degrees[i]) {
        lon_grid_cell <- n_lon_steps
      } else {
        repeat {
          n_lon_steps = n_lon_steps + 1
          if (n_lon_steps > (dim(TSA_lon_bounds)[2])) { break }
          if (TSA_lon_bounds[1, n_lon_steps] <= rc$Longitude.Degrees[i]) {
            if (TSA_lon_bounds[2, n_lon_steps] > rc$Longitude.Degrees[i]) {
              break
            }
          }
        }
        lon_grid_cell <- n_lon_steps
      }
    }
    
    if (TSA_lon_bounds[1, n_lon_steps] > rc$Longitude.Degrees[i]) {
      repeat {
        n_lon_steps = n_lon_steps - 1
        if (n_lon_steps == 0) { break }
        if (TSA_lon_bounds[1, n_lon_steps] <= rc$Longitude.Degrees[i]) {
          if (TSA_lon_bounds[2, n_lon_steps] > rc$Longitude.Degrees[i]) {
            break
          }
        }
      }
      lon_grid_cell <- n_lon_steps
    }
  }
  Bleaching_cortad_lon_cell[i] <- lon_grid_cell
}

#formatting dates and accounting for leap years 
#For each survey date: the date string is split into day, month, and year.
#The month is converted to a “day offset” (e.g. Feb starts after 31 days, Mar after 59 days, etc.).
# The two-digit year is converted to a full year by deciding whether it belongs to the 1900s or 2000s.
#Leap days are added if the year is a leap year and the date is past February.
#Finally, the total number of days since December 31, 1981 is computed. This matches the time coordinate used in the CoRTAD NetCDF files, which is in “days since 19811231.”

Bleaching_days_since_19811231 <- array(0, dim=number_of_surveys)
for (i in 1:number_of_surveys) {
  date_string <- str_split(rc$Date[i], "-")
  day_string <- date_string[[1]][1]
  day_numeric <- as.numeric(day_string)
  month_string <- date_string[[1]][2]
  if (month_string=="Jan"){ days_since_19811231_due_to_month_number <- 0 }
  if (month_string=="Feb"){ days_since_19811231_due_to_month_number <- 31 }
  if (month_string=="Mar"){ days_since_19811231_due_to_month_number <- 59 }
  if (month_string=="Apr"){ days_since_19811231_due_to_month_number <- 90 }
  if (month_string=="May"){ days_since_19811231_due_to_month_number <- 120 }
  if (month_string=="Jun"){ days_since_19811231_due_to_month_number <- 151 }
  if (month_string=="Jul"){ days_since_19811231_due_to_month_number <- 181 }
  if (month_string=="Aug"){ days_since_19811231_due_to_month_number <- 212 }
  if (month_string=="Sep"){ days_since_19811231_due_to_month_number <- 243 }
  if (month_string=="Oct"){ days_since_19811231_due_to_month_number <- 273 }
  if (month_string=="Nov"){ days_since_19811231_due_to_month_number <- 304 }
  if (month_string=="Dec"){ days_since_19811231_due_to_month_number <- 334 }
  
  year_string <- date_string[[1]][3]
  year_numeric <- as.numeric(year_string)
  century <- 1900
  if (year_numeric < 25) {  # e.g., 95 means 1995; 25 means 2025
    century <- 2000
  }
  full_year <- century + year_numeric
  
  # Adjust for leap years (adding extra days for February)
  leap_year_days <- 0
  if ((full_year > 1984) | (full_year == 1984 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 1 }
  if ((full_year > 1988) | (full_year == 1988 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 2 }
  if ((full_year > 1992) | (full_year == 1992 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 3 }
  if ((full_year > 1996) | (full_year == 1996 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 4 }
  if ((full_year > 2000) | (full_year == 2000 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 5 }
  if ((full_year > 2004) | (full_year == 2004 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 6 }
  if ((full_year > 2008) | (full_year == 2008 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 7 }
  if ((full_year > 2012) | (full_year == 2012 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 8 }
  if ((full_year > 2016) | (full_year == 2016 & month_string!="Jan" & month_string!="Feb")) { leap_year_days <- 9 }
  
  days_since_19811231 <- ((full_year - 1982) * 365) + days_since_19811231_due_to_month_number + day_numeric + leap_year_days
  Bleaching_days_since_19811231[i] <- days_since_19811231
}

#calculate day index:
#converts the “days since” value into an index for the CoRTAD time dimension. It appears that the CoRTAD data is weekly (hence the division by 7).
#If the computed index exceeds the number of time steps available, it is set to NA

Bleaching_cortad_day_index <- array(0, dim=number_of_surveys)
max_index_of_CoRTAD <- dim(TSA_time_bounds)[2] + 1
for (i in 1:number_of_surveys) {
  Bleaching_cortad_day_index[i] <- floor((Bleaching_days_since_19811231[i] + TSA_time_bounds[1,1]) / 7) + 1
  
  if (Bleaching_cortad_day_index[i] > max_index_of_CoRTAD) {
    Bleaching_cortad_day_index[i] <- NA
  }
}

#create arrays to hold other extracted Cortad variables:
Wind.Speed<- array(NA, dim=number_of_surveys)
TSA<- array(NA, dim=number_of_surveys)
TSA.Stand.Dev<- array(NA, dim=number_of_surveys)
TSA.Min<- array(NA, dim=number_of_surveys)
TSA.Max <- array(NA, dim=number_of_surveys)
TSA.Mean<- array(NA, dim=number_of_surveys)
TSA.Freq<- array(NA, dim=number_of_surveys)
TSA.Freq.Max<- array(NA, dim=number_of_surveys)
TSA.Freq.Mean<- array(NA, dim=number_of_surveys)
TSA.Freq.Stand.Dev <- array(NA, dim=number_of_surveys)
TSA.DHW<- array(NA, dim=number_of_surveys)
TSA.DHW.Max<- array(NA, dim=number_of_surveys)
TSA.DHW.Mean<- array(NA, dim=number_of_surveys)
TSA.DHW.Stand.Dev <- array(NA, dim=number_of_surveys)

#required NetCDF files 
TSA_nc <- nc_open("cortadv6_TSA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(TSA_nc$var) 

WindSpeed <- nc_open("cortadv6_WindSpeed.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(WindSpeed$var)

HarmonicsClimatology <- nc_open("cortadv6_HarmonicsClimatology.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(HarmonicsClimatology$var) #ClimSST

#defining functions to extract data from CorTAD grid:
first_pass_function_Harmonics <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], (Bleaching_cortad_day_index[i] %% 52 + 1)), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}

first_pass_function_2d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i]), 
                          count=c(1,1)), silent=TRUE)
  return(result)
}

first_pass_function_3d <- function(netcdf_variable_name, variable_id) {
  result <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                          start=c(Bleaching_cortad_lon_cell[i], Bleaching_cortad_lat_cell[i], Bleaching_cortad_day_index[i]), 
                          count=c(1,1,1)), silent=TRUE)
  return(result)
}



#second pass
second_pass_2d <- function(netcdf_variable_name, variable_id) {
  expand = 1
  result = NA
  repeat {
    expanded_grid <- try(ncvar_get(netcdf_variable_name, varid=variable_id, 
                                   start=c(Bleaching_cortad_lon_cell[i] - expand, Bleaching_cortad_lat_cell[i] - expand), 
                                   count=c(1 + 2 * expand, 1 + 2 * expand)), silent=TRUE)
    
    if (sum(is.na(expanded_grid)) == ((1 + 2 * expand) * (1 + 2 * expand))) {
      expand = expand + 1
      if (expand >= 3) { break }
    } else {
      result <- mean(expanded_grid, na.rm=TRUE)
      break
    }
  }
  return(result)
}

second_pass_3d<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand),Bleaching_cortad_day_index[i]), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}

second_pass_Harmonics<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand), (Bleaching_cortad_day_index[i]%%52)+1), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}

#loop reef check survey to grab cortad data
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_day_index[i])) {
    if (!is.na(Bleaching_cortad_lon_cell[i])) {
      if (!is.na(Bleaching_cortad_lat_cell[i])) {
        Wind.Speed[i] <- first_pass_function_Harmonics(WindSpeed, "wind_speed")
        TSA[i] <- first_pass_function_3d(TSA_nc, "TSA")
        TSA.Stand.Dev[i] <- first_pass_function_2d(TSA_nc, "TSA_StandardDeviation")
        TSA.Min[i] <- first_pass_function_2d(TSA_nc, "TSA_Minimum")
        TSA.Max[i] <- first_pass_function_2d(TSA_nc, "TSA_Maximum")
        TSA.Mean[i] <- first_pass_function_2d(TSA_nc, "TSA_Mean")
        TSA.Freq[i] <- first_pass_function_3d(TSA_nc, "TSA_Frequency")
        TSA.Freq.Stand.Dev <- first_pass_function_2d(TSA_nc, "TSA_FrequencyStandardDeviation")
        TSA.Freq.Max[i] <- first_pass_function_2d(TSA_nc, "TSA_FrequencyMax")
        TSA.Freq.Mean[i] <- first_pass_function_2d(TSA_nc, "TSA_FrequencyMean")
        TSA.DHW[i] <- first_pass_function_3d(TSA_nc, "TSA_DHW")
        TSA.DHW.Stand.Dev[i] <- first_pass_function_2d(TSA_nc, "TSA_DHWStandardDeviation")
        TSA.DHW.Max[i] <- first_pass_function_2d(TSA_nc, "TSA_DHWMax")
        TSA.DHW.Mean[i] <- first_pass_function_2d(TSA_nc, "TSA_DHWMean")
      }
    }
  }
  print(i)
}

str(Wind.Speed)  # Check Wind.Speed data type
str(TSA)         # Check TSA data type
class(Wind.Speed)
class(TSA)

TSA <- unlist(TSA)
TSA.Stand.Dev<- unlist(TSA.Stand.Dev)
TSA.Min<- unlist(TSA.Min)
TSA.Max<- unlist(TSA.Max)
TSA.Mean<- unlist(TSA.Mean)
TSA.Freq<- unlist(TSA.Freq)
TSA.Freq.Max<- unlist(TSA.Freq.Max)
TSA.Freq.Mean<- unlist(TSA.Freq.Mean)
TSA.Freq.Stand.Dev <- unlist(TSA.Freq.Stand.Dev)
TSA.DHW<- unlist(TSA.DHW)
TSA.DHW.Max<- unlist(TSA.DHW.Max)
TSA.DHW.Mean<- unlist(TSA.DHW.Mean)
TSA.DHW.Stand.Dev<- unlist(TSA.DHW.Stand.Dev)

#numeric conversion:
Wind.Speed<-as.numeric(Wind.Speed)
TSA<- as.numeric(TSA)
TSA.Stand.Dev<- as.numeric(TSA.Stand.Dev)
TSA.Min<- as.numeric(TSA.Min)
TSA.Max<- as.numeric(TSA.Max)
TSA.Mean<- as.numeric(TSA.Mean)
TSA.Freq<- as.numeric(TSA.Freq)
TSA.Freq.Max<- as.numeric(TSA.Freq.Max)
TSA.Freq.Mean<- as.numeric(TSA.Freq.Mean)
TSA.Freq.Stand.Dev <- as.numeric(TSA.Freq.Stand.Dev)
TSA.DHW<- as.numeric(TSA.DHW)
TSA.DHW.Max<- as.numeric(TSA.DHW.Max)
TSA.DHW.Mean<- as.numeric(TSA.DHW.Mean)
TSA.DHW.Stand.Dev<- as.numeric(TSA.DHW.Stand.Dev)

#second passing:
for (i in 1:number_of_surveys) {
  if (!is.na(Bleaching_cortad_lon_cell[i])) {
    if (!is.na(Bleaching_cortad_lat_cell[i])) {
      if (!is.na(Bleaching_cortad_day_index[i])) {
        
        if (is.na(Wind.Speed[i])) {
          Wind.Speed[i] <- second_pass_3d(WindSpeed, "wind_speed")
        }
        if (is.na(TSA[i])) {
          TSA[i] <- second_pass_3d(TSA_nc, "TSA")
        }
        if (is.na(TSA.Stand.Dev[i])) {
          TSA.Stand.Dev[i] <- second_pass_2d(TSA_nc, "TSA_StandardDeviation")
        }
        if (is.na(TSA.Min[i])) {
          TSA.Min[i] <- second_pass_2d(TSA_nc, "TSA_Minimum")
        }
        if (is.na(TSA.Max[i])) {
          TSA.Max[i] <- second_pass_2d(TSA_nc, "TSA_Maximum")
        }
        if (is.na(TSA.Mean[i])) {
          TSA.Mean[i] <- second_pass_2d(TSA_nc, "TSA_Mean")
        }
        if (is.na(TSA.Freq[i])){
          TSA.Freq[i] <- second_pass_3d(TSA_nc, "TSA_Frequency")
        }
        if (is.na(TSA.Freq.Stand.Dev[i])) {
          TSA.Freq.Stand.Dev[i] <- second_pass_2d(TSA_nc, "TSA_StandardDeviation")
        }
        if (is.na(TSA.Freq.Max[i])) {
          TSA.Freq.Max[i] <- second_pass_2d(TSA_nc, "TSA_FrequencyMax")
        }
        if (is.na(TSA.Freq.Mean[i])) {
          TSA.Freq.Mean[i] <- second_pass_2d(TSA_nc, "TSA_FrequencyMean")
        }
        if (is.na(TSA.DHW[i])) {
          TSA.DHW[i] <- second_pass_3d(TSA_nc, "TSA_DHW")
        }
        if (is.na(TSA.DHW.Stand.Dev[i])) {
          TSA.DHW.Stand.Dev[i] <- second_pass_2d(TSA_nc, "TSA_DHWStandardDeviation")
        }
        if (is.na(TSA.DHW.Max[i])) {
          TSA.DHW.Max[i] <- second_pass_2d(TSA_nc, "TSA_DHWMax")
        }
        if (is.na(TSA.DHW.Mean[i])) {
          TSA.DHW.Mean[i] <- second_pass_2d(TSA_nc, "TSA_DHWMean")
        }
        print(i)
      }
    }
  }
}

min_rows <- min(nrow(rc), length(Wind.Speed), length(TSA_nc))

# Trim each variable
Wind.Speed <- Wind.Speed[1:min_rows]
TSA <- TSA[1:min_rows]
TSA.Stand.Dev <- TSA.Stand.Dev[1:min_rows]
TSA.Min <- TSA.Min[1:min_rows]
TSA.Max <- TSA.Max[1:min_rows]
TSA.Mean <- TSA.Mean[1:min_rows]
TSA.Freq <- TSA.Freq[1:min_rows]
TSA.Freq.Stand.Dev <- TSA.Freq.Stand.Dev[1:min_rows]
TSA.Freq.Max <- TSA.Freq.Max[1:min_rows]
TSA.Freq.Mean <- TSA.Freq.Mean[1:min_rows]
TSA.DHW <- TSA.DHW[1:min_rows]
TSA.DHW.Stand.Dev <- TSA.DHW.Stand.Dev[1:min_rows]
TSA.DHW.Max <- TSA.DHW.Max[1:min_rows]
TSA.DHW.Mean <- TSA.DHW.Mean[1:min_rows]

rc_with_cortad_variables <- cbind(rc, Wind.Speed, TSA, TSA.Stand.Dev, TSA.Min, 
                                      TSA.Max, TSA.Mean, TSA.Freq, TSA.Freq.Stand.Dev, 
                                      TSA.Freq.Max, TSA.Freq.Mean, TSA.DHW, TSA.DHW.Stand.Dev,
                                      TSA.DHW.Max, TSA.DHW.Mean)

#problem with dataframe, not matching 
print(nrow(rc)) 
print(length(Wind.Speed))  
print(length(TSA)) 
print(length(TSA.Mean))  
print(length(TSA.Stand.Dev))  
print(length(TSA.Min))  
print(length(TSA.Max))  
print(length(TSA.Freq)) 
print(length(TSA.Freq.Stand.Dev)) #12678 not 12679
print(length(TSA.Freq.Max)) 
print(length(TSA.Freq.Mean)) 
print(length(TSA.DHW)) 
print(length(TSA.DHW.Stand.Dev)) 
print(length(TSA.DHW.Max))  
print(length(TSA.DHW.Mean)) 

TSA.Freq.Stand.Dev[length(TSA.Freq.Stand.Dev) + 1] <- NA

Wind.Speed <- round(Wind.Speed, 2)
TSA <- round(TSA, 2)
TSA.Stand.Dev <- round(TSA.Stand.Dev, 2)
TSA.Min <- round(TSA.Min, 2)
TSA.Max <- round(TSA.Max, 2)
TSA.Mean <- round(TSA.Mean, 2)
TSA.Freq <- round(TSA.Freq, 2)
TSA.Freq.Stand.Dev <- round(TSA.Freq.Stand.Dev, 2)
TSA.Freq.Max <- round(TSA.Freq.Max, 2)
TSA.Freq.Mean <- round(TSA.Freq.Mean, 2)
TSA.DHW <- round(TSA.DHW, 2)
TSA.DHW.Stand.Dev <- round(TSA.DHW.Stand.Dev, 2)
TSA.DHW.Max <- round(TSA.DHW.Max, 2)
TSA.DHW.Mean <- round(TSA.DHW.Mean, 2)

rc_with_cortad_variables <- cbind(rc, Wind.Speed, TSA, TSA.Stand.Dev, TSA.Min, 
                                  TSA.Max, TSA.Mean, TSA.Freq, TSA.Freq.Stand.Dev, 
                                  TSA.Freq.Max, TSA.Freq.Mean, TSA.DHW, TSA.DHW.Stand.Dev,
                                  TSA.DHW.Max, TSA.DHW.Mean)


write.csv(rc_with_cortad_variables, file = "rc_with_cortad_variables_new.csv")

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

sum (is.na(Temperature_Kelvin))  #1744
sum(is.na(Temperature_Kelvin_Standard_Deviation)) #1744
sum(is.na(Windspeed))  #0
sum(is.na(SSTA_data)) #2
sum(is.na(SSTA_Standard_Deviation)) #2
sum(is.na(SSTA_Mean)) #2
sum(is.na(SSTA_Maximum)) #2
sum(is.na(SSTA_Freq)) #2
sum(is.na(SSTA_Freq_Standard_Deviation)) #2
sum(is.na(SSTA_Freq_Max)) #2
sum(is.na(SSTA_Freq_Mean)) #2
sum(is.na(SSTA_DHW)) #2
sum(is.na(SSTA_DHW_Standard_Deviation)) #2
sum(is.na(SSTA_DHW_Max)) #2
sum(is.na(SSTA_DHW_Mean)) #2
sum(is.na(TSA_data)) #2
sum(is.na(TSA_Standard_Deviation)) #2
sum(is.na(TSA_Maximum)) #2
sum(is.na(TSA_Mean)) #2
sum(is.na(TSA_Freq)) #2
sum(is.na(TSA_Freq_Standard_Deviation)) #2
sum(is.na(TSA_Freq_Max)) #2
sum(is.na(TSA_Freq_Mean)) #2
sum(is.na(TSA_DHW)) #2
sum(is.na(TSA_DHW_Standard_Deviation)) #2
sum(is.na(TSA_DHW_Max)) #2
sum(is.na(TSA_DHW_Mean)) #2

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

# Remove surveys from years 2023 and 2024
bleaching_with_cortad_variables <- bleaching_with_cortad_variables %>%
  mutate(Date2 = as.Date(Date, format = "%d-%b-%y")) %>%
  filter(!(format(Date2, "%Y") %in% c("2023", "2024")))

write.csv(bleaching_with_cortad_variables, "cw_cortad_2022.csv", row.names = FALSE)



