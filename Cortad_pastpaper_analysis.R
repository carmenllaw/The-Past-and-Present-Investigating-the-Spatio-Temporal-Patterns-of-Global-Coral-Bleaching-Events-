rm(list=ls())

library(ncdf4)
library(raster)
library(dplyr)
library(ggplot2)
library(stringr)

#combining pastpaper cleaned data and new data from Dec 2017 onwards:
pp<-read.csv("pp_data.csv")
rc_new<-read.csv("rc_cortad_2022.csv")
rc_manual<-read.csv("rc_manual_ecoregions.csv")
final<-read.csv("final_everything_last_one.csv")
#rate of change calculations----
sst<-stack("sst.mon.mean.nc")

r1982<-mean(stack(sst$X1982.01.01,sst$X1982.02.01,sst$X1982.03.01,sst$X1982.04.01,sst$X1982.05.01,sst$X1982.06.01,sst$X1982.07.01,sst$X1982.08.01,sst$X1982.09.01,sst$X1982.10.01,sst$X1982.11.01,sst$X1982.12.01))
r1983<-mean(stack(sst$X1983.01.01,sst$X1983.02.01,sst$X1983.03.01,sst$X1983.04.01,sst$X1983.05.01,sst$X1983.06.01,sst$X1983.07.01,sst$X1983.08.01,sst$X1983.09.01,sst$X1983.10.01,sst$X1983.11.01,sst$X1983.12.01))
r1984<-mean(stack(sst$X1984.01.01,sst$X1984.02.01,sst$X1984.03.01,sst$X1984.04.01,sst$X1984.05.01,sst$X1984.06.01,sst$X1984.07.01,sst$X1984.08.01,sst$X1984.09.01,sst$X1984.10.01,sst$X1984.11.01,sst$X1984.12.01))
r1985<-mean(stack(sst$X1985.01.01,sst$X1985.02.01,sst$X1985.03.01,sst$X1985.04.01,sst$X1985.05.01,sst$X1985.06.01,sst$X1985.07.01,sst$X1985.08.01,sst$X1985.09.01,sst$X1985.10.01,sst$X1985.11.01,sst$X1985.12.01))
r1986<-mean(stack(sst$X1986.01.01,sst$X1986.02.01,sst$X1986.03.01,sst$X1986.04.01,sst$X1986.05.01,sst$X1986.06.01,sst$X1986.07.01,sst$X1986.08.01,sst$X1986.09.01,sst$X1986.10.01,sst$X1986.11.01,sst$X1986.12.01))
r1987<-mean(stack(sst$X1987.01.01,sst$X1987.02.01,sst$X1987.03.01,sst$X1987.04.01,sst$X1987.05.01,sst$X1987.06.01,sst$X1987.07.01,sst$X1987.08.01,sst$X1987.09.01,sst$X1987.10.01,sst$X1987.11.01,sst$X1987.12.01))
r1988<-mean(stack(sst$X1988.01.01,sst$X1988.02.01,sst$X1988.03.01,sst$X1988.04.01,sst$X1988.05.01,sst$X1988.06.01,sst$X1988.07.01,sst$X1988.08.01,sst$X1988.09.01,sst$X1988.10.01,sst$X1988.11.01,sst$X1988.12.01))
r1989<-mean(stack(sst$X1989.01.01,sst$X1989.02.01,sst$X1989.03.01,sst$X1989.04.01,sst$X1989.05.01,sst$X1989.06.01,sst$X1989.07.01,sst$X1989.08.01,sst$X1989.09.01,sst$X1989.10.01,sst$X1989.11.01,sst$X1989.12.01))

r1990<-mean(stack(sst$X1990.01.01,sst$X1990.02.01,sst$X1990.03.01,sst$X1990.04.01,sst$X1990.05.01,sst$X1990.06.01,sst$X1990.07.01,sst$X1990.08.01,sst$X1990.09.01,sst$X1990.10.01,sst$X1990.11.01,sst$X1990.12.01))
r1991<-mean(stack(sst$X1991.01.01,sst$X1991.02.01,sst$X1991.03.01,sst$X1991.04.01,sst$X1991.05.01,sst$X1991.06.01,sst$X1991.07.01,sst$X1991.08.01,sst$X1991.09.01,sst$X1991.10.01,sst$X1991.11.01,sst$X1991.12.01))
r1992<-mean(stack(sst$X1992.01.01,sst$X1992.02.01,sst$X1992.03.01,sst$X1992.04.01,sst$X1992.05.01,sst$X1992.06.01,sst$X1992.07.01,sst$X1992.08.01,sst$X1992.09.01,sst$X1992.10.01,sst$X1992.11.01,sst$X1992.12.01))
r1993<-mean(stack(sst$X1993.01.01,sst$X1993.02.01,sst$X1993.03.01,sst$X1993.04.01,sst$X1993.05.01,sst$X1993.06.01,sst$X1993.07.01,sst$X1993.08.01,sst$X1993.09.01,sst$X1993.10.01,sst$X1993.11.01,sst$X1993.12.01))
r1994<-mean(stack(sst$X1994.01.01,sst$X1994.02.01,sst$X1994.03.01,sst$X1994.04.01,sst$X1994.05.01,sst$X1994.06.01,sst$X1994.07.01,sst$X1994.08.01,sst$X1994.09.01,sst$X1994.10.01,sst$X1994.11.01,sst$X1994.12.01))
r1995<-mean(stack(sst$X1995.01.01,sst$X1995.02.01,sst$X1995.03.01,sst$X1995.04.01,sst$X1995.05.01,sst$X1995.06.01,sst$X1995.07.01,sst$X1995.08.01,sst$X1995.09.01,sst$X1995.10.01,sst$X1995.11.01,sst$X1995.12.01))
r1996<-mean(stack(sst$X1996.01.01,sst$X1996.02.01,sst$X1996.03.01,sst$X1996.04.01,sst$X1996.05.01,sst$X1996.06.01,sst$X1996.07.01,sst$X1996.08.01,sst$X1996.09.01,sst$X1996.10.01,sst$X1996.11.01,sst$X1996.12.01))
r1997<-mean(stack(sst$X1997.01.01,sst$X1997.02.01,sst$X1997.03.01,sst$X1997.04.01,sst$X1997.05.01,sst$X1997.06.01,sst$X1997.07.01,sst$X1997.08.01,sst$X1997.09.01,sst$X1997.10.01,sst$X1997.11.01,sst$X1997.12.01))
r1998<-mean(stack(sst$X1998.01.01,sst$X1998.02.01,sst$X1998.03.01,sst$X1998.04.01,sst$X1998.05.01,sst$X1998.06.01,sst$X1998.07.01,sst$X1998.08.01,sst$X1998.09.01,sst$X1998.10.01,sst$X1998.11.01,sst$X1998.12.01))
r1999<-mean(stack(sst$X1999.01.01,sst$X1999.02.01,sst$X1999.03.01,sst$X1999.04.01,sst$X1999.05.01,sst$X1999.06.01,sst$X1999.07.01,sst$X1999.08.01,sst$X1999.09.01,sst$X1999.10.01,sst$X1999.11.01,sst$X1999.12.01))
r2000<-mean(stack(sst$X2000.01.01,sst$X2000.02.01,sst$X2000.03.01,sst$X2000.04.01,sst$X2000.05.01,sst$X2000.06.01,sst$X2000.07.01,sst$X2000.08.01,sst$X2000.09.01,sst$X2000.10.01,sst$X2000.11.01,sst$X2000.12.01))
r2001<-mean(stack(sst$X2001.01.01,sst$X2001.02.01,sst$X2001.03.01,sst$X2001.04.01,sst$X2001.05.01,sst$X2001.06.01,sst$X2001.07.01,sst$X2001.08.01,sst$X2001.09.01,sst$X2001.10.01,sst$X2001.11.01,sst$X2001.12.01))
r2002<-mean(stack(sst$X2002.01.01,sst$X2002.02.01,sst$X2002.03.01,sst$X2002.04.01,sst$X2002.05.01,sst$X2002.06.01,sst$X2002.07.01,sst$X2002.08.01,sst$X2002.09.01,sst$X2002.10.01,sst$X2002.11.01,sst$X2002.12.01))
r2003<-mean(stack(sst$X2003.01.01,sst$X2003.02.01,sst$X2003.03.01,sst$X2003.04.01,sst$X2003.05.01,sst$X2003.06.01,sst$X2003.07.01,sst$X2003.08.01,sst$X2003.09.01,sst$X2003.10.01,sst$X2003.11.01,sst$X2003.12.01))
r2004<-mean(stack(sst$X2004.01.01,sst$X2004.02.01,sst$X2004.03.01,sst$X2004.04.01,sst$X2004.05.01,sst$X2004.06.01,sst$X2004.07.01,sst$X2004.08.01,sst$X2004.09.01,sst$X2004.10.01,sst$X2004.11.01,sst$X2004.12.01))
r2005<-mean(stack(sst$X2005.01.01,sst$X2005.02.01,sst$X2005.03.01,sst$X2005.04.01,sst$X2005.05.01,sst$X2005.06.01,sst$X2005.07.01,sst$X2005.08.01,sst$X2005.09.01,sst$X2005.10.01,sst$X2005.11.01,sst$X2005.12.01))
r2006<-mean(stack(sst$X2006.01.01,sst$X2006.02.01,sst$X2006.03.01,sst$X2006.04.01,sst$X2006.05.01,sst$X2006.06.01,sst$X2006.07.01,sst$X2006.08.01,sst$X2006.09.01,sst$X2006.10.01,sst$X2006.11.01,sst$X2006.12.01))
r2007<-mean(stack(sst$X2007.01.01,sst$X2007.02.01,sst$X2007.03.01,sst$X2007.04.01,sst$X2007.05.01,sst$X2007.06.01,sst$X2007.07.01,sst$X2007.08.01,sst$X2007.09.01,sst$X2007.10.01,sst$X2007.11.01,sst$X2007.12.01))
r2008<-mean(stack(sst$X2008.01.01,sst$X2008.02.01,sst$X2008.03.01,sst$X2008.04.01,sst$X2008.05.01,sst$X2008.06.01,sst$X2008.07.01,sst$X2008.08.01,sst$X2008.09.01,sst$X2008.10.01,sst$X2008.11.01,sst$X2008.12.01))
r2009<-mean(stack(sst$X2009.01.01,sst$X2009.02.01,sst$X2009.03.01,sst$X2009.04.01,sst$X2009.05.01,sst$X2009.06.01,sst$X2009.07.01,sst$X2009.08.01,sst$X2009.09.01,sst$X2009.10.01,sst$X2009.11.01,sst$X2009.12.01))

r2010<-mean(stack(sst$X2010.01.01,sst$X2010.02.01,sst$X2010.03.01,sst$X2010.04.01,sst$X2010.05.01,sst$X2010.06.01,sst$X2010.07.01,sst$X2010.08.01,sst$X2010.09.01,sst$X2010.10.01,sst$X2010.11.01,sst$X2010.12.01))
r2011<-mean(stack(sst$X2011.01.01,sst$X2011.02.01,sst$X2011.03.01,sst$X2011.04.01,sst$X2011.05.01,sst$X2011.06.01,sst$X2011.07.01,sst$X2011.08.01,sst$X2011.09.01,sst$X2011.10.01,sst$X2011.11.01,sst$X2011.12.01))
r2012<-mean(stack(sst$X2012.01.01,sst$X2012.02.01,sst$X2012.03.01,sst$X2012.04.01,sst$X2012.05.01,sst$X2012.06.01,sst$X2012.07.01,sst$X2012.08.01,sst$X2012.09.01,sst$X2012.10.01,sst$X2012.11.01,sst$X2012.12.01))
r2013<-mean(stack(sst$X2013.01.01,sst$X2013.02.01,sst$X2013.03.01,sst$X2013.04.01,sst$X2013.05.01,sst$X2013.06.01,sst$X2013.07.01,sst$X2013.08.01,sst$X2013.09.01,sst$X2013.10.01,sst$X2013.11.01,sst$X2013.12.01))
r2014<-mean(stack(sst$X2014.01.01,sst$X2014.02.01,sst$X2014.03.01,sst$X2014.04.01,sst$X2014.05.01,sst$X2014.06.01,sst$X2014.07.01,sst$X2014.08.01,sst$X2014.09.01,sst$X2014.10.01,sst$X2014.11.01,sst$X2014.12.01))
r2015<-mean(stack(sst$X2015.01.01,sst$X2015.02.01,sst$X2015.03.01,sst$X2015.04.01,sst$X2015.05.01,sst$X2015.06.01,sst$X2015.07.01,sst$X2015.08.01,sst$X2015.09.01,sst$X2015.10.01,sst$X2015.11.01,sst$X2015.12.01))
r2016<-mean(stack(sst$X2016.01.01,sst$X2016.02.01,sst$X2016.03.01,sst$X2016.04.01,sst$X2016.05.01,sst$X2016.06.01,sst$X2016.07.01,sst$X2016.08.01,sst$X2016.09.01,sst$X2016.10.01,sst$X2016.11.01,sst$X2016.12.01))
r2017<-mean(stack(sst$"X2017.01.01",sst$"X2017.02.01",sst$"X2017.03.01" ,sst$"X2017.04.01" ,sst$"X2017.05.01" ,sst$"X2017.06.01" ,sst$"X2017.07.01" ,sst$"X2017.08.01" ,sst$"X2017.09.01",sst$"X2017.10.01" ,sst$"X2017.11.01" ,sst$"X2017.12.01"))

r2018<-mean(stack(sst$"X2018.01.01",sst$"X2018.02.01",sst$"X2018.03.01" ,sst$"X2018.04.01" ,sst$"X2018.05.01" ,sst$"X2018.06.01" ,sst$"X2018.07.01" ,sst$"X2018.08.01" ,sst$"X2018.09.01",sst$"X2018.10.01" ,sst$"X2018.11.01" ,sst$"X2018.12.01"))
r2019<-mean(stack(sst$"X2019.01.01",sst$"X2019.02.01",sst$"X2019.03.01" ,sst$"X2019.04.01" ,sst$"X2019.05.01" ,sst$"X2019.06.01" ,sst$"X2019.07.01" ,sst$"X2019.08.01" ,sst$"X2019.09.01",sst$"X2019.10.01" ,sst$"X2019.11.01" ,sst$"X2019.12.01"))
r2020<-mean(stack(sst$"X2020.01.01",sst$"X2020.02.01",sst$"X2020.03.01" ,sst$"X2020.04.01" ,sst$"X2020.05.01" ,sst$"X2020.06.01" ,sst$"X2020.07.01" ,sst$"X2020.08.01" ,sst$"X2020.09.01",sst$"X2020.10.01" ,sst$"X2020.11.01" ,sst$"X2020.12.01"))
r2021<-mean(stack(sst$"X2021.01.01",sst$"X2021.02.01",sst$"X2021.03.01" ,sst$"X2021.04.01" ,sst$"X2021.05.01" ,sst$"X2021.06.01" ,sst$"X2021.07.01" ,sst$"X2021.08.01" ,sst$"X2021.09.01",sst$"X2021.10.01" ,sst$"X2021.11.01" ,sst$"X2021.12.01"))
r2022<-mean(stack(sst$"X2022.01.01",sst$"X2022.02.01",sst$"X2022.03.01" ,sst$"X2022.04.01" ,sst$"X2022.05.01" ,sst$"X2022.06.01" ,sst$"X2022.07.01" ,sst$"X2022.08.01" ,sst$"X2022.09.01",sst$"X2022.10.01" ,sst$"X2022.11.01" ,sst$"X2022.12.01"))

#Calculate rate of change using years 1984 and 2017. We did not want to select ENSO years when doing the calculation. Treat the timeframe as if it were mid 1984 to mid 2017 instead of beginning of 1984 and end of 2017, because we are using the mean value for each year
#rc_2022
rate<-(r2022-r2017)/5

rate<-(r2022-r1998)/24

rate<-(r2022-r2003)/19

#cw
rate<-(r2022-r2003)/19
rate<-rotate(rate)

png(filename = "SST_rate.png4", res = 300, width = 2600, height = 1000)

# Set graphical parameters and plot the raster
par(mgp = c(0.01, 0.01, 0), mar = c(1, 1, 1, 1))
plot(rate, main = "SST Rate of Change")

# Load coastline and country data and overlay them
library(rworldmap)
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = TRUE)
plot(countriesLow, add = TRUE)
dev.off()

writeRaster(rate,'SST rate',overwrite=TRUE)
#make spatial points data frame from the Reef Check data
coordinates(rc_joined)<-~Longitude.Degrees+Latitude.Degrees 
coordinates(cw_joined)<-~Longitude.Degrees+Latitude.Degrees 
coordinates(final)<-~Longitude.Degrees+Latitude.Degrees 
#For each Reef Check survey, get the corresponding rate of SST change
SST_rate_of_change<-raster::extract(rate, rc_joined)
SST_rate_of_change<-raster::extract(rate, cw_joined)
SST_rate_of_change<-raster::extract(rate, final)
write.csv(SST_rate_of_change, "All_SST_rate_of_change_NOAA_1_km_res.csv")

#combining rc data with rate of SST change and ecoregions ----
rm(list=ls())
library(sf)
library(nngeo)
library(dplyr) 

rc_new<-read.csv("rc_cortad_2022.csv")
diversity<-read.csv("Coral_diversity.csv")

ECO <- st_read(
  dsn   = "ecoregions_github", 
  layer = "ecoregion_exportPolygon"
)

ECO_noholes <- st_remove_holes(ECO)
print(ECO_noholes)

#extract geometry to rc
rc_eco <- st_as_sf(rc_new, 
                  coords = c("Longitude.Degrees", "Latitude.Degrees"), 
                  crs = 4326, 
                  remove = FALSE)

rc_eco <- st_transform(rc_eco, st_crs(ECO_noholes))
rc_joined <- st_join(rc_eco, ECO_noholes[, c("ERG", "Ecoregion")])

#fix errors: 
#ECO_noholes_valid <- st_make_valid(ECO_noholes)
#rc_joined <- st_join(rc_eco, ECO_noholes_valid[, c("ECO_CODE", "ECOREGION")])

rc_joined <- rc_joined %>%
  rename(Ecoregion_code = ERG) 

rc_joined$Ecoregion <- as.character(rc_joined$Ecoregion)
diversity$ECOREGION <- as.character(diversity$ECOREGION)

rc_joined$Ecoregion <- trimws(rc_joined$Ecoregion)
diversity$ECOREGION <- trimws(diversity$ECOREGION)

rc_joined <- rc_joined %>%
  left_join(diversity, by = c("Ecoregion" = "ECOREGION"))

#check for missing data
rc_joined <- rc_joined[!rowSums(is.na(rc_joined[, c("ClimSST", "Temperature_Kelvin", "Temperature_Mean", "Temperature_Minimum", "Temperature_Maximum", 
                                      "Temperature_Kelvin_Standard_Deviation", "Windspeed",
                                      "SSTA", "SSTA_Standard_Deviation", "SSTA_Mean", "SSTA_Minimum", "SSTA_Maximum", 
                                      "SSTA_Freq", "SSTA_Freq_Standard_Deviation", "SSTA_Freq_Max", "SSTA_Freq_Mean", "SSTA_DHW", 
                                      "SSTA_DHW_Standard_Deviation", "SSTA_DHW_Max", "SSTA_DHW_Mean", 
                                      "TSA", "TSA_Standard_Deviation", "TSA_Mean", "TSA_Minimum", "TSA_Maximum", 
                                      "TSA_Freq", "TSA_Freq_Standard_Deviation", "TSA_Freq_Mean", "TSA_Freq_Max", "TSA_DHW", 
                                      "TSA_DHW_Standard_Deviation", "TSA_DHW_Max", "TSA_DHW_Mean")])), ]
sum(is.na(rc_joined$Diversity)) #586

sum(is.na(pp$Diversity)) #586

unique(rc_joined$Ecoregion)
unique(diversity$ECOREGION)

sum(rc_joined$Ecoregion %in% diversity$ECOREGION)
mismatched_ecoregions <- rc_joined$Ecoregion[!rc_joined$Ecoregion %in% diversity$ECOREGION]
unique(mismatched_ecoregions)

#manually match unmatched ecoregions:
#library(writexl)
#write_xlsx(rc_joined, "rc_joined_manual_ecoregions.xlsx")

#load manually inputted NA ecoregions and windspeed.csv:
rc_joined<- read.csv("rc_manual_ecoregions.csv")

#join rate of SST change
annual_rate_of_SST_change<- read.csv(file="SST_rate_of_change_NOAA_1_km_res.csv", header=TRUE, sep=",")
annual_rate_of_SST_change<- read.csv(file="All_SST_rate_of_change_NOAA_1_km_res.csv", header=TRUE, sep=",")
names(annual_rate_of_SST_change)[2]<-"annual_rate_of_SST_change"
rc_cortad_eco_rate<-cbind(rc_joined, "rate_of_SST_change"=annual_rate_of_SST_change[,2])
final_rate<-cbind(final, "rate_of_SST_change"=annual_rate_of_SST_change[,2])
write.csv(final_rate,"final_changed_rate.csv", row.names=FALSE)


#further cleaning to make everything consistent 
names(rc_cortad_eco_rate)[32]<-"SSTA_Frequency"
names(rc_cortad_eco_rate)[33]<-"SSTA_Frequency_Standard_Deviation"
names(rc_cortad_eco_rate)[34]<-"SSTA_FrequencyMax"
names(rc_cortad_eco_rate)[35]<-"SSTA_FrequencyMean"
names(rc_cortad_eco_rate)[38]<-"SSTA_DHWMax"
names(rc_cortad_eco_rate)[39]<-"SSTA_DHWMean"
names(rc_cortad_eco_rate)[45]<-"TSA_Frequency"
names(rc_cortad_eco_rate)[46]<-"TSA_Frequency_Standard_Deviation"
names(rc_cortad_eco_rate)[47]<-"TSA_FrequencyMean"
names(rc_cortad_eco_rate)[48]<-"TSA_FrequencyMax"
names(rc_cortad_eco_rate)[51]<-"TSA_DHWMax"
names(rc_cortad_eco_rate)[52]<-"TSA_DHWMean"
names(rc_cortad_eco_rate)[53]<-"Region"
names(rc_cortad_eco_rate)[54]<-"Ecoregion_name"

#FIX THE NAMES, RECOUNT THE COLUMNS
colnames(rc_cortad_eco_rate)
rc_backup <- rc_cortad_eco_rate
rc_cortad_eco_rate_1 <- rc_cortad_eco_rate[,c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
                                              "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",                                   
                                              "Errors.","What.errors.", "Average_bleaching","ClimSST","Temperature_Kelvin",                    
                                              "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",                  
                                              "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",                                  
                                              "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",                        
                                              "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",     
                                              "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",                             
                                              "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",                                  
                                              "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",                          
                                              "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                              "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                              "TSA_DHWMean","Region","Ecoregion_name","Diversity","rate_of_SST_change","Longitude.Degrees",                    
                                              "Latitude.Degrees")]

write.csv(rc_cortad_eco_rate_1,"final_rc_cer.csv", row.names=FALSE)

#clean NAs and add Bleached and Severity columns----
rm(list=ls())
library(dplyr)

rc_new<-read.csv("final_rc_cer.csv")
pp<-read.csv("pp_data.csv")

#checking dataset for NAs, and data distribution
nrow(rc_new)
sum(is.na(rc_new$Longitude_degrees))
sum(is.na(rc_new$Errors.))
sum(is.na(rc_new$Average_bleaching))
sum(is.na(rc_new$Temperature_Kelvin))
sum(is.na(rc_new$Temperature_Mean))
sum(is.na(rc_new$SSTA))
sum(is.na(rc_new$SSTA_DHW))
sum(is.na(rc_new$TSA))
sum(is.na(rc_new$Ecoregion)) 
sum(is.na(rc_new$Diversity))
table(rc_new$Year)
table(rc_new$Ocean)


nrow(pp)
sum(is.na(pp$Longitude_degrees))
sum(is.na(pp$Average_bleaching))
sum(is.na(pp$Temperature_Kelvin))
sum(is.na(pp$Temperature_Mean))
sum(is.na(pp$SSTA))
sum(is.na(pp$SSTA_DHW))
sum(is.na(pp$TSA))
sum(is.na(pp$Ecoregion)) 
sum(is.na(pp$Diversity))
table(pp$Year)
table(pp$Ocean)

#exclude NA's
pp_cleaned <- pp[rowSums(is.na(pp[, intersect(c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
                                        "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",
                                        "Average_bleaching","ClimSST","Temperature_Kelvin",
                                        "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",
                                        "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",
                                        "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",
                                        "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",
                                        "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",
                                        "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",
                                        "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",
                                        "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                        "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                        "TSA_DHWMean","Region","Diversity","rate_of_SST_change","Longitude.Degrees",
                                        "Latitude.Degrees"), names(pp))])) == 0, ]

#add column of ecoregion names for pp:
regions_code <- pp %>% distinct(Region)
regions_code_2 <- rc_new %>% distinct(Region)
pp <- pp %>%
  left_join(ECO_noholes %>% select(ERG, Ecoregion), by = c("Region" = "ERG"))
pp <- pp %>%
  rename(Ecoregion_name = Ecoregion)
names(pp)
pp <- pp[,c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
                                              "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",                                   
                                              "Errors.","What.errors.", "Average_bleaching","ClimSST","Temperature_Kelvin",                    
                                              "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",                  
                                              "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",                                  
                                              "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",                        
                                              "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",     
                                              "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",                             
                                              "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",                                  
                                              "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",                          
                                              "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                              "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                              "TSA_DHWMean","Region","Ecoregion_name","Diversity","rate_of_SST_change","Longitude.Degrees",                    
                                              "Latitude.Degrees")]
pp_cleaned <- pp[rowSums(is.na(pp[, intersect(c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
            "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",                                   
            "Errors.","What.errors.", "Average_bleaching","ClimSST","Temperature_Kelvin",                    
            "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",                  
            "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",                                  
            "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",                        
            "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",     
            "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",                             
            "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",                                  
            "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",                          
            "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
            "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
            "TSA_DHWMean","Region","Ecoregion_name","Diversity","rate_of_SST_change","Longitude.Degrees",                    
            "Latitude.Degrees"),names(pp))])) == 0, ]

rc_cleaned <- rc_new[rowSums(is.na(rc_new[, intersect(c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
                                                "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",
                                                "Average_bleaching","ClimSST","Temperature_Kelvin",
                                                "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",
                                                "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",
                                                "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",
                                                "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",
                                                "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",
                                                "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",
                                                "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",
                                                "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                                "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                                "TSA_DHWMean","Region","Ecoregion_name", "Diversity","rate_of_SST_change","Longitude.Degrees",
                                                "Latitude.Degrees"), names(rc_new))])) == 0, ]

sum(is.na(pp_cleaned))
sum(is.na(rc_cleaned))
sum(is.na(rc_cleaned$Errors.)) #NAs in Errors but it is okay because only surveys with values in S1,S2,S3,S4 were calculated for Avg_bleaching and others removed 

ocean_survey_counts <- table(rc_cleaned$Ocean)
ocean_survey_df <- as.data.frame(ocean_survey_counts)
colnames(ocean_survey_df) <- c("Ocean", "Total_Surveys")

eco_survey_counts <- table(rc_cleaned$Region)
eco_survey_df <- as.data.frame(eco_survey_counts)
colnames(eco_survey_df) <- c("Region", "Total_Surveys")

years_survey_counts <- table(rc_cleaned$Year)
years_survey_df <- as.data.frame(years_survey_counts)
colnames(years_survey_df) <- c("Year", "Total_Surveys")

ppocean_survey_counts <- table(pp_cleaned$Ocean)
ppocean_survey_df <- as.data.frame(ppocean_survey_counts)
colnames(ppocean_survey_df) <- c("Ocean", "Total_Surveys")

ppeco_survey_counts <- table(pp_cleaned$Region)
ppeco_survey_df <- as.data.frame(ppeco_survey_counts)
colnames(ppeco_survey_df) <- c("Region", "Total_Surveys")

ppyears_survey_counts <- table(pp_cleaned$Year)
ppyears_survey_df <- as.data.frame(ppyears_survey_counts)
colnames(ppyears_survey_df) <- c("Year", "Total_Surveys")

pp_cleaned$Diversity <- as.numeric(pp_cleaned$Diversity)
rc_cleaned$Diversity <- as.numeric(rc_cleaned$Diversity)

#one survey with NA diversity after as.numeric function
rc_cleaned <- rc_cleaned[rowSums(is.na(rc_cleaned[, intersect(c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
                                                        "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",
                                                        "Average_bleaching","ClimSST","Temperature_Kelvin",
                                                        "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",
                                                        "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",
                                                        "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",
                                                        "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",
                                                        "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",
                                                        "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",
                                                        "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",
                                                        "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                                        "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                                        "TSA_DHWMean","Ecoregion","Diversity","rate_of_SST_change","Longitude.Degrees",
                                                        "Latitude.Degrees"), names(rc_cleaned))])) == 0, ]
combined <- bind_rows(pp_cleaned, rc_cleaned)
write.csv(combined,"final_rc_combined.csv", row.names=FALSE)

combined <-read.csv("final_rc_combined.csv")
#at least 10 surveys per ecoregions, otherwise remove:
#combined_eco_survey_counts <- table(combined$Region)
#combined_eco_survey_df <- as.data.frame(combined_eco_survey_counts)
#colnames(combined_eco_survey_df) <- c("Region", "Total_Surveys")
#ecoregions_remove<- c("ERG004","ERG007", "ERG010","ERG012", "ERG021","ERG025", "ERG026",  
                      "ERG034", "ERG037" , "ERG041",
                      "ERG048", "ERG063", "ERG065", "ERG069", 
                      "ERG072", "ERG075", "ERG092","ERG094","ERG112","ERG114")

combined <- combined %>% 
  filter(!Region %in% ecoregions_remove)

#check if regions are removed
combined_eco_survey_counts_cleaned <- table(combined$Region)
combined_eco_survey_cleaned_df <- as.data.frame(combined_eco_survey_counts_cleaned)
colnames(combined_eco_survey_cleaned_df) <- c("Region", "Total_Surveys")

#check NAs:
sum(is.na(combined)) #908 (Errors. column)

#make columns for yes/no bleaching and severity of bleaching based on % -  
combined <- combined %>%
  mutate(Bleached = ifelse(Average_bleaching > 0, "Yes", "No"),
         Severity = case_when(
           Average_bleaching == 0 ~ "None",
           Average_bleaching > 0 & Average_bleaching <= 10 ~ "Mild",
           Average_bleaching > 10 & Average_bleaching <= 50 ~ "Moderate",
           Average_bleaching > 50 ~ "Severe",
         )) #scale taken from another paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC10552771/ 


combined<- combined %>% relocate(Bleached, Severity, .after = Average_bleaching)
write.csv(combined, "final_combined_severity.csv", row.names=FALSE )

#cortad calculate means per ecoregions as predictors:----
#calculate means per ecoregions as predictors in simpler way compared to past paper github:
agg_data <- combined %>%
  group_by(Region) %>%
  summarise(
    Mean_lat = mean(Latitude.Degrees, na.rm = TRUE),
    Mean_lon = mean(Longitude.Degrees, na.rm = TRUE),
    Mean_temp = mean(Temperature_Kelvin, na.rm = TRUE),
    Mean_SSTA_DHW = mean(SSTA_DHW, na.rm = TRUE),
    Mean_rate_of_SST_change = mean(rate_of_SST_change, na.rm = TRUE),
    Mean_temp_stdev = mean(Temperature_Kelvin_Standard_Deviation, na.rm = TRUE),
    Mean_SSTA_freq_stdev = mean(SSTA_Frequency_Standard_Deviation, na.rm = TRUE),
    Mean_SSTA_freq_mean = mean(SSTA_FrequencyMean, na.rm = TRUE),
    Mean_TSA_freq = mean(TSA_Frequency, na.rm = TRUE),
    Mean_TSA_freq_stdev = mean(TSA_Frequency_Standard_Deviation, na.rm = TRUE),
    Mean_TSA_DHW_mean = mean(TSA_DHWMean, na.rm = TRUE),
    Mean_TSA_DHW_stdev = mean(TSA_DHW_Standard_Deviation, na.rm = TRUE),
    Mean_Average_bleaching = mean(Average_bleaching, na.rm = TRUE),
    Sd_Average_bleaching = sd(Average_bleaching, na.rm = TRUE),
    Mean_Diversity = mean(Diversity, na.rm = TRUE),
    Sample_count = n()
  ) 


write.csv(agg_data, "Ecoregions_mean_variables.csv", row.names = FALSE)

#exploratory data analysis:----
library(ggplot2)
rc<-read.csv("final_combined_severity.csv")

rc$ClimSST <- rc$ClimSST - 273.15
rc$Temperature_Kelvin <- rc$Temperature_Kelvin - 273.15
rc$Temperature_Mean <- rc$Temperature_Mean - 273.15
rc$Temperature_Maximum <- rc$Temperature_Maximum - 273.15
rc$Temperature_Minimum <- rc$Temperature_Minimum - 273.15

#history of average bleaching
ggplot(rc, aes(x = Average_bleaching)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Average Bleaching",
       x = "Average Bleaching", y = "Count")

ggplot(rc, aes(x = Temperature_Mean, y = Average_bleaching)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Average Bleaching vs Temperature Mean",
       x = "Temperature Mean", y = "Average Bleaching")

#pearson correlation test:
library(corrplot)
variables <- c("Latitude.Degrees","Year","Depth","Temperature_Kelvin","Temperature_Kelvin_Standard_Deviation",
                "Temperature_Maximum","SSTA","SSTA_Maximum","SSTA_Minimum",
                "SSTA_Frequency","SSTA_Frequency_Standard_Deviation","SSTA_DHW","TSA_Frequency",
                "TSA_Frequency_Standard_Deviation","TSA_DHW_Standard_Deviation","ClimSST",
                "rate_of_SST_change")
sub_data <- rc[, variables]
#spearman correlation matrix, ignoring rows with missing values
cor_matrix <- cor(sub_data, method = "pearson", use = "complete.obs")
plot.new()
dev.off()
quartz(width = 10, height = 10)  # or quartz(), x11() depending on your OS
corrplot(cor_matrix,
         method = "color",            
         type = "full",              
         tl.col = "black",            
         tl.cex = 0.7,
         tl.srt = 45,
         addCoef.col = "black",       
         number.cex = 0.7, 
         mar = c(0.45, 0.45, 0.45, 0.45))

#no removing 0.65 beacause threshold is 0.70 but can note that "TSA_Frequency_Standard_Deviation","TSA_DHW_Standard_Deviation"
#were above threshold

#cw ecoregions and diversity:---- 
rm(list=ls())
library(sf)
library(nngeo)
library(dplyr) 

cw<-read.csv("cw_cortad_2022.csv")
diversity<-read.csv("Coral_diversity.csv")

ECO <- st_read(
  dsn   = "ecoregions_github", 
  layer = "ecoregion_exportPolygon"
)

ECO_noholes <- st_remove_holes(ECO)
print(ECO_noholes)

#extract geometry to rc
cw_eco <- st_as_sf(cw, 
                   coords = c("Longitude.Degrees", "Latitude.Degrees"), 
                   crs = 4326, 
                   remove = FALSE)

cw_eco <- st_transform(cw_eco, st_crs(ECO_noholes))
cw_joined <- st_join(cw_eco, ECO_noholes[, c("ERG", "Ecoregion")])

#fix errors: 
#ECO_noholes_valid <- st_make_valid(ECO_noholes)
#rc_joined <- st_join(rc_eco, ECO_noholes_valid[, c("ECO_CODE", "ECOREGION")])

cw_joined <- cw_joined %>%
  rename(Ecoregion_code = ERG) 

cw_joined$Ecoregion <- as.character(cw_joined$Ecoregion)
diversity$ECOREGION <- as.character(diversity$ECOREGION)

cw_joined$Ecoregion <- trimws(cw_joined$Ecoregion)
diversity$ECOREGION <- trimws(diversity$ECOREGION)

cw_joined <- cw_joined %>%
  left_join(diversity, by = c("Ecoregion" = "ECOREGION"))

#check for missing data
cw_joined <- cw_joined[!rowSums(is.na(cw_joined[, c("ClimSST", "Temperature_Kelvin", "Temperature_Mean", "Temperature_Minimum", "Temperature_Maximum", 
                                                    "Temperature_Kelvin_Standard_Deviation", "Windspeed",
                                                    "SSTA", "SSTA_Standard_Deviation", "SSTA_Mean", "SSTA_Minimum", "SSTA_Maximum", 
                                                    "SSTA_Freq", "SSTA_Freq_Standard_Deviation", "SSTA_Freq_Max", "SSTA_Freq_Mean", "SSTA_DHW", 
                                                    "SSTA_DHW_Standard_Deviation", "SSTA_DHW_Max", "SSTA_DHW_Mean", 
                                                    "TSA", "TSA_Standard_Deviation", "TSA_Mean", "TSA_Minimum", "TSA_Maximum", 
                                                    "TSA_Freq", "TSA_Freq_Standard_Deviation", "TSA_Freq_Mean", "TSA_Freq_Max", "TSA_DHW", 
                                                    "TSA_DHW_Standard_Deviation", "TSA_DHW_Max", "TSA_DHW_Mean")])), ]
sum(is.na(cw_joined$Diversity)) #586

unique(cw_joined$Ecoregion)
unique(diversity$ECOREGION)

sum(cw_joined$Ecoregion %in% diversity$ECOREGION)
mismatched_ecoregions <- cw_joined$Ecoregion[!cw_joined$Ecoregion %in% diversity$ECOREGION]
unique(mismatched_ecoregions)

#manually match unmatched ecoregions:
library(writexl)
write_xlsx(cw_joined, "cw_joined_manual_ecoregions.xlsx")

#load manually inputted NA ecoregions:
cw_joined<- read.csv("cw_ecoregions_diversity_manual.csv")

#code to fill in blank diversity values for these ecoregions:
cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Central and northern Great Barrier Reef" & is.na(Diversity),
    404,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Taiwan and coastal China" & is.na(Diversity),
    375,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Birds Head Peninsula, Papua" & is.na(Diversity),
    553,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Birds Head Peninsula, Papua" & is.na(Diversity),
    553,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Kenya and Tanzania coast" & is.na(Diversity),
    301,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Western Mexico and Revillagigedo Islands" & is.na(Diversity),
    34,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Lakshadweep Islands" & is.na(Diversity),
    141,
    Diversity
  ))

cw_joined<- cw_joined %>%
  mutate(Diversity = if_else(
    Ecoregion == "Milne Bay, Papua New Guinea" & is.na(Diversity),
    511,
    Diversity
  ))

#join rate of SST change
annual_rate_of_SST_change<- read.csv(file="CW_SST_rate_of_change_NOAA_1_km_res.csv", header=TRUE, sep=",")
names(annual_rate_of_SST_change)[2]<-"annual_rate_of_SST_change"
cw_cortad_eco_rate<-cbind(cw_joined, "rate_of_SST_change"=annual_rate_of_SST_change[,2])

#further cleaning to make everything consistent 
names(cw_cortad_eco_rate)[34]<-"SSTA_Frequency"
names(cw_cortad_eco_rate)[35]<-"SSTA_Frequency_Standard_Deviation"
names(cw_cortad_eco_rate)[36]<-"SSTA_FrequencyMax"
names(cw_cortad_eco_rate)[37]<-"SSTA_FrequencyMean"
names(cw_cortad_eco_rate)[40]<-"SSTA_DHWMax"
names(cw_cortad_eco_rate)[41]<-"SSTA_DHWMean"
names(cw_cortad_eco_rate)[47]<-"TSA_Frequency"
names(cw_cortad_eco_rate)[48]<-"TSA_Frequency_Standard_Deviation"
names(cw_cortad_eco_rate)[49]<-"TSA_FrequencyMean"
names(cw_cortad_eco_rate)[50]<-"TSA_FrequencyMax"
names(cw_cortad_eco_rate)[53]<-"TSA_DHWMax"
names(cw_cortad_eco_rate)[54]<-"TSA_DHWMean"
names(cw_cortad_eco_rate)[55]<-"Region"
names(cw_cortad_eco_rate)[56]<-"Ecoregion_name"

#FIX THE NAMES, RECOUNT THE COLUMNS
colnames(cw_cortad_eco_rate)
cw_backup <- cw_cortad_eco_rate
cw_cortad_eco_rate <- cw_cortad_eco_rate[,c("Reef.ID","Reef.Name","Ocean","Country","State.Province.Island", 
                                              "City.Town","Year","Date","Depth","Organism.Code","S1","S2","S3","S4",                                   
                                              "Errors.","What.errors.", "Average_bleaching","Bleached", "Severity", "ClimSST","Temperature_Kelvin",                    
                                              "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",                  
                                              "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",                                  
                                              "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",                        
                                              "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",     
                                              "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",                             
                                              "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",                                  
                                              "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",                          
                                              "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                              "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                              "TSA_DHWMean","Region","Ecoregion_name","Diversity","rate_of_SST_change","Longitude.Degrees",                    
                                              "Latitude.Degrees")]

write.csv(cw_cortad_eco_rate,"final_cw_cer.csv", row.names=FALSE)

#combiningn cw and rc
rm(list=ls())
#checking dataset for NAs, and data distribution
cw<-read.csv("final_cw_cer.csv")
nrow(cw)
sum(is.na(cw$Longitude_degrees))
sum(is.na(cw$Average_bleaching))
sum(is.na(cw$Temperature_Kelvin))
sum(is.na(cw$Temperature_Mean))
sum(is.na(cw$SSTA))
sum(is.na(cw$SSTA_DHW))
sum(is.na(cw$TSA))
sum(is.na(cw$Ecoregion)) 
sum(is.na(cw$Diversity))
table(cw$Year)
table(cw$Ocean)


#exclude NA's
cw_cleaned <- cw[rowSums(is.na(cw[, intersect(c("Reef.ID","Reef.Name","Country",
                                                        "Year","Date","Depth",
                                                        "Average_bleaching","Bleached","Severity","ClimSST","Temperature_Kelvin",
                                                        "Temperature_Mean","Temperature_Minimum","Temperature_Maximum",
                                                        "Temperature_Kelvin_Standard_Deviation","Windspeed","SSTA",
                                                        "SSTA_Standard_Deviation","SSTA_Mean","SSTA_Minimum",
                                                        "SSTA_Maximum","SSTA_Frequency","SSTA_Frequency_Standard_Deviation",
                                                        "SSTA_FrequencyMax","SSTA_FrequencyMean","SSTA_DHW",
                                                        "SSTA_DHW_Standard_Deviation","SSTA_DHWMax","SSTA_DHWMean","TSA",
                                                        "TSA_Standard_Deviation","TSA_Minimum","TSA_Maximum","TSA_Mean",
                                                        "TSA_Frequency","TSA_Frequency_Standard_Deviation","TSA_FrequencyMax",
                                                        "TSA_FrequencyMean","TSA_DHW","TSA_DHW_Standard_Deviation","TSA_DHWMax",
                                                        "TSA_DHWMean","Region","Ecoregion_name","Diversity","rate_of_SST_change","Longitude.Degrees",
                                                        "Latitude.Degrees"), names(cw))])) == 0, ]


sum(is.na(cw_cleaned$Region))
sum(is.na(cw_cleaned$Ecegion_name))
sum(is.na(cw_cleaned$Diversity))
sum(is.na(cw_cleaned$Errors.)) #NAs in Errors but it is okay because only surveys with values in S1,S2,S3,S4 were calculated for Avg_bleaching and others removed 

eco_survey_counts <- table(cw_cleaned$Region)
eco_survey_df <- as.data.frame(eco_survey_counts)
colnames(eco_survey_df) <- c("Region", "Total_Surveys")

years_survey_counts <- table(cw_cleaned$Year)
years_survey_df <- as.data.frame(years_survey_counts)
colnames(years_survey_df) <- c("Year", "Total_Surveys")

#match ocean names to cw:
unique_countries <- cw_cleaned %>%
  distinct(Country)

unique_reefname <- cw_cleaned %>%
  distinct(Reef.Name)

#change name "honduras" to "Honduras":
cw_cleaned <- cw_cleaned %>%
  mutate(Country = if_else(tolower(Country) == "honduras", "Honduras", Country))

#take Reef.Name to fill in missing location name columns:
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

cw_cleaned <- cw_cleaned %>%
  as_tibble() %>% 
  mutate(
    parts = str_split(Reef.Name, ",\\s*")
  ) %>%
  mutate(
    Country = map_chr(parts, ~ tail(.x, 1)),
    State.Province.Island = map_chr(parts, ~ if (length(.x) >= 3) .x[length(.x) - 1] else NA_character_),
    City.Town = map_chr(parts, ~ if (length(.x) >= 2) .x[2] else NA_character_)
  ) %>%
  dplyr::select(-parts)

#ocean, match with rc data:
cw_cleaned <- cw_cleaned %>%
  mutate(Ocean = case_when(
    Country == "United States of America" & str_detect(State.Province.Island, regex("Hawaii", ignore_case = TRUE)) ~ "Hawaii",
    Country == "United States of America" ~ "Atlantic",
    Country %in% c("Iran", "Kuwait", "United Arab Emirates", "Bahrain", "Oman") ~ "Arabian Gulf",
    Country == "Australia" & str_detect(State.Province.Island, regex("Cocos (Keeling) Islands", ignore_case = TRUE)) ~ "Indian",
    Country == "Australia" ~ "Pacific",
    Country %in% c("Egypt", "Eritrea", "Sudan", "Saudi Arabia") ~ "Red Sea",
    Country %in% c("Costa Rica") ~ "East Pacific",
    Country %in% c("Fiji", "Cook Islands", "Niue", "French Polynesia", 
                   "Northern Mariana Islands", "Japan", "Guam", 
                   "Micronesia", "Marshall Islands", "Papua New Guinea", "Tonga", 
                   "Vanuatu", "American Samoa", "Blue Buoy", "Hong Kong", "New Caledonia","Solomon Islands", "Taiwan") ~ "Pacific",
    Country %in% c("Honduras", "Belize", "Bahamas", "Nicaragua", 
                   "British Virgin Islands", "Cayman Islands", "Cuba", "Aruba", "Dominica",
                   "Dominican Republic", "Grenada", "Caribbean Netherlands", 
                   "Caribbean Netherlands (BES)", "Curacao", "Sint Maarten","St. Johns", "U.S. Virgin Islands","Jamaica",
                   "Brazil", "Turks and Caicos Islands", "Saint Vincent and the Grenadines", "Barbados", "Colombia", "Britsh Virgin Islands") ~ "Atlantic",
    Country %in% c("Mexico", "Ecuador", "Panama") ~ "Pacific",
    Country %in% c("Seychelles", "Mozambique", "India", "Mauritius", "Tanzania", 
                   "Madagascar", "Kenya", "Maldives", "South Africa") ~ "Indian",
    Country %in% c("Thailand", "Cambodia", "Malaysia", "Philippines", "Vietnam", "Indonesia", "Brunei", "Timor-Leste") ~ "Indo-Pacific"
  ))

rc<-read.csv("final_combined_severity.csv")
rc <- rc %>%
  mutate(Ocean = if_else(Country %in% c("Thailand", "Cambodia", "Malaysia", "Philippines", "Vietnam", "Indonesia", "Brunei", "Myanmar"),
                         "Indo-Pacific",
                         Ocean))
rc <- rc %>%
  mutate(Ocean = if_else(Country %in% c("Taiwan","Taiwan, Province of China"),
                         "Pacific",
                         Ocean))

rc_unique_locations<- rc %>%
  distinct(Country, Ocean, State.Province.Island)

table(rc$Ocean)

#name source for each dataset 
rc <- rc %>%
  mutate(Source = "Reef Check")

cw_cleaned <- cw_cleaned %>%
  mutate(Source = "Coral Watch")

#combine rc and cw:
combined <- bind_rows(rc, cw_cleaned)
combined <- combined %>%
  relocate(Source, .before = Reef.ID)

write.csv(combined,"cw_rc_combined.csv", row.names=FALSE)
