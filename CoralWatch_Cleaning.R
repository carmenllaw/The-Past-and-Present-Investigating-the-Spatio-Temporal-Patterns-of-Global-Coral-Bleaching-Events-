#clear environment
rm(list=ls())

#install packages
library(tidyr)
library(dplyr)
library(lubridate)

#read in CW data: 
cw<-read.csv("CWdata.csv")
site.id<-read.csv("Site ID CW.csv")
cw <- cw %>%
  filter(Coral.Type != "Soft corals") 
site.id <- site.id %>%
  rename(Reef.ID = Site.ID) %>%
  select(-Description, -Latitude.Degrees, -Longitude.Degrees)
site.id <- site.id  %>%
  rename(Reef.Name = Name)

#calculate bleaching percentage
cw <- cw %>%
  group_by(Activity.ID) %>%
  filter(n() >= 10) %>% 
  summarise(
    Reef.ID = first(Reef.ID),
    Latitude.Degrees = first(Latitude),
    Longitude.Degrees = first(Longitude),
    Date = first(Observation.date), 
    Depth = first(Depth..metres.),
    Country = first(Country),
    total = n(),
    bleached_count = sum((Average. == 1) | (Average. < 2.1), na.rm = TRUE),
    perc_bleached = 100 * bleached_count / total
  )

#join, adding reef names but matching by reef.id 
cw <- cw %>%
  left_join(site.id %>% select(Reef.ID, Reef.Name), by = "Reef.ID")

#rename columns and remove columns
cw <- cw %>%
  rename(Average_bleaching = perc_bleached) %>%
  select(-total, -bleached_count, -Activity.ID)

#make depth is above 0.1
cw <- cw %>% 
  filter(Depth >= 0.1)

#remove any NAs
cw<- drop_na(cw)

#provide severity + bleached categories 
cw <- cw %>%
  mutate(Severity = case_when(
    Average_bleaching == 0 ~ "None",
    Average_bleaching > 0 & Average_bleaching =< 10 ~ "Mild",
    Average_bleaching > 10 & Average_bleaching <= 50 ~ "Moderate",
    Average_bleaching > 50 ~ "Severe",
    TRUE ~ NA_character_
  ))

cw<-cw%>%
  mutate(Bleached = if_else(Average_bleaching > 0, "Yes", "No"))

#format dates
cw <- cw %>%
  mutate(Date = ymd(Date)) %>%  
  mutate(Date = format(Date, "%y-%b-%d"))

#further formatting
cw <- cw %>%
  mutate(Year = year(dmy(Date)))

severity_by_year <- cw %>%
  group_by(Year, Severity) %>%
  summarise(count = n(), .groups = "drop")

#reorganize columns and add new columns to match RC dataset:
cw <- cw %>%
  mutate(
    Ocean = "", 
    `State.Province.Island` = "", 
    `City.Town` = "", 
    `Organism.Code` = "", 
    S1 = "", 
    S2 = "", 
    S3 = "", 
    S4 = "", 
    `Errors.` = "", 
    `What.errors.` = ""
  ) %>%
  select(
    Reef.ID, 
    `Reef.Name`, 
    Ocean, 
    Country, 
    `State.Province.Island`, 
    `City.Town`, 
    Year, 
    Date, 
    Depth, 
    `Organism.Code`, 
    S1, 
    S2, 
    S3, 
    S4, 
    `Errors.`, 
    `What.errors.`, 
    Average_bleaching, 
    Bleached, 
    Severity, 
    Latitude.Degrees, 
    Longitude.Degrees
  )

#reformat coordinates 
to_dms_str <- function(dec, pos_char, neg_char) {
  # Use the absolute value for conversion.
  abs_dec <- abs(dec)
  deg <- floor(abs_dec)
  remainder <- (abs_dec - deg) * 60
  min <- floor(remainder)
  sec <- (remainder - min) * 60
  sec <- round(sec, 1)
  hem <- ifelse(dec >= 0, pos_char, neg_char)
  paste0(deg, ".", sprintf("%02d", min), ".", sprintf("%.1f", sec), hem)
}

cw <- cw%>%
  mutate(Reef.ID = paste0(
    to_dms_str(Longitude.Degrees, "E", "W"), ".",
    to_dms_str(Latitude.Degrees, "N", "S")
  ))

#save csv: 
write.csv(cw, "cw_cleaned.csv")

cw<-read.csv("cw_cleaned.csv")
