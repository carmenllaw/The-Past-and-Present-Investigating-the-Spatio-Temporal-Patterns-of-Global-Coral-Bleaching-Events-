rm(list=ls())

library(dplyr)

#read in reefcheck raw data from past paper:
rc<-read.csv("rc_raw.csv")
rc 

#focusing on % bleaching of population only, exclude % of colony (Sully et al., 2019)
data=subset(rc, Organism.Code=="Bleaching (% of population)")

#formatting lat and lon values - combines lat/long degrees, minutes, seconds and direction and removes their columns (Sully et al., 2019)
clean <- data %>%
  mutate(
    Longitude.Degrees = Longitude.Degrees + Longitude.Minutes / 60 + Longitude.Seconds / 3600,
    Longitude.Degrees = ifelse(Longitude.Cardinal.Direction == 'W', Longitude.Degrees * -1, Longitude.Degrees),
    Latitude.Degrees = Latitude.Degrees + Latitude.Minutes / 60 + Latitude.Seconds / 3600,
    Latitude.Degrees = ifelse(Latitude.Cardinal.Direction == 'S', Latitude.Degrees * -1, Latitude.Degrees)
  ) %>%
  select(-Latitude.Minutes, -Latitude.Seconds, -Latitude.Cardinal.Direction, 
         -Longitude.Minutes, -Longitude.Seconds, -Longitude.Cardinal.Direction) 

#calculate average bleaching per transect, if NA present in S1,S2,S3,S4, no calculation performed 
if (all(c("S1", "S2", "S3", "S4") %in% colnames(clean))) {
  clean <- clean %>%
    rowwise() %>%
    mutate(
      Average_bleaching = if (any(is.na(c(S1, S2, S3, S4)))) {
        NA
      } else {
        sum(c(S1, S2, S3, S4), na.rm = TRUE) / 
          sum(!is.na(c(S1, S2, S3, S4))) 
      }
    ) %>%
    ungroup()
} 

#remove rows with NA values in key columns (Sully et al., 2019)
clean1 <- clean %>%
  filter(
    !is.na(Latitude.Degrees),
    !is.na(Longitude.Degrees),
    !is.na(Average_bleaching),
    !is.na(Depth),
    !is.na(Date)
  )

#save cleaned reef check dataset
write.csv(clean1, "~/Desktop/Reef Check Diss/Diss Directory/rc_cleaned.csv", row.names = FALSE)

#read in reefcheck 2017 onwards data
rc2017<-read.csv("rc2017_raw.csv")
rc2017 

#focusing on % bleaching of population only, exclude % of colony:
data2017=subset(rc2017, organism_code=="Bleaching (% Of Population)")

#format it to match "data" dataset
clean2017 <- data2017 %>%
  mutate(
    Longitude.Degrees = longitude_degrees + longitude_minutes / 60 + longitude_seconds / 3600,
    Longitude.Degrees = ifelse(longitude_cardinal_direction == 'W', longitude_degrees * -1, longitude_degrees),
    Latitude.Degrees = latitude_degrees + latitude_minutes / 60 + latitude_seconds / 3600,
    Latitude.Degrees = ifelse(latitude_cardinal_direction == 'S', latitude_degrees * -1, latitude_degrees)
  ) %>%
  select(-site_id, -survey_id, -coordinates_in_decimal_degree_format, -longitude_degrees, -latitude_degrees, -latitude_minutes, -latitude_seconds, -latitude_cardinal_direction, 
         -longitude_minutes, -longitude_seconds, -longitude_cardinal_direction,-type, -fish_recorded_by, -inverts_recorded_by)

#renaming columns to match
clean2017<- clean2017 %>%
  dplyr::rename(Reef.Name = reef_name,
    Country = country,
    State.Province.Island = state_province_island,
    City.Town = city_town,
    Ocean = region,
    Year = year,
    Date = date,
    Depth = depth..m.,
    Organism.Code = organism_code,
    S1 = s1..0.20m.,
    S2 = s2..25.45m.,
    S3 = s3..50.70m., 
    S4 = s4..75.95m.,
    Errors.= errors,
    What.errors. = what_errors)

#this column could not be renamed in previous code, worked if renamed in separate code
clean2017<-clean2017 %>%
  rename(Reef.ID=static_descriptors_reef_id..archived.field.)

#rearranging columns to match
clean2017 <-clean2017 %>%
  select(Reef.ID,Reef.Name, Longitude.Degrees, Latitude.Degrees, Ocean, Country, State.Province.Island, City.Town,
         Year, Date, Depth,Organism.Code, S1, S2, S3, S4, Errors.,What.errors.)

#convert inputs in S1,S2,S3,S4 as numeric, if unable to convert then it is labelled as NA. *problem warnings*
clean2017 <- clean2017 %>%
  mutate(
    S1 = as.numeric(S1),
    S2 = as.numeric(S2),
    S3 = as.numeric(S3),
    S4 = as.numeric(S4)
  )

#inspect invalid data rows
clean2017 %>%
  select(S1, S2, S3, S4) %>%
  summarise_all(~ sum(is.na(as.numeric(.))), na.rm = TRUE) #Results = S1: 1, S2:2, S3: 29, S4:38 

#identify these rows
invalid.rows <- clean2017 %>%
  filter(
    !is.na(S1) & is.na(as.numeric(S1)) |
      !is.na(S2) & is.na(as.numeric(S2)) |
      !is.na(S3) & is.na(as.numeric(S3)) |
      !is.na(S4) & is.na(as.numeric(S4))
  )

#attemp again: convert inputs in S1,S2,S3,S4 as numeric, if unable to convert then it is labelled as NA.
clean2017 <- clean2017 %>%
  mutate(
    S1 = as.numeric(S1),
    S2 = as.numeric(S2),
    S3 = as.numeric(S3),
    S4 = as.numeric(S4)
  )

#indicate errors in surveys with incomplete bleaching data.
if (any(c("S1", "S2", "S3", "S4") %in% colnames(clean2017))) {
  clean2017 <- clean2017 %>%
    mutate(
      Errors. = ifelse(
        rowSums(is.na(select(., all_of(intersect(c("S1", "S2", "S3", "S4"), colnames(clean2017)))))) > 0,
        TRUE,
        Errors.
      ),
      What.errors. = ifelse(
        rowSums(is.na(select(., all_of(intersect(c("S1", "S2", "S3", "S4"), colnames(clean2017)))))) > 0,
        ifelse(is.na(What.errors.), "Incomplete survey", paste0(What.errors., "Incomplete survey")),
        What.errors.
      )
    )
}

#calculate average bleaching per transect, if NA present in S1,S2,S3,S4, no calculation performed
if (all(c("S1", "S2", "S3", "S4") %in% colnames(clean2017))) {
  clean2017 <- clean2017 %>%
    rowwise() %>%
    mutate(
      Average_bleaching = if (any(is.na(c(S1, S2, S3, S4)))) {
        NA
      } else {
        sum(c(S1, S2, S3, S4), na.rm = TRUE) / 
          sum(!is.na(c(S1, S2, S3, S4))) 
      }
    ) %>%
    ungroup()
} 

#remove rows with NA values in key columns
clean2017.1 <- clean2017 %>%
  filter(
    !is.na(Latitude.Degrees),
    !is.na(Longitude.Degrees),
    !is.na(Average_bleaching),
    !is.na(Depth),
    !is.na(Date)
  )

#save cleaned reef check 2017-present 
write.csv(clean2017.1, "~/Desktop/Reef Check Diss/Diss Directory/rccleaned_2017.csv", row.names = FALSE)

#combing clean 1 and clean2017.1 (cleaned dataset (1998-2017) from published paper and new dataset(2017- present) from Reef Check)
finaldata <- rbind(clean1, clean2017.1)

#save combined dataset
write.csv(finaldata, "~/Desktop/Reef Check Diss/Diss Directory/rc_combined.csv", row.names = FALSE)

