rm(list=ls())
library(ggplot2)
library(glmmTMB)

#rc<-read.csv("final_combined_severity.csv")
#test removing ecoregions and sensitivity check done for previous dataset without coral watch (old):----

#survey counts per ecoregion
ecoregion_counts <- rc_cleaned %>%
  group_by(Region) %>%
  summarise(total_surveys = n()) %>%
  arrange(total_surveys)
print(ecoregion_counts)

valid_ecoregions_100 <- ecoregion_counts %>%
  filter(total_surveys >= 100) %>%
  pull(Region)

rc_filtered_100 <- rc_cleaned %>% 
  filter(Region %in% valid_ecoregions_100)

rc_filtered_100 <- rc_filtered_100 %>%
  filter(!(Region %in% c("ERG051", "ERG055")))

valid_ecoregions <- ecoregion_counts %>%
  filter(total_surveys >= 10) %>%
  pull(Region)

rc_filtered <- rc_cleaned %>% 
  filter(Region %in% valid_ecoregions)

#glmm test with interaction term - didn't not work
model_100 <- glmmTMB(Bleached ~ Year * Region + (1 | Reef.ID),
                     data = rc_filtered_100,
                     family = binomial(link = "logit"))
summary(model_100)

#filter out ecoregions without data for all years, keep ecoregions that have data for most years with exception of a couple years missing:
selected_ecoregions <- c("ERG141", "ERG139", "ERG138", "ERG137", "ERG136", 
                         "ERG115", "ERG078", "ERG049", "ERG045", "ERG028", 
                         "ERG020", "ERG001")

rc_cleaned_2 <- rc_cleaned %>%
  filter(Region %in% selected_ecoregions)

years_counts_2 <- table(data_cleaned_2$Year)
years_df_2 <- as.data.frame(years_counts_2)
colnames(years_df_2) <- c("Year", "Total_Surveys")

ocean_counts_2 <- table(rc_cleaned_2$Ocean)
ocean_df_2 <- as.data.frame(ocean_counts_2)
colnames(ocean_df_2) <- c("Ocean", "Total_Surveys")

eco_counts_2 <- table(rc_cleaned_2$Region)
eco_df_2 <- as.data.frame(eco_counts_2)
colnames(eco_df_2) <- c("Region", "Total_Surveys")

#glmm with interaction for ecoregions with almost all years: 
model <- glmmTMB(Bleached ~ Year * Region + Temperature_Kelvin + SSTA + SSTA_DHW + TSA_DHW + rate_of_SST_change +(1 | Reef.ID),
                 data = rc_cleaned_2,
                 family = binomial(link = "logit"))
summary(model)

#without interaction term, glmm: ecoregions with data for almost all years only 
model_no_interaction <- glmmTMB(Bleached ~ Year + Region + (1 | Reef.ID),
                                data = rc_cleaned_2,
                                family = binomial(link = "logit"))
summary(model_no_interaction)

#separate year and ecoregion, here we test bleached, year and cortad variables known to influence bleaching probability
model_ni_cortad_year <- glmmTMB(Bleached ~ Year + Temperature_Kelvin + SSTA + SSTA_DHW + TSA_DHW + rate_of_SST_change + (1 | Reef.ID),
                                data = rc_cleaned_2,
                                family = binomial(link = "logit"))
summary(model_ni_cortad_year)

#use this model:
model_ni_cortad <- glmmTMB(Bleached ~ Year + Region + Temperature_Kelvin + SSTA + SSTA_DHW + TSA_DHW + rate_of_SST_change + (1 | Reef.ID),
                           data = rc_cleaned_2,
                           family = binomial(link = "logit"))
summary(model_ni_cortad)

#no interaction: removed ecoregions with less than 100 surveys (worked - but should probably use dataset with ecoregions that cover almost all years for consistency) 
model_ni_year_2 <- glmmTMB(Bleached ~ Year + Region + Temperature_Kelvin + SSTA + SSTA_DHW + TSA_DHW + rate_of_SST_change + (1 | Reef.ID),
                           data = rc_filtered_100,
                           family = binomial(link = "logit"))
summary(model_ni_year_2) 

#no interaction: no removing ecoregions (did not work) 
model_ni_uncleaned <- glmmTMB(Bleached ~ Year + Region + Temperature_Kelvin + SSTA + SSTA_DHW + TSA_DHW + rate_of_SST_change + (1 | Reef.ID),
                              data = rc_filtered,
                              family = binomial(link = "logit"))
summary(model_ni_uncleaned) 

#testing severity
library(dplyr)
library(glmmTMB)
rc_cleaned_2 <- rc_cleaned_2 %>%
  mutate(Severity = factor(Severity, 
                           levels = c("None", "Mild", "Moderate", "Severe"), 
                           ordered = TRUE))

rc_cleaned_2 <- rc_cleaned_2 %>%
  mutate(Severity_numeric = case_when(
    Severity == "None"     ~ 0,
    Severity == "Mild"     ~ 1,
    Severity == "Moderate" ~ 2,
    Severity == "Severe"   ~ 3,
    TRUE ~ NA
  ))


rc_cleaned_2 <- rc_cleaned_2 %>% 
  relocate(Severity_numeric, .after = Severity)

model_severity<- glmmTMB(Severity_numeric ~ Year + Region + Temperature_Kelvin + SSTA +
                           SSTA_DHW + TSA_DHW + rate_of_SST_change + (1 | Reef.ID),
                         data = rc_cleaned_2,
                         family = poisson(link = "log"))
summary(model_severity)


#visualisation models: 
library(maps)
library(viridis)
library(ggplot2)

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray70") +
  geom_point(data = rc_cleaned_2, aes(x = Longitude.Degrees, y = Latitude.Degrees, color = Average_bleaching, size = Average_bleaching), alpha = 0.8)+
  scale_color_gradientn(colors = c("darkgreen", "yellow", "orange", "red"), 
                        values = scales::rescale(c(0, 1, 50, 100)), 
                        name = "Bleaching %", 
                        limits = c(0, 100)) +
  scale_size_continuous(range = c(1, 4), guide = "none") +  
  coord_fixed(1.3) +
  annotate("text", x = -120, y = 0, label = "Pacific Ocean", size = 2, fontface = "italic", color = "black") +
  annotate("text", x = 75, y = -10, label = "Indian Ocean", size = 2, fontface = "italic", color = "black") +
  annotate("text", x = -45, y = 30, label = "Atlantic Ocean", size = 2, fontface = "italic", color = "black") +
  labs(title = "Coral Bleaching Distribution (2003-2022)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "gray50"),
    legend.position = "bottom"
  )


#stacked bar chart for only ecoregions with almost all years 
ggplot(rc_cleaned_2, aes(x = factor(Year), fill = Severity)) +
  geom_bar(position = "stack", color = "black") +  
  scale_fill_manual(values = c("None" = "grey", 
                               "Mild" = "yellow", 
                               "Moderate" = "orange", 
                               "Severe" = "red")) +
  labs(x = "Year", y = "Number of Surveys") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black")) 

#lat/long
lat_long <- rc_cleaned_2 %>%
  distinct(Reef.ID, Latitude.Degrees, Longitude.Degrees)

#latitude
ggplot(lat_long, aes(x = Latitude.Degrees)) +
  geom_histogram(bins = 16, fill = "lightblue", color = "black") +
  labs(x = "Latitude", y = "Number of Sites") +
  theme_minimal()+
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#longitude
ggplot(lat_long, aes(x = Longitude.Degrees)) +
  geom_histogram(bins = 36, fill = "lightblue", color = "black") +
  labs(x = "Longitude", y = "Number of Sites") +
  theme_minimal()+
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#stacked bar lat/long:
rc_lat <- rc_cleaned_2 %>%
  filter(!is.na(Latitude.Degrees)) %>%
  mutate(Lat_bin = cut(Latitude.Degrees, breaks = seq(-40, 40, 5), include.lowest = TRUE)) %>%
  group_by(Lat_bin, Severity) %>%
  summarise(count = n(), .groups = "drop")

ggplot(rc_lat, aes(x = Lat_bin, y = count, fill = Severity)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("None" = "grey", "Mild" = "yellow", 
                               "Moderate" = "orange", "Severe" = "red")) +
  labs(x = "Latitude", y = "Number of Surveys") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

rc_lon <- rc_cleaned_2 %>%
  filter(!is.na(Longitude.Degrees)) %>%
  mutate(Lon_bin = cut(Longitude.Degrees, 
                       breaks = seq(-180, 180, by = 20), 
                       include.lowest = TRUE)) %>%
  group_by(Lon_bin, Severity) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(Severity = factor(Severity, levels = c("None", "Mild", "Moderate", "Severe")))

ggplot(rc_lon, aes(x = Lon_bin, y = count, fill = Severity)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  scale_fill_manual(values = c("None" = "grey", 
                               "Mild" = "yellow", 
                               "Moderate" = "orange", 
                               "Severe" = "red")) +
  labs(x = "Longitude", 
       y = "Number of Surveys") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#boxplots for environmental variables (github):
bin_breaks <- c(-35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35)
rc_cleaned_2 <- rc_cleaned_2 %>%
  mutate(group = cut(Latitude.Degrees, breaks = bin_breaks, include.lowest = TRUE, right = TRUE))

boxplot_save <- function(var, y_label) {
  tiff(filename = paste0(y_label, "_at_Latitude_boxplot.tif"), res = 300, width = 3000, height = 2000)
  boxplot(var ~ group, 
          data = rc_cleaned_2, 
          las = 2, 
          xlab = "Latitude Range", 
          ylab = y_label)
  dev.off()
}

boxplot_save(rc_cleaned_2$SSTA_Frequency, "SSTA_Frequency")
boxplot_save(rc_cleaned_2$SSTA_Frequency_Standard_Deviation, "SSTA_Frequency_stdev")
boxplot_save(rc_cleaned_2$TSA_Frequency, "TSA_Frequency")
boxplot_save(rc_cleaned_2$Temperature_Kelvin - 273.15, "SST")
boxplot_save(rc_cleaned_2$TSA_Maximum, "TSA_Max")
boxplot_save(rc_cleaned_2$Temperature_Kelvin_Standard_Deviation, "SST_stdev")
boxplot_save(rc_cleaned_2$SSTA, "SSTA")
boxplot_save(rc_cleaned_2$SSTA_DHW, "SSTA_DHW")
boxplot_save(rc_cleaned_2$TSA, "TSA")
boxplot_save(rc_cleaned_2$TSA_DHW, "TSA_DHW")
boxplot_save(rc_cleaned_2$TSA_Frequency_Standard_Deviation, "TSA_Freq_stdev")
boxplot_save(rc_cleaned_2$TSA_DHWMean, "TSA_DHW_Mean")
boxplot_save(rc_cleaned_2$TSA_DHW_Standard_Deviation, "TSA_DHW_stdev")
boxplot_save(rc_cleaned_2$rate_of_SST_change, "SST Annual Rate of Change")

#look at each ecoregion and diversity 
library(dplyr)
library(ggplot2)

# Summarize mean Diversity for each ecoregion
diversity_summary <- rc_cleaned_2 %>%
  group_by(Region) %>%
  summarise(mean_diversity = mean(Diversity, na.rm = TRUE),
            .groups = "drop")

# Summarize bleaching count (number of surveys with bleached == 1) per ecoregion
bleached_counts <- rc_cleaned_2 %>%
  group_by(Region) %>%
  summarise(bleached_count = sum(Bleached, na.rm = TRUE),
            .groups = "drop")

# Merge the summaries into one data frame
plot_data_summary <- left_join(diversity_summary, bleached_counts, by = "Region")

# Calculate a scaling factor to bring bleached_count onto the diversity scale.
# Here we use the ratio of the maximum bleaching count to the maximum mean diversity.
scale_factor <- max(plot_data_summary$bleached_count, na.rm = TRUE) /
  max(plot_data_summary$mean_diversity, na.rm = TRUE)

# Inspect the summary
print(plot_data_summary)
print(scale_factor)

ggplot(plot_data_summary, aes(x = reorder(Region, mean_diversity))) +
  geom_bar(aes(y = mean_diversity), stat = "identity",color = "black", fill = "lightblue") +
  geom_line(aes(y = bleached_count / scale_factor, group = 1), color = "red", linewidth = 1) +
  geom_point(aes(y = bleached_count / scale_factor), color = "red", size = 1) +
  scale_y_continuous(
    name = "Mean Diversity",
    sec.axis = sec_axis(~ . * scale_factor, name = "Bleaching Count")
  ) +
  labs(x = "Ecoregion") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

rc_cleaned_2 <- rc_cleaned_2 %>%
  mutate(Region = recode(Region,
                         "ERG141" = "Bahamas and Florida Keys",
                         "ERG139" = "Jamaica",
                         "ERG138" = "Hispaniola, Puerto Rico and Lesser Antilles",  
                         "ERG137" = "Netherlands Antilles and South Carribean",
                         "ERG136" = "Belize and West Caribbean",
                         "ERG115" = "Society Islands, French Polynesia",
                         "ERG078" = "Central and Northern Great Barrier Reef",
                         "ERG049" = "Sunda Shelf, South East Asia",
                         "ERG045" = "Sulu Sea",
                         "ERG028" = "Andaman Sea",
                         "ERG020" = "Maldvies",
                         "ERG001" = "North and Central Red Sea"))

#using combined cw and rc data here----
data<-read.csv("cw_rc_combined.csv")

data$Bleached <- ifelse(data$Bleached == "Yes", 1, 0)

years_counts <- table(data$Year)
years_df <- as.data.frame(years_counts)
colnames(years_df) <- c("Year", "Total_Surveys")

ocean_counts <- table(data$Ocean)
ocean_df <- as.data.frame(ocean_counts)
colnames(ocean_df) <- c("Ocean", "Total_Surveys")

eco_counts <- table(data$Region)
eco_df <- as.data.frame(eco_counts)
colnames(eco_df) <- c("Region", "Total_Surveys")

sum(is.na(data))
sum(is.na(data$Reef.ID))
sum(is.na(data$Errors.))  #all of cw data and 908 of rc data, so can ignore
sum(is.na(data$What.errors.)) #all of cw data  
sum(is.na(data$Longitude_degrees))
sum(is.na(data$Average_bleaching))
sum(is.na(data$Bleached))
sum(is.na(data$Severity))
sum(is.na(data$Temperature_Kelvin))
sum(is.na(data$Temperature_Mean))
sum(is.na(data$Windspeed))
sum(is.na(data$SSTA))
sum(is.na(data$SSTA_DHW))
sum(is.na(data$TSA))
sum(is.na(data$TSA_Standard_Deviation))
sum(is.na(data$Region)) 
sum(is.na(data$Diversity))
sum(is.na(data$rate_of_SST_change))

library(dplyr)
library(tidyr)

region_year_counts <- data %>%
  group_by(Region, Year) %>%
  summarise(count = n(), .groups = "drop")

region_year_wide <- region_year_counts %>%
  pivot_wider(
    names_from = Year,      
    values_from = count,  
    values_fill = list(count = 0),
    names_sort = TRUE   
  )

write.csv(region_year_wide, "cw_rc_ecoregions_count_per_year.csv", row.names=FALSE)

#remove years 1998 (3), 2000 (2), 2001 (3), and 2002 (59): minimum 100 surveys required
data_cleaned <- data %>% filter(!Year %in% c(1998, 2000, 2001, 2002))

#check surveys again:
years_counts_2 <- table(data_cleaned$Year)
years_df_2 <- as.data.frame(years_counts_2)
colnames(years_df_2) <- c("Year", "Total_Surveys")

ocean_counts_2 <- table(data_cleaned$Ocean)
ocean_df_2 <- as.data.frame(ocean_counts_2)
colnames(ocean_df_2) <- c("Ocean", "Total_Surveys")

eco_counts_2 <- table(data$Region)
eco_df_2 <- as.data.frame(eco_counts)
colnames(eco_df_2) <- c("Region", "Total_Surveys")


#missing information for some surveys in ocean column, manually fill in (16):
data_cleaned <- data_cleaned %>%
  mutate(Ocean = ifelse(Ocean == "" & grepl("Sabah", State.Province.Island),
                        "Indo-Pacific", Ocean))
data_cleaned <- data_cleaned %>%
  mutate(Ocean = ifelse(Ocean == "" & grepl("Maldives", Country),
                        "Indian", Ocean))
data_cleaned <- data_cleaned %>%
  mutate(Ocean = ifelse(Ocean == "" & grepl("Oman", Country),
                        "Arabian Gulf", Ocean))
data_cleaned <- data_cleaned %>%
  mutate(Ocean = ifelse(Ocean == "" & grepl("Paea", Reef.Name),
                        "Pacific", Ocean))
data_cleaned <- data_cleaned %>%
  mutate(Ocean = ifelse(Ocean == "" & grepl("Carriacou", State.Province.Island),
                        "Atlantic", Ocean))

#recheck if oceans names are filled in ocean_df_2
#check ecoregions count per year:
region_year_counts_2 <- data_cleaned %>%
  group_by(Region, Year) %>%
  summarise(count = n(), .groups = "drop")

region_year_wide_2 <- region_year_counts_2 %>%
  pivot_wider(
    names_from = Year,      
    values_from = count,  
    values_fill = list(count = 0),
    names_sort = TRUE   
  )

region_year_wide_2 <- region_year_wide_2 %>%
  mutate(total = rowSums(select(., -Region)))

total_row <- region_year_wide_2[1, ]
total_row[] <- NA          
total_row$Region <- "Total" 
for(j in 2:ncol(region_year_wide_2)) {
  total_row[[j]] <- sum(region_year_wide_2[[j]], na.rm = TRUE)
}
region_year_wide_total <- rbind(region_year_wide_2, total_row)

#remove ecoregions: ecoregions must have at least 100 surveys but after looking at distribution of surveys over the years,
# remove ecoregions that do not cover almost all years. Keeping ecoregions with at most 4/5 years missing in a row or spread out.
selected_ecoregions <- c("ERG001","ERG005","ERG006","ERG020","ERG028", "ERG033","ERG035","ERG038","ERG045", "ERG046",
                         "ERG047","ERG049","ERG050","ERG055", "ERG065", "ERG078", "ERG081", "ERG082","ERG111","ERG115","ERG136",
                         "ERG137","ERG138","ERG139","ERG141")
data_cleaned_2 <- data_cleaned %>%
  filter(Region %in% selected_ecoregions)

years_counts_3 <- table(data_cleaned_2$Year)
years_df_3 <- as.data.frame(years_counts_3)
colnames(years_df_3) <- c("Year", "Total_Surveys")

ocean_counts_3 <- table(data_cleaned_2$Ocean)
ocean_df_3 <- as.data.frame(ocean_counts_3)
colnames(ocean_df_3) <- c("Ocean", "Total_Surveys")

eco_counts_3 <- table(data_cleaned_2$Region)
eco_df_3 <- as.data.frame(eco_counts_3)
colnames(eco_df_3) <- c("Region", "Total_Surveys")

#removing depths greater than 20 because reef check from past paper max is 17m
data_cleaned_2 <- data_cleaned_2 %>% 
  filter(Depth <= 20.00)

#change bleaching severity levels, accidentally did different levels for coral watch surveys:
data_cleaned_2 <- data_cleaned_2 %>%
  mutate(Severity = case_when(
           Average_bleaching == 0 ~ "None",
           Average_bleaching > 0 & Average_bleaching <= 10 ~ "Mild",
           Average_bleaching > 10 & Average_bleaching <= 50 ~ "Moderate",
           Average_bleaching > 50 ~ "Severe",
         )) #scale taken from another paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC10552771/ 

write.csv(data_cleaned_2, "cw_rc_combined_filtered_ecoregions.csv")

data_cleaned_2<-read.csv("cw_rc_combined_filtered_ecoregions.csv")

#pearson correlation test to look at explanatory variables:
library(corrplot)
#change to match original matrix
variables <- c("Latitude.Degrees", "Year","Depth","Temperature_Kelvin",
               "Temperature_Kelvin_Standard_Deviation","Temperature_Maximum", 
               "SSTA", "SSTA_Maximum","SSTA_Standard_Deviation",
               "SSTA_Frequency","SSTA_Frequency_Standard_Deviation",
               "SSTA_DHW","SSTA_DHW_Standard_Deviation","TSA","TSA_Maximum",
               "TSA_Standard_Deviation","TSA_Frequency","TSA_Frequency_Standard_Deviation",
               "TSA_DHW","TSA_DHW_Standard_Deviation")

library(dplyr)
renamed_variables <- data_cleaned_2 %>%
  rename(
    Lat = Latitude.Degrees,
    Year = Year,
    Depth = Depth,
    SST= Temperature_Kelvin,
    SST_SD = Temperature_Kelvin_Standard_Deviation,
    SST_Max = Temperature_Maximum,
    SSTA= SSTA,
    SSTA_Max= SSTA_Maximum,
    SSTA_SD= SSTA_Standard_Deviation,
    SSTA_Freq   = SSTA_Frequency,
    SSTA_Freq_SD= SSTA_Frequency_Standard_Deviation,
    SSTA_DHW    = SSTA_DHW,
    SSTA_DHW_SD = SSTA_DHW_Standard_Deviation,
    TSA         = TSA,
    TSA_Max     = TSA_Maximum,
    TSA_SD      = TSA_Standard_Deviation,
    TSA_Freq    = TSA_Frequency,
    TSA_Freq_SD = TSA_Frequency_Standard_Deviation,
    TSA_DHW     = TSA_DHW,
    TSA_DHW_SD  = TSA_DHW_Standard_Deviation
  )

variables<-c("Lat","Year","Depth","SST","SST_SD","SST_Max","SSTA","SSTA_Max",
             "SSTA_SD","SSTA_Freq","SSTA_Freq_SD","SSTA_DHW", "SSTA_DHW_SD",
             "TSA","TSA_Max","TSA_SD","TSA_Freq", "TSA_Freq_SD","TSA_DHW",
             "TSA_DHW_SD")

sub_data <- renamed_variables[, variables]
#pearson's correlation matrix, ignoring rows with missing values
cor_matrix <- cor(sub_data, method = "pearson", use = "complete.obs")
plot.new()
dev.off()
quartz(width = 40, height = 40)
corrplot(cor_matrix,
         method = "color",
         type = "full",
         tl.col = "black",
         tl.cex = 0.7, 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,   
         mar = c(0.2, 0.2, 0.2, 0.2))  

#no removing 0.62 beacause threshold is 0.70 in general, but pp threshold is 0.65: can note that "TSA_Frequency_Standard_Deviation","TSA_DHW_Standard_Deviation"

#GLMM models:
#binomial bleached (0/1) model
library(glmmTMB)

model_ni_year_region <- glmmTMB(Bleached ~ Year + Region + (1 | Reef.ID),
                    data = data_cleaned_2,
                    family = binomial(link = "logit"))
summary(model_ni_year_region)

model_ni_cortad_separate <- glmmTMB(Bleached ~ Year + Region + Temperature_Kelvin + SSTA_DHW + TSA_DHW + (1 | Reef.ID),
                           data = data_cleaned_2,
                           family = binomial(link = "logit"))
summary(model_ni_cortad_separate)

#poisson severity levels model:
data_cleaned_2 <- data_cleaned_2 %>%
  mutate(Severity = factor(Severity, 
                           levels = c("None", "Mild", "Moderate", "Severe"), 
                           ordered = TRUE))

data_cleaned_2 <- data_cleaned_2 %>%
  mutate(Severity_numeric = case_when(
    Severity == "None"     ~ 0,
    Severity == "Mild"     ~ 1,
    Severity == "Moderate" ~ 2,
    Severity == "Severe"   ~ 3,
    TRUE ~ NA
  ))

data_cleaned_2 <- data_cleaned_2 %>% 
  relocate(Severity_numeric,.after = Severity)

write.csv(data_cleaned_2, "final_everything_last_one.csv")


#visualisations:
library(maps)
library(viridis)
library(ggplot2)

#spatial map of bleaching percentage and overall survey distribution
world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray70") +
  geom_point(data = data_cleaned_2, aes(x = Longitude.Degrees, y = Latitude.Degrees, color = Average_bleaching, size = Average_bleaching), alpha = 0.8)+
  scale_color_gradientn(colors = c("darkgreen", "yellow", "orange", "red"), 
                        values = scales::rescale(c(0, 1, 50, 100)), 
                        name = "Bleaching %", 
                        limits = c(0, 100)) +
  scale_size_continuous(range = c(1, 4), guide = "none") +  
  coord_fixed(1.3) +
  annotate("text", x = -120, y = 0, label = "Pacific Ocean", size = 2, fontface = "italic", color = "black") +
  annotate("text", x = 75, y = -10, label = "Indian Ocean", size = 2, fontface = "italic", color = "black") +
  annotate("text", x = -45, y = 30, label = "Atlantic Ocean", size = 2, fontface = "italic", color = "black") +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "gray50"),
    legend.position = "bottom"
  )


#stacked bar chart for only ecoregions with almost all years 
ggplot(data_cleaned_2, aes(x = factor(Year), fill = Severity)) +
  geom_bar(position = "stack", color = "black") +  
  scale_fill_manual(values = c("None" = "grey", 
                               "Mild" = "yellow", 
                               "Moderate" = "orange", 
                               "Severe" = "red")) +
  labs(x = "Year", y = "Number of Surveys") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "black")) 

#depth:
data_depth <- filtered_glmm_regions %>%
  mutate(depth_bin = cut(Depth, breaks = seq(0, 20, by = 1), include.lowest = TRUE),
         Bleached) %>%
  group_by(depth_bin, Bleached) %>%
  summarise(count = n(), .groups = "drop")

data_depth <- data_depth %>%
  mutate(Bleached = factor(Bleached))

ggplot(data_depth, aes(x = depth_bin, y = count, fill = Bleached)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("1" = "red", "0" = "grey")) +
  labs(x = "Depth (m)", y = "Number of Surveys") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#diversity, region and bleaching:
#summarize bleaching count per ecoregion that had significance from glmm:
#- w/o diversity in all combined glmm = glmm_regions<- c("ERG006", "ERG018", "ERG020", "ERG035", "ERG045", 
                         "ERG046", "ERG047", "ERG065", "ERG078", "ERG081", 
                         "ERG082", "ERG136", "ERG137", "ERG138", "ERG139", "ERG141")
#- w/ diversity in all combined glmm = glmm_regions<- c("ERG018", "ERG020", "ERG033", "ERG035", "ERG045", "ERG046","ERG047",
                 "ERG049", "ERG055", "ERG078", "ERG081", "ERG082", "ERG111")

#- baseline:
glmm_regions<- c("ERG001","ERG005", "ERG006", "ERG020", "ERG035", "ERG045", "ERG046","ERG047",
                 "ERG065", "ERG078", "ERG081", "ERG082", "ERG136", "ERG137", "ERG138", "ERG139",
                 "ERG141")

#baseline from only ecoregion glmm: 
glmm_regions<- c("ERG001","ERG005", "ERG006", "ERG020", "ERG035", "ERG045", "ERG046","ERG047",
                 "ERG065", "ERG078", "ERG081", "ERG082","ERG111", "ERG136", "ERG137", "ERG138", "ERG139",
                 "ERG141")
#glmm with sst, ssta_dhw, tsa_dhw
glmm_regions<- c("ERG006", "ERG020", "ERG035", "ERG045", "ERG046", "ERG047", "ERG065", 
                 "ERG078", "ERG081", "ERG082", "ERG136", "ERG137", "ERG138", "ERG139", "ERG141")

filtered_glmm_regions <- data_cleaned_2 %>%
  filter(Region %in% glmm_regions)

table(filtered_glmm_regions$Ecoregion_name)

diversity_summary <- data_cleaned_2 %>%
  group_by(Region, Ecoregion_name) %>%
  summarise(
    mean_diversity = mean(Diversity, na.rm = TRUE),
    bleached_count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(asterisk = if_else(Region %in% glmm_regions, "*", ""))

#calculate a scaling factor to bring bleached_count onto the diversity scale.
#use the ratio of the maximum bleaching count to the maximum mean diversity.
y_axis_scale <- max(diversity_summary$bleached_count, na.rm = TRUE)/
  max(diversity_summary$mean_diversity, na.rm = TRUE)

#significant ecoregions
ggplot(diversity_summary, aes(x = reorder(Ecoregion_name, mean_diversity))) +
  geom_bar(aes(y = mean_diversity), stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(y = mean_diversity, label = asterisk), vjust = -0.5, size = 5, color = "black") +
  geom_line(aes(y = bleached_count / y_axis_scale, group = 1), color = "red", size = 1) +
  geom_point(aes(y = bleached_count / y_axis_scale), color = "red", size = 2) +
  scale_y_continuous(
    name = "Mean Diversity",
    sec.axis = sec_axis(~ . * y_axis_scale, name = "Bleaching Count")
  ) +
  labs(x = "Ecoregion") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

kruskal_result_region <- kruskal.test(Diversity~Region, data = filtered_glmm_regions)
print(kruskal_result_region)

#boxplot for coRTAD variables ~ ecoregion + coRTAD variables ~ years
#4 enviro variables 
ns_regions <- c("ERG001", "ERG005", "ERG028", "ERG033", "ERG038", "ERG049", "ERG050", "ERG055",
                "ERG111", "ERG115")

filtered_ns_regions <- data_cleaned_2 %>%
  filter(Region %in% ns_regions)

#add column in dataset for "significance":
filtered_glmm_regions <- filtered_glmm_regions %>%
  mutate(Significance = "Significant")

filtered_ns_regions <- filtered_ns_regions %>%
  mutate(Significance = "Not Significant")

#combine these subsets:
data_cleaned_3 <- bind_rows(filtered_glmm_regions, filtered_ns_regions)

#boxplot w/ cortad variables
ecoboxplot_SST <- ggplot(data_cleaned_3, aes(x=Significance, y=Temperature_Kelvin - 273.15, fill = Significance))+
  geom_boxplot()+
  labs(x ="GLMM Ecoregion Significance", y ="SST") +
  theme_minimal()+
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ecoboxplot_SSTA_DHW <- ggplot(data_cleaned_3, aes(x=Significance, y=SSTA_DHW, fill = Significance))+
  geom_boxplot()+
  labs(x ="GLMM Ecoregion Significance",y ="SSTA_DHW") +
  theme_minimal()+
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ecoboxplot_TSA_DHW <-ggplot(data_cleaned_3, aes(x=Significance, y=TSA_DHW, fill=Significance))+
  geom_boxplot()+
  labs(x ="GLMM Ecoregion Significance",y ="TSA_DHW") +
  theme_minimal()+
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ecoboxplot_div <- ggplot(data_cleaned_3, aes(x=Significance, y=Diversity, fill = Significance))+
  geom_boxplot()+
  labs(x ="GLMM Ecoregion Significance",y ="Diversity") +
  theme_minimal()+
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#multi-plot panel:
combined_boxplot <- (ecoboxplot_SST | ecoboxplot_SSTA_DHW )/
                    (ecoboxplot_TSA_DHW |ecoboxplot_div)
combined_boxplot

#playing around with cortad variables over time for each ecoregion: 
ggplot(filtered_glmm_regions, aes (x = Year, y = SSTA, colour = Ecoregion_name)) +
  geom_point(size = 2) +                                              
  geom_smooth(method = "lm", aes(fill = Ecoregion_name)) +            
  theme_minimal() +   
  labs(x ="Year", y ="Temperature Anomaly (SSTA)") +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Each ECOREGIONS and CORTAD and BLEACHing
library(dplyr)
library(ggplot2)

plot_ecoregion <- function(ecoregion_name) {
  data_e <- filtered_glmm_regions %>%
    filter(Ecoregion_name == ecoregion_name)
  summary_all <- data_e %>%
    group_by(Year) %>%
    summarise(
      mean_SST = mean(Temperature_Kelvin, na.rm = TRUE) - 273.15,
      mean_SSTA_DHW = mean(SSTA_DHW, na.rm = TRUE),
      mean_TSA_DHW = mean(TSA_DHW, na.rm = TRUE),
      Bleaching_Count = sum(Bleached, na.rm = TRUE),
      .groups = "drop"
    )
#scaling
  scale_SST<- max(summary_all$Bleaching_Count, na.rm = TRUE) / max(summary_all$mean_SST, na.rm = TRUE)
  scale_SSTA_DHW <- max(summary_all$Bleaching_Count, na.rm = TRUE) / max(summary_all$mean_SSTA_DHW, na.rm = TRUE)
  scale_TSA_DHW<- max(summary_all$Bleaching_Count, na.rm = TRUE) / max(summary_all$mean_TSA_DHW, na.rm = TRUE)
#SST
  plot_SST <- ggplot(summary_all, aes(x = Year)) +
    geom_line(aes(y = mean_SST), color = "blue", size = 1) +
    geom_point(aes(y = mean_SST), color = "blue", size = 2) +
    geom_bar(aes(y = Bleaching_Count / scale_SST), stat = "identity", 
             fill = "red", alpha = 0.5, width = 0.7) +
    scale_y_continuous(
      name = "Mean SST (°C)", 
      sec.axis = sec_axis(~ . * scale_SST, name = "Bleaching Count")
    ) +
    labs(x = "Year") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1))
#SSTA_DHW
  plot_SSTA_DHW <- ggplot(summary_all, aes(x = Year)) +
    geom_line(aes(y = mean_SSTA_DHW), color = "blue", size = 1) +
    geom_point(aes(y = mean_SSTA_DHW), color = "blue", size = 2) +
    geom_bar(aes(y = Bleaching_Count / scale_SSTA_DHW), stat = "identity", 
             fill = "red", alpha = 0.5, width = 0.7) +
    scale_y_continuous(
      name = "Mean SSTA_DHW",
      sec.axis = sec_axis(~ . * scale_SSTA_DHW, name = "Bleaching Count")
    ) +
    labs(x = "Year") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1))
#TSA_DHW
  plot_TSA_DHW <- ggplot(summary_all, aes(x = Year)) +
    geom_line(aes(y = mean_TSA_DHW), color = "blue", size = 1) +
    geom_point(aes(y = mean_TSA_DHW), color = "blue", size = 2) +
    geom_bar(aes(y = Bleaching_Count / scale_TSA_DHW), stat = "identity", 
             fill = "red", alpha = 0.5, width = 0.7) +
    scale_y_continuous(
      name = "Mean TSA_DHW",
      sec.axis = sec_axis(~ . * scale_TSA_DHW, name = "Bleaching Count")
    ) +
    labs(x = "Year") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1))
    combined_plot <- (plot_SST | plot_SSTA_DHW) / (plot_TSA_DHW + plot_spacer()) +
      plot_layout(heights = c(1, 1)) +
    plot_annotation(title = ecoregion_name)
  return(combined_plot)
}

ecoregion_names <- c(
  "Bahamas and Florida Keys",
  "Belize and west Caribbean",
  "Central and northern Great Barrier Reef",
  "Hispaniola, Puerto Rico and Lesser Antilles",
  "Jamaica",
  "Lesser Sunda Islands and Savu Sea",
  "Maldive Islands",
  "Marianas",
  "Moreton Bay, eastern Australia",
  "Netherlands Antilles and south Caribbean",
  "North Philippines",
  "Persian Gulf",
  "South-east Philippines",
  "Southern Great Barrier Reef",
  "Sulu Sea"
)

ecoregion_plots <- list()
for (eco in ecoregion_names) {
  ecoregion_plots[[eco]] <- plot_ecoregion(eco)
}

print(ecoregion_plots[["Central and northern Great Barrier Reef"]])
print(ecoregion_plots[["Bahamas and Florida Keys"]])
print(ecoregion_plots[["Belize and west Caribbean"]])
print(ecoregion_plots[["Hispaniola, Puerto Rico and Lesser Antilles"]])
print(ecoregion_plots[["Jamaica"]])
print(ecoregion_plots[["Lesser Sunda Islands and Savu Sea"]])
print(ecoregion_plots[["Maldive Islands"]])
print(ecoregion_plots[["Marianas"]])
print(ecoregion_plots[["Moreton Bay, eastern Australia"]])
print(ecoregion_plots[["Netherlands Antilles and south Caribbean"]])
print(ecoregion_plots[["North Philippines"]])
print(ecoregion_plots[["Persian Gulf"]])
print(ecoregion_plots[["South-east Philippines"]])
print(ecoregion_plots[["Southern Great Barrier Reef"]])
print(ecoregion_plots[["Sulu Sea"]])

#mean cortad, bleaching, year
coRTAD_summary <- data_cleaned_2 %>%
  group_by(Year) %>%
  summarise(
    mean_SST = mean(Temperature_Kelvin, na.rm = TRUE) - 273.15,  
    mean_SSTA_DHW = mean(SSTA_DHW, na.rm = TRUE),
    mean_TSA_DHW = mean(TSA_DHW, na.rm = TRUE),
    Bleaching_Count = sum(Bleached, na.rm = TRUE)
  )

coRTAD_long <- coRTAD_summary %>%
  pivot_longer(
    cols = c(mean_SST, mean_SSTA_DHW,mean_TSA_DHW,mean_rate),
    names_to = "variable",
    values_to = "value"
  )

summary_stats <- coRTAD_long %>%
  group_by(variable) %>%
  summarise(
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE)
  )


#calculate each range for scaling factor
sst_range <- range(data_cleaned_2$Temperature_Kelvin - 273.15, na.rm = TRUE)
sst_range

ssta_range <- range(data_cleaned_2$SSTA, na.rm = TRUE)
ssta_range

ssta_dhw_range <- range(data_cleaned_2$SSTA_DHW, na.rm = TRUE)
ssta_dhw_range

tsa_range <- range(data_cleaned_2$TSA, na.rm = TRUE)
tsa_range

tsa_dhw_range <- range(data_cleaned_2$TSA_DHW, na.rm = TRUE)
tsa_dhw_range

max_bleaching <- max(coRTAD_summary$Bleaching_Count, na.rm = TRUE)

#scaling factors
scale_SST<- (diff(sst_range)) / max_bleaching 
scale_SSTA_DHW <- (diff(ssta_dhw_range)) / max_bleaching
scale_TSA_DHW<- (diff(tsa_dhw_range)) / max_bleaching

plot_SST <- ggplot(coRTAD_summary , aes(x = Year)) +
  geom_line(aes(y = mean_SST), color = "blue", size = 1) +
  geom_point(aes(y = mean_SST), color = "blue", size = 2) +
  geom_bar(aes(y = Bleaching_Count * scale_SST), stat = "identity",
           fill = "red", alpha = 0.5, width = 0.7) +
  scale_y_continuous(
    name = "Mean SST (°C)",
    sec.axis = sec_axis(~ . / scale_SST, name = "Bleaching Count")
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

plot_SSTA_DHW <- ggplot(coRTAD_summary, aes(x = Year)) +
  geom_line(aes(y = mean_SSTA_DHW), color = "blue", size = 1) +
  geom_point(aes(y = mean_SSTA_DHW), color = "blue", size = 2) +
  geom_bar(aes(y = Bleaching_Count * scale_SSTA_DHW), stat = "identity",
           fill = "red", alpha = 0.5, width = 0.7) +
  scale_y_continuous(
    name = "Mean SSTA_DHW",
    sec.axis = sec_axis(~ . / scale_SSTA_DHW, name = "Bleaching Count")
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

plot_TSA_DHW <- ggplot(coRTAD_summary, aes(x = Year)) +
  geom_line(aes(y = mean_TSA_DHW), color = "blue", size = 1) +
  geom_point(aes(y = mean_TSA_DHW), color = "blue", size = 2) +
  geom_bar(aes(y = Bleaching_Count * scale_TSA_DHW), stat = "identity",
           fill = "red", alpha = 0.5, width = 0.7) +
  scale_y_continuous(
    name = "Mean TSA_DHW",
    sec.axis = sec_axis(~ . / scale_TSA_DHW, name = "Bleaching Count")
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- (plot_SST | plot_SSTA_DHW) /
  (plot_TSA_DHW + plot_spacer()) +
  plot_layout(heights = c(1, 1))
combined_plot


#heatmap:
#find combinations of ecoregion and year that do not exist and fill it with 0 so the grid is gap free
data_filled <- data_cleaned_2 %>%
  complete(Ecoregion_name, Year, fill = list(Average_bleaching = 0))

ggplot(data_filled, aes(x = Year, y = Ecoregion_name, fill = Average_bleaching)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(
    low = "white", high = "red",
    limits = c(0, 100),             
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0%", "25%", "50%", "75%", "100%"),
  ) +
  scale_x_continuous(breaks = unique(data_filled$Year), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Year",
       y = "Ecoregion",
       fill = "Bleaching %") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(color = "black"), 
        axis.ticks.length = unit(5, "pt"))

#other information:
#number of study sites - 5867 study sites: group by coordinates because the same coordinates in cw may have been sampled in rc but have different reef.ID 
study_sites_counts <- data_cleaned_2 %>%
  group_by(Latitude.Degrees, Longitude.Degrees) %>%
  summarise(num_surveys = n(), .groups = "drop")

#case study: Belize and west Caribbean + Central and Northern Australia
#number of ecoregions - 26 ecoregions 
print(eco_df_3)

#count countries:
country_counts_3 <- table(data_cleaned_2$Country)
country_df_3 <- as.data.frame(country_counts_3)
colnames(eco_df_3) <- c("Country", "Total_Surveys")

#count sources:
source_counts_3 <- table(data_cleaned_2$Source)
source_df_3 <- as.data.frame(source_counts_3)
colnames(source_df_3) <- c("Source", "Total_Surveys")

citation() 

erg136_country_counts <- data_cleaned_2 %>%
  filter(Region == "ERG136") %>%
  group_by(Country) %>%
  summarise(survey_count = n(), .groups = "drop")

erg136_country_counts

ERG078_country_counts <- data_cleaned_2 %>%
  filter(Region == "ERG078") %>%
  group_by(Country) %>%
  summarise(survey_count = n(), .groups = "drop")

ERG078_country_counts

ERG035_country_counts <- data_cleaned_2 %>%
  filter(Region == "ERG035") %>%
  group_by(Country) %>%
  summarise(survey_count = n(), .groups = "drop")

ERG035_country_counts

mean_SST_per_year <- data_cleaned_2 %>%
  group_by(Year) %>%
  summarise(mean_SST = mean(Temperature_Kelvin-273.15, na.rm = TRUE))

print(mean_SST_per_year)

SSTA_per_year <- data_cleaned_2 %>%
  group_by(Year) %>%
  summarise(mean_SSTA = mean(SSTA_DHW, na.rm = TRUE))

print(SSTA_per_year)

mean_SST_ERG136 <- data_cleaned_2%>%
  filter(Region == "ERG136") %>%
  group_by(Year) %>%
  summarise(mean_SST = mean(Temperature_Kelvin-273.15, na.rm = TRUE), .groups = "drop")

mean_ERG078 <- data_cleaned_2 %>%
  filter(Region == "ERG078") %>%
  group_by(Year) %>%
  summarise(
    mean_SST = mean(Temperature_Kelvin - 273.15, na.rm = TRUE),
    mean_SSTA = mean(SSTA_DHW, na.rm = TRUE),
    mean_TSA = mean(TSA_DHW, na.rm = TRUE),
    Bleaching_Count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  )

mean_ERG081 <- data_cleaned_2 %>%
  filter(Region == "ERG081") %>%
  group_by(Year) %>%
  summarise(
    mean_SST = mean(Temperature_Kelvin - 273.15, na.rm = TRUE),
    mean_SSTA = mean(SSTA_DHW, na.rm = TRUE),
    mean_TSA = mean(TSA_DHW, na.rm = TRUE),
    Bleaching_Count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  )

mean_ERG136 <- data_cleaned_2 %>%
  filter(Region == "ERG136") %>%
  group_by(Year) %>%
  summarise(
    mean_SST = mean(Temperature_Kelvin - 273.15, na.rm = TRUE),
    mean_SSTA = mean(SSTA_DHW, na.rm = TRUE),
    mean_TSA = mean(TSA_DHW, na.rm = TRUE),
    Bleaching_Count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  )

totalbleached_ERG136 <- data_cleaned_2 %>%
  filter(Region == "ERG136") %>%
  group_by(Year) %>%
  summarise(
    Bleaching_Count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  )

totalbleached_ERG078 <- data_cleaned_2 %>%
  filter(Region == "ERG078") %>%
  group_by(Year) %>%
  summarise(
    Bleaching_Count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  )

totalbleached_ERG081 <- data_cleaned_2 %>%
  filter(Region == "ERG081") %>%
  group_by(Year) %>%
  summarise(
    Bleaching_Count = sum(Bleached, na.rm = TRUE),
    .groups = "drop"
  )
