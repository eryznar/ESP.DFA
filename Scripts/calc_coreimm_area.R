# notes ----

# Determine core habitat of immature snow crab in EBS
  #1) Determine stations that compose average core habitat across the long-term timeseries 
  #2) Determine average bottom temperature of core habitat within each yr
  
# Author: Erin Fedewa
#Last update: 1/5/2023

#NOTES:
#Given that stations differed pre 1980, only 1980+ dataset was used to calculate 
  #avg core habitat across timeseries
#Bottom temps need to be data corrected and imputed for final timeseries dataset

source("./Scripts/load_libs_params.R")

## EBS haul data ----
sc_catch <- read.csv(paste0("Y:/KOD_Survey/EBS Shelf/", current.year, "/Tech Memo/Data/crabhaul_opilio.csv"))

#EBS strata data ----
sc_strata <- read.csv(paste0("Y:/KOD_Survey/EBS Shelf/", current.year, "/Tech Memo/Data/STRATA_OPILIO_NEWTIMESERIES.csv"))


###################################################
# data exploration ----

#Stations sampled in each year
sc_catch <- sc_catch %>%
  mutate(AKFIN_SURVEY_YEAR = lubridate::year(lubridate::parse_date_time(START_DATE, orders = "mdy"))) 

sc_catch %>%
  group_by(AKFIN_SURVEY_YEAR) %>%
  summarise(num_stations = length(unique(GIS_STATION)))

#Plot pre-standardization data
sc_catch %>%
  filter(AKFIN_SURVEY_YEAR < 1988) %>%
  group_by(AKFIN_SURVEY_YEAR, MID_LONGITUDE, MID_LATITUDE) %>%
  distinct(GIS_STATION) %>%
ggplot() +
  geom_point(aes(x = MID_LONGITUDE, y = MID_LATITUDE), size=.5) +
  labs(x = "Longitude", y = "Latitude") +
  facet_wrap(~AKFIN_SURVEY_YEAR)

#Earliest yrs don't survey prime snow crab habitat so 90% of habitat index
  #will be biased in these years 

# Specify corner stations for exclusion
corner <- list("QP2625","ON2625","HG2019","JI2120","IH1918",
               "GF2221","HG1918","GF2019","ON2524","PO2726",
               "IH2221","GF1918","JI2221","JI2019","JI1918",
               "HG2221","QP2726","PO2423","IH2019","PO2625",
               "QP2423","IH2120","PO2524","HG2120","GF2120",
               "QP2524")

#Calculate CPUE by station for all immature snow crab 
sc_catch %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         AKFIN_SURVEY_YEAR > 1979) %>%
  mutate(MAT_SEX = case_when((SEX == 1 & WIDTH < 95 & SHELL_CONDITION <= 2) ~ "Immature Male",
                             (SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature Female")) %>%
  filter(MAT_SEX %in% c("Immature Male", "Immature Female")) %>%
  group_by(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT,MID_LATITUDE, MID_LONGITUDE, GEAR_TEMPERATURE) %>%
  summarise(N_CRAB = sum(SAMPLING_FACTOR, na.rm = T),
            CPUE = N_CRAB / mean(AREA_SWEPT)) %>%
  ungroup() %>%
  #join to zero catch stations
  right_join(., sc_strata %>%
                    filter(SURVEY_YEAR > 1979) %>%
                    distinct(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
                    rename_all(~c("GIS_STATION", "AKFIN_SURVEY_YEAR",
                             "STRATUM", "TOTAL_AREA_SQ_NM")))  %>%
              replace_na(list(CPUE = 0)) -> cpue

 # filter out corner stations
 cpue %>%
   filter(!GIS_STATION %in% corner) -> cpue2
  
# Determine stations that compose average core habitat across the long-term timeseries 

#Histogram with percentiles 
cpue2 %>%
  group_by(GIS_STATION) %>%
  summarise(AVG_CPUE = mean(CPUE)) -> data

p50 <- quantile(data$AVG_CPUE, 0.50)

#stations in 50-100 CPUE percentile range
cpue2 %>%
  group_by(GIS_STATION) %>%
  summarise(AVG_CPUE = mean(CPUE)) %>%
  filter(AVG_CPUE > quantile(AVG_CPUE, 0.50)) -> perc50 #174 stations
#Lets go with the 50th percentile for defining core immature area 

#Join lat/long back in to perc50 dataset and plot
sc_strata %>%
      filter(SURVEY_YEAR == 2021) %>% #Just selecting a yr when all stations were sampled
      dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
      dplyr::rename(GIS_STATION = STATION_ID) %>%
      right_join(perc50) -> perc50_core

#Write csv for stations in 50th percentile of avg CPUE  
write.csv(perc50_core, file="./Output/imm_area_50perc.csv")


