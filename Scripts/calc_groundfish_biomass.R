# notes ----

#1) Calculate biomass of pacific cod within imm snow crab 50th percentile home range 
#2) Calculate biomass of subarctic fish complex within imm snow crab 50th percentile home range

## LOAD PACKAGES + UNIVERSAL PARAMS --------
source("./Scripts/load_libs_params.R")

## LOAD DATA --------

  ## Groundfish specimen data
  gf_catch <- read.csv("./Data/gf_cpue_timeseries_2024.csv")
  gf_catch2 <- read.csv("./Data/groundfish_timeseries.csv")
  
  #Core immature snow crab stations
  imm_area <- read.csv("./Output/imm_area_50perc.csv")
  
  imm_stations <- pull(imm_area, GIS_STATION)
  
## CALCULATE BIOMASS WITH IMM SNOW CRAB 50th PERCENTILE HOME RANGE --------

# Pacific cod -----
  # separate cod data
  gf_catch %>%
    filter(SPECIES_CODE %in% c(21720), # filter by cod
           STATION %in% imm_stations, # filter to snow crab core area
           YEAR >= 1988) %>% # filter by years where full station number in core area is sampled (174)
    group_by(YEAR) %>%
    reframe(mean_cod_CPUE = mean(log(CPUE_KGKM2+1))) -> mean_cod_cpue
  
  # Plot
  ggplot(mean_cod_cpue, aes(YEAR, mean_cod_CPUE)) +
    geom_line() +
    geom_point()+
    ggtitle("1988-2024 cod biomass (log(CPUE_KGKM2+1))")
  
  # Old data
  # separate cod data
  gf_catch2 %>%
    filter(SID %in% c(21720),
           STATION %in% imm_stations) -> cod
  
  stratum <- sc_strata %>%
    select(STATION_ID, SURVEY_YEAR) %>% 
    rename(STATION = STATION_ID,
           YEAR = SURVEY_YEAR) %>%
    filter(YEAR >= 1988,
           STATION %in% imm_stations)
  
  cod <- left_join(stratum, cod) 
  
  # replace NA catch with 0
  change <- is.na(cod$WTCPUE)
  cod$WTCPUE[change] <- 0
  
  cod %>%
    filter(YEAR >= 1988) %>% # limit to years with at least 174 stations
    group_by(YEAR) %>%
    mutate(WTCPUE = WTCPUE *100) %>% # change to kg km-2
    reframe(mean_cod_CPUE = mean(log(WTCPUE+1))) -> mean_cod_cpue
  
  ggplot(mean_cod_cpue, aes(YEAR, mean_cod_CPUE)) +
    geom_line() +
    geom_point()+
    ggtitle("1988-2022 cod biomass (log(WTCPUE+1))")
  

# Arctic complex -----
  gf_catch %>%
    filter(SPECIES_CODE %in% c(10285,21725,10140,21348,10115,66045,24185,10212), # filter arctic complex (alaska plaice, arctic cod, bering flounder, butterfly sculpin, greenland turbot, marbled eelpout, wattled eelpout)
           STATION %in% imm_stations, # filter to snow crab core area
           YEAR >= 1988) %>% # filter by years where full station number in core area is sampled (174)
    group_by(YEAR, STATION) %>% 
    reframe(sum_CPUE = sum(CPUE_KGKM2)) %>% # sum by year and station
    group_by(YEAR) %>%
    reframe(mean_arctic_CPUE = mean(log(sum_CPUE+1))) -> mean_arctic_cpue

  
  ggplot(mean_arctic_cpue, aes(YEAR, mean_arctic_CPUE)) +
    geom_line() +
    geom_point() +
    ggtitle("1988-2024 arctic biomass (log(CPUE_KGKM2+1))")
  
  