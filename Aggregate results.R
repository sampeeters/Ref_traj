
Dates_res=format(seq(as.POSIXct(strptime("04-Jul-2019","%d-%b-%Y", tz="UTC")),
                     as.POSIXct(strptime("04-Jul-2019","%d-%b-%Y", tz="UTC")),
                     24*3600),
                 "%d_%m_%Y")

# CDO

Lvl_seg_all=data.frame()
Flight_data_all=data.frame()

for (i in 1:length(Dates_res)){
  curr_year=strftime(as.POSIXct(strptime(Dates_res[i],"%d_%m_%Y", tz="UTC")),format = "%Y", tz="UTC")
  Lvl_seg_temp=readRDS(paste0(dir_PRU, 'Results/', curr_year, '/Lvl_seg_CDO_', Dates_res[i]))
  Lvl_seg_all=rbind(Lvl_seg_all, Lvl_seg_temp)
  Flight_data_temp=readRDS(paste0(dir_PRU, 'Results/', curr_year, '/Flight_data_CDO_', Dates_res[i]))
  Flight_data_all=rbind(Flight_data_all, Flight_data_temp)
}

Lvl_seg_all=Lvl_seg_all %>% 
  filter(CONSIDERED==1) %>%
  unique() %>%
  cbind(nbr=1)

Flight_data_all=Flight_data_all %>% 
  filter(DESCENT_DISTANCE_NM>0 & DESCENT_TIME_SECONDS>0) %>% 
  unique()

Saved=saveRDS(Lvl_seg_all, paste0(dir_PRU, "Results/Lvl_seg_all_CDO_", curr_year))
Saved=saveRDS(Flight_data_all, paste0(dir_PRU, "Results/Flight_data_all_CDO_", curr_year))

CDO_results_per_flight=Flight_data_all %>%
  left_join(Lvl_seg_all, by=c("FLIGHT_ID")) %>%
  group_by(FLIGHT_ID, ADES) %>%
  summarise(DIST_LEVEL_NM=sum(DIST_LEVEL_NM, na.rm=TRUE),PERC_DIST_LEVEL=sum(DIST_LEVEL_NM, na.rm=TRUE)/min(DESCENT_DISTANCE_NM),
            TIME_LEVEL_SECONDS=sum(TIME_LEVEL_SECONDS, na.rm=TRUE),
            PERC_TIME_LEVEL=sum(TIME_LEVEL_SECONDS, na.rm=TRUE)/as.numeric(min(DESCENT_TIME_SECONDS)),
            CDO_ALT=ifelse(is.na(min((ALT_START+ALT_END)/2)), min(ALT_MAX), min((ALT_START+ALT_END)/2)),
            NBR_LVL_SEG=sum(nbr, na.rm=TRUE),
            CDO=ifelse(TIME_LEVEL_SECONDS==0, 1, 0)) %>% 
  mutate(CDO_BELOW_7000=ifelse(CDO_ALT>=CDO_limit_alt, 1, 0)) %>%
  as.data.frame()

Saved=saveRDS(CDO_results_per_flight, paste0(dir_PRU, "Results/CDO_results_per_flight_", curr_year))


# CCO

Lvl_seg_all=data.frame(SAM_ID=numeric(), START_TIME=character(), LON_START=numeric(), LAT_START=numeric(), ALT_START=numeric(), TIME_START=as.POSIXct(character()),
                       LON_END=numeric(), LAT_END=numeric(), ALT_END=numeric(), TIME_END=as.POSIXct(character()), DIST_LEVEL_NM=numeric(),
                       TIME_LEVEL_SECONDS=numeric(), CONSIDERED=numeric(), EXCL_BOX=numeric(), TOO_LOW=numeric(), TOO_SHORT=numeric())
Flight_data_all=data.frame(SAM_ID=numeric(), START_TIME=character(), START_LON=numeric(), START_LAT=numeric(), START_FLIGHT_LEVEL=numeric(),
                           START_TIME_OVER=as.POSIXct(character()), TOC_LON=numeric(), TOC_LAT=numeric(), TOC_FLIGHT_LEVEL=numeric(),
                           TOC_TIME_OVER=as.POSIXct(character()), alt_max_CPF=numeric(), CLIMB_DISTANCE_NM=numeric(),
                           CLIMB_TIME_SECONDS=numeric(), alt_max_FTFM=numeric(), ADEP=character(), ADES_FILED=character())

for (i in 1:length(Dates_res)){
  curr_year=strftime(as.POSIXct(strptime(Dates_res[i],"%d_%m_%Y", tz="UTC")),format = "%Y", tz="UTC")
  Lvl_seg_temp=readRDS(paste0(dir_PRU, 'Results/Final/', curr_year, '/Lvl_seg_CCO_', Dates_res[i]))
  Lvl_seg_all=rbind(Lvl_seg_all, Lvl_seg_temp)
  Flight_data_temp=readRDS(paste0(dir_PRU, 'Results/Final/', curr_year, '/Flight_data_CCO_', Dates_res[i]))
  Flight_data_all=rbind(Flight_data_all, Flight_data_temp)
}

Lvl_seg_all=Lvl_seg_all %>% 
  filter(CONSIDERED==1 &
           START_TIME>=paste0(year, ifelse(month>=10, "-", "-0"), month, "-01") & 
           START_TIME<paste0(ifelse(month==12, year+1, year), ifelse(month>=9 & month<12, "-", "-0"), ifelse(month==12, 1, month+1), "-01")) %>%
  unique() %>%
  cbind(nbr=1)

Flight_data_all=Flight_data_all %>% 
  filter(CLIMB_DISTANCE_NM>0 & CLIMB_TIME_SECONDS>0 & 
           START_TIME>=paste0(year, ifelse(month>=10, "-", "-0"), month, "-01") & 
           START_TIME<paste0(ifelse(month==12, year+1, year), ifelse(month>=9 & month<12, "-", "-0"), ifelse(month==12, 1, month+1), "-01")) %>% 
  unique()

Saved=saveRDS(Lvl_seg_all, paste0(dir_PRU, "Results/Final/Lvl_seg_all_CCO_", ifelse(month>=10, "", "0"), month, "_", year))
Saved=saveRDS(Flight_data_all, paste0(dir_PRU, "Results/Final/Flight_data_all_CCO_", ifelse(month>=10, "", "0"), month, "_", year))

CCO_results_per_flight=Flight_data_all %>%
  left_join(Lvl_seg_all, by=c("FLIGHT_ID", "START_TIME")) %>%
  group_by(FLIGHT_ID, SAM_ID=ID, START_TIME, ADEP, AIRCRAFT_OPERATOR) %>%
  summarise(DIST_LEVEL_NM=sum(DIST_LEVEL_NM, na.rm=TRUE),PERC_DIST_LEVEL=sum(DIST_LEVEL_NM, na.rm=TRUE)/min(CLIMB_DISTANCE_NM),
            TIME_LEVEL_SECONDS=sum(TIME_LEVEL_SECONDS, na.rm=TRUE),
            PERC_TIME_LEVEL=sum(TIME_LEVEL_SECONDS, na.rm=TRUE)/as.numeric(min(CLIMB_TIME_SECONDS)),
            CCO_ALT=ifelse(is.na(min((ALT_START+ALT_END)/2)), min(ALT_MAX), min((ALT_START+ALT_END)/2)),
            NBR_LVL_SEG=sum(nbr, na.rm=TRUE),
            CCO=ifelse(TIME_LEVEL_SECONDS==0, 1, 0)) %>%
  mutate(CCO_BELOW_10000=ifelse(CCO_ALT>=CCO_limit_alt, 1, 0)) %>%
  as.data.frame()

Saved=saveRDS(CCO_results_per_flight, paste0(dir_PRU, "Results/Final/CCO_results_per_flight_", ifelse(month>=10, "", "0"), month, "_", year))
