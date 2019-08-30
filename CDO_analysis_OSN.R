
library("ROracle")
library("dplyr")
library("zoo")
library("reshape2")
library("ggplot2")
library("scales")

Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")
dir_PRU="C:/Users/speeters/repos/Ref_traj/"
CDO_limit_alt=7000
date_start="2019-07-04"
date_end="2019-07-04"

# Create year folder for a new year if necessary

curr_year=strftime(date_start, format = "%Y", tz="UTC")
if (!file.exists(paste0(dir_PRU, "Results/", curr_year))){
  dir.create(file.path(paste0(dir_PRU, "Results/", curr_year)))
}


Radius=200 #NM
VV_threshold=3/60 #FL/second
# Time_threshold=20 #seconds
# Alt_threshold=VV_threshold*Time_threshold #FL
Min_time=20 #seconds
Tol=0.90 #Percentage of the maximum altitude
Min_time_tol=5*60 #seconds
Alt_min=18 #FL
start_date=format(as.POSIXct(date_start), "%d-%b-%Y")
end_date=format(as.POSIXct(date_end), "%d-%b-%Y")
# int_method=1 # 1=linear, 2=spline
Min_points=5
# Top_N_apts=120

Gap_max_duration=60 #seconds
VV_glitch_limit=15000/60 #feet/second
Dist_glitch_limit=50 #NM
Minpoints=10

Timegap=1 #0=Active,1=Not active
Vertical_glitch=0
Horizontal_glitch=1
# Sequence_error=0
AnyError=1

drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
APT_data <- dbGetQuery(con, "SELECT * FROM SP_AIRPORT_INFO")
dbDisconnect(con)


Start_time=Sys.time()
Dates=seq(as.POSIXct(strptime(start_date,"%d-%b-%Y", tz="UTC")),as.POSIXct(strptime(end_date,"%d-%b-%Y", tz="UTC")),24*3600)
for (date in 1:length(Dates)){
  
  start_time_data <- Sys.time()
  
  # prev_date=strftime(Dates[Date]-24*3600,format = "%d-%b-%Y", tz="UTC")
  # curr_date=strftime(Dates[Date],format = "%d-%b-%Y", tz="UTC")
  # next_date=strftime(Dates[Date]+24*3600,format = "%d-%b-%Y", tz="UTC")
  # next_date2=strftime(Dates[Date]+48*3600,format = "%d-%b-%Y", tz="UTC")
  # curr_year=strftime(Dates[Date],format = "%Y", tz="UTC")
  # 
  # source("//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/PRU-ENV/Extract_data_CDO_v6_FR24.R")
  
  Airport="EIDW"
  Phase="ARR" # ARR DEP
  source("Read_OSN_data_from_csv.R")
  data_temp=filter(Traj_data, as.POSIXct(Date, tz="UTC")==Dates[date])
  processing_time_data <- Sys.time()-start_time_data
  
  
  
  # Level segment detection
  
  start_time_lvl <- Sys.time()
  ADES=Airport
  lvl_seg_temp=data.frame()
  Flight_data_temp=data.frame()
  
  for (APT in ADES){
    APT_elev=as.numeric(select(filter(APT_data, ICAO_CODE==APT), ELEVATION_FEET))/100
    
    # Verify that only descent part is taken into account (look for last point at highest altitude)
    last_seq_id=data_temp %>%
      group_by(FLIGHT_ID) %>%
      filter(ALT==max(ALT)) %>%
      mutate(last_seqid=max(SEQ_ID)) %>%
      as.data.frame() %>%
      select(FLIGHT_ID,last_seqid) %>%
      unique()
    
    data_temp=data_temp %>%
      left_join(last_seq_id) %>%
      filter(SEQ_ID>=last_seqid) %>%
      select(-matches("last_seqid"))
    
    # Check the number of data points for each flight
    Nbr_data_points=data_temp %>%
      group_by(FLIGHT_ID) %>%
      summarise(Count=n()) %>%
      filter(Count>=Min_points)
    
    # Remove flights with too few data points
    data_extended=Nbr_data_points %>%
      left_join(data_temp) %>%
      select(-Count) %>% 
      mutate(USED=1)
    
    if (nrow(data_extended)>0){
      
      # Check if vertical velocity in the last box (which mostly doesn't have the same time span as Time_threshold) is <= VV_threshold
      # max_seqid_2nd=aggregate(SEQ_ID ~ FLIGHT_ID, filter(data_extended, USED==1), function(x) sort(x, decreasing=T)[2])
      # max_seqid_2nd=rename(max_seqid_2nd, max_2=SEQ_ID)
      
      data_int=data_extended %>%
        filter(USED==1) %>%
        group_by(FLIGHT_ID) %>%
        mutate(LEVEL=ifelse(is.na(abs((ALT-lead(ALT))/as.numeric(difftime(EVENT_TIME, lead(EVENT_TIME), units="secs")))),
                            0,
                            ifelse(abs((ALT-lead(ALT))/as.numeric(difftime(EVENT_TIME, lead(EVENT_TIME), units="secs")))<=VV_threshold, 1, 0)),
               BEGIN=ifelse(LEVEL==1 & (lag(LEVEL)==0 | EVENT_TIME==min(EVENT_TIME)), 1, 0),
               END=ifelse(LEVEL==0 & EVENT_TIME!=min(EVENT_TIME) & lag(LEVEL)==1, 1, 0)) %>%
        select(-matches("max_2")) %>%
        as.data.frame()
      
      Flight_data=summarise(group_by(data_int, FLIGHT_ID), ALT_MAX=max(round(ALT/10)*10))
      
      Begins=data_int %>%
        filter(BEGIN==1) %>%
        left_join(Flight_data, by="FLIGHT_ID") %>%
        select(FLIGHT_ID, LON_START=LON, LAT_START=LAT, ALT_START=ALT, TIME_START=EVENT_TIME, DIST_START=POINT_DIST)
      Ends=data_int %>%
        ungroup() %>%
        filter(END==1) %>%
        select(LON_END=LON, LAT_END=LAT, ALT_END=ALT, TIME_END=EVENT_TIME, DIST_END=POINT_DIST)
      lvl_seg=cbind(Begins, Ends)
      
      lvl_seg=lvl_seg %>% 
        left_join(Flight_data) %>% 
        mutate(DIST_LEVEL_NM=DIST_END-DIST_START,
               TIME_LEVEL_SECONDS=as.numeric(difftime(TIME_END, TIME_START, units="secs")),
               CONSIDERED=ifelse((TIME_LEVEL_SECONDS>=Min_time & (ALT_START+ALT_END)/2>=(Alt_min+APT_elev)), 
                                 ifelse(round((ALT_START+ALT_END)/2/10)*10>=ALT_MAX,
                                        0,
                                        ifelse(!((ALT_START+ALT_END)/2>=Tol*ALT_MAX &
                                                   abs(as.numeric(difftime(TIME_START, TIME_END, units="secs")))>Min_time_tol),1,0)),
                                 0),
               EXCL_BOX=ifelse(round((ALT_START+ALT_END)/2/10)*10>=ALT_MAX,
                               1,
                               ifelse(!((ALT_START+ALT_END)/2>=Tol*ALT_MAX &
                                          abs(as.numeric(difftime(TIME_START, TIME_END, units="secs")))>Min_time_tol),0,1)),
               TOO_LOW=ifelse((ALT_START+ALT_END)/2>=(Alt_min+APT_elev), 0, 1),
               TOO_SHORT=ifelse(TIME_LEVEL_SECONDS>=Min_time, 0, 1)
        ) %>%
        select(-matches("ALT_MAX"),-matches("DIST_START"),-matches("DIST_END"))
      
      lvl_seg_temp=rbind(lvl_seg_temp, lvl_seg)
      
      # Flight data extraction
      
      TODA200=data_int %>%
        left_join(Flight_data, by="FLIGHT_ID") %>%
        group_by(FLIGHT_ID) %>%
        mutate(TOD_A200=ifelse(max(END==1 & round(ALT/10)*10==ALT_MAX)==1,
                               SEQ_ID[END==1 & round(ALT/10)*10==ALT_MAX],
                               SEQ_ID[round(ALT/10)*10==ALT_MAX])) %>%
        select(FLIGHT_ID, TOD_A200) %>%
        unique() %>%
        as.data.frame()
      
      if (nrow(lvl_seg)>0){
        TODCDO=data_int %>%
          left_join(lvl_seg, by="FLIGHT_ID") %>%
          group_by(FLIGHT_ID) %>%
          filter(EVENT_TIME==TIME_END & EXCL_BOX==1) %>%
          mutate(TOD_CDO=max(SEQ_ID)) %>%
          select(FLIGHT_ID, TOD_CDO) %>%
          unique() %>%
          as.data.frame()
      } else {
        TODCDO=cbind(Samids, TOD_CDO=NA)
      }
      
      TOD=TODA200 %>%
        left_join(TODCDO) %>%
        group_by(FLIGHT_ID) %>%
        mutate(TOD=ifelse(is.na(TOD_CDO),TOD_A200,TOD_CDO)) %>%
        select(TOD) %>%
        as.data.frame()
      
      if (nrow(lvl_seg)>0){
        end_lvl_index=data_int %>%
          left_join(filter(lvl_seg, TOO_LOW==0), by="FLIGHT_ID") %>%
          group_by(FLIGHT_ID) %>%
          filter(EVENT_TIME==TIME_END) %>%
          mutate(end_lvl_idx=max(SEQ_ID)) %>%
          select(end_lvl_idx) %>%
          unique() %>%
          as.data.frame()
      } else {
        end_lvl_index=cbind(Samids, end_lvl_idx=NA)
      }
      
      end_index_temp=data_int %>%
        group_by(FLIGHT_ID) %>%
        filter(ALT>=(Alt_min+APT_elev)) %>%
        mutate(end_idx=max(SEQ_ID)) %>%
        select(end_idx) %>%
        unique() %>%
        as.data.frame()
      
      if (nrow(end_index_temp)==0 & nrow(end_lvl_index)==0) {
        end_index=end_index_temp
      } else {
        end_index=end_index_temp %>%
          left_join(end_lvl_index) %>%
          group_by(FLIGHT_ID) %>%
          mutate(end_idx2=max(end_lvl_idx,end_idx, na.rm=TRUE)) %>%
          select(end_idx2) %>%
          rename(end_idx=end_idx2) %>%
          as.data.frame()
      }
      
      Flight_data=Flight_data %>%
        left_join(TOD) %>%
        left_join(data_int, by="FLIGHT_ID") %>%
        group_by(FLIGHT_ID) %>%
        filter(SEQ_ID==TOD) %>%
        select(FLIGHT_ID, TOD_LON=LON, TOD_LAT=LAT, TOD_FLIGHT_LEVEL=ALT,
               TOD_EVENT_TIME=EVENT_TIME, TOD_DIST=POINT_DIST, ALT_MAX) %>%
        left_join(end_index) %>%
        left_join(data_int, by="FLIGHT_ID") %>%
        filter(SEQ_ID==end_idx) %>%
        select(FLIGHT_ID, ADES, TOD_LON, TOD_LAT, TOD_FLIGHT_LEVEL, TOD_EVENT_TIME, TOD_DIST, END_LON=LON, END_LAT=LAT, END_FLIGHT_LEVEL=ALT,
               END_EVENT_TIME=EVENT_TIME, END_DIST=POINT_DIST, ALT_MAX) %>%
        mutate(DESCENT_DISTANCE_NM=END_DIST-TOD_DIST, DESCENT_TIME_SECONDS=as.numeric(difftime(END_EVENT_TIME, TOD_EVENT_TIME, units="secs"))) %>%
        select(-matches("TOD_DIST"),-matches("END_DIST")) %>%
        as.data.frame()
      
      Flight_data_temp=rbind(Flight_data_temp, Flight_data)
      rm(lvl_seg, Flight_data)
    }
    # print(paste0(APT, " on ", curr_date))
    
  }
  
  # if (!file.exists(paste0(dir_PRU, "Results/Final/Lvl_seg_all_CDO_", curr_year))){
  #   Lvl_seg_all=data.frame(FLIGHT_ID=numeric(), LOBT=character(), LON_START=numeric(), LAT_START=numeric(), ALT_START=numeric(), TIME_START=as.POSIXct(character()),
  #                          LON_END=numeric(), LAT_END=numeric(), ALT_END=numeric(), TIME_END=as.POSIXct(character()), DIST_LEVEL_NM=numeric(),
  #                          TIME_LEVEL_SECONDS=numeric(), CONSIDERED=numeric(), EXCL_BOX=numeric(), TOO_LOW=numeric(), TOO_SHORT=numeric())
  #   Flight_data_all=data.frame(FLIGHT_ID=numeric(), LOBT=character(), TOD_LON=numeric(), TOD_LAT=numeric(), TOD_FLIGHT_LEVEL=numeric(),
  #                              TOD_EVENT_TIME=as.POSIXct(character()), END_LON=numeric(), END_LAT=numeric(), END_FLIGHT_LEVEL=numeric(),
  #                              END_EVENT_TIME=as.POSIXct(character()), alt_max_CPF=numeric(), DESCENT_DISTANCE_NM=numeric(),
  #                              DESCENT_TIME_SECONDS=numeric(), alt_max_FTFM=numeric(), ADEP=character(), ADES_FILED=character())
  #   Saved=saveRDS(Lvl_seg_all,paste0(dir_PRU, 'Results/Final/Lvl_seg_all_CDO_',curr_year))
  #   Saved=saveRDS(Flight_data_all,paste0(dir_PRU, 'Results/Final/Flight_data_all_CDO_',curr_year))
  # }
  # 
  lvl_seg_extended=lvl_seg_temp %>%
    mutate(METHOD="CCO_CDO_PRU", INSERT_DATE_UTC=Sys.time())
  Lvl_seg_save=saveRDS(lvl_seg_extended,paste0(dir_PRU, 'Results/', curr_year, '/Lvl_seg_CDO_',strftime(Dates[date],format = "%d_%m_%Y")))
  
  Flight_data_extended=Flight_data_temp %>%
    mutate(METHOD="CCO_CDO_TF", INSERT_DATE_UTC=Sys.time())
  Flight_data_save=saveRDS(Flight_data_extended,paste0(dir_PRU, 'Results/', curr_year, '/Flight_data_CDO_',strftime(Dates[date],format = "%d_%m_%Y")))
  
  # con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
  # dbWriteTable(con, name="SP_CDO_LVL_SEG", value=lvl_seg_extended, row.names = FALSE, overwrite = FALSE, append = TRUE, schema="PRUTEST")
  # dbWriteTable(con, name="SP_CDO_FLIGHT_DATA", value=Flight_data_extended, row.names = FALSE, overwrite = FALSE, append = TRUE, schema="PRUTEST")
  # dbDisconnect(con)
  
  # lvl_seg_extended=lvl_seg_extended %>%
  #   filter(CONSIDERED==1) %>%
  #   unique() %>%
  #   mutate(nbr=1)
  # 
  # Flight_data_extended=Flight_data_extended %>% 
  #   filter(DESCENT_DISTANCE_NM>0, DESCENT_TIME_SECONDS>0) %>%
  #   unique()
  # 
  # # Add level segments and flight data to Lvl_seg_all_CDO_yyyy and Flight_data_all_CDO_yyyy
  # 
  # Lvl_seg_all=readRDS(paste0(dir_PRU, 'Results/Final/Lvl_seg_all_CDO_', curr_year))
  # Lvl_seg_all=rbind(Lvl_seg_all, lvl_seg_extended) %>%
  #   unique()
  # Saved=saveRDS(Lvl_seg_all,paste0(dir_PRU, 'Results/Final/Lvl_seg_all_CDO_',curr_year))
  # rm(Lvl_seg_all)
  # Flight_data_all=readRDS(paste0(dir_PRU, 'Results/Final/Flight_data_all_CDO_', curr_year))
  # Flight_data_all=rbind(Flight_data_all, Flight_data_extended) %>%
  #   unique()
  # Saved=saveRDS(Flight_data_all,paste0(dir_PRU, 'Results/Final/Flight_data_all_CDO_',curr_year))
  # rm(Flight_data_all)
  
  processing_time_lvl=Sys.time()-start_time_lvl
  
  processing_time_data+processing_time_lvl
  
  rm(ADES, lvl_seg_temp, Flight_data_temp)
  
  print(Sys.time())
}

Elapsed_time=Sys.time()-Start_time
