
library(openxlsx)
library(dplyr)
library(lubridate)

SP_dist_NM <- function(lon1, lat1, lon2, lat2){
  a=6378.137/1.852 #NM
  b=6356.7523/1.852 #NM
  deg2rad=pi/180
  LAT_avg=(lat1+lat2)/2*deg2rad;
  R=sqrt(((a^2*cos(LAT_avg))^2+(b^2*sin(LAT_avg))^2)/((a*cos(LAT_avg))^2+(b*sin(LAT_avg))^2));
  Delta_LON=(lon2-lon1)*deg2rad;
  Delta_LAT=(lat2-lat1)*deg2rad;
  Distance=2*R*asin(sqrt((sin(Delta_LAT/2))^2+cos(lat1*deg2rad)*cos(lat2*deg2rad)*(sin(Delta_LON/2))^2));
  
  return(Distance)
}

Traj_data=data.frame()
if (Airport=="EGLL") {
  Files=list.files(path = "Data/OSN data/", pattern = paste0("^",Airport, "-adsb-2018-06-.*\\-", Phase, ".csv"))
} else if (Airport=="EIDW") {
  Files=list.files(path = "Data/OSN data/", pattern = paste0("^",Airport, "-adsb-2019-07-.*\\-", Phase, ".csv"))
} else if (Airport=="EDDM") {
  Files=list.files(path = "Data/OSN data/", pattern = paste0("^",Airport, "-adsb-2019-06-.*\\-", Phase, ".csv"))
} else if (Airport=="LSZH") {
  Files=list.files(path = "Data/OSN data/", pattern = paste0("^",Airport, "-adsb-2019-06-.*\\-", Phase, ".csv"))
}

for (file in Files) {
  Traj_data_temp=read.csv(paste0("Data/OSN data/", file), header = TRUE, sep = ",")
  Traj_data=rbind(Traj_data, Traj_data_temp)
}

Traj_data=mutate(Traj_data, 
                 EVENT_TIME=as.POSIXct(item_time, origin="1970-01-01"),
                 Date=format(as.POSIXct(day, origin="1970-01-01"), "%Y-%m-%d"),
                 FLIGHT_ID=paste(icao24, firstseen, sep="-")) %>% 
  rename(LON=item_longitude,
         LAT=item_latitude,
         ALT=item_altitude,
         ADES=estarrivalairport
  ) %>% 
  arrange(Date, FLIGHT_ID, EVENT_TIME) %>% 
  group_by(FLIGHT_ID) %>% 
  mutate(SEQ_ID=row_number(),
         POINT_DIST_TMP=ifelse(is.na(SP_dist_NM(LON, LAT, lag(LON), lag(LAT))), 0, SP_dist_NM(LON, LAT, lag(LON), lag(LAT))),
         POINT_DIST=cumsum(POINT_DIST_TMP)) %>% 
  select(-POINT_DIST_TMP)
rm(Traj_data_temp)
