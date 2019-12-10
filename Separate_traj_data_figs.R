
library("ROracle")
library("dplyr")
library("zoo")
library("reshape2")
library("ggplot2")
library("scales")
library("maptools")
library("openxlsx")

Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")
dir_PRU="C:/Users/speeters/repos/Ref_traj/"
worldmap <- readShapeSpatial("//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/R/TM_WORLD_BORDERS-0.3.shp")
Airspaces_2015 = readShapeSpatial("//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/Civ-Mil/AIS_AIRSPACE_MILITARY/AIS_AIRSPACE_20160610",
                                  delete_null_obj=TRUE)
Country_FIR=subset(Airspaces_2015, TYPE_CODE == "FIR")

Radius=200

Airport="EDDF"
Phase="ARR" # ARR DEP
if (Airport=="EDDF") {
  source("Read_OSN_data_from_csv2.R")
} else {
  source("Read_OSN_data_from_csv.R")
}

drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
APT_data <- dbGetQuery(con, "SELECT * FROM SP_AIRPORT_INFO")
dbDisconnect(con)

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

Traj_data=left_join(Traj_data, select(APT_data, ICAO_CODE, ARP_LAT, ARP_LON), by=c("ADES"="ICAO_CODE")) %>% 
  mutate(InRadius=ifelse(SP_dist_NM(LON, LAT, ARP_LON, ARP_LAT)<=Radius, 1, 0)) %>% 
  group_by(FLIGHT_ID)

Flight_ids=unique(Traj_data$FLIGHT_ID)[1:10]

wb<-createWorkbook()

for (Flight_id in Flight_ids) {
  
  Traj_data_temp=filter(Traj_data, FLIGHT_ID==Flight_id)
  addWorksheet(wb, Flight_id)
  writeData(wb, Flight_id, Traj_data_temp)
  
  # Vertical plot
  
  ggplot() +
    geom_path(data = Traj_data_temp, aes(x=EVENT_TIME, y=ALT)) +
    labs(x="Time", y="Altitude (feet)", title=paste0("OSN data for FLIGHT_ID ", Flight_id)) +
    theme_bw() + 
    theme(plot.title = element_text(size=40, face="bold", hjust=0.5),
          axis.text=element_text(size=36),
          axis.title=element_text(size=38))
  ggsave(paste0(dir_PRU, "Figures/Time_alt_", Flight_id, ".jpeg"), width = 50, height = 30, units = "cm", dpi=200)
  
  # Lateral plot
  
  latlimits <- c(min(Traj_data$LAT)-5, max(Traj_data$LAT)+5)
  longlimits <- c(min(Traj_data$LON)-5, max(Traj_data$LON)+5)
  R=6371/1.852
  r=200/R;
  ang=seq(0,2*pi,0.01)
  Airport=select(ungroup(Traj_data[1,]), ADES) %>% as.character()
  ARPX=as.numeric(select(filter(APT_data, ICAO_CODE==Airport), ARP_LON))
  ARPY=as.numeric(select(filter(APT_data, ICAO_CODE==Airport), ARP_LAT))
  lat=asin(sin(ARPY/180*pi)*cos(r)+cos(ARPY/180*pi)*sin(r)*cos(ang));
  lon=atan2(sin(ang)*sin(r)*cos(ARPY/180*pi),cos(r)-sin(ARPY/180*pi)*sin(lat));
  lat=lat*180/pi
  lon=lon*180/pi
  Radius_coord=as.data.frame(cbind(LON=lon+ARPX, LAT=lat))
  
  ggplot() + 
    geom_polygon(data=worldmap, aes(x = long, y = lat, group = group), fill = "#FCF4C3") + 
    geom_path(data=worldmap, aes(x = long, y = lat, group = group), colour = "#CCCCCC") + 
    # geom_path(data=Country_FIR, aes(x = long, y = lat, group = group), colour = "green", size=1.5) +
    geom_path(data = Traj_data_temp, aes(x=LON, y=LAT, colour=InRadius)) +
    geom_point(data = Traj_data_temp, aes(x=LON, y=LAT, colour=InRadius)) +
    # geom_path(data=Country_FIR, aes(x = long, y = lat, group = group), colour = "green", size=1.5, linetype = 2) +
    geom_path(data = Radius_coord, aes(x=LON, y=LAT), colour="blue") +
    labs(x="Longitude (°)", y="Latitude (°)", title=paste0("OSN data for FLIGHT_ID ", Flight_id)) +
    coord_fixed(ratio=1, xlim = longlimits, ylim = latlimits) +
    theme_bw() + 
    theme(plot.title = element_text(size=40, face="bold", hjust=0.5),
          axis.text=element_text(size=38),
          axis.title=element_text(size=38))
  ggsave(paste0(dir_PRU, "Figures/X_Y_", Flight_id, ".jpeg"), width = 50, height = 30, units = "cm", dpi=200)
  
  if (nrow(filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1))>0) {
    
    latlimits <- c(min(filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1)$LAT)-2, 
                   max(filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1)$LAT)+2)
    longlimits <- c(min(filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1)$LON)-2, 
                    max(filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1)$LON)+2)
    ggplot() + 
      geom_polygon(data=worldmap, aes(x = long, y = lat, group = group), fill = "#FCF4C3") + 
      geom_path(data=worldmap, aes(x = long, y = lat, group = group), colour = "#CCCCCC") + 
      geom_path(data = filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1), aes(x=LON, y=LAT, colour=InRadius)) +
      geom_point(data = filter(Traj_data, FLIGHT_ID==Flight_id & InRadius==1), aes(x=LON, y=LAT, colour=InRadius)) +
      geom_path(data = Radius_coord, aes(x=LON, y=LAT), colour="blue") +
      labs(x="Longitude (°)", y="Latitude (°)", title=paste0("OSN data for FLIGHT_ID ", Flight_id)) +
      coord_fixed(ratio=1, xlim = longlimits, ylim = latlimits) +
      theme_bw() + 
      theme(plot.title = element_text(size=40, face="bold", hjust=0.5),
            axis.text=element_text(size=38),
            axis.title=element_text(size=38))
    ggsave(paste0(dir_PRU, "Figures/X_Y_", Flight_id, "_radius.jpeg"), width = 50, height = 30, units = "cm", dpi=200)
    
  }
  
}

saveWorkbook(wb, "Data/Traj_data_sample.xlsx", overwrite = TRUE)
