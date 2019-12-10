
library("ROracle")
library("raster")
library("dplyr")
library("ggplot2")
library("reshape2")
library("maptools")

Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")
dir_PRU="C:/Users/speeters/repos/Ref_traj/"
Results_all=data.frame()
Results_radius=data.frame()

Airport="EDDF"
Phase="ARR" # ARR DEP

x_min=-35
x_max=55
y_min=20
y_max=75
resolution=0.1
Raster_data=data.frame()

drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
APT_data <- dbGetQuery(con, "SELECT * FROM SP_AIRPORT_INFO")
dbDisconnect(con)

if (Airport=="EDDF") {
  source("Read_OSN_data_from_csv2.R")
} else {
  source("Read_OSN_data_from_csv.R")
}

data_temp1=distinct(Traj_data, FLIGHT_ID, EVENT_TIME, .keep_all = TRUE)
data_temp2=data_temp1 %>% 
  group_by(FLIGHT_ID) %>% 
  summarise(nbr_points=n()) %>% 
  filter(nbr_points>=5)
data_temp3=filter(data_temp1, FLIGHT_ID %in% data_temp2$FLIGHT_ID) %>%
  left_join(dplyr::select(APT_data, ICAO_CODE, ARP_LAT, ARP_LON), by=c("ADES"="ICAO_CODE")) %>% 
  mutate(In_radius=ifelse(SP_dist_NM(LON, LAT, ARP_LON, ARP_LAT)<=200, 1, 0)) %>%
  dplyr::select(-ADES, -ARP_LAT, -ARP_LON) %>% 
  group_by(FLIGHT_ID)
data_temp4=filter(data_temp3, In_radius==1) %>% 
  summarise(nbr_points=n()) %>% 
  filter(nbr_points>=5)
data_temp_radius=filter(data_temp3, In_radius==1 & FLIGHT_ID %in% data_temp4$FLIGHT_ID) %>%
  mutate(Time_diff=as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")),
         Alt_diff=abs((ALT-lag(ALT))),
         VV_diff=abs((ALT-lag(ALT))/as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs"))))
data_temp=mutate(data_temp3, Time_diff=as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")),
                 Alt_diff=abs((ALT-lag(ALT))),
                 VV_diff=abs((ALT-lag(ALT))/as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs"))))

Time_gap_durations_all=group_by(data_temp, FLIGHT_ID, Date) %>% 
  summarise(Total_gaps_duration=sum(Time_diff[Time_diff>60], na.rm = TRUE),
            Total_flight_time=as.numeric(difftime(max(EVENT_TIME), min(EVENT_TIME), units="secs")))
Perc_time_gap_all=group_by(Time_gap_durations_all, Date) %>% 
  summarise(Perc_time_gap=sum(Total_gaps_duration)/sum(Total_flight_time))

Nbr_points_all=ungroup(data_temp) %>% 
  group_by(Date) %>% 
  summarise(Nbr_points=n())

AllGapsAndGlitches_all=mutate(filter(data_temp, (Time_diff>0 | is.na(Time_diff))),
                              TimeGap=ifelse(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")<=60, 0, 1),
                              VertGlitch=ifelse(abs((ALT-lag(ALT))/as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")))
                                                <=10000/60, 0, 1),
                              HorGlitch=ifelse(SP_dist_NM(LON, LAT, lag(LON), lag(LAT))<=50, 0, 1),
                              AnyError=ifelse((difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")<=60 &
                                                 abs((ALT-lag(ALT))/as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")))
                                               <=10000/60 &
                                                 SP_dist_NM(LON, LAT, lag(LON), lag(LAT))<=50), 0, 1))

GapAndGlitchOverview_all=group_by(AllGapsAndGlitches_all, FLIGHT_ID, Date) %>% 
  summarise(Time_Gap=max(TimeGap, na.rm=TRUE),
            Vert_Glitch=max(VertGlitch, na.rm=TRUE),
            Hor_Glitch=max(HorGlitch, na.rm=TRUE),
            Any_Error=max(AnyError, na.rm=TRUE))

Perc_flights_all=ungroup(GapAndGlitchOverview_all) %>% 
  group_by(Date) %>% 
  summarise(Nbr_flights=n(),
            Perc_flights_timegap=sum(Time_Gap)/Nbr_flights,
            Perc_flights_vertglitch=sum(Vert_Glitch)/Nbr_flights,
            Perc_flights_horglitch=sum(Hor_Glitch)/Nbr_flights,
            Perc_flights_anyerror=sum(Any_Error)/Nbr_flights)
Results_all=rbind(Results_all, left_join(left_join(Nbr_points_all, Perc_flights_all), Perc_time_gap_all))



Time_gap_durations_radius=group_by(data_temp_radius, FLIGHT_ID, Date) %>% 
  summarise(Total_gaps_duration=sum(Time_diff[Time_diff>60], na.rm = TRUE),
            Total_flight_time=as.numeric(difftime(max(EVENT_TIME), min(EVENT_TIME), units="secs")))
Perc_time_gap_radius=group_by(Time_gap_durations_radius, Date) %>% 
  summarise(Perc_time_gap=sum(Total_gaps_duration)/sum(Total_flight_time))

Nbr_points_radius=ungroup(data_temp_radius) %>% 
  group_by(Date) %>% 
  summarise(Nbr_points=n())

AllGapsAndGlitches_radius=mutate(filter(data_temp_radius, (Time_diff>0 | is.na(Time_diff))),
                                 TimeGap=ifelse(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")<=60, 0, 1),
                                 VertGlitch=ifelse(abs((ALT-lag(ALT))/as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")))
                                                   <=10000/60, 0, 1),
                                 HorGlitch=ifelse(SP_dist_NM(LON, LAT, lag(LON), lag(LAT))<=50, 0, 1),
                                 AnyError=ifelse((difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")<=60 &
                                                    abs((ALT-lag(ALT))/as.numeric(difftime(EVENT_TIME, lag(EVENT_TIME), units="secs")))
                                                  <=10000/60 &
                                                    SP_dist_NM(LON, LAT, lag(LON), lag(LAT))<=50), 0, 1))
GapAndGlitchOverview_radius=group_by(AllGapsAndGlitches_radius, FLIGHT_ID, Date) %>% 
  summarise(Time_Gap=max(TimeGap, na.rm=TRUE),
            Vert_Glitch=max(VertGlitch, na.rm=TRUE),
            Hor_Glitch=max(HorGlitch, na.rm=TRUE),
            Any_Error=max(AnyError, na.rm=TRUE))
Perc_flights_radius=ungroup(GapAndGlitchOverview_radius) %>% 
  group_by(Date) %>% 
  summarise(Nbr_flights=n(),
            Perc_flights_timegap=sum(Time_Gap)/Nbr_flights,
            Perc_flights_vertglitch=sum(Vert_Glitch)/Nbr_flights,
            Perc_flights_horglitch=sum(Hor_Glitch)/Nbr_flights,
            Perc_flights_anyerror=sum(Any_Error)/Nbr_flights)
Results_radius=rbind(Results_radius, left_join(left_join(Nbr_points_radius, Perc_flights_radius), Perc_time_gap_radius))

for (date in Results_all$Date) {
  
  if (filter(Results_all, Date==date)$Nbr_flights>0) {
    
    df1=data_temp[,c("LON", "LAT")]
    spdf=SpatialPoints(coords = df1, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    r <- raster(xmn=x_min, ymn=y_min, xmx=x_max, ymx=y_max, res=resolution)
    r[] <- 0
    tab <- table(cellFromXY(r, spdf))
    r[as.numeric(names(tab))] <- tab
    
    # convert raster to a data frame
    ras.df <- as.data.frame(as.matrix(r))
    
    # setting 'x' column names
    colnames(ras.df) <- seq(x_min+resolution/2, x_max-resolution/2, resolution)
    
    # creating 'y' column names
    ras.df$y <- seq(y_max-resolution/2, y_min+resolution/2, -resolution)
    
    Raster_data=rbind(Raster_data, cbind(date, ras.df))
    
  }
  
}

Results_all=mutate(Results_all, 
                   Date=as.POSIXct(strptime(Date,"%Y-%m-%d", tz="UTC")))
Results_radius=mutate(Results_radius, 
                      Date=as.POSIXct(strptime(Date,"%Y-%m-%d", tz="UTC")))
# Results=rbind(Results_saved, cbind(Results_all, Type="All"), cbind(Results_radius, Type="Radius")) %>% 
#   arrange(Date)
Results=rbind(cbind(Results_all, Type="All"), cbind(Results_radius, Type="Radius")) %>% 
  arrange(Date)

Saved=saveRDS(Results, paste0(dir_PRU, "Results/OSN_completeness_Results"))
Saved=saveRDS(Raster_data, paste0(dir_PRU, "Results/OSN_completeness_Raster_data"))


Results=readRDS(paste0(dir_PRU, "Results/OSN_completeness_Results"))

ggplot() +
  geom_line(data=Results, aes(x=Date, y=Nbr_points, group=Type, linetype=Type), colour="red")

ggplot() +
  geom_line(data=Results, aes(x=Date, y=Nbr_flights, group=Type, linetype=Type), colour="blue")

Results2=melt(Results, id.vars=c("Date", "Type"), measure.vars = c("Nbr_points", "Nbr_flights", "Perc_flights_timegap",
                                                        "Perc_flights_vertglitch", "Perc_flights_horglitch",
                                                        "Perc_flights_anyerror", "Perc_time_gap")) %>% 
  mutate(Group=ifelse(variable=="Nbr_points", "Number of data points",
                      ifelse(variable=="Nbr_flights", "Number of flights", "Percentages"))) %>% 
  filter(variable!="Perc_flights_anyerror")
Results2$Date=rep(Results$Date, 6)

ggplot(Results2) +
  geom_line(aes(x=Date, y=value, colour=variable, linetype=Type), size=1) +
  facet_grid(rows = vars(Group), scales = "free_y") +
  labs(x="", y="", title="OSN stats") +
  theme_bw() + 
  theme(plot.title = element_text(size=30, face="bold", hjust=0.5),
        axis.text=element_text(size=28),
        axis.title=element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 24))
ggsave(paste0(dir_PRU, "Figures/OSN_data_completeness_", format(min(Results_all$Date),"%d%m%Y"), "_",
              format(max(Results_all$Date), "%d%m%Y"), ".jpeg"),
       width = 50, height = 30, units = "cm", dpi=200)



# Coverage on map

worldmap <- readShapeSpatial("//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/R/TM_WORLD_BORDERS-0.3.shp")
Airspaces_2015 = readShapeSpatial("//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/Civ-Mil/AIS_AIRSPACE_MILITARY/AIS_AIRSPACE_20160610",
                                  delete_null_obj=TRUE)
Country_FIR=subset(Airspaces_2015, TYPE_CODE == "FIR")

Raster_data=readRDS(paste0(dir_PRU, "Results/OSN_completeness_Raster_data"))
Raster_data_month=mutate(Raster_data,
                         y=as.numeric(as.character(y)),
                         date=as.POSIXct(date, format="%Y-%m-%d"),
                         Month=strftime(date, format = "%b"),
                         Year=strftime(date, format = "%Y"))

for (year in seq(strftime(min(Results_all$Date), format = "%Y"), strftime(max(Results_all$Date), format = "%Y"))) {
  
  for (month in month.abb) {
    
    Raster_data_filter=filter(Raster_data_month,
                              Month==month,
                              Year==year) %>% 
      dplyr::select(-Month, -Year) %>% 
      melt(id.vars=c("date", "y")) %>% 
      rename("x"="variable") %>% 
      mutate(x=as.numeric(as.character(x))) %>% 
      group_by(x, y) %>% 
      summarise(value=sum(value, na.rm = TRUE)) %>% 
      mutate(value=ifelse(value==0, NA, value))
    
    ggplot() +
      geom_raster(data = Raster_data_filter , aes(x = x, y = y, fill=value)) +
      scale_fill_gradient(low="yellow", high="blue", na.value="white", limits=c(0, 120000)) +
      coord_fixed(ratio=1, xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
      geom_path(data=worldmap, aes(x = long, y = lat, group = group), colour = "grey") +
      labs(x="Longitude (°)", y="Latitude (°)", title=paste0("OSN data coverage in ", month, " ", year)) +
      theme_bw() + 
      theme(plot.title = element_text(size=40, face="bold", hjust=0.5),
            axis.text=element_text(size=38),
            axis.title=element_text(size=38),
            legend.title = element_blank())
    ggsave(paste0(dir_PRU, "Figures/OSN coverage/OSN_data_coverage_", month, "_", year, ".jpeg"),
           width = 50, height = 30, units = "cm", dpi=200)
    
  }
  
}
