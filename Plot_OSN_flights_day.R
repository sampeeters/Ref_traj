
library("ROracle")
library("dplyr")
library("ggplot2")
library("scales")

Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")
dir_PRU="//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/PRU-ENV/"

Airport="EGLL"
Phase="ARR" # ARR DEP
source("Read_OSN_data_from_csv.R")

# Data point counts by EVENT_TIME

Counts=group_by(Traj_data, EVENT_TIME) %>% 
  summarise(Count=n())

ggplot() +
  geom_point(data = Counts, aes(x=EVENT_TIME, y=Count)) +
  labs(x="Time", y="Count", title=paste0("Number of data points")) +
  theme_bw() + 
  theme(plot.title = element_text(size=40, face="bold", hjust=0.5),
        axis.text=element_text(size=36),
        axis.title=element_text(size=38))
ggsave(paste0(dir_PRU, "Figures/Time_count_",
              format(as.POSIXct(strptime(min(Traj_data$Date),"%Y-%m-%d", tz="UTC")), "%d%m%Y"), "_",
              format(as.POSIXct(strptime(max(Traj_data$Date),"%Y-%m-%d", tz="UTC")), "%d%m%Y"), ".jpeg"),
       width = 50, height = 30, units = "cm", dpi=200)
