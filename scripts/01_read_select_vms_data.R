# Prepare the VMS data for opening in QGIS, etc.

library(tidyverse)
library(lubridate)
library(dbscan) #https://www.jstatsoft.org/article/view/v091i01
#https://www.rdocumentation.org/packages/dbscan/versions/1.1-8
library(sf)

data_raw_yearly <- read_csv("C:/Users/Phillip Haupt/Documents/VMS/data/2019/vms_tecfo_2019.csv")
selected_yearly_data <- data_raw_yearly %>% 
  transmute(vessel_name = Vessel,
         active_beacon = `Active beacon`,
         location_date_time = lubridate::dmy_hms(`Location date time`),#as.Date(`Location date time` ,'dd/MM/yyyy hms'), #format_date(to_date("location_date", 'dd/MM/yyyy')  +to_interval('1 Days'),'dd/MM/yyyy')
         location_date = dmy(`Location date`),#'dd/MM/yyyy'
         location_time = `Location time`,
         X = as.numeric(gsub("[°]", "", Longitude)),
         Y = as.numeric(gsub("[°]", "", Latitude)),
         heading = as.numeric(gsub("[°]", "", Heading)),
         heading_avg = as.numeric(gsub("[°]", "", `Avg. Heading`)),
         speed = as.numeric(gsub("kt", "", Speed)),
         speed_avg = as.numeric(gsub("kt", "", `Avg. Speed`)),
         distance = as.numeric(gsub("nmi", "", `Distance`))
) %>% filter(speed > 1.9 & speed < 6.1)

# cluster analysis
clusters <- dbscan::dbscan(x = as.matrix(selected_yearly_data[6:7]), eps = 0.01, minPts = 200)
vms_clustered_dat <- cbind(selected_yearly_data, clusters$cluster) %>% 
  rename(cluster = `clusters$cluster`)



# make sf
vms_clustered_wgs84 <- st_as_sf(vms_clustered_dat, coords = c("X", "Y"), crs = 4326, agr = "constant")

# plot
vms_clustered_wgs84 %>% 
  filter(cluster != 0) %>% 
  ggplot2::ggplot()+
  geom_sf(aes(col = cluster))

sf::st_write(vms_clustered_wgs84, "vms_2019_clustered_wgs84.gpkg", layer = "vms_2019")
getwd()
