# only points in area 8 fished area
library(tidyverse)
# tecfo <- sf::st_read("C:/Users/Phillip Haupt/Documents/GIS/TECFO/tecfo_areas_utm31n.gpkg", layer = "tecfo_areas_topo_corr_utm31n")
# tecfo_wgs84 <- sf::st_transform(tecfo, 4326)

area_8_stock <- sf::st_read(dsn = "C:/Users/Phillip Haupt/Documents/GIS/TEMP/COCKLE_SURVEY_AREAS/area_14_cockle_stock.gpkg",
layer = "area_8_stock")

#points in stock area
vms_in_area_wgs84 <- sf::st_intersection(vms_clustered_wgs84, area_8_stock)

# summarise points by vessel and day?
vms_in_area <- vms_in_area_wgs84 %>% 
  st_drop_geometry() %>% 
  select(vessel_name, active_beacon, location_date) %>% 
  group_by(vessel_name) %>% 
  distinct(location_date) %>% 
  summarise(trips = n())

number_or_days_in_areas <- vms_in_area %>% summarise(sum(trips))


