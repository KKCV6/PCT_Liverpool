install.packages("remotes")
pkgs = c(
  "cyclestreets",
  "mapview",
  "pct",
  "sf",
  "stats19",
  "stplanr",
  "tidyverse",
  "devtools"
)

library(mapview)
library(pct)
library(sf)
library(stats19)
library(stplanr)
library(tidyverse)
library(devtools)
library(tmap)
library(tmaptools)
library(dplyr)
library(leaflet)
library(classInt)
library(ggplot2)
library(cyclestreets)

remotes::install_cran(pkgs)
# remotes::install_github("ITSLeeds/pct")

#install Liverpool data and geometry from PCT

region_name = "liverpool-city-region"
max_distance = 7
lines_test = get_pct_lines(region_name)
zones_all = get_pct_zones(region_name)
L_original_lsoa = get_pct_lines("liverpool-city-region", geography = "lsoa")

Liverpool_filter = filter(L_original_lsoa, lad_name1 %in% c("Liverpool")) # filter out Liverpool only

tmap_mode("view")

#Bicycle Dependent Desire Lines
bicycle = Liverpool_filter %>%
  mutate('Percentage Cycling' = (bicycle) / all * 100) %>%
  filter(e_dist_km < max_distance)
tm_shape(bicycle) +
  tm_lines("Percentage Cycling", palette = "RdYlBu", lwd = "bicycle", scale = 9)

#Car Dependent Desire Lines
car_dependent = Liverpool_filter %>% 
  mutate(`Percent Drive` = (car_driver) / all * 100) %>% 
  filter(e_dist_km < max_distance)
tm_shape(car_dependent) +
  tm_lines("Percent Drive", palette = "-RdYlBu", lwd = "all", scale = 9)+
  tm_layout(legend.text.size = 1.2)

#note: these desire lines can be created for any travel mode, adapt the code above to show desired mode

#top 10 OD desire lines cycled in Liverpool (existing, Government Target and Go Dutch)
Top10_Liverpool = Liverpool_filter %>% top_n(10, bicycle)

Top10_Liverpool_data = Top10_Liverpool %>% select(id, geo_code1, geo_code2, geo_name1, geo_name2, lad11cd1, lad11cd2, lad_name1, lad_name2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other, govtarget_slc, dutch_slc)

#turn the OD desire lines into routes

Top10_Liverpool_route = stplanr::line2route(Top10_Liverpool_data, route_fun = stplanr::route_cyclestreet)
Top10_Liverpool_route$govtarget = Top10_Liverpool_data$govtarget_slc
Top10_Liverpool_route$dutch = Top10_Liverpool_data$dutch_slc
Top10_Liverpool_route$bicycle <- Top10_Liverpool_data$bicycle

#estimate cycling uptake (Liverpool)

sum(Liverpool_filter$govtarget_slc) - sum(Liverpool_filter$bicycle)
#additional 4762 cyclists in the Government Target Scenario (overall)
 
sum(Liverpool_filter$dutch_slc) - sum(Liverpool_filter$bicycle)
#additional 27004 cyclistsin the Go Dutch scenario (overall)

#map showing the difference between gradient and distance in the Government Target and Go Dutch Scenarios

distances = 1:20
hilliness = 0:5
uptake_df = data.frame(
  distances = rep(distances, 6),
  hilliness = rep(hilliness, each = 20)
)
p_govtarget = uptake_pct_govtarget(
  distance = uptake_df$distances,
  gradient = uptake_df$hilliness
)
p_godutch = uptake_pct_godutch(
  distance = uptake_df$distances,
  gradient = uptake_df$hilliness
)
uptake_df = rbind(
  cbind(uptake_df, scenario = "govtarget", pcycle = p_govtarget),
  cbind(uptake_df, scenario = "godutch", pcycle = p_godutch)
)
library(ggplot2)
ggplot(uptake_df) +
  geom_line(aes(
    distances,
    pcycle,
    linetype = scenario,
    colour = as.character(hilliness)
  )) +
  scale_color_discrete("Gradient (%)")+
  labs(x = "Distance (km)", y = "Propensity to Cycle (Multiplier)")

# Health Data
#read in LSOA shapefile for UK with IMD 2019 data joined
Liverpool_lsoa_shp <- st_read("C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Shp/Indices_of_Multiple_Deprivation_IMD_2019/Indices_of_Multiple_Deprivation_IMD_2019.shp")

#filter for Liverpool
Liverpool_filter_shp = filter(Liverpool_lsoa_shp, LADnm %in% c("Liverpool"))

#visualise the routes on top of the IMD polygon data
library(leaflet)

pal <- colorFactor(get_brewer_pal("-YlGnBu", n = 10), domain = Liverpool_filter_shp$IMD_Decile, n = 10)
pal2 <- colorFactor(get_brewer_pal("-YlGnBu", n = 10), domain = Liverpool_filter_shp$HDDDec, n = 10)

IMDandroutes<- leaflet() %>%
  # add basemap options
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri Grey") %>%
  addTiles(group = "OSM") %>%
  
  #add our Borough polygons, linking to the tables we just made
  addPolygons(data=Liverpool_filter_shp,
              color="white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.6,
              fillColor = ~pal(Liverpool_filter_shp$IMD_Decile),
              group = "Index of Multiple Deprivation")%>%
  
  # add a legend for boroughs
  addLegend(pal = pal, 
            values = Liverpool_filter_shp$IMD_Decile,
            group=c("Index of Multiple Deprivation"), 
            title ="IMD Decile",
            position ="bottomleft")%>%
  
  #add our ward polygons, linking to the tables we just made
  addPolygons(data=Liverpool_filter_shp,
              color="white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.6,
              fillColor = ~pal2(Liverpool_filter_shp$HDDDec),
              group = "Health Deprivation and Disability Decile")%>%
  
  # add a legend for wards
  addLegend(pal = pal2, 
            values = Liverpool_filter_shp$HDDDec,
            group=c("Health Deprivation and Disability Decile"), 
            position ="bottomleft",
            title ="HDD Decile")%>%
  
  #add routes
  addPolylines(data=Top10_Liverpool_route, color = "black", 
               weight = 2.5, opacity = 1, group = c("Top 10 Cycle Routes in Liverpool")) %>%
  
  # specify layers control
  addLayersControl(
    baseGroups = c("Esri Grey Canvas", "OSM"),
    overlayGroups = c("Index of Multiple Deprivation", "Health Deprivation and Disability Decile","Top 10 Cycle Routes in Liverpool"),
    options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(c("Index of Multiple Deprivation", "Health, Deprivation and Disability Decile", "Top 10 Cycle Routes in Liverpool"))

# show us the map
IMDandroutes

#intersect the Top Routes with the IMD polygon data
IMD_route_intersect <- st_join(Top10_Liverpool_route, Liverpool_filter_shp, join = st_intersects)

qtm(Top10_Liverpool_route)

#export this to csv and use Microsoft Excel to calculate the average for each route
write.csv(IMD_route_intersect,"C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Data//IMD_route_average.csv", row.names = FALSE)

#read the IMD average back into R
IMD_route_average <- read.csv("C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Data/IMD_average.csv")

#merge with the route data 
route_merge <-merge (Top10_Liverpool_route, 
                     IMD_route_average, 
                     by.x="length", 
                     by.y="length",
                     no.dups = TRUE)

st_write(route_merge,
         "C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Jan2020/route_merge.shp", driver = "ESRI Shapefile")

#the change in mMETS was calculated in excel, the below code joins the resultant increases to the routes

#link MET data to Top_10_routes

mMET <- read.csv("C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/liverpool_mMET.csv")

#merge mMET with Routes
route_mMET <-merge (route_merge, 
                    mMET, 
                    by.x="route", 
                    by.y="Route",
                    no.dups = TRUE)

st_write(route_mMET,
         "C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Jan2020/mMET_merge.shp", driver = "ESRI Shapefile")

#Sensitivity Test

#top 10 OD desire lines cycled in Liverpool (Government Target and Go Dutch)
Top10_gov = Liverpool_filter %>% top_n(10, govtarget_slc)
Top10_dutch = Liverpool_filter %>% top_n(10, dutch_slc)

Top10_gov_data = Top10_gov %>% select(id, geo_code1, geo_code2, geo_name1, geo_name2, lad11cd1, lad11cd2, lad_name1, lad_name2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other, govtarget_slc, dutch_slc)
Top10_dutch_data = Top10_dutch %>% select(id, geo_code1, geo_code2, geo_name1, geo_name2, lad11cd1, lad11cd2, lad_name1, lad_name2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other, govtarget_slc, dutch_slc)

#turn the OD desire lines into routes

Top10_gov_route = stplanr::line2route(Top10_gov_data, route_fun = stplanr::route_cyclestreet)
Top10_gov_route$govtarget = Top10_gov_data$govtarget_slc
Top10_gov_route$dutch = Top10_gov_data$dutch_slc
Top10_gov_route$bicycle <- Top10_gov_data$bicycle


Top10_dutch_route = stplanr::line2route(Top10_dutch_data, route_fun = stplanr::route_cyclestreet)
Top10_dutch_route$govtarget = Top10_dutch_data$govtarget_slc
Top10_dutch_route$dutch = Top10_dutch_data$dutch_slc
Top10_dutch_route$bicycle <- Top10_dutch_data$bicycle

#export as shapefiles
st_write(Top10_Liverpool_route,
         "C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Jan2020/Top10_liverpool_route.shp", driver = "ESRI Shapefile")
st_write(Top10_gov_route,
         "C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Shp/Top10_gov_bng.shp", driver = "ESRI Shapefile")
st_write(Top10_dutch_route,
         "C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/Top10_dutch_route.shp", driver = "ESRI Shapefile")

