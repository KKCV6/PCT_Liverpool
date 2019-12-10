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

library(cyclestreets)
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

remotes::install_cran(pkgs)
# remotes::install_github("ITSLeeds/pct")

source("https://raw.githubusercontent.com/ITSLeeds/TDS/master/code-r/setup.R")

#cycle streets API Key

API_key = "64de24d7c380c043"

Sys.getenv("API_key")
Sys.setenv(CYCLESTREETS_KEY = "64de24d7c380c043")
Sys.getenv("CYCLESTREETS_KEY")

#blogdown
install.packages("blogdown")
library(blogdown)
blogdown::install_hugo()
#https://bookdown.org/yihui/blogdown/a-quick-example.html

blogdown::new_site("C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/GitHub/Blogdown2")

blogdown

#install Liverpool data and geometry

region_name = "liverpool-city-region"
max_distance = 7
lines_test = get_pct_lines(region_name)
zones_all = get_pct_zones(region_name)
lines_all = get_pct_lines(region_name)
# basic plot
plot(zones_all$geometry)
plot(lines_all$geometry[lines_all$all > 500], col = "red", add = TRUE)

#1 create cycling and driver desire lines (less than 5 km)
tmap_mode("view")

  #1abicycle desire lines # interactive map

lines_all_lsoa = get_pct_lines(region_name, geography = "lsoa")

lines_liverpool = filter(lines_all_lsoa, lad_name1 %in% c("Liverpool"))

bicycle = lines_liverpool %>%
  mutate('Percentage Cycling' = (bicycle) / all * 100) %>%
  filter(e_dist_km < max_distance)
tm_shape(bicycle) +
  tm_lines("Percentage Cycling", palette = "RdYlBu", lwd = "bicycle", scale = 9)

  #1b Create car dependent desire lines
car_dependent = lines_liverpool %>% 
  mutate(`Percent Drive` = (car_driver) / all * 100) %>% 
  filter(e_dist_km < max_distance)
tm_shape(car_dependent) +
  tm_lines("Percent Drive", palette = "-RdYlBu", lwd = "all", scale = 9)+
  tm_layout(legend.text.size = 1.2)

#extract Liverpool pct zones

Liverpool = get_pct_zones(region = region_name)
L = Liverpool %>%
  select(geo_code, geo_name, all, bicycle, car_driver, foot)
plot(L)

L2 = Liverpool

#plot highest cycling ptc zone
plot (L$geometry)
plot(L[62,"bicycle"], add = TRUE, col = "purple")

# Aim: get top 5 cycle routes
library(tidyverse)

L_original_msoa = get_pct_lines("liverpool-city-region")
L_msoa = L_original_msoa %>% 
  select(geo_code1, geo_code2, all, bicycle, car_driver, rf_avslope_perc, rf_dist_km)
over46 <- L_msoa %>% filter(bicycle > 46)
plot(L$geometry)
plot(MSOA_5["bicycle"], add = TRUE, col = "red")
qtm(MSOA_5)

#get pct lines

L_original_lsoa = get_pct_lines("liverpool-city-region", geography = "lsoa")
L_lsoa = L_original_lsoa %>% 
  select(geo_code1, geo_code2, all, bicycle, car_driver, rf_avslope_perc, rf_dist_km, govtarget_slc, govnearmkt_slc, gendereq_slc, dutch_slc, ebike_slc)
LSOA_300 = L_lsoa %>% 
  top_n(n = 300, wt = bicycle)
plot(L$geometry)
plot(LSOA_5["bicycle"], add = TRUE, col = "red")

#extract routes for Liverpool census area only

Liverpool_filter = filter(L_original_lsoa, lad_name1 %in% c("Liverpool"))

#subset of cycle routes, greater than 30 people
plot(L$geometry)
plot(subset(L_msoa["pcycle_godutch"], pcycle_godutch >= 30), add = TRUE)

pct_uptake_godutch("liverpool-city-region")

#propensity to cycle (Go Dutch, Govt. and Gender Equality)
#scenarios of change
library(tidyverse)
L_msoa$pcycle = L_msoa$bicycle / L_msoa$all * 100
plot(L$geometry)
plot(L_msoa["pcycle"], add = TRUE, palette = "Purples", breaks = c(2, 4, 6, 10), vwlwd = L_msoa$bicycle)


Liverpoolroutenetwork = get_pct_rnet("liverpool-city-region")
plot(Liverpoolroutenetwork["bicycle"])
plot(Liverpoolroutenetwork["dutch_slc"])
plot(Liverpoolroutenetwork["gendereq_slc"])
plot(Liverpoolroutenetwork["ebike_slc"])

L_msoa$euclidean_distance = as.numeric(sf::st_length(L_msoa))
L_msoa$pcycle_godutch = uptake_pct_godutch(
  distance = L_msoa$rf_dist_km,
  gradient = L_msoa$rf_avslope_perc
) * 100 + L_msoa$pcycle


#top 10 routes cycled in Liverpool
Top10_Liverpool = Liverpool_filter %>% top_n(10, bicycle)
Top10_gov = Liverpool_filter %>% top_n(10, govtarget_slc)
Top10_dutch = Liverpool_filter %>% top_n(10, dutch_slc)

Top10_Liverpool_data = Top10_Liverpool %>% select(id, geo_code1, geo_code2, geo_name1, geo_name2, lad11cd1, lad11cd2, lad_name1, lad_name2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other, govtarget_slc, dutch_slc)
Top10_gov_data = Top10_gov %>% select(id, geo_code1, geo_code2, geo_name1, geo_name2, lad11cd1, lad11cd2, lad_name1, lad_name2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other, govtarget_slc, dutch_slc)
Top10_dutch_data = Top10_dutch %>% select(id, geo_code1, geo_code2, geo_name1, geo_name2, lad11cd1, lad11cd2, lad_name1, lad_name2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other, govtarget_slc, dutch_slc)


Top10_Liverpool_route = stplanr::line2route(Top10_Liverpool_data, route_fun = stplanr::route_cyclestreet)
Top10_Liverpool_route$govtarget = Top10_Liverpool_data$govtarget_slc
Top10_Liverpool_route$dutch = Top10_Liverpool_data$dutch_slc
Top10_Liverpool_route$bicycle <- Top10_Liverpool_data$bicycle

Top10_gov_route = stplanr::line2route(Top10_gov_data, route_fun = stplanr::route_cyclestreet)
Top10_gov_route$govtarget = Top10_gov_data$govtarget_slc
Top10_gov_route$dutch = Top10_gov_data$dutch_slc
Top10_gov_route$bicycle <- Top10_gov_data$bicycle

tmap_mode("view")
qtm(Top10_gov_route)

Top10_dutch_route = stplanr::line2route(Top10_dutch_data, route_fun = stplanr::route_cyclestreet)
Top10_dutch_route$govtarget = Top10_dutch_data$govtarget_slc
Top10_dutch_route$dutch = Top10_dutch_data$dutch_slc
Top10_dutch_route$bicycle <- Top10_dutch_data$bicycle

qtm(Top10_dutch_route)


#bounding box

qtm(Top10_Liverpool_route)
library(spatstat)
liverpool_bb <- bbox(Liverpool_lsoa_shp)
  
lb <- get_map(liverpool_bb,
             maptype = "roadmap",
             source = "google")
plot(b)

b_bw <- get_stamenmap(Liverpoolboundingbox,
                      zoom =10,
                      source = "OSM",
                      maptype = "toner-lite")



tmap_mode("view")
tm_basemap("Stamen.TonerLite", alpha = 0.5) +
  tm_shape(Top10_Liverpool_route)+
    tm_lines(Top10_Liverpool_route$bicycle, lwd = 2)

library(ggmap)
library(BAMMtools)
b <- get_map(Liverpoolboundingbox,
             maptype = "roadmap",
             source = "google")
plot(b)

b_bw <- get_stamenmap(Liverpoolboundingbox,
                      zoom =10,
                      source = "OSM",
                      maptype = "toner-lite")

plot(b_bw)


#estimate cycling uptake (Liverpool)

sum(Liverpool_filter$govtarget_slc) - sum(Liverpool_filter$bicycle)
#additional 4762 cyclists 
 
sum(Liverpool_filter$dutch_slc) - sum(Liverpool_filter$bicycle)
#additional 27004 cyclists

#prioritise infras
library(Rnets)
library(pbapply)
rnet_gov = stplanr::overline2(Top10_Liverpool_data, attrib = c("bicycle", "govtarget_slc"))
lwd = rnet_gov$govtarget_slc / mean(rnet_gov$govtarget_slc)

lwd_govtarget = Top5_existing_route$govtarget_slc/ mean(Top5_existing_route$govtarget_slc)
plot (L$geometry)
plot(rnet_gov["govtarget_slc"], lwd = lwd, add = TRUE)

lwd_dutch = Top5_existing_route$dutch_slc/ mean(Top5_existing_route$dutch_slc)

lwd_all = top300$govtarget_slc / mean(top300$govtarget_slc)

lwd_liverpool_govtarget = Liverpool_filter$govtarget_slc / mean(Liverpool_filter$govtarget_slc)
lwd_liverpool_dutch = Liverpool_filter$dutch_slc / mean(Liverpool_filter$dutch_slc)

mapview::mapview(Top5_existing_route, zcol = "govtarget_slc", lwd_govtarget = lwd_govtarget * 2, layer.name = "Infrastructure Priority (GovTarget)")
mapview::mapview(Top5_existing_route, zcol = "dutch_slc", lwd_dutch = lwd_dutch * 2, layer.name = "Infrastructure Priority (Go Dutch)")

mapview::mapview(top300, zcol = "all", layer.name = "Infrastructure Priority (GovTarget)") 

mapview::mapview(Liverpool_filter, zcol = "govtarget_slc", lwd = lwd_liverpool_govtarget * 2, layer.name = "Infrastructure Priority (GovTarget)") 
mapview::mapview(Liverpool_filter, zcol = "dutch_slc", lwd = lwd_liverpool_dutch * 2, layer.name = "Infrastructure Priority (Dutch)") 


#stintersect # sf #health statistics
#export as a csv and bring it back in
st_intersection()


#difference between gradient and distance

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


#calculate MET equivalent per week

MET = 7.5
trips_per_week = 6 # National Travel Survey
speed = 15 #km/h
Top10_Liverpool_route$'length km' <- Top10_Liverpool_route$length / 1000
Top10_gov_route$'length km' <- Top10_gov_route$length / 1000
Top10_dutch_route$'length km' <- Top10_dutch_route$length / 1000

#govtarget
Top10_Liverpool_route$'additional cyclists govtarget' <- Top10_Liverpool_route$govtarget - Top10_Liverpool_data$bicycle
Top10_Liverpool_route$'increase in physical expenditure (govtarget)' <- (Top10_Liverpool_route$`additional cyclists govtarget` * trips_per_week *(Top10_Liverpool_route$`length km`/ speed))

Top10_gov_route$'additional cyclists govtarget' <- Top10_gov_route$govtarget - Top10_gov_route$bicycle
Top10_gov_route$'increase in physical expenditure (govtarget)' <- (Top10_gov_route$`additional cyclists govtarget` * trips_per_week *(Top10_gov_route$`length km`/ speed))

Top10_dutch_route$'additional cyclists govtarget' <- Top10_dutch_route$govtarget - Top10_dutch_route$bicycle
Top10_dutch_route$'increase in physical expenditure (govtarget)' <- (Top10_dutch_route$`additional cyclists govtarget` * trips_per_week *(Top10_dutch_route$`length km`/ speed))

#dutch

Top10_Liverpool_route$'additional cyclists dutch' <- Top10_Liverpool_route$dutch - Top10_Liverpool_data$bicycle
Top10_Liverpool_route$'increase in physical expenditure (dutch)' <- (Top10_Liverpool_route$`additional cyclists dutch` * trips_per_week *(Top10_Liverpool_route$`length km`/ speed))

Top10_gov_route$'additional cyclists dutch' <- Top10_gov_route$dutch - Top10_gov_route$bicycle
Top10_gov_route$'increase in physical expenditure (dutch)' <- (Top10_gov_route$`additional cyclists dutch` * trips_per_week *(Top10_gov_route$`length km`/ speed))

Top10_dutch_route$'additional cyclists dutch' <- Top10_dutch_route$dutch - Top10_dutch_route$bicycle
Top10_dutch_route$'increase in physical expenditure (dutch)' <- (Top10_dutch_route$`additional cyclists dutch` * trips_per_week *(Top10_dutch_route$`length km`/ speed))

#plot the top 10 routes

str(Top10_Liverpool_route)


str(lines)

plot(Top10_Liverpool_route["bicycle"])

basemap <- get_map(location=c(lon = 2, lat = 50), zoom=11, maptype = 'roadmap', source = 'osm')

liverpoolwgs <- st_transform(Liverpool_filter_shp, 4326)
Top10_Liverpool_route_wgs <- st_transform(Top10_Liverpool_route, 4326)

liverpoolbox <- as.vector(st_bbox(liverpoolwgs))

liverpool_bm <- get_map(liverpoolbox,
                           maptype = "roadmap",
                           source = "osm",
                           zoom = 12)
plot(liverpool_bm)

liverpool_bm_stamen <- get_stamenmap(liverpoolbox,
                                     zoom = 12,
                                     source = "osm",
                                     maptype = "toner-lite")
#basemap
plot(liverpool_bm_stamen)

plot(liverpool_bm)

palette1 <-scale_fill_distiller(type = "seq",
                                palette = "YlGnBu", direction = -1)

labels <- labs(title = "Top 10 Routes Cycled in Liverpool (2011 Census)",
               x = "Longitude",
               y = "Latitude")

g <- ggplot()+
  geom_sf(mapping = aes(geometry = geometry,
                        color = bicycle),
          data = Top10_Liverpool_route_wgs)+
  theme_minimal()+
  labels

write


ggmap(liverpool_bm_stamen) +
  labs(x = "Longitude", y = "Latitude") + geom_sf(data = Top10_Liverpool_route_wgs)

mymap <- ggmap::ggmap(liverpool_bm)+
  geom_sf(data = Top10_Liverpool_route_wgs,
          aes(geomtry = geometry),
          color = bicycle)+
  theme_minimal()+
  labels

library(GISTools)
library(rgdal)
Top10_bng <- st_transform(route_merge, 27700)
Top10_gov_bng <- st_transform(gov_merge, 27700)
Top10_dutch_bng <- st_transform(dutch_merge, 27700)

st_write(Top10_bng,
         "C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/Shp/Top10_bng.shp", driver = "ESRI Shapefile")
st_write(Top10_gov_bng,
         "C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/Shp/Top10_gov_bng.shp", driver = "ESRI Shapefile")
st_write(Top10_dutch_route,
         "C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/Top10_dutch_route.shp", driver = "ESRI Shapefile")

write.csv()

class(Top10_Liverpool_route_wgs)



palette_explorer()


pool_bm
print(Top10_Liverpool_route_wgs)
print(liverpool_bm)

library(tmap)
tmap_mode("plot")
tm_basemap(server = "Esri.WorldGrayCanvas", group = Top10_Liverpool_route) +
  tm_shape(Top10_Liverpool_route) + 
    tm_lines(style = "cat", col = "bicycle", palette = "Set1") +
  tm_scale_bar(breaks = 0, position = "left")

  palette_explorer()


#health indicators

Liverpool_lsoa_shp <- st_read("C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/Shp/Indices_of_Multiple_Deprivation_IMD_2019/Indices_of_Multiple_Deprivation_IMD_2019.shp")

liverpool <- st_read("C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/Shp/Liverpool.shp")

Liverpool_filter_shp = filter(Liverpool_lsoa_shp, LADnm %in% c("Liverpool"))

qtm(Liverpool_filter_shp$HDDDec)

#merge IMD with LSOA

#plot health indicators
library(tmap)
library(tmaptools)
tmap_mode("view")

qtm(L_lsoa)

qtm(Liverpool_filter_shp,
    fill = "HDDDec") +
  qtm(Top10_Liverpool_route)

qtm(Top10_Liverpool_route)

library(geosphere)
HealthMerge$area <- areaPolygon(HealthMerge)
HealthMerge$areasqkm <- HealthMerge$area / 1000000

IMD_gov_intersect <- st_join(Top10_gov_route, Liverpool_filter_shp, join = st_intersects)
IMD_dutch_intersect <- st_join(Top10_dutch_route, Liverpool_filter_shp, join = st_intersects)

write.csv(IMD_route_intersect,"C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment//IMD_route_average.csv", row.names = FALSE)
write.csv(IMD_gov_intersect,"C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment//IMD_gov_average.csv", row.names = FALSE)
write.csv(IMD_dutch_intersect,"C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment//IMD_dutch_average.csv", row.names = FALSE)

write.csv(Liverpool_filter, "C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment//liverpool_filter.csv", row.names = FALSE)

IMD_route_average <- read.csv("C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/IMD_average.csv")
IMD_gov_average <- read.csv("C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/IMD_gov_average2.csv")
IMD_dutch_average <- read.csv("C:/Users/Holly.Mizser-Jones/OneDrive - Arup/UCL/CASA005/Assessment/IMD_dutch_average_2.csv")

route_merge <-merge (Top10_Liverpool_route, 
                     IMD_route_average, 
                     by.x="length", 
                     by.y="length",
                     no.dups = TRUE)

gov_merge <-merge (Top10_gov_route, 
                     IMD_gov_average, 
                     by.x="length", 
                     by.y="length",
                     no.dups = TRUE)

dutch_merge <-merge (Top10_dutch_route, 
                     IMD_dutch_average, 
                     by.x="length", 
                     by.y="length",
                     no.dups = TRUE)

dutch_merge$govtarget.y <- NULL
dutch_merge$dutch.y <- NULL

qtm(route_merge["increase in physical expenditure (dutch)"])

plot(route_merge["increase in physical expenditure (dutch)"])

plot(route_merge["IMDDec"])

write.csv(route_merge,"C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment//route_merge.csv", row.names = FALSE)

str(Liverpool)

str(Liverpool_lsoa_shp)

str(Top10_Liverpool_route)


str(Top10_Liverpool_route)
str(Liverpool_filter_shp)


#map the IMD Data below the Top 10 Routes

Liverpool_filter_shp$'IMD_Decile' <- as.numeric(as.character(Liverpool_filter_shp$'IMD_Decile')) # convert IMD_Decile to numeric
Liverpool_filter_shp$'HDDDec' <- as.numeric(as.character(Liverpool_filter_shp$'HDDDec')) # convert HDDDec to numeric
sapply(Liverpool_filter_shp, class) #check the conversion has worked

#interactive

str(Top10_Liverpool_route)

tmap_mode("plot")
tm_basemap("Stamen.TonerLite", alpha = 0.5) +
  tm_shape(Liverpool_filter_shp) +
   tm_polygons("IMD_Decile", palette = "-YlGnBu", alpha = 0.8, title = "IMD Decile", n = 10, style = "cat")+
  tm_shape(Top10_Liverpool_route)+
    tm_lines(style = "cont", col = "white", lwd = 2)

tmap_mode("view")
tm_basemap("Stamen.TonerLite", alpha = 0.5) +
  tm_shape(Liverpool_filter_shp) +
  tm_polygons("HDDDec", palette = "-YlGnBu", alpha = 0.8, title = "HDD Decile", n = 10, style = "cat")+
  tm_shape(Top10_Liverpool_route)+
  tm_lines(style = "cont", col = "white", lwd = 2)

#top 10 routes leaflet


Top10

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
            title ="IMD",
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
            title ="HDD Rank")%>%
 
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

tmap_mode("view")
qtm(route_merge)


