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
blogdown::install_hugo()
#https://bookdown.org/yihui/blogdown/a-quick-example.html


#install Liverpool data and geometry

region_name = "liverpool-city-region"
max_distance = 7
zones_all = get_pct_zones(region_name)
lines_all = get_pct_lines(region_name)
# basic plot
plot(zones_all$geometry)
plot(lines_all$geometry[lines_all$all > 500], col = "red", add = TRUE)

#1 create cycling and driver desire lines (less than 5 km)
tmap_mode("view")

  #1abicycle desire lines # interactive map

bicycle = lines_all %>%
  mutate('Percentage Cycling' = (bicycle) / all * 100) %>%
  filter(e_dist_km < max_distance)
tm_shape(bicycle) +
  tm_lines("Percentage Cycling", palette = "RdYlBu", lwd = "bicycle", scale = 9)

  #1b Create car dependent desire lines
car_dependent = lines_all %>% 
  mutate(`Percent Drive` = (car_driver) / all * 100) %>% 
  filter(e_dist_km < max_distance)
tm_shape(car_dependent) +
  tm_lines("Percent Drive", palette = "-RdYlBu", lwd = "all", scale = 9)

#extract Liverpool pct zones

Liverpool = get_pct_zones(region = region_name)
L = Liverpool %>%
  select(geo_code, geo_name, all, bicycle, car_driver, foot)
plot(L)

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

st_length(MSOA_5[4,4])

L_original_lsoa = get_pct_lines("liverpool-city-region", geography = "lsoa")
L_lsoa = L_original_lsoa %>% 
  select(geo_code1, geo_code2, all, bicycle, car_driver, rf_avslope_perc, rf_dist_km, govtarget_slc, govnearmkt_slc, gendereq_slc, dutch_slc, ebike_slc)
LSOA_300 = L_lsoa %>% 
  top_n(n = 300, wt = bicycle)
plot(L$geometry)
plot(LSOA_5["bicycle"], add = TRUE, col = "red")

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

#top 5 routes cycled and routes via CycleStreet - LSOA

Top5_existing = L_original_lsoa %>% top_n(5, bicycle)
Top5_existing_route = stplanr::line2route(Top5_existing, route_fun = stplanr::route_cyclestreet)

Top5_godutch = L_original_lsoa %>% top_n(5, dutch_slc)
Top5_godutch_route = stplanr::line2route(Top5_godutch, route_fun = stplanr::route_cyclestreet)

Top5_ebikes = L_original_lsoa %>% top_n(5, ebike_slc)
Top5_ebikes_route = stplanr::line2route(Top5_ebikes, route_fun = stplanr::route_cyclestreet)

Top5_gendereq = L_original_lsoa %>% top_n(5, gendereq_slc)
Top5_gendereq_route = stplanr::line2route(Top5_gendereq, route_fun = stplanr::route_cyclestreet)

Top5_govtarget = L_original_lsoa %>% top_n(5, govtarget_slc)
Top5_govtarget_route = stplanr::line2route(Top5_govtarget, route_fun = stplanr::route_cyclestreet)

Top5_govnearmkt = L_original_lsoa %>% top_n(5, govnearmkt_slc)
Top5_govnearmkt_route = stplanr::line2route(Top5_govnearmkt, route_fun = stplanr::route_cyclestreet)

tmap_mode("view")

qtm(Top5_existing_route)


#estimate cycling uptake govtarget (top 5)

Top5_existing_route$uptakegovtarget = uptake_pct_govtarget(distance = Top5_existing_route$length, gradient = Top5_existing_route$av_incline)
Top5_existing_route$bicycle_govtarget = Top5_existing$bicycle +
  round(Top5_existing_route$uptakegovtarget * Top5_existing$all)

sum(Top5_existing_route$bicycle_govtarget) - sum(Top5_existing$bicycle)

sum(Top5_existing_route$bicycle_govtarget) / sum(Top5_existing$all)
sum(Top5_existing$bicycle) / sum(Top5_existing$all)
#18.9% to 26.6% (GovTarget Existing) additional 21 people cycling

#estimate cycling uptake go dutch (top 5)

Top5_existing_route$uptakegodutch = uptake_pct_godutch(distance = Top5_existing_route$length, gradient = Top5_existing_route$av_incline)
Top5_existing_route$bicycle_godutch = Top5_existing$bicycle +
  round(Top5_existing_route$uptakegodutch * Top5_existing$all)

sum(Top5_existing_route$bicycle_godutch) - sum(Top5_existing$bicycle)

sum(Top5_existing_route$bicycle_godutch) / sum(Top5_existing$all)
sum(Top5_existing$bicycle) / sum(Top5_existing$all)
#18.9% to 62% (GovTarget Existing) additional 120 people cycling



#prioritise infras
library(Rnets)
library(pbapply)
rnet_5 = stplanr::overline2(tpo_5, attrib = c("bicycle", "govtarget_slc"))
lwd = rnet$govtarget_slc / mean(rnet$govtarget_slc)

rnet_100 = stplanr::overline2(top_desire_line_100, attrib = c("bicycle", "govtarget_slc"))
lwd = rnet_100$govtarget_slc / mean(rnet$govtarget_slc)

plot (L$geometry)
plot(rnet_5["govtarget_slc"], lwd = lwd, add = TRUE)

mapview::mapview(rnet, zcol = "govtarget_slc", lwd = lwd * 2)
mapview::mapview(rnet_100, zcol = "govtarget_slc", lwd = lwd * 2)

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
  scale_color_discrete("Gradient (%)")


#calculate MET equivalent per week

Top5_godutch_route$'Cycling Uptake' <- Top5_godutch$dutch_slc - Top5_godutch$bicycle




#health indicators
health_csv <- read_csv("C:/Users/Holly.Mizser-Jones/Documents/UCL/CASA005/Assessment/Census Data/liverpool_health.csv")

#merge health indicators with LSOA
HealthMerge <-merge (L, 
                     health_csv, 
                     by.x="geo_code", 
                     by.y="area_code",
                     no.dups = TRUE)

#plot health indicators
library(tmap)
library(tmaptools)
tmap_mode("view")

qtm(HealthMerge,
    fill = "% Very good health") +
  qtm(Top5_existing_route)




tm_shape(HealthMerge) +
  tm_polygons(c("Very good health", "Very bad health"), 
              style=c("jenks", "pretty"),
              palette=list("YlOrBr", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Very Good Health", "Very Bad Health"))

tmaptools::palette_explorer()

tm_shape(HealthMerge) +
  tm_polygons(c("Very good health", "Very bad health"), 
              style=c("jenks", "pretty"),
              palette=list("YlOrBr", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Very Good Health", "Very Bad Health"))

plot(L$geomtry)
plot(LSOA_5["bicycle"], add = TRUE, col = "red")


tm_shape(HealthMerge) +
  tm_polygons(c("% Very good health", "% Very bad health"), 
              style=c("cont", "cont"),
              palette=list("YlOrBr", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("% Very Good Health", "% Very Bad Health"))


remove(Liverpoolmap)

str(HealthMerge)

library(geosphere)
HealthMerge$area <- areaPolygon(HealthMerge)
HealthMerge$areasqkm <- HealthMerge$area / 1000000

st_intersects(x = Top5_existing_route, y = HealthMerge, sparse = TRUE)



