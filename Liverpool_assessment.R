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
cyclestreets_api_key("64de24d7c380c043", install = TRUE)

Sys.getenv("CYCLESTREETS_KEY")
Sys.setenv(CYCLESTREETS_KEY = "64de24d7c380c043")
Sys.getenv("CYCKESTREETS_KEY")

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

  #1abicycle desire lines

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

#2 top 300 cycle routes

top300 = L_original_lsoa %>% top_n(300, bicycle)

  #2a existing top 300 routes cycled 
plot(L$geometry)
plot(top300["bicycle"], add = TRUE, col = "green", vwlwd = top300$bicycle)

  #2b top 300 routes cycles GoDutch

plot(L$geometry)
plot(top300["dutch_slc"], add = TRUE, col = "green", vwlwd = top300$dutch_slc)



#propensity to cycle (Go Dutch, Govt. and Gender Equality)
#scenarios of change
L_msoa$newcolumn <- NA
library(tidyverse)
colnames(L_msoa)
names(L_msoa)[names(L_msoa) == "newcolumn"] <- "pcycle"

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
  select(geo_code1, geo_code2, all, bicycle, car_driver, rf_avslope_perc, rf_dist_km)
LSOA_5 = L_lsoa %>% filter(bicycle > 9)
plot(L$geometry)
plot(LSOA_5["bicycle"], add = TRUE, col = "red")



#subset of cycle routes, greater than 30 people
plot(L$geometry)
plot(subset(L_msoa["pcycle_godutch"], pcycle_godutch >= 30), add = TRUE)


pct_uptake_godutch("liverpool-city-region")

#Routing
#find routes assocaited with the most cycles desire line in Liverpool
library(stplanr)
L_top = L_msoa %>% 
  top_n(n = 1, wt = bicycle)
from <- c(-2.999, 53.651) # geo_code1("E02001432")
to <- c(-2.988, 53.635) # geo_code("E02001436")
r <- route_osrm(from, to)
plot(r)
r_many <- line2route(LSOA_5[1:5, ], route_osrm, time_delay = 1)
qtm(r_many)

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
