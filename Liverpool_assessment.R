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

remotes::install_cran(pkgs)
# remotes::install_github("ITSLeeds/pct")

source("https://raw.githubusercontent.com/ITSLeeds/TDS/master/code-r/setup.R") 

library(pct)
library(dplyr)   # in the tidyverse
library(tmap) # installed alongside mapview
region_name = "liverpool-city-region"
max_distance = 7
zones_all = get_pct_zones(region_name)
lines_all = get_pct_lines(region_name)
# basic plot
plot(zones_all$geometry)
plot(lines_all$geometry[lines_all$all > 500], col = "red", add = TRUE)

# create 'active' desire lines (less than 5 km)
active = lines_all %>% 
  mutate(`Percent Active` = (bicycle + foot) / all * 100) %>% 
  filter(e_dist_km < max_distance)

# interactive plot
tmap_mode("view")
tm_shape(active) +
  tm_lines("Percent Active", palette = "RdYlBu", lwd = "all", scale = 9)

#bicycle desire lines
bicycle = lines_all %>%
  mutate('Percentage Cycling' = (bicycle) / all * 100) %>%
  filter(e_dist_km < max_distance)

# interactive plot
tmap_mode("view")
tm_shape(bicycle) +
  tm_lines("Percentage Cycling", palette = "RdYlBu", lwd = "bicycle", scale = 9)

remove(bicycle300)


# Create car dependent desire lines
car_dependent = lines_all %>% 
  mutate(`Percent Drive` = (car_driver) / all * 100) %>% 
  filter(e_dist_km < max_distance)
tm_shape(car_dependent) +
  tm_lines("Percent Drive", palette = "-RdYlBu", lwd = "all", scale = 9)
#> Legend for line widths not available in view mode.

library(pct)
library(dplyr)
library(tidyverse)
Liverpool = get_pct_zones(region = region_name)
L = Liverpool %>%
  select(geo_code, geo_name, all, bicycle, car_driver, foot)
plot(L)

plot (L$geometry)
plot(L[62,"bicycle"], add = TRUE, col = "purple")

# Aim: get top 5 cycle routes
library(tidyverse)

L_original_msoa = get_pct_lines("liverpool-city-region")
L_msoa = L_original_msoa %>% 
  select(geo_code1, geo_code2, all, bicycle, car_driver, rf_avslope_perc, rf_dist_km)
L_msoa %>% filter(bicycle > 46)
plot(L$geometry)
plot(MSOA_5["bicycle"], add = TRUE, col = "red")

st_length(MSOA_5[4,4])

L_original_lsoa = get_pct_lines("liverpool-city-region", geography = "lsoa")
L_lsoa = L_original_lsoa %>% 
  select(geo_code1, geo_code2, all, bicycle, car_driver, rf_avslope_perc, rf_dist_km)
LSOA_5 = L_lsoa %>% filter(bicycle > 9)
plot(L$geometry)
plot(LSOA_5["bicycle"], add = TRUE, col = "red")

# top 300 routes cycled 
top300 = L_lsoa %>% top_n(300, bicycle)
plot(L$geometry)
plot(top300["bicycle"], add = TRUE, col = "green", vwlwd = top300$bicycle)

#propensity to cycle (Go Dutch, Govt. and Gender Equality)
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

L_msoa$euclidean_distance = as.numeric(sf::st_length(L_msoa))
L_msoa$pcycle_godutch = uptake_pct_godutch(
  distance = L_msoa$rf_dist_km,
  gradient = L_msoa$rf_avslope_perc
) * 100 + L_msoa$pcycle

plot(L$geometry)
plot(subset(L_msoa["pcycle_godutch"], pcycle_godutch >= 30), add = TRUE)


pct_uptake_godutch("liverpool-city-region")

library(stplanr)
L_top = L_msoa %>% 
  top_n(n = 1, wt = bicycle)
from <- c(-2.999, 53.651) # geo_code1("E02001432")
to <- c(-2.988, 53.635) # geo_code("E02001436")
r <- route_osrm(from, to)
plot(r)
r_many <- line2route(flowlines_sf[2:9, ], route_osrm, time_delay = 1)
plot(cents)
plot(r_many$geometry)