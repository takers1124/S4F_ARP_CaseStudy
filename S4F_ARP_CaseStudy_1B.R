# description ----

# Seeds 4 the Future ARP Case Study
  # part 1-B, add attributes to PCUs

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project
  # as well as information about where to download raw data

# setup ----

library(terra) 
library(tidyterra) 
library(dplyr)
library(ggplot2) 

# load PCU SpatVector created in Part 1-A
ARP_all_PCUs_vect <- vect("ARP_all_PCUs_vect.shp")
  # only has PCU_ID and area_acres as attributes


# add attributes ----
# in this step, we add attributes (metadata) to our polygons
# this is how we further filter & select PCUs for scouting


## (1) ranger district ----
ranger_districts <- vect("S_USA.BdyAdm_LSRS_RangerDistrict.shp")
ranger_districts <- project(ranger_districts, "EPSG:5070")

# ARP_RDs <- ranger_districts %>% 
#   select(FORESTNAME, DISTRICTNA) %>% 
#   filter(FORESTNAME == "Arapaho and Roosevelt National Forests")
# plot(ARP_RDs)

RDs <- ranger_districts %>% 
  select(FORESTNAME, DISTRICTNA)

### try with extract ----
extract_RDs <- extract(RDs, ARP_all_PCUs_vect)
str(extract_RDs)

# add attributes 
ARP_all_PCUs_vect$FORESTNAME <- extract_RDs$FORESTNAME 
ARP_all_PCUs_vect$DISTRICTNA <- extract_RDs$DISTRICTNA 

ARP_all_PCUs_df <- as.data.frame(ARP_all_PCUs_vect)
  # many are NA...


### diagnostic ----

is.valid(RDs) # many are false
new_RDs <- makeValid(RDs)
is.valid(new_RDs) # all are true

is.valid(ARP_all_PCUs_vect) # true

# try extract again
extract_new_RDs <- extract(new_RDs, ARP_all_PCUs_vect)
str(extract_new_RDs)

# add attributes 
ARP_all_PCUs_vect$FORESTNAME <- extract_new_RDs$FORESTNAME 
ARP_all_PCUs_vect$DISTRICTNA <- extract_new_RDs$DISTRICTNA 

ARP_all_PCUs_df <- as.data.frame(ARP_all_PCUs_vect)
  # many are still NA

### try with relate ----
intersecting_pairs <- relate(ARP_all_PCUs_vect, RDs, "intersects", pairs = TRUE)

joined_polygons <- cbind(
  ARP_PCUs_vect[intersecting_pairs[,1],],
  ARP_RDs[intersecting_pairs[,2],]
)

is_na <- joined_polygons %>% 
  filter(is.na(FORESTNAME))

joined_df <- as.data.frame(joined_polygons)

joined_groups <- group_by(joined_df, PCU_ID) %>% 
  summarise(reps = n()) %>% 
  filter(reps == 2)

unique_joined_polygons <- distinct(joined_polygons, PCU_ID, .keep_all = TRUE)
joined2_df <- as.data.frame(unique_joined_polygons)


# add attributes 
ARP_PCUs_vect$FORESTNAME <- intersect_RDs$FORESTNAME 
ARP_PCUs_vect$DISTRICTNA <- intersect_RDs$DISTRICTNA 

ARP_PCU_df <- as.data.frame(ARP_PCUs_vect)




##
## (2) seed zone ----
# this shapefiles came fom Katie
CO_SZ <- vect("Colo_Seed_Zones.shp")
CO_SZ <- project(CO_SZ, "EPSG:5070")

### extract ----
extract_SZ <- extract(CO_SZ, ARP_PCUs_vect)
str(extract_SZ)

# add attributes 
ARP_PCUs_vect$seed_zone <- extract_SZ$ZONE_NO 

##
## (3) elevation band ----
# using ARP_DEM created in part 3.b
ARP_DEM <- rast("ARP_DEM.tif")

### classify EB ----
# the DEM is in meters, but we want 500ft EBs
# convert m to ft
meters_to_feet_factor <- 3.28084
ARP_DEM_ft <- ARP_DEM * meters_to_feet_factor 
summary(ARP_DEM_ft) # min = 4839, max = 14227 

# create a matrix 
elev.matrix <- matrix(c(seq(4000, 14000, 500), seq(4500, 14500, 500), seq(4500, 14500, 500)), ncol=3, byrow = FALSE)
# becomes the top of the interval (max of range)

# classify the DEM
ARP_DEM_EB_rast <- classify(ARP_DEM_ft, rcl=elev.matrix, right=TRUE) # right = T ensures upper bound of interval included in band 
plot(ARP_DEM_EB_rast, type = "classes")
unique(ARP_DEM_EB_rast) # 20 unique values (EBs)

### write & read ----
writeRaster(ARP_DEM_EB_rast, "ARP_DEM_EB_rast.tif") 
ARP_DEM_EB_rast <- rast("ARP_DEM_EB_rast.tif")

summary(ARP_DEM_EB_rast)
plot(is.na(ARP_DEM_EB_rast))

### extract max ----
extract_EB_max <- extract(ARP_DEM_EB_rast, ARP_PCUs_vect, fun=max)
str(extract_EB_max)

is_na <- extract_EB_max %>% 
  filter(is.na(USGS_1_n41w106_20230314))

# rename col
extract_EB_max <- extract_EB_max %>% 
  rename(EB_max = USGS_1_n41w106_20230314)

### extract min ----
extract_EB_min <- extract(ARP_DEM_EB_rast, ARP_PCUs_vect, fun=min)
str(extract_EB_min)
# rename col
extract_EB_min <- extract_EB_min %>% 
  rename(EB_min = USGS_1_n41w106_20230314)

##
## (4) EVH ----
# using EVH_classified3 (ARP_height_rast) from Part 3.d above (before making binary score)
ARP_height_rast

extract_EVH <- extract(ARP_height_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
# need na.rm = TRUE because some areas (height < 20 ft) have already been filtered out
str(extract_EVH)
# rename col
extract_EVH <- extract_EVH %>% 
  rename(EVH_ft = CLASSNAMES)

##
## (5) FLEP8 ----
# using FLEP8_CO
FLEP8_CO <- rast("FLEP8_CO.tif")

extract_FLEP8 <- extract(FLEP8_CO, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_FLEP8)
# rename col
extract_FLEP8 <- extract_FLEP8 %>% 
  rename(FLEP8_sc = FLEP8_CO)

## 
## (6) road ----
# using ARP_road_dist_rast from Part 3.c (above) - units are in meters 

extract_road_dist <- extract(ARP_road_dist_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_road_dist)
# convert meters to miles
extract_road_dist <- extract_road_dist %>% 
  mutate(road_miles = layer * 0.000621371,
         layer = NULL) # remove distance in meters

## 
## (7) road_score ----
# using ARP_road_score_rast from Part 3.c (above)

extract_road_score <- extract(ARP_road_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_road_score)
# rename col
extract_road_score <- extract_road_score %>% 
  rename(road_sc = layer)

## 
## (8) slope ----
# using DEM_ARP from above, units in degrees 
### calc slope ----
ARP_slope = terrain(DEM_ARP, v="slope", neighbors=8, unit="degrees")
plot(ARP_slope)

extract_slope <- extract(ARP_slope, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_slope)
# rename col
extract_slope <- extract_slope %>% 
  rename(slope_deg = slope)

##
## (8) slope_score ----
# using ARP_slope_score_rast, created in Hotspots_Part1b.R

extract_slope_score <- extract(ARP_slope_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_slope_score)
# rename col
extract_slope_score <- extract_slope_score %>% 
  rename(slope_sc = slope)

##
## (9) combine_sc ----
# using combined_rast from Part 4 (above)

extract_combined_score <- extract(combined_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_combined_score)
# rename col
extract_combined_score <- extract_combined_score %>% 
  rename(combine_sc = score)

extract_combined_score_df <- as.data.frame(extract_combined_score)

##
## (10) PIPO ----
# read 
PIPO_BigMap <- rast("Hosted_AGB_0122_2018_PONDEROSA_PINE_08142023231656.tif")

# project
crs(PIPO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIPO_projected <- project(PIPO_BigMap, "EPSG:5070")
crs(PIPO_projected) # EPSG 5070

# crop & mask
PIPO_ARP <- crop(PIPO_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PiPo_ARP)

# extract
extract_PIPO <- extract(PIPO_ARP, ARP_PCUs_vect, fun=mean, na.rm=TRUE)
str(extract_PIPO)

# rename col
extract_PIPO <- extract_PIPO %>% 
  rename(PIPO_tons = Hosted_AGB_0122_2018_PONDEROSA_PINE_08142023231656)

##
## (11) PIEN ----
# read 
PIEN_BigMap <- rast("Hosted_AGB_0093_2018_ENGELMANN_SPRUCE_05042023231614.tif")

# project
crs(PIEN_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIEN_projected <- project(PIEN_BigMap, "EPSG:5070")
crs(PIEN_projected) # EPSG 5070

# crop & mask
PIEN_ARP <- crop(PIEN_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PIEN_ARP)

# extract
extract_PIEN <- extract(PIEN_ARP, ARP_PCUs_vect, fun=mean, na.rm=TRUE)
str(extract_PIEN)

# rename col
extract_PIEN <- extract_PIEN %>% 
  rename(PIEN_tons = Hosted_AGB_0093_2018_ENGELMANN_SPRUCE_05042023231614)

##
## (12) PIFL2 ----
# read 
PIFL2_BigMap <- rast("Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457.tif")

# project
crs(PIFL2_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIFL2_projected <- project(PIFL2_BigMap, "EPSG:5070")
crs(PIFL2_projected) # EPSG 5070

# crop & mask
PIFL2_ARP <- crop(PIFL2_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PIFL2_ARP)

# extract
extract_PIFL2 <- extract(PIFL2_ARP, ARP_PCUs_vect, fun=mean, na.rm=TRUE)
str(extract_PIFL2)

# rename col
extract_PIFL2 <- extract_PIFL2 %>% 
  rename(PIFL2_tons = Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457)

##
## (13) PICO ----
# read 
PICO_BigMap <- rast("Hosted_AGB_0108_2018_LODGEPOLE_PINE_05272023153302.tif")

# project
crs(PICO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PICO_projected <- project(PICO_BigMap, "EPSG:5070")
crs(PICO_projected) # EPSG 5070

# crop & mask
PICO_ARP <- crop(PICO_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PICO_ARP)

# extract
extract_PICO <- extract(PICO_ARP, ARP_PCUs_vect, fun=mean, na.rm=TRUE)
str(extract_PICO)

# rename col
extract_PICO <- extract_PICO %>% 
  rename(PICO_tons = Hosted_AGB_0108_2018_LODGEPOLE_PINE_05272023153302)

##
## (14) PSME ----
# read 
PSME_BigMap <- rast("Hosted_AGB_0202_2018_DOUGLAS_FIR_06012023172436.tif")

# project
crs(PSME_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PSME_projected <- project(PSME_BigMap, "EPSG:5070")
crs(PSME_projected) # EPSG 5070

# crop & mask
PSME_ARP <- crop(PSME_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PSME_ARP)

# extract
extract_PSME <- extract(PSME_ARP, ARP_PCUs_vect, fun=mean, na.rm=TRUE)
str(extract_PSME)

# rename col
extract_PSME <- extract_PSME %>% 
  rename(PSME_tons = Hosted_AGB_0202_2018_DOUGLAS_FIR_06012023172436)

##
## (15) nursery ----
### *need to do ----



# combine ----
ARP_PCUs_vect # has PCU_ID, area_acres, FORESTNAME, DISTRICTNA, seed_zone

# attributes to add
attribute_list <- list(extract_combined_score, extract_EB_max, extract_EB_min, 
                       extract_EVH, extract_FLEP8,
                       extract_slope, extract_slope_score,
                       extract_road_dist, extract_road_score,
                       extract_PIPO, extract_PIEN, extract_PIFL2, extract_PICO, extract_PSME)
str(attribute_list)

# change "ID" to "PCU_ID" in attribute_list
renamed_list <- lapply(attribute_list, function(df) {
  df %>% rename(PCU_ID = ID)
})
str(renamed_list)

attributes_combined_df <- Reduce(function(x, y) full_join(x, y, by = "PCU_ID"), renamed_list)

ARP_PCUs_atts_vect <- ARP_PCUs_vect %>% 
  left_join(attributes_combined_df, by = "PCU_ID")

ARP_PCUs_atts_vect_df <- as.data.frame(ARP_PCUs_atts_vect)
str(ARP_PCUs_atts_vect_df)

write.csv(ARP_PCUs_atts_vect_df, "ARP_PCUs_atts_vect_df.csv", row.names = FALSE)

### write & read ----
writeVector(ARP_PCUs_atts_vect, "ARP_PCUs_atts_vect.shp")
ARP_PCUs_atts_vect <- vect("ARP_PCUs_atts_vect.shp")