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

# load spatial data
ARP_vect <- vect("ARP_vect.shp")
SRME_vect <- vect("SRME_vect.shp")

# PCU SpatVector created in Part 1-A
ARP_all_PCUs_vect <- vect("ARP_all_PCUs_vect.shp")
  # only has PCU_ID and area_acres as attributes

## nursery data integration ----

# goal is to see if any of these PCUs that we identified are already represented in the nursery inventory
  # nursery inventories are not standardized across the forest service
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of how nursery inventory was obtained and modified 

### import & adjust ----
  # read in nursery inventories
  # adjust species naming
  # add nursery name column
  # build cleaned lat/long columns & fix sign errors
b_seed <- read.csv("20230707_besseylots_editedlatlongs.csv") %>% 
  rename(source_code=SOURCE.CODE) %>% 
  mutate(species=sub(".*- ", "", SPECIES)) %>% 
  mutate(nursery = "b_seed") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), LAT, alt_lat), long_2 = ifelse(is.na(alt_long), LONG, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

str(b_seed)

lp_seed <- read.csv("LuckyPeak_inventory_2022_editedlatlongs.csv") %>% 
  rename(species=SP.Name) %>% 
  mutate(nursery = "lp_seed") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), Lat, alt_lat), long_2 = ifelse(is.na(alt_long), Long, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

str(lp_seed)

cda_seed <- read.csv("CdA_inventory_2023_editedlatlongs.csv") %>% 
  rename(species = COMMON.NAME) %>% 
  mutate(species = ifelse(SP=="PSME", "Douglas-fir",
                          ifelse(SP=="PIEN", "Engelmann spruce",
                                 ifelse(SP=="PICO", "lodgepole pine",
                                        ifelse(SP=="PIPO", "ponderosa pine",
                                               ifelse(SP=="PIFL2", "limber pine",
                                                      species)))))) %>% 
  mutate(nursery = "cda_seed") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), LAT, alt_lat), long_2 = ifelse(is.na(alt_long), LONG, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

# investigate
head(cda_seed)
cda_spp <- unique(cda_seed$species)

cda_seed %>% arrange(species) %>% View()

### combine ----
# added long, lat, and seed to select()

seed.nums.all<- 
  bind_rows(
    b_seed %>% 
      left_join(b_seed %>% 
                  group_by(species) %>% 
                  summarise(mean.gross.unit=mean(GROSS.UNIT, na.rm=T),mean.germ.pct = mean(GERM.PCT, na.rm=T)), by="species") %>% 
      mutate(n.seeds = BALANCE*mean.gross.unit) %>% 
      mutate(exp.n.germ = n.seeds*GERM.PCT/100, mean.exp.n.germ=n.seeds*mean.germ.pct/100) %>% 
      dplyr::select(species,source_code, DATE.CLCTD, BALANCE, GROSS.UNIT, GERM.PCT, mean.gross.unit,mean.germ.pct, n.seeds, exp.n.germ, mean.exp.n.germ, lat_2, long_2, nursery) %>% 
      dplyr::rename(Lot = source_code, Balance=BALANCE, SPP=GROSS.UNIT, mean.spp=mean.gross.unit, Year = DATE.CLCTD) %>% 
      mutate(Year=as.character(Year)),
    lp_seed %>% 
      left_join(lp_seed %>% 
                  group_by(species) %>% 
                  summarise(mean.spp=mean(SPP, na.rm=T),mean.germ.pct = mean(Germ.., na.rm=T)), by="species") %>% 
      mutate(n.seeds = Balance*mean.spp) %>% 
      mutate(exp.n.germ = n.seeds*Germ../100, mean.exp.n.germ=n.seeds*mean.germ.pct/100, Year=as.character(Year)) %>% 
      dplyr::select(species, Lot, Year, Balance, SPP, Germ.., mean.spp, mean.germ.pct, n.seeds, exp.n.germ, mean.exp.n.germ, lat_2, long_2, nursery) %>% 
      dplyr::rename(GERM.PCT = Germ..),
    cda_seed %>% 
      left_join(cda_seed %>% 
                  group_by(species) %>% 
                  summarise(mean.spp=mean(SEED.LB, na.rm=T),mean.germ.pct = mean(GERM.., na.rm=T)), by="species") %>% 
      mutate(n.seeds = LOT.BAL*mean.spp) %>% 
      mutate(exp.n.germ = n.seeds*GERM../100, mean.exp.n.germ=n.seeds*mean.germ.pct/100, Year=as.character(YR.CLCT)) %>% 
      dplyr::select(species, LOT, Year, LOT.BAL, SEED.LB, GERM.., mean.spp, mean.germ.pct, n.seeds, exp.n.germ, mean.exp.n.germ, lat_2, long_2, nursery) %>% 
      dplyr::rename(GERM.PCT = GERM.., Lot = LOT, Balance = LOT.BAL, SPP=SEED.LB)
  ) %>% 
  mutate(across(everything(), ~replace(., . == "NaN" , NA))) %>% 
  rowwise() %>% 
  mutate(lowest.exp.germ = ifelse(is.na(exp.n.germ) & is.na(mean.exp.n.germ), NA, min(exp.n.germ, mean.exp.n.germ, na.rm=T)))

str(seed.nums.all)

### filter ----
# only for target spp
conifer_seed_all <- seed.nums.all %>% 
  filter(tolower(species) %in% c("western white pine", "Douglas-fir",
                                 "Engelmann spruce", "lodgepole pine",
                                 "ponderosa pine", "limber pine",
                                 "white fir", "whitebark pine",
                                 "singleleaf pinyon pine", "southwestern white pine",
                                 "two-needle pinyon pine", "Arizona pine",
                                 "blue spruce", "bristlecone pine",
                                 "Great Basin bristlecone pine", "western white pine"))
# has 1715 rows (seedlots)

# filter only seedots that have lat/long info
conifer_seed <- conifer_seed_all %>% 
  filter(!is.na(lat_2), !is.na(long_2))
# 1296 rows

(1296/1715)*100 # 75.6 % of seedlots have coords

# add ID col
conifer_seed$SL_ID <- 1:nrow(conifer_seed)

# fix messed up row... 1096 has lat/long switched...
conifer_seed_coords <- conifer_seed %>% 
  mutate(
    temp_lat = lat_2,# temp column to hold lat values
    lat_2 = if_else(
      SL_ID == "1096", # ID the problem row
      long_2 * -1, # if it's the problem row, swap long_2 value to into the lat_2 col and fix the sign
      lat_2 # if not the problem row, keep original lat_2 value
    ),
    long_2 = if_else(
      SL_ID == "1096",
      temp_lat * -1,
      long_2
    )
  ) %>% 
  select(-temp_lat)

### convert to spatvector ----

seed_points <- vect(x = conifer_seed_coords, geom = c("long_2", "lat_2"), crs = "epsg:4326")
plot(seed_points)

# project
seed_points_vect <- project(seed_points, "epsg:5070")
plot(seed_points_vect)

#### write & read ----
writeVector(seed_points_vect, "seed_points_vect.shp")
seed_points_vect <- vect("seed_points_vect.shp")

seed_points_df <- as.data.frame(seed_points_vect)


# make a circular buffer poly with 500 m width
seed_poly_vect <- buffer(seed_points_vect, width = 500)
plot(seed_poly_vect)

#### write & read ----
writeVector(seed_poly_vect, "seed_poly_vect.shp")
seed_poly_vect <- vect("seed_poly_vect.shp")

# but want to filter for only those that are located within the SRME
seed_within <- relate(seed_poly_vect, SRME_vect, relation = "within")
# add as attribute to the spatvector
seed_poly_vect$within_SRME <- seed_within

# and we don't need the other attributes
seed_SRME_vect <- seed_poly_vect %>% 
  filter(within_SRME == "TRUE") %>% 
  select(SL_ID, Lot, species, Year, Balance, nursery)

# has 158 geoms 
(158/1296)*100 # = 12.19136 % of seedlot polys are within the SRME

# simplify for relating
seed_SRME_simp <- simplifyGeom(seed_SRME_vect, tolerance = 50) # 50 meters




# add attributes ----
# in this step, we add attributes (metadata) to our PCU polygons
# this is how we further filter & select PCUs for scouting


##
## (15) nursery ----
# we want to know if any of the nursery seedlots (SLs) overlap with our PCUs
  # and if they do overlap, the PCU will have the SL_ID added as an attribute for looking up later

# compute spatial relationship
SL_relate <- relate(ARP_all_PCUs_vect, seed_SRME_simp, relation = "intersects", pairs = TRUE)
str(SL_relate) # a list with intersecting pairs

# make an empty list
SL_list <- rep(NA_character_, nrow(ARP_all_PCUs_vect))
# keep first match per PCU (some have >1 intersection)
first_hit <- SL_relate[!duplicated(SL_relate[, 1]), ]
# assign Lot values (may want to change attribute later)
SL_list[first_hit[, 1]] <- seed_poly_vect$Lot[first_hit[, 2]]
# attach attribute
ARP_all_PCUs_vect$seedlot <- SL_list







## (1) ranger district ----
ranger_districts <- vect("S_USA.BdyAdm_LSRS_RangerDistrict.shp")
ranger_districts <- project(ranger_districts, "EPSG:5070")

# only need these 2 attributes
RDs <- ranger_districts %>% 
  select(FORESTNAME, DISTRICTNA)

# many PCUs span multiple RDs
  # use the centroid (convert polys to points)
  # more straight-forward than using relate() (as with the seedlots)
PCU_centroids <- centroids(ARP_all_PCUs_vect)

# extract RDs at centroids
RD_at_centroid <- extract(RDs, PCU_centroids)

# add attributes 
ARP_all_PCUs_vect$FORESTNAME <- RD_at_centroid$FORESTNAME 
ARP_all_PCUs_vect$DISTRICTNA <- RD_at_centroid$DISTRICTNA 

ARP_PCU_df <- as.data.frame(ARP_all_PCUs_vect)




##
## (2) seed zone ----
# this shapefiles came fom Katie
CO_SZ <- vect("Colo_Seed_Zones.shp")
CO_SZ <- project(CO_SZ, "EPSG:5070")

### extract ----
extract_SZ <- extract(CO_SZ, PCU_centroids)
str(extract_SZ)

# add attributes 
ARP_PCUs_vect$seed_zone <- extract_SZ$ZONE_NO 




##
## (3) elevation band ----
# using ARP_DEM created in Part1_A_3b
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
# like the way we used EVH in Part1_A_3d, but without the filtering
  # we just want to know the mean EVH for each PCU
EVH_CONUS <- rast("LC23_EVH_240.tif")
EVH_ARP <- crop(EVH_CONUS, ARP_vect, mask=TRUE)

# define conversion factor
meters_to_feet_factor <- 3.28084

# reclassify with ifel()
ARP_tree_height <- ifel(
  # condition 1: it is dominant veg type trees? (values 100-199)
  EVH_ARP >= 100 & EVH_ARP < 200,
  # if TRUE, subtract offset & convert units
    (EVH_ARP - 100) * meters_to_feet_factor,
  # if FALSE, not a tree value, reclassify to NA
   NA 
)

# extract value
extract_EVH <- extract(ARP_tree_height, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
  # need na.rm = TRUE because some areas (EVH < 100 or > 199) have already been filtered out
str(extract_EVH)
# rename col
extract_EVH <- extract_EVH %>% 
  rename(EVH_ft = CLASSNAMES)

##
## (4) QMD ----
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
# already in 5070, value units are in inches
plot(QMD_CONUS)

# crop and mask
QMD_ARP <- crop(QMD_CONUS, ARP_vect, mask=TRUE)

# extract value
extract_QMD <- extract(QMD_ARP, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_QMD)
# rename col
extract_QMD <- extract_QMD %>% 
  rename(QMD_in = CLASSNAMES)


##
## (5) CFP ----
# using crown fire probability data from Part1_A_3a
ARP_risk_score_rast <- rast("ARP_risk_score_rast.tif")

extract_CFP <- extract(ARP_risk_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_CFP)
# rename col
extract_CFP <- extract_CFP %>% 
  rename(CFP_prob = CLASSNAMES) # may need to change CLASSNAMES

## 
## (6) road ----
# using ARP_road_rast from Part1_A_3c 
ARP_road_rast <- rast("ARP_road_rast.tif") 

# calculate the distance to nearest road for each raster cell (pixel)
ARP_road_dist_rast <- distance(ARP_road_rast, unit="m", method="haversine") 

extract_road_dist <- extract(ARP_road_dist_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_road_dist)
# convert meters to miles
extract_road_dist <- extract_road_dist %>% 
  mutate(road_miles = layer * 0.000621371,
         layer = NULL) # remove distance in meters

## 
## (7) road_score ----
# # using ARP_road_score_rast from Part 3.c (above)
# 
# extract_road_score <- extract(ARP_road_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
# str(extract_road_score)
# # rename col
# extract_road_score <- extract_road_score %>% 
#   rename(road_sc = layer)

## 
## (8) slope ----
# using DEM from Part1_A_3b
ARP_DEM <- rast("ARP_DEM.tif")

# calc slope
ARP_slope = terrain(DEM_ARP, v="slope", unit="degrees")
plot(ARP_slope)

extract_slope <- extract(ARP_slope, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_slope)
# rename col
extract_slope <- extract_slope %>% 
  rename(slope_deg = slope)

##
# ## (8) slope_score ----
# # using ARP_slope_score_rast, created in Hotspots_Part1b.R
# 
# extract_slope_score <- extract(ARP_slope_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
# str(extract_slope_score)
# # rename col
# extract_slope_score <- extract_slope_score %>% 
#   rename(slope_sc = slope)

##
## (9) priority_s ----
# combined "score" or "sum" from Part1_A_4
ARP_priority_rast <- rast("ARP_priority_rast.tif")

extract_priority_s <- extract(ARP_priority_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_priority_s)
# rename col
extract_priority_s <- extract_priority_s %>% 
  rename(priority_s = score)


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