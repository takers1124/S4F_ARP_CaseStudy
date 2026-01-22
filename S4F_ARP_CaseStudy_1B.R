# description ----

# Seeds 4 the Future ARP Case Study
  # part 1B, add attributes to PCUs

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project
  # as well as information about where to download raw data

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

# read AOIs
ARP_vect <- vect("ARP_vect.shp")
SRME_vect <- vect("SRME_vect.shp")

# read PCU SpatVector created in Part 1A
ARP_PCUs_1A_vect <- vect("ARP_PCUs_1A_vect.shp")
  # only has PCU_ID and area_acres as attributes
  # this Part 1B script will add all the general attributes to it

## (A) risk data integration ----
# using the crown fire probability (CFP) dataset from Pyrologix 

### load & process ----
CFP_49_41 <- rast("crown_fire_2025_c00049_r00041.tif")
CFP_50_41 <- rast("crown_fire_2025_c00050_r00041.tif")
CFP_50_40 <- rast("crown_fire_2025_c00050_r00040.tif")
  # CRS already in 5070

# combine
CFP_mosaic <- mosaic(CFP_49_41, CFP_50_41, CFP_50_40, fun = "first")
plot(CFP_mosaic)
  # the raster values are 0-1 (probability)

# crop and mask  
ARP_CFP_rast <- crop(CFP_mosaic, ARP_vect, mask=TRUE)

### viz ----
plot(ARP_CFP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)

### stats ----
global(ARP_CFP_rast, fun = "notNA") # 7776004 cells 
  # this dataset covers 100% of the ARP  

### write & read file ----
writeRaster(ARP_CFP_rast, "ARP_CFP_rast.tif")
ARP_CFP_rast <- rast("ARP_CFP_rast.tif")


## (B) nursery data integration ----

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

lp_seed <- read.csv("LuckyPeak_inventory_2022_editedlatlongs.csv") %>% 
  rename(species=SP.Name) %>% 
  mutate(nursery = "lp_seed") %>% 
  mutate(lat_2 = ifelse(is.na(alt_lat), Lat, alt_lat), long_2 = ifelse(is.na(alt_long), Long, alt_long)) %>% 
  mutate(long_2 = ifelse(long_2>0, long_2*-1, long_2))

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

### convert to points ----
seed_points <- vect(x = conifer_seed_coords, geom = c("long_2", "lat_2"), crs = "epsg:4326")
plot(seed_points)

# project
seed_points_vect <- project(seed_points, "epsg:5070")
plot(seed_points_vect)

#### write & read ----
writeVector(seed_points_vect, "seed_points_vect.shp")
seed_points_vect <- vect("seed_points_vect.shp")

seed_points_df <- as.data.frame(seed_points_vect)

### convert to polys ----
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

#### write & read ----
writeVector(seed_SRME_vect, "seed_SRME_vect.shp")
seed_SRME_vect <- vect("seed_SRME_vect.shp")



# (2) attribute creation ----
# in this step, we add attributes (metadata) to our PCU polygons
# this is how we further filter & select PCUs for scouting
# we will divide the attributes into 4 groups (A-D)

## (A) attributes: geographic info ----
# these attributes were not used to create PCUs
# we are adding them to provide basic geographic information about each PCU for reference and filtering


### nursery SL ----
# we want to know if any of the nursery seedlots (SLs) overlap with our PCUs
  # and if they do overlap, the PCU will have the SL_ID added as an attribute for looking up later

# compute spatial relationship
SL_relate <- relate(ARP_PCUs_1A_vect, seed_SRME_vect, relation = "intersects", pairs = TRUE, na.rm = TRUE)
str(SL_relate) # a list of vectors (one per PCU) with intersecting pairs

# adjust data
rel_df <- as.data.frame(SL_relate)
colnames(rel_df) <- c("pcu_idx", "seed_idx")
str(rel_df)

# compute per-PCU hit order
rel_df <- rel_df %>%
  group_by(pcu_idx) %>%
  mutate(hit = row_number()) %>%
  ungroup()

# slice the first and second hits
first_hit  <- filter(rel_df, hit == 1)
second_hit <- filter(rel_df, hit == 2)
  # when expanding PCU creation to SRME, the # hits may be >2, so may need to adjust 

# prepare attribute vectors
SL_A <- rep(NA_character_, nrow(ARP_PCUs_1A_vect))
SL_B <- rep(NA_character_, nrow(ARP_PCUs_1A_vect))

# assign seedlot names (Lot attribute) by index
SL_A[first_hit$pcu_idx]  <- seed_poly_vect$Lot[first_hit$seed_idx]
SL_B[second_hit$pcu_idx] <- seed_poly_vect$Lot[second_hit$seed_idx]

# attach attributes
ARP_PCUs_1A_vect$seedlot_A <- SL_A
ARP_PCUs_1A_vect$seedlot_B <- SL_B

# stats
count_non_na <- sum(!is.na(ARP_PCUs_1A_vect$seedlot_A))
  # 32 PCUs overlap with at least 1 seedlot



### ranger district ----
ranger_districts <- vect("S_USA.BdyAdm_LSRS_RangerDistrict.shp")
ranger_districts <- project(ranger_districts, "EPSG:5070")

# only need these 2 attributes
RDs <- ranger_districts %>% 
  select(FORESTNAME, DISTRICTNA)

# many PCUs span multiple RDs
  # use the centroid (convert polys to points)
  # more straight-forward than using relate() (as with the seedlots)
PCU_centroids <- centroids(ARP_PCUs_1A_vect)

# extract RDs at centroids
RD_at_centroid <- extract(RDs, PCU_centroids)

# add attributes 
ARP_PCUs_1A_vect$FORESTNAME <- RD_at_centroid$FORESTNAME 
ARP_PCUs_1A_vect$DISTRICTNA <- RD_at_centroid$DISTRICTNA 


### seed zone ----
# this shapefile came fom Katie
CO_SZ <- vect("Colo_Seed_Zones.shp")
CO_SZ <- project(CO_SZ, "EPSG:5070")

# extract
extract_SZ <- extract(CO_SZ, PCU_centroids)
str(extract_SZ)

# add attributes 
ARP_PCUs_1A_vect$seed_zone <- extract_SZ$ZONE_NO 


### elevation ----
# using ARP_DEM created in Part1A_3b
ARP_DEM_rast <- rast("ARP_DEM_rast.tif")
summary(ARP_DEM_rast) # min = 1638, max = 4276

# the DEM is in meters, but we want ft
# convert m to ft
meters_to_feet_factor <- 3.28084
ARP_DEM_ft <- ARP_DEM_rast * meters_to_feet_factor 
summary(ARP_DEM_ft) # min = 5374, max = 14030  

# extract median
Elv_med_df <- extract(ARP_DEM_ft, ARP_PCUs_1A_vect, fun=median)
str(Elv_med_df)
# rename col
Elv_med_df <- Elv_med_df %>% 
  rename(Elv_med_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


## (B) attributes: thresholds for PCUs ----
# these attributes were used to create PCUs 
# we used them to set thresholds (e.g. only areas with QMD > 5 inches) in Part1A_3
# here we will just extract the median value for the metric across each PCU polygon

### QMD ----
ARP_QMD_rast <- rast("ARP_QMD_rast.tif")

# extract value
extract_QMD <- extract(ARP_QMD_rast, ARP_PCUs_1A_vect, fun = median, na.rm = TRUE)
str(extract_QMD)
# adjust
extract_QMD <- extract_QMD %>% 
  rename(QMD_in = TreeMap2022_CONUS_QMD) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### EVH ----
ARP_EVH_rast <- rast("ARP_EVH_rast.tif")

# extract value
extract_EVH <- extract(ARP_EVH_rast, ARP_PCUs_1A_vect, fun = median, na.rm = TRUE)
str(extract_EVH)
# adjust
extract_EVH <- extract_EVH %>% 
  rename(EVH_ft = CLASSNAMES) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### slope ----
ARP_slope_rast <- rast("ARP_slope_rast.tif")

extract_slope <- extract(ARP_slope_rast, ARP_PCUs_1A_vect, fun = median, na.rm = TRUE)
str(extract_slope)
# adjust
extract_slope <- extract_slope %>% 
  rename(slope_deg = slope) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### road ----
ARP_road_dist_rast <- rast("ARP_road_dist_rast.tif")

extract_road_dist <- extract(ARP_road_dist_rast, ARP_PCUs_1A_vect, fun = median, na.rm = TRUE)
str(extract_road_dist)
# adjust
extract_road_dist <- extract_road_dist %>% 
  mutate(road_mi = layer * 0.000621371, # convert meters to miles
         layer = NULL, # remove distance in meters column
         PCU_ID = ARP_PCUs_1A_vect$PCU_ID)  %>% 
  select(-1)


## (C) attributes: other objectives ----
# these attributes are added to the PCU as a tool for objective-focused filtering


### CFP ----
# load & process
CFP_49_41 <- rast("crown_fire_2025_c00049_r00041.tif")
CFP_50_41 <- rast("crown_fire_2025_c00050_r00041.tif")
CFP_50_40 <- rast("crown_fire_2025_c00050_r00040.tif")
  # CRS already in 5070
# combine
CFP_mosaic <- mosaic(CFP_49_41, CFP_50_41, CFP_50_40, fun = "first")
plot(CFP_mosaic)
# crop & mask
ARP_CFP_rast <- crop(CFP_mosaic, ARP_vect, mask=TRUE)
plot(ARP_CFP_rast)

#### write & read ----
writeRaster(ARP_CFP_rast, "ARP_CFP_rast.tif")
ARP_CFP_rast <- rast("ARP_CFP_rast.tif")

# extract
extract_CFP <- extract(ARP_CFP_rast, ARP_PCUs_1A_vect, fun = median, na.rm = TRUE)
str(extract_CFP)
# adjust
extract_CFP <- extract_CFP %>% 
  rename(CFP_prob = crown_fire_2025_c00049_r00041) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### BALIVE ----
# read 
BA_CONUS <- rast("TreeMap2022_CONUS_BALIVE.tif") # already in EPSG: 5070
# crop & mask 
ARP_BA_rast <- crop(BA_CONUS, ARP_vect, mask = TRUE)
# extract
extract_BA <- extract(ARP_BA_rast, ARP_PCUs_1A_vect, fun = median, na.rm=TRUE)
str(extract_BA)
# adjust
extract_BA <- extract_BA %>% 
  rename(BA_ft_sq = TreeMap2022_CONUS_BALIVE) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### PIPO ----
# read 
PIPO_BigMap <- rast("Hosted_AGB_0122_2018_PONDEROSA_PINE_08142023231656.tif")
# project
crs(PIPO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIPO_projected <- project(PIPO_BigMap, "EPSG:5070")
crs(PIPO_projected) # EPSG 5070
# crop & mask
ARP_PIPO_rast <- crop(PIPO_projected, ARP_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(ARP_PIPO_rast, "ARP_PIPO_rast.tif")
ARP_PIPO_rast <- rast("ARP_PIPO_rast.tif")

# extract
extract_PIPO <- extract(ARP_PIPO_rast, ARP_PCUs_1A_vect, fun = median, na.rm=TRUE)
str(extract_PIPO)
# adjust
extract_PIPO <- extract_PIPO %>% 
  rename(PIPO_tons = Hosted_AGB_0122_2018_PONDEROSA_PINE_08142023231656) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### PIEN ----
# read 
PIEN_BigMap <- rast("Hosted_AGB_0093_2018_ENGELMANN_SPRUCE_05042023231614.tif")
# project
crs(PIEN_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIEN_projected <- project(PIEN_BigMap, "EPSG:5070")
crs(PIEN_projected) # EPSG 5070
# crop & mask
ARP_PIEN_rast <- crop(PIEN_projected, ARP_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(ARP_PIEN_rast, "ARP_PIEN_rast.tif")
ARP_PIEN_rast <- rast("ARP_PIEN_rast.tif")

# extract
extract_PIEN <- extract(ARP_PIEN_rast, ARP_PCUs_1A_vect, fun = median, na.rm=TRUE)
str(extract_PIEN)
# adjust
extract_PIEN <- extract_PIEN %>% 
  rename(PIEN_tons = Hosted_AGB_0093_2018_ENGELMANN_SPRUCE_05042023231614) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### PIFL ----
# read 
PIFL_BigMap <- rast("Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457.tif")
# project
crs(PIFL_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PIFL_projected <- project(PIFL_BigMap, "EPSG:5070")
crs(PIFL_projected) # EPSG 5070
# crop & mask
ARP_PIFL_rast <- crop(PIFL_projected, ARP_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(ARP_PIFL_rast, "ARP_PIFL_rast.tif")
ARP_PIFL_rast <- rast("ARP_PIFL_rast.tif")

# extract
extract_PIFL <- extract(ARP_PIFL_rast, ARP_PCUs_1A_vect, fun = median, na.rm=TRUE)
str(extract_PIFL)
# adjust
extract_PIFL <- extract_PIFL %>% 
  rename(PIFL_tons = Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### PICO ----
# read 
PICO_BigMap <- rast("Hosted_AGB_0108_2018_LODGEPOLE_PINE_05272023153302.tif")
# project
crs(PICO_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PICO_projected <- project(PICO_BigMap, "EPSG:5070")
crs(PICO_projected) # EPSG 5070
# crop & mask
ARP_PICO_rast <- crop(PICO_projected, ARP_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(ARP_PICO_rast, "ARP_PICO_rast.tif")
ARP_PICO_rast <- rast("ARP_PICO_rast.tif")

# extract
extract_PICO <- extract(ARP_PICO_rast, ARP_PCUs_1A_vect, fun = median, na.rm=TRUE)
str(extract_PICO)
# adjust
extract_PICO <- extract_PICO %>% 
  rename(PICO_tons = Hosted_AGB_0108_2018_LODGEPOLE_PINE_05272023153302) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### PSME ----
# read 
PSME_BigMap <- rast("Hosted_AGB_0202_2018_DOUGLAS_FIR_06012023172436.tif")
# project
crs(PSME_BigMap) # EPSG 9001 - USA_Contiguous_Albers_Equal_Area_Conic_USGS_version
PSME_projected <- project(PSME_BigMap, "EPSG:5070")
crs(PSME_projected) # EPSG 5070
# crop & mask
ARP_PSME_rast <- crop(PSME_projected, ARP_vect, mask=TRUE, touches=TRUE)

#### write & read ----
writeRaster(ARP_PSME_rast, "ARP_PSME_rast.tif")
ARP_PSME_rast <- rast("ARP_PSME_rast.tif")

# extract
extract_PSME <- extract(ARP_PSME_rast, ARP_PCUs_1A_vect, fun = median, na.rm=TRUE)
str(extract_PSME)
# adjust
extract_PSME <- extract_PSME %>% 
  rename(PSME_tons = Hosted_AGB_0202_2018_DOUGLAS_FIR_06012023172436) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


## (D) attributes: planting site-specific ----
# these attributes will be added to the PCUs in part 3
# we are keeping them separate because the attributes added in Part1B are not dependent on the case study planting units



# (3) combine ----
ARP_PCUs_1A_vect # has PCU_ID, area_acres, seedlot_A, seedlot_B, FORESTNAME, DISTRICTNA, seed_zone
  # we added these one at a time (because they came from SpatVectors)

# now add the others
match_join_df <- Elv_med_df %>% 
  left_join(extract_EVH, by = "PCU_ID") %>% 
  left_join(extract_QMD, by = "PCU_ID") %>% 
  left_join(extract_slope, by = "PCU_ID") %>% 
  left_join(extract_road_dist, by = "PCU_ID") %>% 
  left_join(extract_CFP, by = "PCU_ID") %>% 
  left_join(extract_BA, by = "PCU_ID") %>% 
  left_join(extract_PIPO, by = "PCU_ID") %>% 
  left_join(extract_PIEN, by = "PCU_ID") %>% 
  left_join(extract_PIFL, by = "PCU_ID") %>% 
  left_join(extract_PICO, by = "PCU_ID")%>% 
  left_join(extract_PSME, by = "PCU_ID")

# merge with previous PCU spatvector, making new one with full attributes
ARP_PCUs_vect <- ARP_PCUs_1A_vect %>% 
  left_join(match_join_df, by = "PCU_ID")

ARP_PCUs_df <- as.data.frame(ARP_PCUs_vect)

## write & read ----
write.csv(ARP_PCUs_df, "ARP_PCUs_df.csv", row.names = FALSE)
writeVector(ARP_PCUs_vect, "ARP_PCUs_vect.shp")
ARP_PCUs_vect <- vect("ARP_PCUs_vect.shp")

