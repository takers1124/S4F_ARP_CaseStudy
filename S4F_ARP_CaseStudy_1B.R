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

# load spatial data
ARP_vect <- vect("ARP_vect.shp")
SRME_vect <- vect("SRME_vect.shp")

# PCU SpatVector created in Part 1A
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
sum(ARP_CFP_rast[] >= 0, na.rm = TRUE) # 7776004 cells
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
# seed_SRME_simp <- simplifyGeom(seed_SRME_vect, tolerance = 50) # 50 meters



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

# assign seedlot names by index
SL_A[first_hit$pcu_idx]  <- seed_poly_vect$Lot[first_hit$seed_idx]
SL_B[second_hit$pcu_idx] <- seed_poly_vect$Lot[second_hit$seed_idx]

# attach attributes
ARP_PCUs_1A_vect$seedlot_A <- SL_A
ARP_PCUs_1A_vect$seedlot_B <- SL_B


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
RD_at_centroid <- extract(RDs, PCU_centroids, na.rm = TRUE)

# add attributes 
ARP_PCUs_1A_vect$FORESTNAME <- RD_at_centroid$FORESTNAME 
ARP_PCUs_1A_vect$DISTRICTNA <- RD_at_centroid$DISTRICTNA 


### seed zone ----
# this shapefiles came fom Katie
CO_SZ <- vect("Colo_Seed_Zones.shp")
CO_SZ <- project(CO_SZ, "EPSG:5070")

# extract
extract_SZ <- extract(CO_SZ, PCU_centroids, na.rm = TRUE)
str(extract_SZ)

# add attributes 
ARP_PCUs_1A_vect$seed_zone <- extract_SZ$ZONE_NO 


### elevation ----
# using ARP_DEM created in Part1_A_3b
ARP_DEM <- rast("ARP_DEM.tif")

# the DEM is in meters, but we want 500ft EBs
# convert m to ft
meters_to_feet_factor <- 3.28084
ARP_DEM_ft <- ARP_DEM * meters_to_feet_factor 
summary(ARP_DEM_ft) # min = 4839, max = 14227 

# extract max
Elv_max_df <- extract(ARP_DEM_ft, ARP_PCUs_1A_vect, fun=max, na.rm = TRUE)
str(Elv_max_df)
# rename col
Elv_max_df <- Elv_max_df %>% 
  rename(Elv_max_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)

# extract min
Elv_min_df <- extract(ARP_DEM_ft, ARP_PCUs_1A_vect, fun=min, na.rm = TRUE)
str(Elv_min_df)
# adjust
Elv_min_df <- Elv_min_df %>% 
  rename(Elv_min_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


## (B) attributes: thresholds for PCUs ----
# these attributes were used to create PCUs 
# we used them to set thresholds (e.g. only areas with QMD > 5 inches)
# here we will just extract the mean value for the metric across the whole PCU


### EVH ----
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
extract_EVH <- extract(ARP_tree_height, ARP_PCUs_1A_vect, fun=mean, na.rm = TRUE)
  # need na.rm = TRUE because some areas (EVH < 100 or > 199) have already been filtered out
str(extract_EVH)
# adjust
extract_EVH <- extract_EVH %>% 
  rename(EVH_ft = CLASSNAMES) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### QMD ----
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
# already in 5070, value units are in inches
plot(QMD_CONUS)

# crop and mask
QMD_ARP <- crop(QMD_CONUS, ARP_vect, mask=TRUE)

# extract value
extract_QMD <- extract(QMD_ARP, ARP_PCUs_1A_vect, fun=mean, na.rm = TRUE)
str(extract_QMD)
# adjust
extract_QMD <- extract_QMD %>% 
  rename(QMD_in = CLASSNAMES) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)


### road ----
# using ARP_road_dist_rast from Part1A_3 
ARP_road_dist_rast <- rast("ARP_road_dist_rast.tif")

extract_road_dist <- extract(ARP_road_dist_rast, ARP_PCUs_1A_vect, fun=mean, na.rm = TRUE)
str(extract_road_dist)
# adjust
extract_road_dist <- extract_road_dist %>% 
  mutate(road_mi = layer * 0.000621371, # convert meters to miles
         layer = NULL, # remove distance in meters column
         PCU_ID = ARP_PCUs_1A_vect$PCU_ID)  %>% 
  select(-1)


### slope ----
# using DEM from Part1_A_3b
ARP_DEM <- rast("ARP_DEM.tif")

# calc slope
ARP_slope = terrain(DEM_ARP, v="slope", unit="degrees")
plot(ARP_slope)

extract_slope <- extract(ARP_slope, ARP_PCUs_1A_vect, fun=mean, na.rm = TRUE)
str(extract_slope)
# adjust
extract_slope <- extract_slope %>% 
  rename(slope_deg = slope) %>% 
  mutate(PCU_ID = ARP_PCUs_1A_vect$PCU_ID) %>% 
  select(-1)



## (C) attributes: other objectives ----
# these attributes are added to the PCU as a tool for objective-focused filtering


### CFP ----
# using crown fire probability data from Part1B_1A (setup)
ARP_CFP_rast <- rast("ARP_CFP_rast.tif")

extract_CFP <- extract(ARP_risk_score_rast, ARP_PCUs_1A_vect, fun=mean, na.rm = TRUE)
str(extract_CFP)
# adjust
extract_CFP <- extract_CFP %>% 
  rename(CFP_prob = CLASSNAMES) %>% # may need to change CLASSNAMES!!!!
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
PIPO_ARP <- crop(PIPO_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PiPo_ARP)
# extract
extract_PIPO <- extract(PIPO_ARP, ARP_PCUs_1A_vect, fun=mean, na.rm=TRUE)
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
PIEN_ARP <- crop(PIEN_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PIEN_ARP)
# extract
extract_PIEN <- extract(PIEN_ARP, ARP_PCUs_1A_vect, fun=mean, na.rm=TRUE)
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
PIFL_ARP <- crop(PIFL_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PIFL_ARP)
# extract
extract_PIFL <- extract(PIFL_ARP, ARP_PCUs_1A_vect, fun=mean, na.rm=TRUE)
str(extract_PIFL)
# adjust
extract_PIFL <- extract_PIFL %>% 
  rename(PIFL2_tons = Hosted_AGB_0113_2018_LIMBER_PINE_05292023073457) %>% 
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
PICO_ARP <- crop(PICO_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PICO_ARP)
# extract
extract_PICO <- extract(PICO_ARP, ARP_PCUs_1A_vect, fun=mean, na.rm=TRUE)
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
PSME_ARP <- crop(PSME_projected, ARP_vect, mask=TRUE, touches=TRUE)
plot(PSME_ARP)
# extract
extract_PSME <- extract(PSME_ARP, ARP_PCUs_1A_vect, fun=mean, na.rm=TRUE)
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
match_join_df <- Elv_max_df %>% 
  left_join(Elv_min_df, by = "PCU_ID") %>% 
  left_join(extract_EVH, by = "PCU_ID") %>% 
  left_join(extract_QMD, by = "PCU_ID") %>% 
  left_join(extract_slope, by = "PCU_ID") %>% 
  left_join(extract_road_dist, by = "PCU_ID") %>% 
  left_join(extract_CFP, by = "PCU_ID") %>% 
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


## can remove later ----
# previous way to add
# attributes to add
attribute_list <- list(Elv_max_df, Elv_min_df, 
                       extract_EVH, extract_QMD,
                       extract_slope, 
                       extract_road_dist,
                       extract_CFP,
                       extract_PIPO, extract_PIEN, extract_PIFL, extract_PICO, extract_PSME)
str(attribute_list)

# change "ID" to "PCU_ID" in attribute_list
renamed_list <- lapply(attribute_list, function(df) {
  df %>% rename(PCU_ID = ID)
})
str(renamed_list)

attributes_combined_df <- Reduce(function(x, y) full_join(x, y, by = "PCU_ID"), renamed_list)

ARP_PCUs_atts_vect <- ARP_PCUs_vect %>% 
  left_join(attributes_combined_df, by = "PCU_ID")