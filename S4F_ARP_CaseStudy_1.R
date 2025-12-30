# description ----

# Seeds 4 the Future ARP Case Study, the full script
# part 1, create Potential Collection Units (PCUs)

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025

# see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project
  # as well as information about where to download raw data

# (1) setup ----

library(terra) # primary spatial data management package
library(tidyterra) # dplyr-style data management for spatial data
library(dplyr)
library(ggplot2) # for fancy plotting


# (2) create AOI ----

## ARP ----
# the Area of Interest (AOI) for this case study is the Arapaho-Roosevelt National Forest (ARP)
### load & process ----
NF_CONUS_vect <- vect("S_USA.FSCommonNames.shp")

plot(NF_CONUS_vect)

# see unique names 
names(NF_CONUS_vect)
unique(NF_CONUS_vect$COMMONNAME)

# select for just ARP 
ARP_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Arapaho and Roosevelt National Forests")

plot(ARP_vect)

# project 
ARP_vect <- project(ARP_vect,"EPSG:5070")

# calc area
expanse(ARP_vect) # 6975245280 m^2
6975245280/4046.86 # 4046.86 m/acre = 1723619 acres

expanse(ARP_vect, unit = "ha")

### write & read ----
writeVector(ARP_vect, "ARP_vect.shp")
ARP_vect <- vect("ARP_vect.shp")

## SRME ----
# Southern Rocky Mountain Ecoregion
### load & process ----
EPA_ecoregions <- vect("us_eco_l3.shp") # 1250 geoms

# see unique names 
names(EPA_ecoregions)
unique(EPA_ecoregions$US_L3NAME)

# select for just SRME
SRME_s_rockies <- EPA_ecoregions %>% 
  filter(US_L3NAME == "Southern Rockies")
  # has 4 separate polygons

# aggregate them together
SRME_aggregated <- terra::aggregate(SRME_s_rockies)
plot(SRME_aggregated)
# has 1 geom & 0 attributes (they are lost after aggregate)

# project 
SRME_vect <- project(SRME_aggregated, "EPSG:5070")

### write & read ----
writeVector(SRME_vect, "SRME_vect.shp")
SRME_vect <- vect("SRME_vect.shp")


# (3) pre-process data ----

## (3.a) risk CFP ----
# using the crown fire probability (CFP) dataset from Pyrologix 

### load & process ----
CFP_49_41 <- rast("crown_fire_2025_c00049_r00041.tif")
CFP_50_41 <- rast("crown_fire_2025_c00050_r00041.tif")
CFP_50_40 <- rast("crown_fire_2025_c00050_r00040.tif")
# CRS already in 5070

# combine
CFP_mosaic <- mosaic(CFP_49_41, CFP_50_41, CFP_50_40, fun = "first")
plot(CFP_mosaic)

# crop and mask  
ARP_risk_score_rast <- crop(CFP_mosaic, ARP_vect, mask=TRUE)
plot(ARP_risk_score_rast)

# the raster values are already 0-1 (probability)
  # value 1 = highest risk
  # no need to classify or calc inverse score (like other priority factors)

### viz ----
plot(ARP_risk_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)

### stats ----
global(ARP_risk_score_rast, fun = "notNA") # 7776004 cells 
sum(ARP_risk_score_rast[] >= 0, na.rm = TRUE) # 7776004 cells
# this dataset covers 100% of the ARP - no filtering areas out for low risk 

### write & read file ----
writeRaster(ARP_risk_score_rast, "ARP_risk_score_rast.tif")
ARP_risk_score_rast <- rast("ARP_risk_score_rast.tif")



## (3.b) slope ----
# this slope raster is generated using  
  # digital elevation model (DEM) tiles, downloaded from The National Map (USGS)
  # they are 1 Arc Sec
  # these tiles have GEOGCRS NAD83, but are not yet projected

### load & process DEMs ----
DEM_n41_w106 <- rast("USGS_1_n41w106_20230314.tif")
DEM_n41_w107 <- rast("USGS_1_n41w107_20230314.tif")
DEM_n40_w106 <- rast("USGS_1_n40w106_20230602.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")
# mosaic 4 tiles together
ARP_DEM <- mosaic(DEM_n41_w106, DEM_n41_w107, DEM_n40_w106, DEM_n40_w107, fun="first")
# project
ARP_DEM <- project(ARP_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of ARP 
ARP_DEM <- crop(ARP_DEM, ARP_vect, mask=TRUE)
plot(ARP_DEM) # min = 1470.285 , max = 4393.409 (meters)

### write & read ----
writeRaster(ARP_DEM, "ARP_DEM.tif")
ARP_DEM <- rast("ARP_DEM.tif")

### calc slope ----
ARP_slope = terrain(ARP_DEM, v="slope", unit="degrees")
plot(ARP_slope)

### adjust values ----
minmax(ARP_slope) 
# min = 0, max = 72.59397 
  # but the max we want to include is 24 degrees
  # and we want 0-24 degree slope to become 0-1 score (normalize)

# make all values > 24 degrees NA, leave other values as-is
slope_filtered <- ifel(ARP_slope > 24, NA, ARP_slope)
plot(slope_filtered)

# normalize scale
slope_norm <- slope_filtered / 24
plot(slope_norm)

# calc inverse score
ARP_slope_score_rast <- (1 - slope_norm)

### viz ----
plot(ARP_slope_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)

plot(is.na(ARP_slope_score_rast))

### stats ----
global(ARP_slope_score_rast, fun = "notNA") # 7433981 cells 

# entire ARP = 7776004 cells 
(7433981/7776004)*100 # 95.60156 % remaining after 24* filter 

### write & read ----
writeRaster(ARP_slope_score_rast, "ARP_slope_score_rast.tif")
ARP_slope_score_rast <- rast("ARP_slope_score_rast.tif")



## (3.c) road ----

# import CO roads shapefile
  # downloaded from The National Map
roads_CONUS <- vect("Trans_RoadSegment_0.shp")
plot(roads_CONUS)
crs(roads_CONUS) # EPSG 4269

road_df <- as.data.frame(roads_CONUS)
# could filter by road type, we did not

# project, crop & mask 
roads_CONUS = project(roads_CONUS, "EPSG:5070")
crs(roads_CONUS) # EPSG 5070

roads_ARP = crop(roads_CONUS, ARP_vect)
plot(roads_ARP)

### rasterize ----
ARP_road_rast <- rasterize(roads_ARP, ARP_risk_score_rast , touches=TRUE)
plot(ARP_road_rast, col="blue") # all values = 1
plot(is.na(ARP_road_rast)) # values not 1 are NA
# TBH, the raster does not look nearly as contiguous as the road lines from the .shp
  # but when I open the .tif in Arc, it looks fine 
  # I think it is too much for R studio to render with plot()

### write & read file ----
writeRaster(ARP_road_rast, "ARP_road_rast.tif")
ARP_road_rast <- rast("ARP_road_rast.tif") 

### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
ARP_road_dist_rast <- distance(ARP_road_rast, unit="m", method="haversine") 
plot(ARP_road_dist_rast)
# cell values = distance to nearest road (in meters)

### adjust values ----
minmax(ARP_road_dist_rast) 
# min = 0, max = 37416.17 
  # but the max we want to include is 917.3261 meters (0.57 miles)
  # and we want 0-917 m distance to become 0-1 score (normalize)

# make NA all values > 917.3261 meters, leave other values as-is
road_filtered <- ifel(ARP_road_dist_rast > 917.3261, NA, ARP_road_dist_rast)
plot(road_filtered)

# normalize scale
road_norm <- road_filtered / 917.3261
plot(road_norm)

# calc inverse
ARP_road_inverse <- (1 - road_norm)
plot(ARP_road_inverse)

### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the ARP
ARP_road_score_rast = crop(ARP_road_inverse, ARP_vect, mask = TRUE)

### viz ----
plot(ARP_road_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
plot(is.na(ARP_road_score_rast))

### stats ----
global(ARP_road_score_rast, fun = "notNA") # 5213776 cells 

# entire ARP = 7776004 cells 
(5213776/7776004)*100 # 67.04955 % remaining  

### write & read ----
writeRaster(ARP_road_score_rast, "ARP_road_score_rast.tif")
ARP_road_score_rast <- rast("ARP_road_score_rast.tif")



## (3.d) tree height ----
# using existing vegetation height (EVH) from LANDFIRE
  # these values are not continuous
  # also the veg height has an offset added
    # e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC23_EVH_240.tif")
crs(EVH_CONUS) # 5070
res(EVH_CONUS) # 30 30

### crop / mask ----
EVH_ARP <- crop(EVH_CONUS, ARP_vect, mask=TRUE)

### adjust values ----
# define conversion factor
meters_to_feet_factor <- 3.28084

# reclassify with ifel()
ARP_height_score_rast <- ifel(
  # condition 1: it is dominant veg type trees? (values 100-199)
  EVH_ARP >= 100 & EVH_ARP < 200,
  # if TRUE, 
    # condition 2: is it > 10 ft tall? 
  ifel(
    (EVH_ARP - 100) * meters_to_feet_factor > 10, # subtract offset, convert units, filter
    100, # if TRUE, reclassify to 100
    NA # if FALSE, reclassify to NA
  ),
  NA # if not a tree value (condition 1 = FALSE), reclassify to NA
)

### stats ----
# entire ARP = 7776004 cells

# all veg area
global(EVH_ARP >= 100, fun = "sum", na.rm = TRUE) # 7004697 cells
(7004697/7776004)*100 # 90.08093 % of ARP is vegetated 

# all tree area
global(EVH_ARP >= 100 & EVH_ARP < 200, fun = "sum", na.rm = TRUE) # 5324379 cells
(5324379/7776004)*100 # 68.47192 % of ARP has trees 

# trees > 10 ft area
global(ARP_height_score_rast == 100, fun = "sum", na.rm = TRUE) # 5219760 cells
(5219760/7776004)*100 # 67.12651 % of ARP has trees > 10 ft

### viz ----
plot(ARP_height_score_rast, col = "forestgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(ARP_height_score_rast, "ARP_height_score_rast.tif")
ARP_height_score_rast <- rast("ARP_height_score_rast.tif")



## (3.e) tree diameter ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
# already in 5070
plot(QMD_CONUS)

### crop and mask ----
QMD_ARP <- crop(QMD_CONUS, ARP_vect, mask=TRUE)
plot(QMD_ARP)

# reclassify with ifel()
ARP_diameter_score_rast <- ifel(
  QMD_ARP >= 5, 50, NA 
)
# if >= 5 inches, reclassify to 50
# if < 5 inches, reclassify to NA

### stats ----
# entire ARP = 7776004 cells 
# all areas with QMD values
global(QMD_ARP, fun = "notNA") # 5697616 cells
(5697616/7776004)*100 # 73.27174 % of ARP has QMD values

# areas with QMD > 5 inches
global(ARP_diameter_score_rast, fun = "notNA") # 4160703
(4160703/7776004)*100 # 53.50696 % of ARP has trees > 5 in QMD

### viz ----
# see classified values
plot(ARP_diameter_score_rast, col = "darkgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(ARP_diameter_score_rast, "ARP_diameter_score_rast.tif")
ARP_diameter_score_rast <- rast("ARP_diameter_score_rast.tif")



# (4) combine data ----

## resample ----
# first, the rasters need to be resampled so their extents align,
  # and they have matching resolutions and origins 
# 4 of the 5 rasters have the same resolution
# 3 of the 5 rasters already have matching extents
  # I am randomly choosing 1 of those 3 to use as the template
    # raster [[5]] in the list

# make a raster list 
raster_list <- list(ARP_risk_score_rast, ARP_slope_score_rast, 
                    ARP_road_score_rast, ARP_height_score_rast, 
                    ARP_diameter_score_rast)

# set template for resampling
template_raster <- raster_list[[5]]

# resample the remaining rasters to match the template
# rasters 1, 2, and 3 in the list have continuous values
  # use bilinear interpolation 
resampled_continuous <- lapply(raster_list[1:3], function(r) {
    resample(r, template_raster, method = "bilinear")
})

# rasters 4 and 5 in the list have binary values 
  # use nearest neighbor
resampled_binary <- lapply(raster_list[4:5], function(r) {
  resample(r, template_raster, method = "near")
})

# combine into new list
resampled_rast_list <- c(resampled_continuous, resampled_binary)
  # all 5 rasters have same extent, resolution, etc

# create a multi-layer raster stack 
resampled_rast_stack <- rast(resampled_rast_list)
  # has 5 layers

## sum ----
combined_raster <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
  # has values: min = 0, max = 153

## viz ----
plot(combined_raster)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)


## stats ----
# entire ARP = 7776004 cells 
# want to know how many cells (what % of ARP) falls into each category

# cat 0: all values not NA
global(combined_raster, fun = "notNA") # 7778382 cells 
(7778382/7776004)*100 # 100.0306 % of ARP 
  # not sure why it is > 100%

# cat 1: values 0-3
  # slope, road, and risk combined & continuous values
  # but does not meet QMD or EVH threshold
global(combined_raster >= 0 & combined_raster <= 3, fun = "sum", na.rm = TRUE) # 2141385 cells
(2141385/7776004)*100 # 27.53837 % of ARP 

# cat 2: values 50-53
  # just meet's QMD threshold
  # dense/wide but short (e.g. shelterwood, post-fire, krummholz, pinyon/limber)
global(combined_raster >= 50 & combined_raster <= 53, fun = "sum", na.rm = TRUE) # 417237 cells
(417237/7776004)*100 # 5.365699 % of ARP 

# cat 3: values 100-103
  # just meet's EVH threshold
  # tall but not dense/wide (e.g. open-canopy lodgepole/pondo)
global(combined_raster >= 100 & combined_raster <= 103, fun = "sum", na.rm = TRUE) # 1476294 cells
(1476294/7776004)*100 # 18.98525 % of ARP 

# cat 4: values 150-153
  # meet both QMD and EVH thresholds
global(combined_raster >= 150 & combined_raster <= 153, fun = "sum", na.rm = TRUE) # 3743466 cells
(3743466/7776004)*100 # 48.14126 % of ARP 

27.53837+5.365699+18.98525+48.14126 # 100.0306 - it adds up! 

## filter & rescale ----
# final values can be 0-3 for ease of interpretation 

ARP_priority_rast <- ifel(
  combined_raster >= 150 & combined_raster <= 153,
  combined_raster - 150, NA
) %>% 
  rename(priority_s = sum)

# just confirm filter
global(ARP_priority_rast, fun = "notNA") # 3743466 cells (same as Cat 4 ^)
(3743466/7776004)*100 # 48.14126 % of ARP

## calc area ----
  # transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
  # default units are m^2
expanse(ARP_priority_rast, transform = FALSE) # 3369119400 m^2
3369119400/4046.86 # 4046.86 m2/acre = 832526.8 acres
  # entire ARP = 1723619 acres
(832526.8/1723619)*100 # 48.30109 % of ARP 
  # area total is a bit off from cell count, but close enough

## viz ----
plot(ARP_priority_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

## write & read ----
writeRaster(ARP_priority_rast, "ARP_priority_rast.tif")
ARP_priority_rast <- rast("ARP_priority_rast.tif")


# note, all these areas meet our basic criteria (3.a - 3.d)
# however, it is still too large of an area to scout
# so, we must narrow it down further (part 1.5)



# (5-A) make PCUs ----

# we need feasible (small) units to send the scouting crew
# so we will filter the data and make "patches" of high-scoring areas
# we call these Potential Collection Units (PCUs)


## filter ----
priority_filtered <- ifel(
  ARP_priority_rast < 2, NA, ARP_priority_rast
)

# sanity checks
freq(priority_filtered)
532342+61856 # 594198
global(priority_filtered, fun = "notNA") # 594198 cells
(594198/7776004)*100 # 7.641431 % of ARP

plot(priority_filtered)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

## patches ----
# in this step, we convert connected raster cells (with values) into "patches"
  # btw this line can take several minutes to run
  # and the more conservative the above filtering is (more area retained), the longer this will take
priority_patches <- patches(priority_filtered, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
  # there are 57069  patches

plot(priority_patches)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

## make polygons ----
patch_polys <- as.polygons(priority_patches, values = FALSE)
  # there are 57069 geometries 

# add a patch_ID attribute for each poly
patch_polys$patch_ID <- 1:nrow(patch_polys) 

## separate sizes ----
# calc area 
patch_polys$patch_acres <- expanse(patch_polys) * 0.000247105

# filt out small poys (< 20 acres)
small_polys_removed <- patch_polys[patch_polys$patch_acres >= 20, ]
  # 800 geoms remain
(800/57069)*100 # 1.401812 % of polys remain (are >= 20 acres)
  # so 98.6 % of patches/polys were < 20 acres (isolated areas)
  # but many of these are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
  # 721 geoms
(721/800)*100 # 90.125 % of polys >= 20 acres are also <= 200 acres
  # these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
  # 79 geoms
  # these do need to be divided

## divide ----
# calculate divisions needed, ensuring at least 2 parts for large polys
num_parts <- pmax(2, round(large_polys$patch_acres / 125))

# use lapply to iterate and divide
divided_polys_list <- lapply(1:nrow(large_polys), function(i) {
  poly <- large_polys[i, ]
  
  # set a seed to ensure reproducibility for the division process
  set.seed(i)
  
  divided_poly <- divide(poly, n = num_parts[i])
  
  # store the original ID and re-calculate the new areas
  divided_poly$patch_ID <- poly$patch_ID
  divided_poly$div_acres <- expanse(divided_poly) * 0.000247105
  
  return(divided_poly)
})

# combine all divided polys into a single SpatVector
divided_polys_vect <- do.call(rbind, divided_polys_list)

# combine the mid-sized polys with the newly divided large polys
ARP_PCUs_vect <- rbind(mid_polys, divided_polys_vect)
  # 1063 geometries

## adjust & stats ----

# add new ID col & new final area col
ARP_PCUs_vect$PCU_ID <- 1:nrow(ARP_PCUs_vect)
ARP_PCUs_vect$area_acres <- expanse(ARP_PCUs_vect) * 0.000247105

summary(ARP_PCUs_vect)
# area_acres min = 20.02, max = 230.31  

# select only new ID and area
ARP_PCUs_vect <- ARP_PCUs_vect[, c("PCU_ID", "area_acres")]

ARP_PCU_df <- as.data.frame(ARP_PCUs_vect)

sum(ARP_PCUs_vect$area_acres) # 78844.96 acres
sum(small_polys_removed$patch_acres) # 78844.96 acres
  # bc these are =, we know the divide function worked (retained all area)

# ARP is 1723619 acres
(78844.96/1723619)*100 # 4.574384 % of ARP are highest priority areas (PCUs)

# for reference, 
(832526.8/1723619)*100 # 48.30109 % of ARP meets basic priorities
  # slope, road, risk, height, diameter
(78844.96/832526.8)*100 # 9.470561 % of the areas that meet basic priorities
  # are the "highest priority" (PCUs)
  # which have been (a) filtered for highest score, and (b) filtered for areas > 20 acres

## viz ----
plot(ARP_PCUs_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeVector(ARP_PCUs_vect, "ARP_PCUs_vect.shp")
ARP_PCUs_vect <- vect("ARP_PCUs_vect.shp")

# (5-B) make PCUs (no filter) ----
## patches ----
# btw this line took 35 minutes to run

priority_patches_all <- patches(ARP_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 134187   patches

## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 134187 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 

## separate sizes ----
# calc area 
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filt out small poys (< 20 acres)
small_all_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1279 geoms remain
(1279/134187)*100 # 0.9531475 % of polys remain (are >= 20 acres)
  # so ~99 % of patches/polys were < 20 acres (isolated areas)
  # but many of these are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_all_polys <- small_all_polys_removed[small_all_polys_removed$patch_acres <= 200, ]
# 1062 geoms
(1062/1279)*100 # 83.03362 % of polys >= 20 acres are also <= 200 acres
  # these don't need to be divided

# separate large polys ( > 200 acres)
large_all_polys <- small_all_polys_removed[small_all_polys_removed$patch_acres > 200, ]
# 217 geoms
  # these do need to be divided

## divide ----
# calculate divisions needed, ensuring at least 2 parts for large polys
num_all_parts <- pmax(2, round(large_all_polys$patch_acres / 125))

# use lapply to iterate and divide
divided_all_polys_list <- lapply(1:nrow(large_all_polys), function(i) {
  poly <- large_all_polys[i, ]
  
  # set a seed to ensure reproducibility for the division process
  set.seed(i)
  
  divided_poly <- divide(poly, n = num_all_parts[i])
  
  # store the original ID and re-calculate the new areas
  divided_poly$patch_ID <- poly$patch_ID
  divided_poly$div_acres <- expanse(divided_poly) * 0.000247105
  
  return(divided_poly)
})

# combine all divided polys into a single SpatVector
divided_all_polys_vect <- do.call(rbind, divided_all_polys_list)
  # 5366 geoms

# combine the mid-sized polys with the newly divided large polys
ARP_all_PCUs_vect <- rbind(mid_all_polys, divided_all_polys_vect)
  # 6428 geoms

## adjust & stats ----

# add new ID col & new final area col
ARP_all_PCUs_vect$PCU_ID <- 1:nrow(ARP_all_PCUs_vect)
ARP_all_PCUs_vect$area_acres <- expanse(ARP_all_PCUs_vect) * 0.000247105

summary(ARP_all_PCUs_vect)
# area_acres min = 10.74, max = 347.78    

# select only new ID and area
ARP_all_PCUs_vect <- ARP_all_PCUs_vect[, c("PCU_ID", "area_acres")]

ARP_all_PCUs_df <- as.data.frame(ARP_all_PCUs_vect)

sum(ARP_all_PCUs_vect$area_acres) # 728204.8 acres
sum(small_all_polys_removed$patch_acres) # 728204.8 acres
  # bc these are =, we know the divide function worked (retained all area)

# ARP is 1723619 acres
(728204.8/1723619)*100 # 42.24859 % of ARP are highest priority areas (PCUs)

# for reference, 
(832526.8/1723619)*100 # 48.30109 % of ARP meets basic priorities
  # slope, road, risk, height, diameter
(728204.8/832526.8)*100 # 87.46923 % of the areas that meet basic priorities
  # are continuous PCUs > 20 acres
  

## viz ----
plot(ARP_all_PCUs_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeVector(ARP_all_PCUs_vect, "ARP_all_PCUs_vect.shp")
ARP_all_PCUs_vect <- vect("ARP_all_PCUs_vect.shp")

## select Lady Moon ----
  # this is the case study PCU that we will use for FWD climate matching in part 2
  # the PCU closest to the Lady Moon trail head has PCU_ID = 212

PCU_LM <- ARP_all_PCUs_vect %>% 
  filter(PCU_ID == 212)
plot(PCU_LM)


# (6) add attributes ----
# in this step, we add attributes (metadata) to our polygons
# this is how we further filter & select PCUs for scouting

## *redo all ----
## (6.a) ranger district ----
ranger_districts <- vect("S_USA.BdyAdm_LSRS_RangerDistrict.shp")
ranger_districts <- project(ranger_districts, "EPSG:5070")

ARP_RDs <- ranger_districts %>% 
  select(FORESTNAME, DISTRICTNA) %>% 
  filter(FORESTNAME == "Arapaho and Roosevelt National Forests")
plot(ARP_RDs)

### extract ----
extract_RDs <- extract(ARP_RDs, ARP_PCUs_vect)
str(extract_RDs)

# add attributes 
ARP_PCUs_vect$FORESTNAME <- extract_RDs$FORESTNAME 
ARP_PCUs_vect$DISTRICTNA <- extract_RDs$DISTRICTNA 

### relate ----
intersecting_pairs <- relate(ARP_PCUs_vect, ARP_RDs, "intersects", pairs = TRUE)

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
## (6.b) seed zone ----
# this shapefiles came fom Katie
CO_SZ <- vect("Colo_Seed_Zones.shp")
CO_SZ <- project(CO_SZ, "EPSG:5070")

### extract ----
extract_SZ <- extract(CO_SZ, ARP_PCUs_vect)
str(extract_SZ)

# add attributes 
ARP_PCUs_vect$seed_zone <- extract_SZ$ZONE_NO 

##
## (6.c) elevation band ----
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
## (6.d) EVH ----
# using EVH_classified3 (ARP_height_rast) from Part 3.d above (before making binary score)
ARP_height_rast

extract_EVH <- extract(ARP_height_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
# need na.rm = TRUE because some areas (height < 20 ft) have already been filtered out
str(extract_EVH)
# rename col
extract_EVH <- extract_EVH %>% 
  rename(EVH_ft = CLASSNAMES)

##
## (6.e) FLEP8 ----
# using FLEP8_CO
FLEP8_CO <- rast("FLEP8_CO.tif")

extract_FLEP8 <- extract(FLEP8_CO, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_FLEP8)
# rename col
extract_FLEP8 <- extract_FLEP8 %>% 
  rename(FLEP8_sc = FLEP8_CO)

## 
## (6.f) road ----
# using ARP_road_dist_rast from Part 3.c (above) - units are in meters 

extract_road_dist <- extract(ARP_road_dist_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_road_dist)
# convert meters to miles
extract_road_dist <- extract_road_dist %>% 
  mutate(road_miles = layer * 0.000621371,
         layer = NULL) # remove distance in meters

## 
## (6.g) road_score ----
# using ARP_road_score_rast from Part 3.c (above)

extract_road_score <- extract(ARP_road_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_road_score)
# rename col
extract_road_score <- extract_road_score %>% 
  rename(road_sc = layer)

## 
## (6.h) slope ----
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
## (6.i) slope_score ----
# using ARP_slope_score_rast, created in Hotspots_Part1b.R

extract_slope_score <- extract(ARP_slope_score_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_slope_score)
# rename col
extract_slope_score <- extract_slope_score %>% 
  rename(slope_sc = slope)

##
## (6.j) combine_sc ----
# using combined_rast from Part 4 (above)

extract_combined_score <- extract(combined_rast, ARP_PCUs_vect, fun=mean, na.rm = TRUE)
str(extract_combined_score)
# rename col
extract_combined_score <- extract_combined_score %>% 
  rename(combine_sc = score)

extract_combined_score_df <- as.data.frame(extract_combined_score)

##
## (6.k) PIPO ----
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
## (6.l) PIEN ----
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
## (6.m) PIFL2 ----
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
## (6.n) PICO ----
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
## (6.o) PSME ----
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
## (6.p) nursery ----
### *need to do ----

##
## (6.q) combine ----
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




# (6) make PPUs ----
# we create potential planting units (PPUs)
  # using the FACTS needs polys from within the Cameron Peak fire boundary
    # last downloaded on August 3rd, 2025
  # we divided the FACTS needs into small (50-200 acre) polygons
  # then assigned a 500 ft elevation band (EB) to each
  # and we are just using the 8500-9000 ft EB for this part of the case study
# we will extract the future clim from these needs in Part 2

## import ----
needs_all <- vect("S_USA.Actv_SilvReforest_Needs.shp")
names(needs_all)
unique(needs_all$ACTIVITY_C)

## filter ----
desired_cols <- c("REGION_COD", "ADMIN_FORE", "DISTRICT_C", "FACTS_ID", "ACTIVITY_C")
  # the column ACTIVITY_C has the activity code
    # if we were looking for all planting needs, would use code 4431
desired_ids <- c("RA20CPPLNT")
  # for our case study, we just want this single polygon with FACTS_ID = RA20CPPLNT
    # this is essentially all the Cameron Peak fire needs in 1 poly
    # which are labeled CL for Canyon Lakes Ranger District

CL_sample_need_poly <- needs_all %>%
  select(all_of(desired_cols)) %>% 
  filter(FACTS_ID %in% desired_ids) 

plot(CL_sample_need_poly)

## project ---- 
CL_sample_need_poly <- project(CL_sample_need_poly, "EPSG:5070")

## divide ----
# calc area for entire polygon
CL_sample_need_poly$area_all <- expanse(CL_sample_need_poly) * 0.000247105
  # 49637 acres
49637/200 # 248 (rough number of parts)

set.seed(100)
CL_PPUs_vect <- divide(CL_sample_need_poly, n = 250)
  # 250 geoms

# add ID 
CL_PPUs_vect$PPU_ID <- 1:nrow(CL_PPUs_vect)

# new area of divided polys
CL_PPUs_vect$area_acres <- expanse(CL_PPUs_vect) * 0.000247105
summary(CL_PPUs_vect$area_acres)
# min = 24.84    , max = 663.06  


## add EBs ----
# using the EBs created in part 1-6.c
  # the ARP_DEM_vect was created in part 1-3.b
ARP_DEM_EB_rast <- rast("ARP_DEM_EB_rast.tif")

# extract max
EB_max_df <- extract(ARP_DEM_EB_rast, CL_PPUs_vect, fun=max)
str(EB_max_df)
# rename col
EB_max_df <- EB_max_df %>% 
  rename(PPU_ID = ID,
         EB_max = USGS_1_n41w106_20230314)

# extract min
EB_min_df <- extract(ARP_DEM_EB_rast, CL_PPUs_vect, fun=min)
str(EB_min_df)
# rename col
EB_min_df <- EB_min_df %>% 
  rename(PPU_ID = ID,
         EB_min = USGS_1_n41w106_20230314)


EB_join_df <- left_join(EB_min_df, EB_max_df, by = "PPU_ID")

CL_PPUs_vect <- CL_PPUs_vect %>% 
  left_join(EB_join_df, by = "PPU_ID")

### write & read ----
writeVector(CL_PPUs_vect, "CL_PPUs_vect.shp")
CL_PPUs_vect <- vect("CL_PPUs_vect.shp")

CL_PPUs_df <- as.data.frame(CL_PPUs_vect)

## select ----
  # for the case study, we are only going to use the planting needs (PPUs)
    # that are within the 8500 - 9000 ft EB
CL_PPU_8500_9000_vect <- CL_PPUs_vect %>% 
  filter(EB_min == 8500, EB_max == 9000)
  # 13 geoms

### write & read ----
writeVector(CL_PPU_8500_9000_vect, "CL_PPU_8500_9000_vect.shp")
CL_PPU_8500_9000_vect <- vect("CL_PPU_8500_9000_vect.shp")


# that are within the 9000 - 9500 ft EB
CL_PPU_9000_9500_vect <- CL_PPUs_vect %>% 
  filter(EB_min == 9000, EB_max == 9500)
# 20 geoms

### write & read ----
writeVector(CL_PPU_9000_9500_vect, "CL_PPU_9000_9500_vect.shp")
CL_PPU_9000_9500_vect <- vect("CL_PPU_9000_9500_vect.shp")

sum(CL_PPU_9000_9500_vect$area_acres) # 3673.813 acres








