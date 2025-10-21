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

# the Area of Interest (AOI) for this case study is the Arapaho-Roosevelt National Forest (ARP)
## ARP ----
### load & process ----
NF_CONUS_vect <- vect("S_USA.FSCommonNames.shp")

plot(NF_CONUS_vect)

# see unique names 
unique(NF_CONUS_vect$COMMONNAME)

# select for just ARP 
ARP_vect <- NF_CONUS_vect %>%
  filter(COMMONNAME == "Arapaho and Roosevelt National Forests")

plot(ARP_vect)

# project 
ARP_vect <- project(ARP_vect,"EPSG:5070")

### write & read ----
writeVector(ARP_vect, "ARP_vect.shp")
ARP_vect <- vect("ARP_vect.shp")

## reference points ----
# we will add these 2 points as references, they represent Fort Collins and Boulder

### write & read ----
CO_refs_vect <- vect("CO_refs_vect.shp")

### viz ----
plot(ARP_vect)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)


# (3) pre-process data ----

## (3.a) risk ----
# using flame length exceedance probability of 8 ft (FLEP8)
# as our proxy for risk of local extinction due to high severity wildfire
# cell values = probability of flame length > 8 ft

### * need to fill in----
  # and/or modify to be CFP instead...

### write & read file ----
ARP_risk_score_rast <- rast("ARP_risk_score_rast.tif")

### viz ----
plot(ARP_risk_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

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

### write & read file ----
writeRaster(ARP_risk_score_rast, "ARP_risk_score_rast.tif")
ARP_risk_score_rast <- rast("ARP_risk_score_rast.tif")

### viz ----
plot(ARP_risk_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

### stats ----
global(ARP_risk_score_rast, fun = "notNA") # 7776004 cells 
all_risk_count <- sum(ARP_risk_score_rast[] >= 0, na.rm = TRUE) # 7776004 cells

7776004*900 # = 6998403600 m^2
6998403600/4046.86 # = 1729342 acres (the entire ARP)


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
ARP_slope = terrain(ARP_DEM, v="slope", neighbors=8, unit="degrees")
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

# calc inverse
ARP_slope_score_rast <- (1 - slope_norm)
plot(ARP_slope_score_rast)
plot(is.na(ARP_slope_score_rast))

### write & read ----
writeRaster(ARP_slope_score_rast, "ARP_slope_score_rast.tif")
ARP_slope_score_rast <- rast("ARP_slope_score_rast.tif")

### viz ----
plot(ARP_slope_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

### stats ----
global(ARP_slope_score_rast, fun = "notNA") # 7433981 cells 
sum(ARP_slope_score_rast[] >= 0, na.rm = TRUE) # 7433981 cells

7433981*900 # = 6690582900 m^2
6690582900/4046.86 # = 1653278 acres (all areas < 24* slope)

# entire ARP = 7776004 cells & 1729342 acres
(7433981/7776004)*100 # 95.60156 % remaining after 24* filter 
(1653278/1729342)*100 # 95.60156 %

## (3.c) road ----

# import CO roads shapefile
  # downloaded from The National Map
roads_CONUS <- vect("Trans_RoadSegment_0.shp")
plot(roads_CONUS)
road_table <- as.data.frame(roads_CONUS)
crs(roads_CONUS) # EPSG 4269

# project, crop & mask 
roads_CONUS = project(roads_CONUS, EVH_CO)
crs(roads_CONUS) # EPSG 5070

roads_CO = crop(roads_CONUS, CO, mask=TRUE)
plot(Roads_CO)

### rasterize ----
CO_road_rast <- rasterize(roads_CO, EVH, touches=TRUE)
plot(CO_road_rast, col="blue") # all values = 1
plot(is.na(CO_road_rast)) # values not 1 are NA
# TBH, the raster does not look nearly as contiguous as the road lines from the .shp
  # but when I open the .tif in Arc, it looks fine 
  # I think it is too much for R studio to render with plot()

### write & read file ----
CO_road_rast <- rast("CO_road_rast.tif") 
plot(CO_road_rast, col = "blue")

# crop & mask 
# here we use the ARP_vect polygon to crop (and mask) only the area we want from the raster
ARP_road_rast <- crop(CO_road_rast, ARP_vect, mask=TRUE)
plot(ARP_road_rast, col = "blue")

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

# make all values > 917.3261 degrees NA, leave other values as-is
road_filtered <- ifel(ARP_road_dist_rast > 917.3261, NA, ARP_road_dist_rast)
plot(road_filtered)

# normalize scale
road_norm <- road_filtered / 917.3261
plot(road_norm)

# calc inverse
ARP_road_score_rast <- (1 - road_norm)
plot(ARP_road_score_rast)
plot(is.na(ARP_road_score_rast))

### viz ----
plot(ARP_road_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

### stats ----
global(ARP_road_score_rast, fun = "notNA") # 5850375 cells 
sum(ARP_road_score_rast[] >= 0, na.rm = TRUE) # 5850375 cells

5850375*900 # = 5265337500 m^2
5265337500/4046.86 # = 1301092 acres (all areas < 0.57 miles from roads)

# entire ARP = 7776004 cells & 1729342 acres
(5850375/7776004)*100 # 75.23627 % remaining after 24 degree filter 
(1301092/1729342)*100 # 75.23627 %

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
    (EVH_ARP - 100) * meters_to_feet_factor > 20, # subtract offset, convert units, filter
    100, # if TRUE, reclassify to 100
    NA # if FALSE, reclassify to NA
  ),
  NA # if not a tree value (condition 1 = FALSE), reclassify to NA
)

### stats ----
# entire ARP = 7776004 cells & 1729342 acres

# all veg area
sum(EVH_ARP[] >= 100, na.rm = TRUE) # 7004697 cells

7004697*900 # = 6304227300 m^2
6304227300/4046.86 # = 1557807 acres (all veg type in ARP)
(1557807/1729342)*100 # 90.08091 % of ARP is vegetated 

# all tree area
sum(EVH_ARP[] >= 100 & EVH_ARP[] < 200, na.rm = TRUE) # 5324379 cells

5324379*900 # = 4791941100 m^2
4791941100/4046.86 # = 1184113 acres (all trees in ARP)
(1184113/1729342)*100 # 68.47188 % of ARP has trees 

# trees > 10 ft area
sum(ARP_height_score_rast[] == 100, na.rm = TRUE) # 4933551 cells

4933551*900 # = 4440195900 m^2
4440195900/4046.86 # = 1097195 acres (all trees in ARP > 10 ft)
(1097195/1729342)*100 # 63.44581 % of ARP has trees


### viz ----
plot(ARP_height_score_rast, col = "forestgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

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
  QMD_ARP >= 6, 50, NA 
)
# if >= 6 inches, reclassify to 50
# if < 6 inches, reclassify to NA


### stats ----
# entire ARP = 7776004 cells & 1729342 acres
# all areas with QMD values
sum(QMD_ARP[] >= 0, na.rm = TRUE) # 5697616 cells 
5697616*900 # 5127854400 meters squared
5127854400/4046.86 # = 1267119 acres (all area with QMD values in ARP)
(1267119/1729342)*100 # 73.27174 % of ARP has QMD values

# areas with QMD > 6 inches
sum(ARP_diameter_score_rast[] == 50, na.rm = TRUE) # 3183744 cells

3183744*900 # = 2865369600 m^2
2865369600/4046.86 # = 708047.6 acres (all trees in ARP > 6 in QMD)
(708047.6/1729342)*100 # 40.94318 % of ARP has trees > 6 in QMD


### viz ----
# see classified values
plot(ARP_diameter_score_rast, col = "darkgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

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
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

# the combined_raster extends a bit beyond the ARP boundary
  # crop & mask again
ARP_combined_rast <- crop(combined_raster, ARP_vect, mask = TRUE)

plot(ARP_combined_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

## stats ----
# want to know how many cells (and how much area) falls into each category
  # ca 0: all values not NA
global(ARP_combined_rast, fun = "notNA") # 7,775,581 cells 
7775581*900 # 6998022900 m^2
6998022900/4046.86 # 1729248 acres (meet at least 1 of the priority factory)
(1729248/1729342)*100 # 99.99456 % of ARP 

# cat 1: values 0-3
    # slope, road, and risk combined & continuous values
global(ARP_combined_rast >= 0 & ARP_combined_rast <= 3, fun = "sum", na.rm = TRUE) # 2469181 cells
2469181*900 # 2222262900 m^2
2222262900/4046.86 # 549132.6 acres (only meets slope, road, and/or risk)
(549132.6/1729342)*100 # 31.75385 % of ARP 

# cat 2: values 50-53
  # just meet's QMD threshold
global(ARP_combined_rast >= 50 & ARP_combined_rast <= 53, fun = "sum", na.rm = TRUE) # 372849 cells
372849*900 # 335564100 m^2
335564100/4046.86 # 82919.62 acres (QMD and cat 1)
(82919.62/1729342)*100 # 4.794865 % of ARP 

# cat 3: values 100-103
  # just meet's EVH threshold
global(ARP_combined_rast >= 100 & ARP_combined_rast <= 103, fun = "sum", na.rm = TRUE) # 2122656 cells
2122656*900 # 1910390400 m^2
1910390400/4046.86 # 472067.3 acres (EVH and cat 1)
(472067.3/1729342)*100 # 27.29751 % of ARP 

# cat 4: values 150-153
  # meet both QMD and EVH thresholds
global(ARP_combined_rast >= 150 & ARP_combined_rast <= 153, fun = "sum", na.rm = TRUE) # 2810895 cells
2810895*900 # 2529805500 m^2
2529805500/4046.86 # 625128 acres (QMD & EVH & cat 1)
(625128/1729342)*100 # 36.14832 % of ARP

## QC ----
# oh fuck, i think I messed up with global() and need to revisit this
global(ARP_combined_rast >= 150 & ARP_combined_rast <= 153, fun = "notNA") # 7,775,581 cells
global(ARP_combined_rast >= 150 & ARP_combined_rast <= 153, fun = "sum", na.rm = TRUE) # 2,810,895 cells


freq(ARP_combined_rast)
  # just add values 150-153
154179+1331187+1273782+51747 # 2810895
sum(ARP_combined_rast[] >= 150 & ARP_combined_rast[] <= 153, na.rm = TRUE) # 2810895


31.75385+4.794865+27.29751+36.14832 # 99.99455 - it adds up! 

## filter & rescale ----
# final values can be 0-3 for ease of interpretation 

ARP_priority_rast <- ifel(
  ARP_combined_rast >= 150 & ARP_combined_rast <= 153,
  ARP_combined_rast - 150, NA
) %>% 
  rename(priority_s = sum)

# just confirm filter
global(ARP_priority_rast, fun = "notNA") # 2810895 cells (same as Cat 4 ^)


## viz ----
plot(ARP_priority_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

## write & read ----
writeRaster(ARP_priority_rast, "ARP_priority_rast.tif")
ARP_priority_rast <- rast("ARP_priority_rast.tif")


## stats V1 ----
# see frequency of values
freq(combined_rast)
# value   count
# 100   73573    - these cells score 0 for risk, slope, and road (min requirements met)
# 101 1456051
# 102 2032222
# 103  199972    - these cells score high for risk, slope, and road (highest priority)

# how large of an area is this?
73573+1456051+2032222+199972 # = 3761818 cells with values (min requirements met)
3761818*900 # = 3385636200 square meters
3385636200/4046.86 # 4046.86 m/acre = 836608.2 acres
(836608.2/1729342)*100 # 48.37726 % of ARP

# note, all these areas meet our basic criteria (3.a - 3.d)
# however, it is still too large of an area to scout
# so, we must narrow it down further (part 1-5)



# (5) make PCUs V2 ----

# we need feasible (small) units to send the scouting crew
# so we will filter the data and make "patches" of high-scoring areas
# we call these Potential Collection Units (PCUs)


## filter ----
priority_filtered <- ifel(
  ARP_priority_rast < 2, NA, ARP_priority_rast
)

# sanity checks
freq(priority_filtered)
409564+51747 # 461311
global(priority_filtered, fun = "notNA") # 461311 cells
sum(priority_filtered[] >= 0, na.rm = TRUE) # 461311

461311*900 # 415179900 m^2
415179900/4046.86 # 102593.1 acres
(102593.1/1729342)*100 # 5.932493 % of ARP

plot(priority_filtered)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

## patches ----
# in this step, we convert connected raster cells (with values) into "patches"
priority_patches <- patches(priority_filtered, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
  # there are 68275 patches

plot(priority_patches)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

## make polygons ----
patch_polys <- as.polygons(priority_patches, values = FALSE)
  # there are 68275 geometries 

## separate sizes ----
# calc area 
patch_polys$area_acres <- expanse(patch_polys) * 0.000247105

# filt out small poys (< 20 acres)
small_polys_removed <- patch_polys[patch_polys$area_acres >= 20, ]

# separate large polys ( > 200 acres) from others (20-200 acres)
small_and_mid_polys <- small_polys_removed[small_polys_removed$area_acres <= 200, ]
large_polys <- small_polys_removed[small_polys_removed$area_acres > 200, ]

## divide ----
# calculate divisions needed, ensuring at least 2 parts for large polys
num_parts <- pmax(2, round(large_polys$area_acres / 125))

# Use lapply to iterate and divide
divided_polys_list <- lapply(1:nrow(large_polys), function(i) {
  poly <- large_polys[i, ]
  
  # Set a seed to ensure reproducibility for the division process
  set.seed(i)
  
  divided_poly <- divide(poly, n = num_parts[i])
  
  # Store the original ID and re-calculate the new areas
  divided_poly$original_id <- poly$patch_ID
  divided_poly$area_acres <- expanse(divided_poly) * 0.000247105
  
  return(divided_poly)
})

# Combine all divided polys into a single SpatVector
divided_polys_vect <- do.call(rbind, divided_polys_list)

# Combine the small/mid-sized polys with the newly divided large polys
ARP_PCUs_vect <- rbind(small_and_mid_polys, divided_polys_vect)

summary(ARP_PCUs_vect)
# min = 20.02


# (5) make PCUs V1 ----

# we need feasible (small) units to send the scouting crew
# so we will filter the data and make "patches" of high-scoring areas
# we call these Potential Collection Units (PCUs)

## (5.a) filter 102 ----
ARP_PCU_102_rast <- combined_rast %>% 
  filter(score >= 102)

# inspect
plot(ARP_PCU_102_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

freq(ARP_PCU_102_rast)
# value  count
# 102 685213
# 103 199972

### write & read ----
# writeRaster(ARP_PCU_102_rast, "ARP_PCU_102_rast.tif")
# ARP_PCU_102_rast <- rast("ARP_PCU_102_rast.tif")

## (5.a) filter 103 ----
ARP_PCU_103_rast <- combined_rast %>% 
  filter(score >= 102.5)

# inspect
plot(ARP_PCU_103_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

freq(ARP_PCU_103_rast)
# value  count
# 103 199972

### write & read ----
# writeRaster(ARP_PCU_103_rast, "ARP_PCU_103_rast.tif")
# ARP_PCU_103_rast <- rast("ARP_PCU_103_rast.tif")

#### feedback ----
# this filtering process is very subjective
# somehow we need to narrow down our search areas
# any alternative ideas for doing so?



## (5.b) patches ----
# in this step, we convert dense raster cells (with values) into "patches"
ARP_PCU_patches <- patches(ARP_PCU_102_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)

plot(ARP_PCU_patches)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)
# looks pretty much the same, just the values are different

### explore ----
# with a filter of >103, there are 47191  patches 
# with a filter of >102, there are 112875 patches 

patch_sizes <- freq(ARP_PCU_patches)
patches_box <- boxplot(ARP_PCU_patches) # lots of low values

## (5.c) remove small patches ----
# we want to remove patches smaller than 20 acres
  # 20 acres = 80937.13 meters squared
  # 1 pixel of 30x30 meters = 900 meters squared
80937.13/900 # 89.93014 = number of pixels 

### need to change this threshold in next run though 
# 225 cells ~ 50 acres
small_patches <- patch_sizes %>% filter(count <= 225) %>% select(value) 
# with a filter of >103, there are 47131 rows (patches)
47191-47131 # = 60 patches > 50 acres

# with a filter of >102, there are 112439 rows (patches)
112875-112439 # = 436 patches > 50 acres

# so, the vast majority of patches are small 

### classify ----
ARP_PCU_patches_classified <- classify(ARP_PCU_patches, rcl = cbind(small_patches, NA))
plot(ARP_PCU_patches_classified)

### explore ----
ARP_PCU_patches_freq <- freq(ARP_PCU_patches_classified) 
# confirmed, with a filter of >103, there are 60 patches
# confirmed, with a filter of >102, there are 436 patches

ARP_PCU_cells <- sum(ARP_PCU_patches_freq$count) 
# with a filter of >103, there are 39565 cells
39404*900 # = 35463600 m^2
35463600/4046.86 # = 8763.239 acres
# before filtering out small patches, the total area of the combined raster was 836608.2 acres
(8763.239/836608.2)*100 # = 1.1 % of total remains for scouting 

# with a filter of >102, there are x cells
387005*900 # = 348304500 m^2
348304500/4046.86 # = 86067.84 acres
# before filtering out small patches, the total area of the combined raster was 836608.2 acres
(86067.84/836608.2)*100 # = 10.3 % of total remains for scouting

# the less intense filtering (of >102) yields nearly 10x as much area to include in scouting

plot(ARP_PCU_patches_classified)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

## (5.d) polygons ----
# in this step, we convert the above "patches" into polygons for ease of map making & calculating stats
ARP_PCU_big_vect <- as.polygons(ARP_PCU_patches_classified, values = FALSE) 
# with a filter of >103, this vect has 60 geoms (polys)
# with a filter of >102, this vect has 436 geoms (polys)

# the vect has no attributes, so will add an "original" ID attribute to correspond with the patch number
ARP_PCU_big_vect$patch_ID <- 1:nrow(ARP_PCU_big_vect)

plot(ARP_PCU_big_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

## (5.e) divide polygons ----
### area ----
# calc
area_sqm <- expanse(ARP_PCU_big_vect, transform = FALSE)
# transform = F bc already in an equal area projection 
# convert units
area_acres <- area_sqm * 0.000247105
# 1 square meter = 0.000247105 acres
# add as attribute
ARP_PCU_big_vect$area_acres <- area_acres
print(ARP_PCU_big_vect)
ARP_PCU_big_df <- as.data.frame(ARP_PCU_big_vect)
# polygon areas range from 50-3490 acres

### divide ----
# we want all polys to be of "operational size",
# which we have set as between 50-200 acres

# select for polys already within 50-200 acres & polys > 200
small_polys <- ARP_PCU_big_vect[ARP_PCU_big_vect$area_acres <= 200, ] # 339 geoms
large_polys <- ARP_PCU_big_vect[ARP_PCU_big_vect$area_acres > 200, ] # 97 geoms

#### loop ----
# create a list to store divided polys
div_polys_list <- list()

# iterate through each large_poly & add to list
for (i in 1:nrow(large_polys)) {
  poly <- large_polys[i, ]
  original_id <- poly$patch_ID # store the original patch_ID as an attribute for divided polys
  
  # calc the number of parts needed
  # we want a division that yields polygons around 100-150 acres (mid-range)
  min_target_area <- 125 # acres
  max_parts <- floor(poly$area_acres / 50) # ensure each poly is at least 50 acres
  min_parts <- ceiling(poly$area_acres / 200) # ensure each poly is not > 200 acres
  
  num_parts <- round(poly$area_acres / min_target_area)
  
  # adjust num_parts to be within the min_parts and max_parts bounds
  num_parts <- max(min_parts, num_parts)
  num_parts <- min(max_parts, num_parts)
  
  # Ensure at least 2 parts if the polygon is large enough to be split
  if (num_parts < 2 && poly$area_acres > 200) {
    num_parts <- 2
  }
  
  # use terra::divide()
  # set diff seed for each poly to be divided
  set.seed(i)
  
  divided_poly <- divide(poly, n = num_parts)
  
  # add the original ID to the new divided polys
  divided_poly$original_id <- original_id
  
  # re-calc area 
  divided_poly$area_acres_new <- expanse(divided_poly, transform = FALSE) * 0.000247105
  
  # store the new divided polys in the list
  div_polys_list[[i]] <- divided_poly
  
}

#### combine ----
# combine all the split polys into a single spatvector
if (length(div_polys_list) > 0) {
  div_polys_vect <- do.call(rbind, div_polys_list)
} else {
  div_polys_vect <- NULL
}

div_polys_vect # has 439 geoms (new divided polys)

# combine the original small_polys with the divided polys 
if(!is.null(div_polys_vect)) {
  ARP_PCUs_vect <- rbind(small_polys, div_polys_vect)
} else {
  ARP_PCUs_vect <- small_polys
}

ARP_PCUs_vect # has 778 geoms (339 original small + 439 new divided)

#### verify ----
# check the area again
ARP_PCUs_vect$area_acres_final <- expanse(ARP_PCUs_vect, transform = FALSE) * 0.000247105
summary(ARP_PCUs_vect$area_acres_final) # min = 41, max = 225 acres (close enough)
hist(ARP_PCUs_vect$area_acres_final)

# check if total area is preserved
sum(ARP_PCU_big_vect$area_acres) # 86067.78 acres
sum(ARP_PCUs_vect$area_acres_final) # 86067.78 - same! 

# adjust attributes 
# add new ID col
ARP_PCUs_vect$PCU_ID <- 1:nrow(ARP_PCUs_vect)
# select only final area and new ID
ARP_PCUs_vect <- ARP_PCUs_vect[, c("PCU_ID", "area_acres_final")]
# rename the area col
ARP_PCUs_vect <- ARP_PCUs_vect %>% 
  rename(area_acres = area_acres_final)

ARP_PCU_df <- as.data.frame(ARP_PCUs_vect)


### write & read ----
writeVector(ARP_PCUs_vect, "ARP_PCUs_vect.shp")
ARP_PCUs_vect <- vect("ARP_PCUs_vect.shp")


# (6) add attributes ----
# in this step, we add attributes (metadata) to our polygons
# this is how we further filter & select PCUs for scouting

##
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
DEM_ARP_ft <- DEM_ARP * meters_to_feet_factor # min = 4970.553 , max = 14250.245 

# create a matrix 
elev.matrix <- matrix(c(seq(4000, 14000, 500), seq(4500, 14500, 500), seq(4500, 14500, 500)), ncol=3, byrow = FALSE)

# classify the DEM
DEM_ARP_EBs <- classify(DEM_ARP_ft, rcl=elev.matrix, right=TRUE) # right = T ensures upper bound of interval included in band 
plot(DEM_ARP_EBs)
unique(DEM_ARP_EBs) # 20 unique values (EBs)

### write & read ----
writeRaster(DEM_ARP_EBs, "DEM_ARP_EBs.tif") 
DEM_ARP_EBs <- rast("DEM_ARP_EBs.tif")

### extract max ----
extract_EB_max <- extract(DEM_ARP_EBs, ARP_PCUs_vect, fun=max)
str(extract_EB_max)
# rename col
extract_EB_max <- extract_EB_max %>% 
  rename(EB_max = USGS_1_n41w106_20230314)

### extract min ----
extract_EB_min <- extract(DEM_ARP_EBs, ARP_PCUs_vect, fun=min)
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
## (6.p) combine ----
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




# (6) make maps ----
# here we would use ggplot2 to make fancy maps and/or export to Arc


















