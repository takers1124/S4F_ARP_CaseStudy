# description ----

# Seeds 4 the Future ARP Case Study, the full script
  # part 1, create Potential Collection Units (PCUs)

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project
  # as well as information about where to download raw data

# (1) setup ----

library(terra) 
library(tidyterra) 
library(dplyr)
library(ggplot2) 


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

#### write & read ----
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

#### write & read ----
writeVector(SRME_vect, "SRME_vect.shp")
SRME_vect <- vect("SRME_vect.shp")


# (3) pre-process data ----

## QMD ----
# this is using quadratic mean diameter (QMD) from TreeMap 2022
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
# already in 5070
plot(QMD_CONUS)

### crop and mask ----
ARP_QMD_rast <- crop(QMD_CONUS, ARP_vect, mask=TRUE)
plot(ARP_QMD_rast)

#### write & read ----
writeRaster(ARP_QMD_rast, "ARP_QMD_rast.tif")
ARP_QMD_rast <- rast("ARP_QMD_rast.tif")

global(ARP_QMD_rast, fun = "notNA") # 5697616 cells

# reclassify with ifel()
ARP_QMD_filt_rast <- ifel(
  QMD_ARP_rast >= 5, 5, NA 
)
# if >= 5 inches, reclassify to 5
# if < 5 inches, reclassify to NA

### viz ----
# see classified values
plot(ARP_QMD_filt_rast, col = "darkgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(ARP_QMD_filt_rast, "ARP_QMD_filt_rast.tif")
ARP_QMD_filt_rast <- rast("ARP_QMD_filt_rast.tif")



## EVH ----
# using existing vegetation height (EVH) from LANDFIRE
  # these values are not continuous
  # also the veg height has an offset added
    # e.g. value 103 = tree height of 3 meters

EVH_CONUS <- rast("LC24_EVH_250.tif")
crs(EVH_CONUS) # 5070
res(EVH_CONUS) # 30 30

### crop / mask ----
EVH_ARP <- crop(EVH_CONUS, ARP_vect, mask=TRUE)
plot(EVH_ARP)

levels_EVH <- levels(EVH_ARP)
is.factor(EVH_ARP) # TRUE

### all treed area ----
  # EVH value = 101 = tree height 1 meter
  # EVH value = 201 = shrub height 0.01 meter
ARP_EVH_rast <- ifel(
  EVH_ARP >= 101 & EVH_ARP < 200,
  EVH_ARP, NA
)

plot(ARP_EVH_rast)
global(ARP_EVH_rast, fun = "notNA") # 5311714

#### write & read ----
writeRaster(ARP_EVH_rast, "ARP_EVH_rast.tif")
ARP_EVH_rast <- rast("ARP_EVH_rast.tif")

### filter ----
# define conversion factor
meters_to_feet_factor <- 3.28084

# reclassify with ifel()
ARP_EVH_filt_rast <- ifel(
  # condition 1: it is dominant veg type trees? (values 101-199)
  EVH_ARP >= 101 & EVH_ARP < 200,
  # if TRUE, 
  # condition 2: is it > 10 ft tall? 
  ifel(
    (EVH_ARP - 100) * meters_to_feet_factor > 10, # subtract offset, convert units, filter
    10, # if TRUE, reclassify to 10
    NA # if FALSE, reclassify to NA
  ),
  NA # if not a tree value (condition 1 = FALSE), reclassify to NA
)

### viz ----
plot(ARP_EVH_filt_rast, col = "forestgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

#### write & read ----
writeRaster(ARP_EVH_filt_rast, "ARP_EVH_filt_rast.tif")
ARP_EVH_filt_rast <- rast("ARP_EVH_filt_rast.tif")



## slope ----
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
ARP_DEM <- mosaic(DEM_n41_w106, DEM_n41_w107, DEM_n40_w106, DEM_n40_w107, fun = "first")
# project
ARP_DEM <- project(ARP_DEM, "EPSG:5070")

# crop and mask the DEM to the extent of ARP 
ARP_DEM_rast <- crop(ARP_DEM, ARP_vect, mask=TRUE)
plot(ARP_DEM_rast) # min = 1470.285 , max = 4393.409 (meters)
plot(is.na(ARP_DEM_rast))

#### write & read ----
writeRaster(ARP_DEM_rast, "ARP_DEM_rast.tif")
ARP_DEM_rast <- rast("ARP_DEM_rast.tif")

### calc slope ----
ARP_slope_rast = terrain(ARP_DEM_rast, v="slope", unit="degrees")
plot(ARP_slope_rast)

#### write & read ----
writeRaster(ARP_slope_rast, "ARP_slope_rast.tif")
ARP_slope_rast <- rast("ARP_slope_rast.tif")

### adjust values ----
minmax(ARP_slope_rast) 
# min = 0, max = 72.59397 
  # but the max we want to include is 24 degrees
  # and we want 0-24 degree slope to become 0-1 score (normalize)

# make all values > 24 degrees NA, make all other values 100
ARP_slope_filt_rast <- ifel(ARP_slope_rast > 24, NA, 100)

### viz ----
plot(ARP_slope_filt_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

plot(is.na(ARP_slope_filt_rast))

#### write & read ----
writeRaster(ARP_slope_filt_rast, "ARP_slope_filt_rast.tif")
ARP_slope_filt_rast <- rast("ARP_slope_filt_rast.tif")



## road ----

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

#### write & read file ----
writeRaster(ARP_road_rast, "ARP_road_rast.tif")
ARP_road_rast <- rast("ARP_road_rast.tif") 

### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
ARP_road_dist_rast <- distance(ARP_road_rast) 
plot(ARP_road_dist_rast)
  # cell values = distance to nearest road (in meters)

#### write & read file ----
writeRaster(ARP_road_dist_rast, "ARP_road_dist_rast.tif")
ARP_road_dist_rast <- rast("ARP_road_dist_rast.tif")

### adjust values ----
minmax(ARP_road_dist_rast) 
# min = 0, max = 37416.17 
  # but the max we want to include is 917.3261 meters (0.57 miles)
  # if < threshold, make value 500

# make NA all values > 917.3261 meters, make others 500
ARP_road_filt_rast <- ifel(ARP_road_dist_rast > 917.3261, NA, 500)
plot(ARP_road_filt_rast)

### crop ----
# need to crop again bc the road distance buffer goes a bit outside of the ARP
ARP_road_filt_rast = crop(ARP_road_filt_rast, ARP_vect, mask = TRUE)

### viz ----
plot(ARP_road_filt_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1)
plot(is.na(ARP_road_filt_rast))

#### write & read ----
writeRaster(ARP_road_filt_rast, "ARP_road_filt_rast.tif")
ARP_road_filt_rast <- rast("ARP_road_filt_rast.tif")



# (4) combine data ----

## resample ----
# first, the rasters need to be resampled so their extents align,
  # and they have matching resolutions and origins

# 3 of the 4 rasters have matching resolutions (QMD, EVH, and road)
# 2 of the 4 rasters have matching extents (QMD, EVH)
# 1 of the 4 rasters has no matching resolution or extent (slope)
  # I am choosing EVH to use as the template
  
slope_resampled <- resample(ARP_slope_filt_rast, ARP_EVH_filt_rast, method = "near")
road_resampled <- resample(ARP_road_filt_rast, ARP_EVH_filt_rast, method = "near")

raster_list <- list(ARP_EVH_filt_rast,
                    ARP_QMD_filt_rast,
                    slope_resampled,
                    road_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
  # has 4 layers, each has same extent and resolution

## sum ----
ARP_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
  # has values: min = 5, max = 615

## viz ----
plot(ARP_combined_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

plot(is.na(ARP_combined_rast))


## stats ----
# we want to know what % of the ARP each priority factor (PF) & combo occupies
  # need a total # cells in the ARP to compare
global(ARP_DEM_rast, fun = "notNA") # 6282487 cells (covers all ARP)
  # but not same resolution as rest of data
DEM_resampled <- resample(ARP_DEM_rast, ARP_EVH_filt_rast, method = "bilinear")
  # now has same "standard" resolution and extent (see above)
global(DEM_resampled, fun = "notNA") # 7773990 cells (covers all ARP)


### independent PFs  ----
#### QMD ----
# all areas with QMD values
global(ARP_QMD_rast, fun = "notNA") # 5697616 cells
(5697616/7773990)*100 # 73.29076 % of ARP has QMD values

# areas with QMD > 5 inches
global(ARP_QMD_filt_rast, fun = "notNA") # 4160703
(4160703/7773990)*100 # 53.52082 % of ARP has trees > 5 in QMD


#### EVH ----
# all veg area
global(EVH_ARP >= 101, fun = "sum", na.rm = TRUE) # 7001131 cells
(7001131/7773990)*100 # 90.0584 % of ARP is vegetated 

# all tree area
global(ARP_EVH_rast, fun = "notNA") # 5311714
(5311714/7773990)*100 # 68.32674 % of ARP has trees 

# trees > 10 ft area
global(ARP_EVH_filt_rast, fun = "notNA") # 5231674
(5231674/7773990)*100 # 67.29715 % of ARP has trees > 10 ft


#### slope ----
  # need to use resampled version (above) to get same resolution and extent
global(slope_resampled, fun = "notNA") # 6282487 cells 
(6282487/7773990)*100 # 80.81419 % remaining after 24* filter

#### road ----
# need to use resampled version (above) to get same extent
global(road_resampled, fun = "notNA") # 5213776 cells 
# entire ARP = 7773990 cells 
(5213776/7773990)*100 # 67.06692 % remaining 


## combined PFs ----
# we want to know what % of the ARP each category falls into after combining

# value 5, QMD only
global(ARP_combined_rast == 5, fun = "sum", na.rm = TRUE) # 25120 cells
(25120/7773990)*100 # 0.3231288 % of ARP

# value 10, EVH only
global(ARP_combined_rast == 10, fun = "sum", na.rm = TRUE) # 90042 cells
(90042/7773990)*100 # 1.158247 % of ARP

# value 15, QMD + EVH
global(ARP_combined_rast == 15, fun = "sum", na.rm = TRUE) # 188102 cells
(188102/7773990)*100 # 2.419633 % of ARP

# value 100, slope only
global(ARP_combined_rast == 100, fun = "sum", na.rm = TRUE) # 601784 cells
(601784/7773990)*100 # 7.740993 % of ARP

# value 105, slope + QMD
global(ARP_combined_rast == 105, fun = "sum", na.rm = TRUE) # 89504 cells
(89504/7773990)*100 # 1.151326 % of ARP

# value 110, slope + EVH
global(ARP_combined_rast == 110, fun = "sum", na.rm = TRUE) # 411384 cells
(411384/7773990)*100 # 5.2918 % of ARP

# value 115, slope + EVH + QMD
global(ARP_combined_rast == 115, fun = "sum", na.rm = TRUE) # 835573 cells
(835573/7773990)*100 # 10.74832 % of ARP

# value 500, road only
global(ARP_combined_rast == 500, fun = "sum", na.rm = TRUE) # 211065 cells
(211065/7773990)*100 # 2.715015 % of ARP

# value 505, road + QMD
global(ARP_combined_rast == 505, fun = "sum", na.rm = TRUE) # 51904 cells
(51904/7773990)*100 # 0.6676623 % of ARP

# value 510, road + EVH
global(ARP_combined_rast == 510, fun = "sum", na.rm = TRUE) # 165873 cells
(165873/7773990)*100 # 2.133692 % of ARP

# value 515, road + EVH + QMD
global(ARP_combined_rast == 515, fun = "sum", na.rm = TRUE) # 440692 cells
(440692/7773990)*100 # 5.668801 % of ARP

# value 600, road + slope
global(ARP_combined_rast == 600, fun = "sum", na.rm = TRUE) # 990481 cells
(990481/7773990)*100 # 12.74096 % of ARP

# value 605, road + slope + QMD
global(ARP_combined_rast == 605, fun = "sum", na.rm = TRUE) # 253753 cells
(253753/7773990)*100 # 3.264128 % of ARP

# value 610, road + slope + EVH
global(ARP_combined_rast == 610, fun = "sum", na.rm = TRUE) # 823953 cells
(823953/7773990)*100 # 10.59884 % of ARP

# value 615, road + slope + QMD + EVH
global(ARP_combined_rast == 615, fun = "sum", na.rm = TRUE) # 2276055 cells
(2276055/7773990)*100 # 29.27782 % of ARP

# value notNA
global(ARP_combined_rast, fun = "notNA") # 7455285 cells
(7455285/7773990)*100 # 95.90037 % of ARP (equals the sum of above %s)
100-95.90037 # 4.09963 % is NA (QMD < 5in, EVH < 10ft, slope >24, road >0.57)



## filter & adjust value ----
  # make cell value = 1 for all areas that meet our PFs & value = NA if not
ARP_priority_rast <- ifel(
  ARP_combined_rast == 615,
  1, NA)

# just confirm filter
global(ARP_priority_rast, fun = "notNA") # 2276055 cells (same as value=615 above)
(2276055/7773990)*100 # 29.27782 % of ARP

## calc area ---- 
  # transform = FALSE bc already an equal-area projection, EPSG: 5070, Conus Albers
  # default units are m^2
expanse(ARP_priority_rast, transform = FALSE) # 2048449500 m^2
2048449500/4046.86 # 4046.86 m2/acre = 506182.4 acres
  # entire ARP = 1723619 acres (calculated from ARP_vect polygon in Part1A_2)
(506182.4/1723619)*100 # 29.36742 % of ARP (same as value=615 above)

## viz ----
plot(ARP_priority_rast, col = "darkgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)

### write & read ----
writeRaster(ARP_priority_rast, "ARP_priority_rast.tif")
ARP_priority_rast <- rast("ARP_priority_rast.tif")



# (5) make PCUs ----
## patches ----
# btw this line took 20 minutes to run

priority_patches_all <- patches(ARP_priority_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)
# there are 93608 patches

## make polygons ----
patch_all_polys <- as.polygons(priority_patches_all, values = FALSE)
# there are 93608 geometries 

# add a patch_ID attribute for each poly
patch_all_polys$patch_ID <- 1:nrow(patch_all_polys) 

## separate sizes ----
# calc area (default in m^2) & convert to acres
patch_all_polys$patch_acres <- expanse(patch_all_polys) * 0.000247105

# filt out small poys (< 20 acres)
small_polys_removed <- patch_all_polys[patch_all_polys$patch_acres >= 20, ]
# 1414 geoms remain
(1414/134187)*100 # 1.053753 % of polys remain (are >= 20 acres)
  # so ~99 % of patches/polys were < 20 acres (isolated areas)
  # but many of these remaining polys are quite large and need to be divided

# separate mid-sized polys (20-200 acres)
mid_polys <- small_polys_removed[small_polys_removed$patch_acres <= 200, ]
# 1183 geoms
(1183/1414)*100 # 83.66337 % of polys >= 20 acres are also <= 200 acres
  # these don't need to be divided

# separate large polys ( > 200 acres)
large_polys <- small_polys_removed[small_polys_removed$patch_acres > 200, ]
# 231 geoms
  # these do need to be divided

## divide ----
# calculate divisions needed for each large poly, ensuring at least 2 parts for large polys
num_all_parts <- pmax(2, round(large_polys$patch_acres / 125))

# use lapply to iterate and divide
divided_polys_list <- lapply(1:nrow(large_polys), function(i) {
  poly <- large_polys[i, ]
  # set a seed to ensure reproducibility for the division process
  set.seed(i)
  # divide by the pre-determined number of parts for that particular poly
  divided_poly <- divide(poly, n = num_all_parts[i])
  # store the original patch_ID and re-calculate the new areas
  divided_poly$patch_ID <- poly$patch_ID
  divided_poly$div_acres <- expanse(divided_poly) * 0.000247105
  
  return(divided_poly)
})

# combine all divided polys into a single SpatVector
divided_polys_vect <- do.call(rbind, divided_polys_list)
  # 2847 geoms

# combine the mid-sized polys with the newly divided large polys
ARP_PCUs_1A_vect <- rbind(mid_polys, divided_polys_vect)
  # 4030 geoms

## adjust ----

# add new ID col & new final area col
ARP_PCUs_1A_vect$PCU_ID <- 1:nrow(ARP_PCUs_1A_vect)
ARP_PCUs_1A_vect$area_acres <- expanse(ARP_PCUs_1A_vect) * 0.000247105

summary(ARP_PCUs_1A_vect)
# area_acres min = 16.78, max = 352.67  
  # not exactly within the desired 20-200 acre range, but close enough
    # this is a step in the method that we could refine in the future

# select only new ID and area
ARP_PCUs_1A_vect <- ARP_PCUs_1A_vect[, c("PCU_ID", "area_acres")]

ARP_PCUs_1A_df <- as.data.frame(ARP_PCUs_1A_vect)

sum(ARP_PCUs_1A_vect$area_acres) # 422214.1 acres
sum(small_polys_removed$patch_acres) # 422214.1 acres
  # bc these are =, we know the divide function worked (retained all area)


## stats ----
# ARP is 1723619 acres 
(422214.1/1723619)*100 # 24.49579 % of ARP are highest priority areas (PCUs)

# for reference, 
(506182.4/1723619)*100 # 29.36742 % of ARP meets PFs (ARP_priority_rast values = 1)

(422214.1/506182.4)*100 # 83.41145 % of the areas that meet basic priorities
  # are continuous PCUs > 20 acres
  

## viz ----
plot(ARP_PCUs_1A_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)


### write & read ----
writeVector(ARP_PCUs_1A_vect, "ARP_PCUs_1A_vect.shp")
ARP_PCUs_1A_vect <- vect("ARP_PCUs_1A_vect.shp")

## select Lady Moon ----
  # this is the case study PCU that we will use for FWD climate matching in part 2
  # the PCU closest to the Lady Moon trail head has PCU_ID = 212

PCU_LM <- ARP_PCUs_1A_vect %>% 
  filter(PCU_ID == 212)
plot(PCU_LM)



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
    # this is essentially all the Cameron Peak (CP) fire needs in 1 poly

CP_big_need_poly <- needs_all %>%
  select(all_of(desired_cols)) %>% 
  filter(FACTS_ID %in% desired_ids) 

plot(CP_big_need_poly)

## project ---- 
CP_big_need_poly <- project(CP_big_need_poly, "EPSG:5070")

## divide ----
# calc area for entire polygon
CP_big_need_poly$area_all <- expanse(CP_big_need_poly) * 0.000247105
  # 49637 acres
49637/200 # 248 (rough number of parts)

set.seed(100)
CP_PPUs_vect <- divide(CP_big_need_poly, n = 250)
  # 250 geoms

# add ID 
CP_PPUs_vect$PPU_ID <- 1:nrow(CP_PPUs_vect)

# new area of divided polys
CP_PPUs_vect$area_acres <- expanse(CP_PPUs_vect) * 0.000247105
summary(CP_PPUs_vect$area_acres)
# min = 24.84    , max = 663.06  


## add Elv ----
# using the DEM created in part 1A_3b
ARP_DEM_rast <- rast("ARP_DEM_rast.tif")

# the DEM is in meters --> convert m to ft
meters_to_feet_factor <- 3.28084
ARP_DEM_ft <- ARP_DEM_rast * meters_to_feet_factor 
summary(ARP_DEM_ft) # min = 5374, max = 14030          

# extract max
Elv_max_df <- extract(ARP_DEM_ft, CP_PPUs_vect, fun=max)
str(Elv_max_df)
# rename col
Elv_max_df <- Elv_max_df %>% 
  rename(Elv_max_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PPU_ID = CP_PPUs_vect$PPU_ID) %>% 
  select(-1)

# extract min
Elv_min_df <- extract(ARP_DEM_ft, CP_PPUs_vect, fun=min)
str(Elv_min_df)
# rename col
Elv_min_df <- Elv_min_df %>% 
  rename(Elv_min_ft = USGS_1_n41w106_20230314) %>% 
  mutate(PPU_ID = CP_PPUs_vect$PPU_ID) %>% 
  select(-1)

Elv_join_df <- left_join(Elv_min_df, Elv_max_df, by = "PPU_ID")

CP_PPUs_vect <- CP_PPUs_vect %>% 
  left_join(Elv_join_df, by = "PPU_ID")


### write & read ----
writeVector(CP_PPUs_vect, "CP_PPUs_vect.shp")
CP_PPUs_vect <- vect("CP_PPUs_vect.shp")

CP_PPUs_df <- as.data.frame(CP_PPUs_vect)

CP_PPUs_vect <- CP_PPUs_vect %>% 
  mutate(Elv_diff = Elv_max_ft - Elv_min_ft)

## select ----
  # for the case study, we are only going to use the planting needs (PPUs)
  # that are within the 9000 - 9500 ft EB
CP_PPU_9000_9500_vect <- CP_PPUs_vect %>% 
  filter(Elv_min_ft >= 9000, Elv_max_ft <= 9500)
# 1 geoms


CP_PPU_8000_8500_vect <- CP_PPUs_vect %>% 
  filter(Elv_min_ft >= 8000, Elv_max_ft <= 8500)
# 0 geoms

CP_PPU_8500_9000_vect <- CP_PPUs_vect %>% 
  filter(Elv_min_ft >= 8500, Elv_max_ft <= 9000)
# 0 geoms


### write & read ----
writeVector(CP_PPU_9000_9500_vect, "CP_PPU_9000_9500_vect.shp")
CP_PPU_9000_9500_vect <- vect("CP_PPU_9000_9500_vect.shp")

sum(CP_PPU_9000_9500_vect$area_acres) # XX acres

V2_CL_PPU_9000_9500_vect <- vect("V2_CL_PPU_9000_9500_vect.shp")







