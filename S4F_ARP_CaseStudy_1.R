# desctiption ----

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

### read file ----
NF_CO_vect <- vect("NF_CO_vect.shp")

### viz ----
plot(NF_CO_vect)

### see unique names ----
unique(NF_CO_vect$COMMONNAME)

### select for just ARP ----
ARP_vect <- NF_CO_vect %>%
  filter(COMMONNAME == "Arapaho and Roosevelt National Forests")

plot(ARP_vect)

## reference points ----
# we will add these 2 points as references, they represent Fort Collins and Boulder

### read file ----
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

### write & read file ----
ARP_risk_score_rast <- rast("ARP_risk_score_rast.tif")

### viz ----
plot(ARP_risk_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)


## (3.b) slope ----
# this slope raster is generated using a 
# digital elevation model (DEM), downloaded from The National Map
# cell values = degrees

### * need to fill in----

### write & read file ----
ARP_slope_score_rast <- rast("ARP_slope_score_rast.tif")

### viz ----
plot(ARP_slope_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)


## (3.c) road ----
# this road raster was generated from a roads shapefile (lines)

### * need to fill in----

### write & read file ----
CO_road_rast <- rast("CO_road_rast.tif") 
plot(CO_road_rast)

### crop & mask ----
# here we use the ARP_vect polygon to crop (and mask) only the area we want from the raster
ARP_road_rast <- crop(CO_road_rast, ARP_vect, mask=TRUE)
plot(ARP_road_rast)

### distance ----
# we will calculate the distance to nearest road for each raster cell (pixel)
ARP_road_dist_rast <- distance(ARP_road_rast, unit="m", method="haversine") 
plot(ARP_road_dist_rast)
# cell values = distance to nearest road (in meters)

### classify ----
# now going to set a 1 mi threshold, and make all other values NA
# 1 mile = 1609.34 meters (second to last value of col 2 & threshold value for score)
1609.34/11 # = 146.3036 (start of col 2 & breaks for cols 1 and 2)
1609.34-146.3036 # = 1463.036 (end of col 1)
minmax(ARP_road_dist_rast) # min = 0, ARP max = 37416.17 (end of col 2)

# set sequence
# from, to, becomes
road_sequence = c(c(seq(0, 1463.036, 146.3036),1609.34), c(seq(146.3036, 1609.34, 146.3036),37416.17), c(seq(0, 1, 0.1),NA))
# 1609.34 is the cutoff (final row of col 1, second to last of col 2)
# 37416.17 is the max road length in the ARP (end of col 2)
# from 1609.34 to 37416.17 becomes NA, while the other lower values become 0-1

# create matrix
road_matrix = matrix(road_sequence, ncol=3, byrow=FALSE)
# viz matrix

# classify
ARP_road_class_rast = classify(ARP_road_dist_rast, road_matrix, right=NA, others=NA)
plot(ARP_road_class_rast)

### inverse score ---- 
ARP_road_inv_rast <- (1 - ARP_road_class_rast)
plot(ARP_road_inv_rast)
plot(is.na(ARP_road_inv_rast)) # just a visual check

### crop & mask ----
# do again because distance made a buffer that goes beyond ARP
ARP_road_score_rast <- crop(ARP_road_inv_rast, ARP_vect, mask=TRUE)

### viz ----
plot(ARP_road_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

### write & read ----
writeRaster(ARP_road_score_rast, "ARP_road_score_rast.tif")
ARP_road_score_rast <- rast("ARP_road_score_rast.tif")


## (3.d) tree height ----
# using existing vegetation height (EVH) from LANDFIRE

### crop / mask ----
EVH_ARP <- crop(EVH_CO, ARP_vect, mask=TRUE)

### classify 1 ----
# create matrix so values <= 100 become NA (see all veg)
EVH_matrix <- matrix(c(-Inf, 100, NA), ncol = 3, byrow = TRUE)

# Classify the raster using the matrix, keeping unmatched values
EVH_classified <- classify(EVH_ARP, EVH_matrix, others = NULL)

#### plot ----
plot(EVH_classified)
polys(ARP, col = "black", alpha=0.01, lwd=2)
points(CSU, pch = 19, col = "orange", cex = 1.5)
points(CU, pch = 19, col = "orange", cex = 1.5)

### classify 2 ----
# create matrix so values <= 100 become NA & values >= 200 become NA (see only trees)
EVH_matrix2 <- matrix(c(-Inf, 100, NA, 200, Inf, NA), ncol = 3, byrow = TRUE)
EVH_classified2 <- classify(EVH_ARP, EVH_matrix2, others = NULL) # values 101:125

# make special color pallet 
green_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(25)  # Creates 25 shades of green

#### plot ----
plot(EVH_classified2, col = green_palette)
polys(ARP, col = "black", alpha=0.01, lwd=2)
points(CSU, pch = 19, col = "orange", cex = 1.5)
points(CU, pch = 19, col = "orange", cex = 1.5)

### classify 3 ----
# create matrix so only including trees > 20ft & also making scale in ft

# 1. Define the conversion factor from meters to feet
# 1 meter is approximately 3.28084 feet
meters_to_feet_factor <- 3.28084

# 2. Create the reclassification matrix
# This matrix will define the "from", "to", and "becomes" values.
#  - First, convert the original meter values (101-125) to their true meter representation (1-25)
#  - Then, convert these true meter values to feet.
#  - Finally, set values less than 20 feet to NA.

# 3. Initialize an empty matrix
rcl_matrix <- matrix(NA, ncol = 2, byrow = TRUE)

# 4. Create rows for the matrix based on meter ranges
# Loop through the *original* meter values (101-125)
for (original_meter_value in 101:125) {
  # Calculate the true meter value by subtracting the offset (100)
  true_meter_value <- original_meter_value - 100
  
  # Convert the true meter value to feet
  feet_value <- true_meter_value * meters_to_feet_factor
  
  if (feet_value < 20) {
    rcl_matrix <- rbind(rcl_matrix, c(original_meter_value, NA))  # Values < 20 ft become NA
  } else {
    rcl_matrix <- rbind(rcl_matrix, c(original_meter_value, feet_value)) # Values >= 20 ft retain converted value
  }
}

# 5. Remove the initial NA row
rcl_matrix <- rcl_matrix[-1, ]

# 6. Classify the raster using the reclassification matrix
EVH_classified3 <- classify(EVH_classified2, rcl_matrix, others = NULL) 

#### plot ----
plot(EVH_classified3, col = green_palette)
polys(ARP, col = "black", alpha=0.01, lwd=2)
points(CSU, pch = 19, col = "orange", cex = 1.5)
points(CU, pch = 19, col = "orange", cex = 1.5)

#### write & read ----
writeRaster(EVH_classified3, "ARP_height_rast.tif")
ARP_height_rast <- rast("ARP_height_rast.tif")

### viz ----
# make special color pallet 
green_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(25)  # Creates 25 shades of green

# see height range
plot(, col = green_palette)

### classify ----
# we want to treat any tree >= 20 ft equally, so make all...
# cell value = 100
ARP_height_rast <- rename(EVH_classified3)
# see current height range
minmax(ARP_height_rast)
# min = 22.96588
# max = 82.02100

# set sequence
# from, to, becomes
height_sequence = c(22.96588,82.02100,100)
# we want to be able to distinguish the height priority from the others
# so we make it 100! 

# create matrix 
height_matrix = matrix(height_sequence, ncol=3, byrow=TRUE)

# classify
ARP_height_score_rast = classify(ARP_height_rast, height_matrix, right=NA, others=NA)

# see values
unique(ARP_height_score_rast) # 100

### viz ----
# see classified values
plot(ARP_height_score_rast, col = "darkgreen")
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

### write & read ----
writeRaster(ARP_height_score_rast, "ARP_height_score_rast.tif")
ARP_height_score_rast <- rast("ARP_height_score_rast.tif")



# (4) combine data ----

# now we will do some raster math
# sum the cell values among the 4 data layers
# if a cell for any layer = NA, then NA is returned

combined_rast <- sum(ARP_risk_score_rast, ARP_slope_score_rast, ARP_road_score_rast, ARP_height_score_rast, na.rm=FALSE) %>% 
  rename(score = FLEP8_CO)

### viz ----
plot(combined_rast)

# note, all these areas meet our basic criteria (3.a - 3.d)
# however, it is still too large of an area to scout
# so, we must narrow it down further

### explore ----
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

### write & read ----
writeRaster(combined_rast, "combined_rast.tif")
combined_rast <- rast("combined_rast.tif")

# (5) make PCUs ----

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
### load & process ----
# these DEM rasters came from The National Map downloader (USGS)
# they are 1 Arc Sec
# these have GEOGCRS NAD83, but are not yet projected
DEM_n41_w106 <- rast("USGS_1_n41w106_20230314.tif")
DEM_n41_w107 <- rast("USGS_1_n41w107_20230314.tif")
DEM_n40_w106 <- rast("USGS_1_n40w106_20230602.tif")
DEM_n40_w107 <- rast("USGS_1_n40w107_20220216.tif")
# mosaic 4 tiles together
DEM_ARP <- mosaic(DEM_n41_w106, DEM_n41_w107, DEM_n40_w106, DEM_n40_w107, fun="first")
# project
DEM_ARP <- project(DEM_ARP, "EPSG:5070")

# crop and mask the DEM to the extent of ARP 
DEM_ARP <- crop(DEM_ARP, ARP_vect, mask=TRUE)
plot(DEM_ARP) # min = 1470.285 , max = 4393.409 

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


















