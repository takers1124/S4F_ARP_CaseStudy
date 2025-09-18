# description ----

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), September 2025

# this script is to outline the data analysis workflow for the 
# Seeds 4 the Future ARP Case Study for teaching purposes

# see the associated S4F_ARP_CS_overview.doc for more details 
# input data can be found in S4F_ARP_CS_data.zip folder 


# (1) setup ----

## download data ----
# go to this google drive folder, and download the .zip file
# https://drive.google.com/file/d/141dBLlpljfMMbwoj5rT_CJ8ySGYQn21Q/view?usp=sharing
# unzip the file, and move the input_data folder into your r project folder 

## load libraries ----

# first go to your output pane (bottom right) -> packages -> install
# type the name of each to search, then install, then run the library for each (below)
# libraries will need to be run with each new session, but package downloaded only once 

library(terra) # primary spatial data management package
library(tidyterra) # dplyr-style data management for spatial data
library(ggplot2) # for fancy plotting


# (2) create AOI ----

# the Area of Interest (AOI) for this case study is the Arapaho-Roosevelt National Forest (ARP)
# we will first read in a larger shapefile of all the NFs in Colorado
  # you will convert the shapefile (.shp) into a "vector" object 
  # vector objects can be points, lines, or polygons
  # vector objects have "geometries" (the unique shapes) and "attributes" (metadata)
# then learn how to filter the data for just the ARP polygon and it's associated attributes 

## ARP ----
# navigate to the "input_data" folder that you downloaded in step 1
# > shp_files 
# click "more" > "set as working directory"

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
# you could do all sorts of fancy additions to a plot like this
# for ex: color code the ranger districts, add scale bar/north arrow/labels/etc

### read file ----
CO_refs_vect <- vect("CO_refs_vect.shp")

### viz ----
plot(ARP_vect)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)


# (3) pre-process data ----

# here we will pre-process the data layers that will be combined in step 4
# most of the data that you will read in has already been pre-processed for this tutorial
  # and only requires a little more processing for demonstration 

# each data layer is a .tif file, which we will convert into a "raster" object
# a raster object is a grid of pixels, with a 30x30 meter resolution (in this ex)
  # which means each pixel represents 900 square meters of the earth's surface
  # at that specific location 


## (3.a) risk ----
# using flame length exceedance probability of 8 ft (FLEP8)
# as our proxy for risk of local extinction due to high severity wildfire
# cell values = probability of flame length > 8 ft

### read file ----
ARP_risk_score_rast <- rast("ARP_risk_score_rast.tif")

### viz ----
plot(ARP_risk_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)


## (3.b) slope ----
# this slope raster was generated using a 
# digital elevation model (DEM), downloaded from The National Map
# cell values = degrees

### read ----
ARP_slope_score_rast <- rast("ARP_slope_score_rast.tif")

### viz ----
plot(ARP_slope_score_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=2)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)


## (3.c) road ----
# this road raster was generated from a roads shapefile (lines)
# if time allows, go through the following steps
# if not, skip to "write & read" below

# read
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
# writeRaster(ARP_road_score_rast, "ARP_road_score_rast.tif")
ARP_road_score_rast <- rast("ARP_road_score_rast.tif")


## (3.d) tree height ----
# using existing vegetation height (EVH) from LANDFIRE
# prior processing:
  # filter for only trees (the data set also has shrubs and herbs)
  # convert height units from m to ft
  # filter only trees >= 20 ft
# cell values = height in ft

### read ----
ARP_height_rast <- rast("ARP_height_rast.tif")

### viz ----
# make special color pallet 
green_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(25)  # Creates 25 shades of green

# see height range
plot(ARP_height_rast, col = green_palette)

### classify ----
# we want to treat any tree >= 20 ft equally, so make all...
  # cell value = 100

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
# writeRaster(ARP_height_score_rast, "ARP_height_score_rast.tif")
ARP_height_score_rast <- rast("ARP_height_score_rast.tif")

#### feedback ----
# Does a 20 ft threshold make sense?
# Does it make sense to make all >20 ft cells equal (not a continuous score)?


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
3385636200/4046.86 # 4046.86 m/acre = 836,608.2 acres

### write & read ----
writeRaster(combined_rast, "combined_rast.tif")
combined_rast <- rast("combined_rast.tif")

# (5) make PCUs ----

# we need feasible (small) units to send the scouting crew - you guys!
  # so we will filter the data and make "patches" of high-scoring areas
    # we call these Potential Collection Units (PCUs)

## filter 102 ----
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

## filter 103 ----
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



## patches ----
# in this step, we convert dense raster cells (with values) into "patches"
ARP_PCU_patches <- patches(ARP_PCU_103_rast, directions=4, values=FALSE, zeroAsNA=FALSE, allowGaps=FALSE)

plot(ARP_PCU_patches)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)
# looks pretty much the same, just the values are different

### explore ----
# there are 47191  patches 
patch_sizes <- freq(ARP_PCU_patches)
patches_box <- boxplot(ARP_PCU_patches) # lots of low values

## remove small patches ----
# 225 cells ~ 50 acres
small_patches <- patch_sizes %>% filter(count <= 225) %>% select(value) # 47131 rows (patches)
# so, the vast majority of patches are small 
47191-47131 # = 60 patches > 50 acres

### classify ----
ARP_PCU_patches_classified <- classify(ARP_PCU_patches, rcl = cbind(small_patches, NA))

### explore ----
ARP_PCU_patches_freq <- freq(ARP_PCU_patches_classified) # confirmed, 60 patches
ARP_PCU_cells <- sum(ARP_PCU_patches_freq$count) # 39,565 cells
39404*900 # = 35463600 m^2
35463600/4046.86 # = 8763.239 acres
# before filtering out small patches, was 836,608.2 acres

plot(ARP_PCU_patches_classified)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

## polygons ----
# in this step, we convert the above "patches" into polygons for ease of map making & calculating stats
ARP_PCU_patches_classified
ARP_PCU_vect <- as.polygons(ARP_PCU_patches_classified, values = FALSE) # has 60 geoms (polys)

plot(ARP_PCU_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=1.5)
points(CO_refs_vect, pch = 19, col = "purple", cex = 1.5)

### write & read ----
writeVector(ARP_PCU_vect, "ARP_PCU_vect.shp")
ARP_PCU_vect <- vect("ARP_PCU_vect.shp")

## attributes ----
# in this step, we add attributes (metadata) to our polygons
  # this is how we further filter & select PCUs for scouting
# ex attributes:
  # mean tree height, slope, road distance, score
  # biomass for target species (species distribution)
  # insect disease history maps, etc


# (6) make maps ----
# here we would use ggplot2 to make fancy maps and/or export to Arc












