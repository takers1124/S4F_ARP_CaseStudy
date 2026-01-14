# description ----

# Seeds 4 the Future ARP Case Study

# Part 3 prioritizes PCUs by overlaying them with the climate match area for our case study PPUs with
  # and investigates whether nursery seedlots overlay with the climate match of PPUs

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
# see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project & earlier parts

# setup ----

library(terra) 
library(tidyterra) 
library(dplyr)


# FWD ----



# REV ----

## (1) sum ----
# we want to combine the output of the match_clims() function (part 2)
  # we will do this by adding raster values for all clim match rasters for all PPUs in the case study
  # pixel values represents the "match sum" (total #) of PPUs that are matched at that pixel location
    # pixels with higher values represent areas where more PPUs share climate match
    # which is important bc we want to collect in areas that will meet larger planting needs
    
# we will do this for each of the climate scenarios (reference, current, ssp2, and ssp5)
  # and both AOIs (the ARP and the SRME)

# for this part 3 of the case study, we are only using PPUs in 9000-9500 ft EB
  # as described in parts 1 and 2


### ref ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ref_match_ARP_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ref_match_ARP_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ref_match_sum_ARP_rast <- app(PPU_ref_match_ARP_rast, fun = "sum", na.rm = TRUE)
# values range from 1 to 19,
# so only 19 of the 20 PPUs in this EB have overlapping climate match areas

plot(PPU_ref_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ref_match_sum_ARP_rast, "PPU_ref_match_sum_ARP_rast.tif")
PPU_ref_match_sum_ARP_rast <- rast("PPU_ref_match_sum_ARP_rast.tif")

##### stats ----
global(PPU_ref_match_sum_ARP_rast, fun = "notNA") # 2799 cells
global(ref_ARP_rast, fun = "notNA") # 6937 cells in all ARP
  # the climate match rasters (1000x1000 m) are not the same resolution as other raster data in part 1 (30x30 m)
(2799/6937)*100 # = 40.34885 % of ARP is climate matched with at least 1 PPU



#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ref_match_SRME_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ref_match_SRME_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ref_match_sum_SRME_rast <- app(PPU_ref_match_SRME_rast, fun = "sum", na.rm = TRUE)

plot(PPU_ref_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ref_match_sum_SRME_rast, "PPU_ref_match_sum_SRME_rast.tif")
PPU_ref_match_sum_SRME_rast <- rast("PPU_ref_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ref_match_sum_SRME_rast, fun = "notNA") # 60636 cells
global(ssp2_SRME_rast, fun = "notNA") # 134124 cells in all SRME
(60636/134124)*100 # = 45.20891 % of SRME is climate matched with at least 1 PPU



### curr ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/curr_match_ARP_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_curr_match_ARP_rast <- rast(rast_files)

# sum rast values across all layers
PPU_curr_match_sum_ARP_rast <- app(PPU_curr_match_ARP_rast, fun = "sum", na.rm = TRUE)

plot(PPU_curr_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_curr_match_sum_ARP_rast, "PPU_curr_match_sum_ARP_rast.tif")
PPU_curr_match_sum_ARP_rast <- rast("PPU_curr_match_sum_ARP_rast.tif")

##### stats ----
global(PPU_curr_match_sum_ARP_rast, fun = "notNA") # 1930 cells
(1930/6937)*100 # = 27.82182 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/curr_match_SRME_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_curr_match_SRME_rast <- rast(rast_files)

# sum rast values across all layers
PPU_curr_match_sum_SRME_rast <- app(PPU_curr_match_SRME_rast, fun = "sum", na.rm = TRUE)

plot(PPU_curr_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_curr_match_sum_SRME_rast, "PPU_curr_match_sum_SRME_rast.tif")
PPU_curr_match_sum_SRME_rast <- rast("PPU_curr_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_curr_match_sum_SRME_rast, fun = "notNA") # 52547 cells
(52547/134124)*100 # = 39.17792 % of SRME is climate matched with at least 1 PPU


### ssp2 ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp2_match_ARP_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ssp2_match_ARP_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ssp2_match_sum_ARP_rast <- app(PPU_ssp2_match_ARP_rast, fun = "sum", na.rm = TRUE)

plot(PPU_ssp2_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp2_match_sum_ARP_rast, "PPU_ssp2_match_sum_ARP_rast.tif")
PPU_ssp2_match_sum_ARP_rast <- rast("PPU_ssp2_match_sum_ARP_rast.tif")

##### stats ----
global(PPU_ssp2_match_sum_ARP_rast, fun = "notNA") # 1584 cells
(1584/6937)*100 # = 22.83408 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp2_match_SRME_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ssp2_match_SRME_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ssp2_match_sum_SRME_rast <- app(PPU_ssp2_match_SRME_rast, fun = "sum", na.rm = TRUE)

plot(PPU_ssp2_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp2_match_sum_SRME_rast, "PPU_ssp2_match_sum_SRME_rast.tif")
PPU_ssp2_match_sum_SRME_rast <- rast("PPU_ssp2_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ssp2_match_sum_SRME_rast, fun = "notNA") # 33068 cells
(33068/134124)*100 # = 24.6548 % of SRME is climate matched with at least 1 PPU



### ssp5 ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp5_match_ARP_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ssp5_match_ARP_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ssp5_match_sum_ARP_rast <- app(PPU_ssp5_match_ARP_rast, fun = "sum", na.rm = TRUE)

plot(PPU_ssp5_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp5_match_sum_ARP_rast, "PPU_ssp5_match_sum_ARP_rast.tif")
PPU_ssp5_match_sum_ARP_rast <- rast("PPU_ssp5_match_sum_ARP_rast.tif")

##### stats ----
global(PPU_ssp5_match_sum_ARP_rast, fun = "notNA") # 803 cells
(803/6937)*100 # = 11.57561 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp5_match_SRME_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ssp5_match_SRME_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ssp5_match_sum_SRME_rast <- app(PPU_ssp5_match_SRME_rast, fun = "sum", na.rm = TRUE)

plot(PPU_ssp5_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp5_match_sum_SRME_rast, "PPU_ssp5_match_sum_SRME_rast.tif")
PPU_ssp5_match_sum_SRME_rast <- rast("PPU_ssp5_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ssp5_match_sum_SRME_rast, fun = "notNA") # 21550 cells
(21550/134124)*100 # = 16.06722 % of SRME is climate matched with at least 1 PPU



## (2) overlay PCUs ----
# we want the PCU vector to have an attribute for each of the climate scenarios
  # above, (1) we created a new sum raster for each scenario
    # where each pixel value represents the "match sum" (total #) of PPUs that are matched at that pixel location
    # pixels with higher values represent areas where more PPUs share climate match
      # which is important bc we want to collect in areas that will meet larger planting needs
  # now, (2) we want to extract the # of matching PPUs across the entire area of each PCU polygon
    # we will average (extract the mean) # of matching PPUs   
    # and add this mean value as an attribute for each PCU, for each climate scenario
  
# using the PCU vector created in part 1
ARP_all_PCUs_vect <- vect("ARP_all_PCUs_vect.shp")
plot(ARP_all_PCUs_vect)

# only using the 4 PPU_X_match_sum_ARP_rast (not the SRME too) because all PCUs are in the ARP
  # so only need the sum extent that covers all PCUs

### (a) extract ----
#### ref ----
# extract max "match value" across match_rast raster layers for each PCU
ref_match_df <- extract(PPU_ref_match_ARP_rast, ARP_all_PCUs_vect, fun = max, na.rm = TRUE)
str(ref_match_df)

# add sum and PCU_ID; remove auto-gen ID from extract()
ref_match_df2 <- ref_match_df %>% 
  mutate(ref_sum = rowSums(ref_match_df[,-1], na.rm = TRUE),
         PCU_ID = ARP_all_PCUs_vect$PCU_ID) %>% 
  select(-1)

#### curr ----
# extract max "match value" across match_rast raster layers for each PCU
curr_match_df <- extract(PPU_curr_match_ARP_rast, ARP_all_PCUs_vect, fun = max, na.rm = TRUE)

# rename, arrange, add rank
curr_match_df2 <- curr_match_df %>% 
  mutate(curr_sum = rowSums(curr_match_df[,-1], na.rm = TRUE),
         PCU_ID = ARP_all_PCUs_vect$PCU_ID) %>% 
  select(-1)

#### ssp2 ----
# extract max "match value" across match_rast raster layers for each PCU
ssp2_match_df <- extract(PPU_ssp2_match_ARP_rast, ARP_all_PCUs_vect, fun = max, na.rm = TRUE)

# rename, arrange, add rank
ssp2_match_df2 <- ssp2_match_df %>% 
  mutate(ssp2_sum = rowSums(ssp2_match_df[,-1], na.rm = TRUE),
         PCU_ID = ARP_all_PCUs_vect$PCU_ID) %>% 
  select(-1)

#### ssp5 ----
# extract max "match value" across match_rast raster layers for each PCU
ssp5_match_df <- extract(PPU_ssp5_match_ARP_rast, ARP_all_PCUs_vect, fun = max, na.rm = TRUE)

# rename, arrange, add rank
ssp5_match_df2 <- ssp5_match_df %>% 
  mutate(ssp5_sum = rowSums(ssp5_match_df[,-1], na.rm = TRUE),
         PCU_ID = ARP_all_PCUs_vect$PCU_ID) %>% 
  select(-1)


### (b) merge ----
# combine all 3 dfs

match_join_df <- ref_match_df2 %>% 
  left_join(curr_match_df2, by = "PCU_ID") %>% 
  left_join(ssp2_match_df2, by = "PCU_ID") %>% 
  left_join(ssp5_match_df2, by = "PCU_ID")

# merge with spatvector
ARP_PCUs_matched_full_vect <- ARP_all_PCUs_vect %>% 
  left_join(match_join_df, by = "PCU_ID")

ARP_PCUs_matched_full_df <- as.data.frame(ARP_PCUs_matched_full_vect)
str(ARP_PCUs_matched_full_df)

#### write & read ----
writeVector(ARP_PCUs_matched_full_vect, "ARP_PCUs_matched_full_vect.shp")
ARP_PCUs_matched_full_vect <- vect("ARP_PCUs_matched_full_vect.shp")

### (c) select ----
ARP_PCUs_matched_vect <- ARP_PCUs_matched_full_vect %>% 
  select(PCU_ID, area_acres, ref_sum, curr_sum, ssp2_sum, ssp5_sum)

ARP_PCUs_matched_df <- as.data.frame(ARP_PCUs_matched_vect)

#### write & read ----
writeVector(ARP_PCUs_matched_vect, "ARP_PCUs_matched_vect.shp")
ARP_PCUs_matched_vect <- vect("ARP_PCUs_matched_vect.shp")


### (d) filter ----
#### *** need to re-run ----

#### ssp2, 1 match----
# filter only PCUs that are within climate-matched areas (have a match score >= 1)
PCUs_matched_1_ssp2_vect <- ARP_PCUs_matched_vect %>%
  filter(ssp2_sum >= 1)

##### stats ----
# has 1679 geoms
(1679/6428)*100 # 26.1201 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 1 PPU

sum(PCUs_matched_1_ssp2_vect$area_acres) # 186385.3 acres
# ARP is 1723619 acres
(186385.3/1723619)*100 # 10.8136 % of area of ARP

plot(PCUs_matched_1_ssp2_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeVector(PCUs_matched_1_ssp2_vect, "PCUs_matched_1_ssp2_vect.shp")
PCUs_matched_1_ssp2_vect <- vect("PCUs_matched_1_ssp2_vect.shp")


#### ssp2, 10 matches ----
# filter only PCUs that meet 1/2 of needs (have a match score >= 10)
PCUs_matched_10_ssp2_vect <- ARP_PCUs_matched_vect %>%
  filter(ssp2_sum >= 10)

##### stats ----
# has 441 geoms
(441/6428)*100 # 6.86061 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 10 PPUs (half of the 9000-9500 EB case study)

sum(PCUs_matched_10_ssp2_vect$area_acres) # 42233.71 acres
  # ARP is 1723619 acres
(42233.71/1723619)*100 # 2.450293 % of ARP area

##### write & read ----
# writeVector(PCUs_matched_10_ssp2_vect, "PCUs_matched_10_ssp2_vect.shp")
# PCUs_matched_10_ssp2_vect <- vect("PCUs_matched_10_ssp2_vect.shp")




## (3) overlay Nursery ----
# we want to see where the climate (all 4 periods/scenarios) of the PPUs 
  # overlays with the reference climate of the current nursery inventory (seedlots) 
# this would tell us if we already have future-focused seedlots that we can plant in our case study
  # and therefor would not need to collect new seed (yet) for those specific planting needs (PPUs)

# we will use the same method as above (2) for PCU overlays

# we will use the large AOI (which covers the whole SRME) bc seedlots are all over interior west

# we will use the nursery lat/long data that we created in Part1_B (nursery attributes for PCUs)

# in paper fig., want to show (1) all SLs in SRME and (2) those matching (above)
  # but the SLs need to be points, bc the polys are so small, they don't show up on Arc when zoomed out to SRME

### ** check final ----
  # I altered the file name and SL_ID col in Part 1_B
  # make sure runs smoothly below with changes

### (a) read SLs ----
# created in Part 1_B
seed_points_vect <- vect("seed_points_vect.shp")
# has 1296 geoms & 14 attributes

seed_pts_df <- as.data.frame(seed_points_vect)

# but want to filter for only those that are located within the SRME
seed_within <- relate(seed_points_vect, SRME_vect, relation = "within")
  # add as attribute to the spatvector
seed_points_vect$within_SRME <- seed_within

seed_SRME <- seed_points_vect %>% 
  filter(within_SRME == "TRUE") %>% 
  select(SL_ID, Lot, species, Year, Balance, name)
# has 159 geoms 
(159/1296)*100 # = 12.26852 % of seedlots are within the SRME
# has one more than when use polys (probably the poly is on SRME boundary)
  # not a problem for now

### (b) extract ----
#### ref ----
ref_match_df <- extract(PPU_ref_match_SRME_rast, seed_SRME)
str(ref_match_df)

ref_match_df2 <- ref_match_df %>% 
  mutate(ref_sum = rowSums(ref_match_df[,-1], na.rm = TRUE), # create match_sum col for filtering
        SL_ID = seed_SRME$SL_ID) %>% # assign proper ID from seedlot df
  select(-1)
str(ref_match_df2)

#### curr ----
curr_match_df <- extract(PPU_curr_match_SRME_rast, seed_SRME)

curr_match_df2 <- curr_match_df %>% 
  mutate(curr_sum = rowSums(curr_match_df[,-1], na.rm = TRUE), # create match_sum col for filtering
         SL_ID = seed_SRME$SL_ID) %>% # assign proper ID from seedlot df
  select(-1)

#### ssp2 ----
ssp2_match_df <- extract(PPU_ssp2_match_SRME_rast, seed_SRME)

ssp2_match_df2 <- ssp2_match_df %>% 
  mutate(ssp2_sum = rowSums(ssp2_match_df[,-1], na.rm = TRUE), # create match_sum col for filtering
         SL_ID = seed_SRME$SL_ID) %>% # assign proper ID from seedlot df
  select(-1)

#### ssp5 ----
# extract mean "match sum" for each PCU
ssp5_match_df <- extract(PPU_ssp5_match_SRME_rast, seed_SRME)

# rename, arrange, add rank
ssp5_match_df2 <- ssp5_match_df %>% 
  mutate(ssp5_sum = rowSums(ssp5_match_df[,-1], na.rm = TRUE), # create match_sum col for filtering
         SL_ID = seed_SRME$SL_ID) %>% # assign proper ID from seedlot df
  select(-1)


### (b) merge ----
# combine all 3 dfs

match_join_df <- ref_match_df2 %>% 
  left_join(curr_match_df2, by = "SL_ID") %>% 
  left_join(ssp2_match_df2, by = "SL_ID") %>% 
  left_join(ssp5_match_df2, by = "SL_ID")

# merge with spatvector
SRME_SLs_matched_full_vect <- seed_SRME %>% 
  left_join(match_join_df, by = "SL_ID")

seedlots_matched_full_df <- as.data.frame(SRME_SLs_matched_full_vect)
str(seedlots_matched_full_df)

##### write & read ----
writeVector(SRME_SLs_matched_full_vect, "SRME_SLs_matched_full_vect.shp")
SRME_SLs_matched_full_vect <- vect("SRME_SLs_matched_full_vect.shp")

### (c) select ----
SRME_SLs_matched_vect <- SRME_SLs_matched_full_vect %>% 
  select(SL_ID, Lot, species, Year, Balance, name, ref_sum, curr_sum, ssp2_sum, ssp5_sum)

#### write & read ----
writeVector(SRME_SLs_matched_vect, "SRME_SLs_matched_vect.shp")
SRME_SLs_matched_vect <- vect("SRME_SLs_matched_vect.shp")


### (c) filter ----

#### ssp2, 1 matches ----
# filter only SLs that meet 1/2 of needs (have a match score >= 10)
SLs_matched_1_ssp2_vect <- SRME_SLs_matched_vect %>%
  filter(ssp2_sum >= 1)

##### stats ----
# has 30 geoms
(30/158)*100 # 18.98734 % of all the SLs in the SRME are climate matched (under ssp2) with at least 1 PPUs (from the 9000-9500 EB case study)

##### write & read ----
writeVector(SLs_matched_10_ssp2_vect, "SLs_matched_10_ssp2_vect.shp")
SLs_matched_10_ssp2_vect <- vect("SLs_matched_10_ssp2_vect.shp")




