# desctiption ----

# Seeds 4 the Future ARP Case Study, the full script
# parts 3 prioritize PCUs by overlaying climate match of PPUs with PCUs

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025

# see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project & earlier parts

# setup ----

library(terra) # primary spatial data management package
library(tidyterra) # dplyr-style data management for spatial data
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
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ref_match_ARP_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_ref_match_sum_ARP_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
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
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ref_match_SRME_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_ref_match_sum_SRME_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to x,
# so only x of the 20 PPUs in this EB have overlapping climate match areas

plot(PPU_ref_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ref_match_sum_SRME_rast, "PPU_ref_match_sum_SRME_rast.tif")
PPU_ref_match_sum_SRME_rast <- rast("PPU_ref_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ref_match_sum_SRME_rast, fun = "notNA") # 60636 cells
global(ssp2_SRME_rast, fun = "notNA") # 134124 cells in all SRME
(60636/134124)*100 # = 45.20891 % of ARP is climate matched with at least 1 PPU



### curr ----
#### ARP ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/curr_match_ARP_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_curr_match_sum_ARP_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 19,
# so only 19 of the 20 PPUs in this EB have overlapping climate match areas

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
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/curr_match_SRME_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_curr_match_sum_SRME_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 19,
# so only 19 of the 20 PPUs in this EB have overlapping climate match areas

plot(PPU_curr_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_curr_match_sum_SRME_rast, "PPU_curr_match_sum_SRME_rast.tif")
PPU_curr_match_sum_SRME_rast <- rast("PPU_curr_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_curr_match_sum_SRME_rast, fun = "notNA") # 52547 cells
(52547/134124)*100 # = 39.17792 % of ARP is climate matched with at least 1 PPU


### ssp2 ----
#### ARP ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp2_match_ARP_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_ssp2_match_sum_ARP_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 12,
# so only 12 of the 13 PPUs in this EB have overlapping climate match areas

# when use 9000-9500 EB, values range from 1-19 (of 20)

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
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp2_match_SRME_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_ssp2_match_sum_SRME_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 12,
# so only 12 of the 13 PPUs in this EB have overlapping climate match areas

plot(PPU_ssp2_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp2_match_sum_SRME_rast, "PPU_ssp2_match_sum_SRME_rast.tif")
PPU_ssp2_match_sum_SRME_rast <- rast("PPU_ssp2_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ssp2_match_sum_SRME_rast, fun = "notNA") # 33068 cells
(33068/134124)*100 # = 24.6548 % of ARP is climate matched with at least 1 PPU



### ssp5 ----
#### ARP ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp5_match_ARP_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_ssp5_match_sum_ARP_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 12,
# so only 12 of the 13 PPUs in this EB have overlapping climate match areas

plot(PPU_ssp5_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp5_match_sum_ARP_rast, "PPU_ssp5_match_sum_ARP_rast.tif")
PPU_ssp5_match_sum_ARP_rast <- rast("PPU_ssp5_match_sum_ARP_rast.tif")

##### stats ----
global(PPU_ssp5_match_sum_ARP_rast, fun = "notNA") # 803 cells
(803/6937)*100 # = 22.83408 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_9000_9500/ssp5_match_SRME_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_ssp5_match_sum_SRME_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 12,
# so only 12 of the 13 PPUs in this EB have overlapping climate match areas

plot(PPU_ssp5_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ssp5_match_sum_SRME_rast, "PPU_ssp5_match_sum_SRME_rast.tif")
PPU_ssp5_match_sum_SRME_rast <- rast("PPU_ssp5_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ssp5_match_sum_SRME_rast, fun = "notNA") # 21550 cells
(21550/134124)*100 # = 16.06722 % of ARP is climate matched with at least 1 PPU



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
# extract mean "match sum" for each PCU
ref_match_mean_df <- extract(PPU_ref_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)
str(ref_match_mean_df)

# rename, arrange, add rank
ref_match_mean_df <- ref_match_mean_df %>% 
  rename(match_ref = match,
         PCU_ID = ID)

#### curr ----
# extract mean "match sum" for each PCU
curr_match_mean_df <- extract(PPU_curr_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)

# rename, arrange, add rank
curr_match_mean_df <- curr_match_mean_df %>% 
  rename(match_curr = match,
         PCU_ID = ID)

#### ssp2 ----
# extract mean "match sum" for each PCU
ssp2_match_mean_df <- extract(PPU_ssp2_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)

# rename, arrange, add rank
ssp2_match_mean_df <- ssp2_match_mean_df %>% 
  rename(match_ssp2 = match,
         PCU_ID = ID)

#### ssp5 ----
# extract mean "match sum" for each PCU
ssp5_match_mean_df <- extract(PPU_ssp5_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)

# rename, arrange, add rank
ssp5_match_mean_df <- ssp5_match_mean_df %>% 
  rename(match_ssp5 = match,
         PCU_ID = ID)


### (b) merge ----
# combine all 3 dfs

match_join_df <- ref_match_mean_df %>% 
  left_join(curr_match_mean_df, by = "PCU_ID") %>% 
  left_join(ssp2_match_mean_df, by = "PCU_ID") %>% 
  left_join(ssp5_match_mean_df, by = "PCU_ID")

# merge with spatvector
ARP_PCUs_matched_vect <- ARP_all_PCUs_vect %>% 
  left_join(match_join_df, by = "PCU_ID")



### (c) filter ----

#### ssp2, 1 match----
# filter only PCUs that are within climate-matched areas (have a match score >= 1)
PCUs_matched_1_ssp2_vect <- ARP_PCUs_matched_vect %>% 
  filter(match_ssp2 >= 1)

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
  filter(match_ssp2 >= 10)

##### stats ----
# has 346 geoms
(346/6428)*100 # 5.382701 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 10 PPUs (half of the 9000-9500 EB case study)

sum(PCUs_matched_10_ssp2_vect$area_acres) # 32673.64 acres
  # ARP is 1723619 acres
(32673.64/1723619)*100 # 1.895642 % of ARP area

##### write & read ----
writeVector(PCUs_matched_10_ssp2_vect, "PCUs_matched_10_ssp2_vect.shp")
PCUs_matched_10_ssp2_vect <- vect("PCUs_matched_10_ssp2_vect.shp")




## (3) overlay Nursery ----
# we want to see where the climate (all 4 periods/scenarios) of the PPUs 
  # overlays with the reference climate of the current nursery inventory (seedlots) 
# this would tell us if we already have future-focused seedlots that we can plant in our case study
  # and therefor would not need to collect new seed (yet) for those specific planting needs (PPUs)

# we will use the same method as above (2) for PCU overlays

# we will use the large AOI (which covers the whole SRME) bc seedlots are all over interior west
  # and the 4 "match sum" rasters that we created in part 3_1 (sum) above

# we will use the nursery lat/long data that we created in part 1_6.p (nursery attributes for PCUs)
### the P1_6.p code needs to be re-written ----

# in paper fig., want to show (1) all SLs in SRME and (2) those matching (above)
# but the SLs need to be points, bc the polys are so small, they don't show up on Arc when zoomed out to SRME

### (a) read SLs ----
# for now, use the pts created in Hotspots_ARP_CS_P3_P4.R script
seed_pts_sv <- vect("seed_points_5070_sv.shp")
# has 1296 geoms & 14 attributes

seed_pts_df <- as.data.frame(seed_pts_sv)

# but want to filter for only those that are located within the SRME
seed_within <- relate(seed_pts_sv, SRME_vect, relation = "within")
seed_pts_sv$within_SRME <- seed_within

seed_SRME <- seed_pts_sv %>% 
  filter(within_SRME == "TRUE") %>% 
  select(species, ID, Year, Balance) %>% 
  rename(SL_ID = ID) # SL for seedlot
# has 159 geoms 
(159/1296)*100 # = 12.26852 % of seedlots are within the SRME
# one more than when use polys (probably the poly is on SRME boundary)
# not a problem


### (b) extract ----
#### ref ----
# extract mean "match sum" for each PCU
ref_match_mean_df <- extract(PPU_ref_match_sum_SRME_rast, seed_SRME, fun = mean, na.rm = TRUE)
str(ref_match_mean_df)

# rename, arrange, add rank
ref_match_mean_df <- ref_match_mean_df %>% 
  rename(match_ref = match,
         SL_ID = ID)

#### curr ----
# extract mean "match sum" for each PCU
curr_match_mean_df <- extract(PPU_curr_match_sum_SRME_rast, seed_SRME, fun = mean, na.rm = TRUE)

# rename, arrange, add rank
curr_match_mean_df <- curr_match_mean_df %>% 
  rename(match_curr = match,
         SL_ID = ID)

#### ssp2 ----
# extract mean "match sum" for each PCU
ssp2_match_mean_df <- extract(PPU_ssp2_match_sum_SRME_rast, seed_SRME, fun = mean, na.rm = TRUE)

# rename, arrange, add rank
ssp2_match_mean_df <- ssp2_match_mean_df %>% 
  rename(match_ssp2 = match,
         SL_ID = ID)

#### ssp5 ----
# extract mean "match sum" for each PCU
ssp5_match_mean_df <- extract(PPU_ssp5_match_sum_SRME_rast, seed_SRME, fun = mean, na.rm = TRUE)

# rename, arrange, add rank
ssp5_match_mean_df <- ssp5_match_mean_df %>% 
  rename(match_ssp5 = match,
         SL_ID = ID)


### (b) merge ----
# combine all 3 dfs

match_join_df <- ref_match_mean_df %>% 
  left_join(curr_match_mean_df, by = "SL_ID") %>% 
  left_join(ssp2_match_mean_df, by = "SL_ID") %>% 
  left_join(ssp5_match_mean_df, by = "SL_ID")

# merge with spatvector
SLs_SRME_matched_vect <- seed_SRME %>% 
  left_join(match_join_df, by = "SL_ID")

seedlots_matched_df <- as.data.frame(SLs_SRME_matched_vect)

##### write & read ----
writeVector(SLs_SRME_matched_vect, "SLs_SRME_matched_vect.shp")
SLs_SRME_matched_vect <- vect("SLs_SRME_matched_vect.shp")


### (c) filter ----

#### ssp2, 10 matches ----
# filter only SLs that meet 1/2 of needs (have a match score >= 10)
SLs_matched_10_ssp2_vect <- SLs_SRME_matched_vect %>% 
  filter(match_ssp2 >= 10)

##### stats ----
# has 8 geoms
(8/158)*100 # 5.063291 % of all the SLs in the SRME are climate matched (under ssp2) with at least 10 PPUs (half of the 9000-9500 EB case study)
  # however, 6 of 8 SLs represent PIPO
    # 2 of 8 represent PIFL, but the total balance is only 3.4 (units?)
  # so if you wanted to plant anything at the case study PPUs besides PIPO, need to collect new seed
    # on the other hand, you could choose to only plant PIPO and you already have seed available
      # but may also want to restrict transfer by x miles or EBs (discussion section)

##### write & read ----
writeVector(SLs_matched_10_ssp2_vect, "SLs_matched_10_ssp2_vect.shp")
SLs_matched_10_ssp2_vect <- vect("SLs_matched_10_ssp2_vect.shp")




