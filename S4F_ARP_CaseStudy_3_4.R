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

## (1) sum V2 ----
# we want to combine the output of match_clims() function (part 2)
  # we will do this by summing raster values for all clim match rasters for all PPUs in the case study
    # we will do this for each of the climate scenarios (reference, current, ssp2, and ssp5)
      # and both AOI (the ARP and the SRME)

# with this sum raster, we will see what areas on the landscape would have the greatest overall match for needs areas (PPUs)
  # for this part 3 of the case study, we are only using PPUs in 9000-9500 ft EB
    # as described in part 2

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
# values range from 1 to x,
# so only x of the 20 PPUs in this EB have overlapping climate match areas

plot(PPU_ref_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ref_match_sum_ARP_rast, "PPU_ref_match_sum_ARP_rast.tif")
PPU_ref_match_sum_ARP_rast <- rast("PPU_ref_match_sum_ARP_rast.tif")

##### stats ----
  # we want to know what proportion of the whole AOI (and/or acres covered)
    # has clim match for at least 1 PPU (and/or half of the PPUs)
# repeat for other periods/scenarior and both AOIs


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



## (2) overlay ----



## (1) sum V1 ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/results/test_results/CL_PPUs_8500_9000"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_fut_match_sum_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 29,
# so only 29 of the 40 PPUs in this EB have overlapping climate match areas

plot(PPU_fut_match_sum_rast)

##### write & read ----
writeRaster(PPU_fut_match_sum_rast, "PPU_fut_match_sum_rast.tif")
PPU_fut_match_sum_rast <- rast("PPU_fut_match_sum_rast.tif")

### overlay ----
# now we want the PCU vector to have an attribute for each of the climate scenarios
  # for now, I will just do the ARP clim match bc the PCU polys are only in the ARP
ARP_all_PCUs_vect <- vect("ARP_all_PCUs_vect.shp")
plot(ARP_all_PCUs_vect)

#### curr ----
# extract "match value" for each PCU
match_curr_sum_df <- extract(PPU_curr_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)
str(match_curr_sum_df)

# rename, arrange, add rank
match_curr_sum_df <- match_curr_sum_df %>% 
  rename(match_curr = match,
         PCU_ID = ID)

#### ssp2 ----
# extract "match value" for each PCU
match_ssp2_sum_df <- extract(PPU_ssp2_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)
str(match_ssp2_sum_df)

# rename, arrange, add rank
match_ssp2_sum_df <- match_ssp2_sum_df %>% 
  rename(match_ssp2 = match,
         PCU_ID = ID)

#### ssp5 ----
# extract "match value" for each PCU
match_ssp5_sum_df <- extract(PPU_ssp5_match_sum_ARP_rast, ARP_all_PCUs_vect, fun = mean, na.rm = TRUE)
str(match_ssp5_sum_df)

# rename, arrange, add rank
match_ssp5_sum_df <- match_ssp5_sum_df %>% 
  rename(match_ssp5 = match,
         PCU_ID = ID)


### merge ----
# combine all 6 dfs

match_join_df <- match_curr_sum_df %>% 
  left_join(match_ssp2_sum_df, by = "PCU_ID") %>% 
  left_join(match_ssp5_sum_df, by = "PCU_ID")

# merge with spatvector
ARP_all_PCUs_vect <- ARP_all_PCUs_vect %>% 
  left_join(match_join_df, by = "PCU_ID")


### filter curr ----
# filter only PCUs that are within climate-matched areas (have a match score >= 1)
ARP_PCUs_curr_clim_matched_vect <- ARP_all_PCUs_vect %>% 
  filter(match_curr >= 1)
# has 1813 geoms
(1813/6428)*100 # 28.20473 % of all the PCUs in the ARP are climate matched (under current climate) with at least 1 PPU

plot(ARP_PCUs_curr_clim_matched_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
polys(CL_PPU_8500_9000_vect, col = "orange", alpha=0.01, lwd=0.1)

#### write & read ----
writeVector(ARP_PCUs_curr_clim_matched_vect, "ARP_PCUs_curr_clim_matched_vect.shp")
ARP_PCUs_curr_clim_matched_vect <- vect("ARP_PCUs_curr_clim_matched_vect.shp")


### filter ssp2 ----
# filter only PCUs that are within climate-matched areas (have a match score >= 1)
ARP_PCUs_ssp2_clim_matched_vect <- ARP_all_PCUs_vect %>% 
  filter(match_ssp2 >= 1)
# has 307 geoms
(307/6428)*100 # 4.77598 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 1 PPU

plot(ARP_PCUs_ssp2_clim_matched_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
polys(CL_PPU_8500_9000_vect, col = "orange", alpha=0.01)

#### write & read ----
writeVector(ARP_PCUs_ssp2_clim_matched_vect, "ARP_PCUs_ssp2_clim_matched_vect.shp")
ARP_PCUs_ssp2_clim_matched_vect <- vect("ARP_PCUs_ssp2_clim_matched_vect.shp")


# repeat with 9000-9500 EB
ARP_PCUs_match_ssp2_sum <- ARP_all_PCUs_vect %>% 
  left_join(match_ssp2_sum_df, by = "PCU_ID")

# filter only PCUs that are within climate-matched areas (have a match score >= 1)
ARP_PCUs_9000_9500_ssp2_vect <- ARP_PCUs_match_ssp2_sum %>% 
  filter(match_ssp2 >= 1)
# has 1679 geoms
(1679/6428)*100 # 26.1201 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 1 PPU

sum(ARP_PCUs_9000_9500_ssp2_vect$area_acres) # 186385.3 acres
# ARP is 1723619 acres
(186385.3/1723619)*100 # 10.8136 %

plot(ARP_PCUs_9000_9500_ssp2_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

#### write & read ----
writeVector(ARP_PCUs_9000_9500_ssp2_vect, "ARP_PCUs_9000_9500_ssp2_vect.shp")
ARP_PCUs_9000_9500_ssp2_vect <- vect("ARP_PCUs_9000_9500_ssp2_vect.shp")


# filter only PCUs that meet 1/2 of needs (have a match score >= 10)
ARP_PCUs_half_match_9000_9500_ssp2_vect <- ARP_PCUs_match_ssp2_sum %>% 
  filter(match_ssp2 >= 10)
# has 346 geoms
(346/6428)*100 # 5.382701 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 10 PPUs (half of the 9000-9500 EB case study)

sum(ARP_PCUs_half_match_9000_9500_ssp2_vect$area_acres) # 32673.64 acres
  # ARP is 1723619 acres
(32673.64/1723619)*100 # 1.895642 % 

#### write & read ----
writeVector(ARP_PCUs_half_match_9000_9500_ssp2_vect, "ARP_PCUs_half_match_9000_9500_ssp2_vect.shp")
ARP_PCUs_half_match_9000_9500_ssp2_vect <- vect("ARP_PCUs_half_match_9000_9500_ssp2_vect.shp")


### filter ssp5 ----
# filter only PCUs that are within climate-matched areas (have a match score >= 1)
ARP_PCUs_ssp5_clim_matched_vect <- ARP_all_PCUs_vect %>% 
  filter(match_ssp5 >= 1)
# has 28 geoms
(28/6428)*100 # 0.4355943 % of all the PCUs in the ARP are climate matched (under ssp5) with at least 1 PPU

plot(ARP_PCUs_ssp5_clim_matched_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
polys(CL_PPU_8500_9000_vect, col = "orange", alpha=0.01)

#### write & read ----
writeVector(ARP_PCUs_ssp5_clim_matched_vect, "ARP_PCUs_ssp5_clim_matched_vect.shp")
ARP_PCUs_ssp5_clim_matched_vect <- vect("ARP_PCUs_ssp5_clim_matched_vect.shp")









