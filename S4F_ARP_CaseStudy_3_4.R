# desctiption ----

# Seeds 4 the Future ARP Case Study, the full script
# parts 3 & 4, prioritize PCUs with...
  # 3 = overlay climate match of PPUs with PCUs
  # 4 = overlay PCUs with nursery inventory

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025

# see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project

# setup ----

library(terra) # primary spatial data management package
library(tidyterra) # dplyr-style data management for spatial data
library(dplyr)


# part 3 ----

## FWD ----

## REV ----

### sum ----
# we want to combine the output of match_clims() function
  # we will do this by summing raster values for all clim match rasters for all PPUs in the case study
    # we will do this for each of the climate scenarios (current, ssp2, and ssp5)
      # and both AOI (the ARP and the SRME)

# we will see what areas on the landscape would have the greatest overall match for needs areas (PPUs)
  # for this part of the case study, only using PPUs in 8500-9000 ft EB

#### V1 ----
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



#### curr ----
##### ARP ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_8500_9000/curr_match_ARP_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_curr_match_sum_ARP_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 12,
# so only 12 of the 13 PPUs in this EB have overlapping climate match areas

plot(PPU_curr_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
polys(CL_PPU_8500_9000_vect, col = "orange", alpha=0.01, lwd=0.1)

##### write & read ----
writeRaster(PPU_curr_match_sum_ARP_rast, "PPU_curr_match_sum_ARP_rast.tif")
PPU_curr_match_sum_ARP_rast <- rast("PPU_curr_match_sum_ARP_rast.tif")


##### SRME ----
# set path to folder with all match_clim() output rasters
rast_folder <- getwd() 
# "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/PPU_8500_9000/curr_match_ARP_ref"

# create a list of raster files in folder 
rast_files <- list.files(path = rast_folder, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster collection
rast_collection <- sds(rast_files)

# sum rast values across all layers
PPU_curr_match_sum_SRME_rast <- app(rast_collection, fun = "sum", na.rm = TRUE)
# values range from 1 to 12,
# so only 12 of the 13 PPUs in this EB have overlapping climate match areas

plot(PPU_curr_match_sum_SRME_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
polys(CL_PPU_8500_9000_vect, col = "orange", alpha=0.01, lwd=0.1)

#### ssp2 ----
##### ARP ----

##### SRME ----


#### ssp5 ----
##### ARP ----

##### SRME ----



### overlay ----
#### curr ----
##### ARP ----
# extract "match value" for each PCU
extract_match_mean_df <- extract(PPU_fut_match_sum_rast, ARP_PCUs_vect, fun = mean, na.rm = TRUE)
str(extract_match_mean_df)

# rename, arrange, add rank
extract_match_mean_df <- extract_match_mean_df %>% 
  rename(match_mean = match,
         PCU_ID = ID) %>% 
  arrange(desc(match_mean)) %>%
  mutate(match_rank = row_number())

##### SRME ----


#### ssp2 ----
##### ARP ----

##### SRME ----


#### ssp5 ----
##### ARP ----

##### SRME ----


### merge ----
# combine all 6 dfs

# merge with spatvector
ARP_PCUs_clim_matched_vect <- ARP_PCUs_vect %>% 
  left_join(extract_match_mean_df, by = "PCU_ID")
plot(ARP_PCUs_clim_matched_vect)

# filter only PCUs that are within climate-matched areas (have a match score >= 1)
ARP_PCUs_clim_matched_vect <- ARP_PCUs_clim_matched_vect %>% 
  filter(match_mean >= 1)
# has 227 geoms
plot(ARP_PCUs_clim_matched_vect)

#### write & read ----
writeVector(ARP_PCUs_clim_matched_vect, "ARP_PCUs_clim_matched_vect.shp")
ARP_PCUs_clim_matched_vect <- vect("ARP_PCUs_clim_matched_vect.shp")





