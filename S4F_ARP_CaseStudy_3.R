# description ----

# Seeds 4 the Future ARP Case Study

# Part 3 prioritizes PCUs by overlaying them with the climate match area for our case study PPUs
  # and investigates whether nursery seedlots overlay with the climate match of PPUs

# Part 3 also adds these planting-site-specific attributes to the PCUs 

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
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ref_match_ARP_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ref_match_ARP_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ref_match_sum_ARP_rast <- app(PPU_ref_match_ARP_rast, fun = "sum", na.rm = TRUE)
# max value = 15

plot(PPU_ref_match_sum_ARP_rast)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ref_match_sum_ARP_rast, "PPU_ref_match_sum_ARP_rast.tif")
PPU_ref_match_sum_ARP_rast <- rast("PPU_ref_match_sum_ARP_rast.tif")

##### stats ----
global(PPU_ref_match_sum_ARP_rast, fun = "notNA") # 2910 cells
global(ref_ARP_rast, fun = "notNA") # 6937 cells in all ARP
  # the climate match rasters (1000x1000 m) are not the same resolution as other raster data in part 1 (30x30 m)
    # so using the ref_ARP_rast here for total # cells
(2910/6937)*100 # = 41.94897 % of ARP is climate matched with at least 1 PPU



#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ref_match_SRME_ref"
setwd(target_directory)

# create a list of raster files in folder 
rast_files <- list.files(path = target_directory, pattern = "\\.tif$", full.names = TRUE)

# create a spatraster with a layer for each rast generated from match_clims() in Part_2
PPU_ref_match_SRME_rast <- rast(rast_files)

# sum rast values across all layers
PPU_ref_match_sum_SRME_rast <- app(PPU_ref_match_SRME_rast, fun = "sum", na.rm = TRUE)
  # max value = 15

plot(PPU_ref_match_sum_SRME_rast)
polys(SRME_vect, col = "black", alpha=0.01, lwd=0.5)

##### write & read ----
writeRaster(PPU_ref_match_sum_SRME_rast, "PPU_ref_match_sum_SRME_rast.tif")
PPU_ref_match_sum_SRME_rast <- rast("PPU_ref_match_sum_SRME_rast.tif")

##### stats ----
global(PPU_ref_match_sum_SRME_rast, fun = "notNA") # 56678 cells
global(ssp2_SRME_rast, fun = "notNA") # 134124 cells in all SRME
(56678/134124)*100 # = 42.25791 % of SRME is climate matched with at least 1 PPU



### curr ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/curr_match_ARP_ref"
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
global(PPU_curr_match_sum_ARP_rast, fun = "notNA") # 1839 cells
(1839/6937)*100 # = 26.51002 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/curr_match_SRME_ref"
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
global(PPU_curr_match_sum_SRME_rast, fun = "notNA") # 53103 cells
(53103/134124)*100 # = 39.59247 % of SRME is climate matched with at least 1 PPU


### ssp2 ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp2_match_ARP_ref"
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
global(PPU_ssp2_match_sum_ARP_rast, fun = "notNA") # 1753 cells
(1753/6937)*100 # = 25.27029 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp2_match_SRME_ref"
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
global(PPU_ssp2_match_sum_SRME_rast, fun = "notNA") # 35018 cells
(35018/134124)*100 # = 26.10868 % of SRME is climate matched with at least 1 PPU



### ssp5 ----
#### ARP ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp5_match_ARP_ref"
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
global(PPU_ssp5_match_sum_ARP_rast, fun = "notNA") # 973 cells
(973/6937)*100 # = 14.02624 % of ARP is climate matched with at least 1 PPU


#### SRME ----
# set path to folder with all match_clim() output rasters
target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp5_match_SRME_ref"
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
global(PPU_ssp5_match_sum_SRME_rast, fun = "notNA") # 23811 cells
(23811/134124)*100 # = 17.75297 % of SRME is climate matched with at least 1 PPU



## (2) overlay PCUs ----
# we want the PCU vector to have an attribute for each of the climate scenarios
  # above, (1) we created a new sum raster for each scenario
    # where each pixel value represents the "match sum" (total #) of PPUs that are matched at that pixel location
    # pixels with higher values represent areas where more PPUs share climate match
      # which is important bc we want to collect in areas that will meet larger planting needs
  # now, (2) we want to extract the # of matching PPUs across the entire area of each PCU polygon
    # we will average (extract the mean) # of matching PPUs   
    # and add this mean value as an attribute for each PCU, for each climate scenario
  
### (a) read PCUs
# using the PCU vector created in part 1
ARP_PCUs_vect <- vect("ARP_PCUs_vect.shp")
plot(ARP_PCUs_vect)

# only using the 4 PPU_X_match_sum_ARP_rast (not the SRME too) because all PCUs are in the ARP


### (b) extract ----
#### ref ----
# (0) confirm names
names(PPU_ref_match_ARP_rast) # each is "ref_U_100" with the PPU_ID number, 1 PPU per layer

# (1) extract the presence of a match for each layer (one for each PPU polygon), per PCU polygon
  # set a custom function which makes the columns int not num
presence_fun <- function(x) as.integer(any(!is.na(x)))
  # x = vector of all pixel values of the raster layer that fall within the polygon
  # !is.na(x) turns that vector into logicals: TRUE for every pixel that is not NA, FALSE for NAs
  # any() returns TRUE if at least one pixel within that polygon-layer is not NA
  # as.integer() converts TRUE -> 1 and FLASE -> 0
    # so we can still use rowSums to get the total number of PPU layers that are matching

ref_match_df <- extract(PPU_ref_match_ARP_rast, ARP_PCUs_vect, fun = presence_fun)
str(ref_match_df)
  # rows = PCU polys
  # cols = raster layers (which correspond to PPU polys)

# (2) drop the auto-generated extract ID column
  # I would name this object something different next time... 
ref_match_df2 <- ref_match_df[ , -1, drop = FALSE]
str(ref_match_df2)

# (3) create 2 attributes
ref_sum <- rowSums(ref_match_df2, na.rm = TRUE)
  # get # of rows with value = 1 (TRUE), meaning the PCU poly could overlap with any # of PPU match rasters (0-18)

  # build per-row list of matching PPU_IDs using layer names
ref_matches <- apply(ref_match_df2, 1, function(row) { 
    # apply() iterates row-wise because of the 1
    # for each row (one PCU poly), it passes a vector row containing the values across all PPU layers
  
  ids <- names(ref_match_df2)[which(row ==1)]
    # finds the indices of layers where polygon has a match (value 1)
      # which() returns integer positions of TRUEs
      # if row may contain NA, consider: which(!is.na(row) & row ==1)
    # names() uses those indices to select the column names from presence_mat
      # these names are the PPU_IDs in abbreviated form and match the raster layer names
      # could also use colnames()
    # if these are no matches (index vector is length 0), ids becomes character(0)
    # ids becomes a character vector 
  
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
    # if else; conditional expression, returns TRUE or FALSE
    # lengths(ids) == 0; 
      # lengths() gives the number of elements in the ids vector
      # == 0 means there were no matches for the polygon (the vector is empty)
    # if there were no 1s in the row, return NA
    # otherwise, join all matching PPU_IDs into a single comma-separated string
      # this string is the compact attribute we want to add to the spatvector for which layers of the raster are matching
      # paste(); if there are matches (lengths(ids) >0), 
        # this takes all the PPU_IDs in ids and combines them into a single string, separated by commas
    
    # apply() collects the return from each row's function into a character vector ref_matches, with one element per PCU poly
})

# (4) make new df
ref_match_df2 <- tibble(
  PCU_ID =  ARP_PCUs_vect$PCU_ID,
  ref_sum = ref_sum,
  ref_matches = ref_matches
)

# (5) attach to PCU_vect
ARP_PCUs_CS_vect <- cbind(ARP_PCUs_vect, ref_match_df2[ , c("ref_sum", "ref_matches")])

ARP_PCUs_CS_df <- as.data.frame(ARP_PCUs_CS_vect)
  # this worked! 


#### curr ----
curr_match_df <- extract(PPU_curr_match_ARP_rast, ARP_PCUs_vect, fun = presence_fun)
curr_match_df2 <- curr_match_df[ , -1, drop = FALSE]
curr_sum <- rowSums(curr_match_df2, na.rm = TRUE)
curr_matches <- apply(curr_match_df2, 1, function(row) { 
  ids <- names(curr_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
curr_match_df2 <- tibble(
  PCU_ID =  ARP_PCUs_vect$PCU_ID,
  curr_sum = curr_sum,
  curr_matches = curr_matches
)
ARP_PCUs_CS_vect <- cbind(ARP_PCUs_CS_vect, curr_match_df2[ , c("curr_sum", "curr_matches")])


#### ssp2 ----
ssp2_match_df <- extract(PPU_ssp2_match_ARP_rast, ARP_PCUs_vect, fun = presence_fun)
ssp2_match_df2 <- ssp2_match_df[ , -1, drop = FALSE]
ssp2_sum <- rowSums(ssp2_match_df2, na.rm = TRUE)
ssp2_matches <- apply(ssp2_match_df2, 1, function(row) { 
  ids <- names(ssp2_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
ssp2_match_df2 <- tibble(
  PCU_ID =  ARP_PCUs_vect$PCU_ID,
  ssp2_sum = ssp2_sum,
  ssp2_matches = ssp2_matches
)
ARP_PCUs_CS_vect <- cbind(ARP_PCUs_CS_vect, ssp2_match_df2[ , c("ssp2_sum", "ssp2_matches")])

#### ssp5 ----
ssp5_match_df <- extract(PPU_ssp5_match_ARP_rast, ARP_PCUs_vect, fun = presence_fun)
ssp5_match_df2 <- ssp5_match_df[ , -1, drop = FALSE]
ssp5_sum <- rowSums(ssp5_match_df2, na.rm = TRUE)
ssp5_matches <- apply(ssp5_match_df2, 1, function(row) { 
  ids <- names(ssp5_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
ssp5_match_df2 <- tibble(
  PCU_ID =  ARP_PCUs_vect$PCU_ID,
  ssp5_sum = ssp5_sum,
  ssp5_matches = ssp5_matches
)
ARP_PCUs_CS_vect <- cbind(ARP_PCUs_CS_vect, ssp5_match_df2[ , c("ssp5_sum", "ssp5_matches")])


##### write & read ----
writeVector(ARP_PCUs_CS_vect, "ARP_PCUs_CS_vect.shp")
ARP_PCUs_CS_vect <- vect("ARP_PCUs_CS_vect.shp")

ARP_PCUs_CS_df <- as.data.frame(ARP_PCUs_CS_vect)
write.csv(ARP_PCUs_CS_df, "ARP_PCUs_CS_df.csv", row.names = FALSE)


### (c) filter 2 ----

#### curr, 1 match----
# filter only PCUs that are within current climate-matched areas (have a match score >= 1 for curr)
PCUs_matched_curr_vect <- ARP_PCUs_CS_vect %>%
  filter(curr_sum >= 1)

##### stats ----
# has 1807 geoms
(1807/6428)*100 # 28.11139 % of all the PCUs in the ARP are climate matched (under curr) with at least 1 PPU

sum(PCUs_matched_curr_vect$area_acres) # 196223.6 acres
# ARP is 1723619 acres
(196223.6/1723619)*100 # 11.3844 % of area of ARP

plot(PCUs_matched_curr_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

###### write & read ----
writeVector(PCUs_matched_curr_vect, "PCUs_matched_curr_vect.shp")
PCUs_matched_curr_vect <- vect("PCUs_matched_curr_vect.shp")

PCUs_curr_df <- as.data.frame(PCUs_matched_curr_vect)



#### ssp2, 1 match----
# filter only PCUs that are within future climate-matched areas (have a match score >= 1 for spp2)
PCUs_matched_ssp2_vect <- ARP_PCUs_CS_vect %>%
  filter(ssp2_sum >= 1)

##### stats ----
# has 1390 geoms
(1390/6428)*100 # 21.62414 % of all the PCUs in the ARP are climate matched (under ssp2) with at least 1 PPU

sum(PCUs_matched_ssp2_vect$area_acres) # 144477.6 acres
# ARP is 1723619 acres
(144477.6/1723619)*100 # 8.382224 % of area of ARP

plot(PCUs_matched_ssp2_vect)
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)

###### write & read ----
writeVector(PCUs_matched_ssp2_vect, "PCUs_matched_ssp2_vect.shp")
PCUs_matched_ssp2_vect <- vect("PCUs_matched_ssp2_vect.shp")

PCUs_ssp2_df <- as.data.frame(PCUs_matched_ssp2_vect)





### (d) filter 3 ----
# these is the objective-specific filter,
# we are inventing the objectives for the case study
# they are to model the options that managers have when prioritizing PCUs

# we could of course combine these filters into one code chunk
# but I want to get the breakdown for each category

#### target sp ----
PCUs_ssp2_f3_sp <- PCUs_matched_ssp2_vect %>% 
  filter(PIPO_tons >= 10)
  # has 641 geoms

#### risk ----
PCUs_ssp2_f3_cfp <- PCUs_ssp2_f3_sp %>% 
  filter(CFP_prob >= 0.5)
# has 299 geoms

#### stats ----
(299/6428)*100 # 4.651525 % of all the PCUs in the ARP meet filter 3 objectives

sum(PCUs_ssp2_f3_cfp$area_acres) # 32088.97 acres
# ARP is 1723619 acres
(32088.97/1723619)*100 # 1.861721 % of area of ARP




#### district ----
# PCUs_ssp2_f3_dist <- PCUs_ssp2_f3_sp %>% 
#   filter(DISTRICTNA == "Canyon Lakes Ranger District")
#   # has 430 geoms
# 
#### elevation ----
# PCUs_ssp2_f3_elv <- PCUs_ssp2_f3_dist %>% 
#   filter(Elv_med_ft >= 8000)
#   # has 67 geoms





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



### (a) read SLs ----
# created in Part 1_B
seed_SRME_vect <- vect("seed_SRME_vect.shp")


### (b) extract ----
#### ref ----
# using same method for extraction as with the PCU overlay (Part 3-2)
ref_match_df_sl <- extract(PPU_ref_match_SRME_rast, seed_SRME_vect, fun = presence_fun)
ref_match_df2 <- ref_match_df_sl[ , -1, drop = FALSE]
ref_sum <- rowSums(ref_match_df2, na.rm = TRUE)
ref_matches <- apply(ref_match_df2, 1, function(row) { 
  ids <- names(ref_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
ref_match_df2 <- tibble(
  SL_ID =  seed_SRME_vect$SL_ID,
  ref_sum = ref_sum,
  ref_matches = ref_matches
)
seed_SRME_CS_vect <- cbind(seed_SRME_vect, ref_match_df2[ , c("ref_sum", "ref_matches")])
seed_SRME_CS_df <- as.data.frame(seed_SRME_CS_vect)


#### curr ----
curr_match_df_sl <- extract(PPU_curr_match_SRME_rast, seed_SRME_vect, fun = presence_fun)
curr_match_df2 <- curr_match_df_sl[ , -1, drop = FALSE]
curr_sum <- rowSums(curr_match_df2, na.rm = TRUE)
curr_matches <- apply(curr_match_df2, 1, function(row) { 
  ids <- names(curr_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
curr_match_df2 <- tibble(
  SL_ID =  seed_SRME_vect$SL_ID,
  curr_sum = curr_sum,
  curr_matches = curr_matches
)
seed_SRME_CS_vect <- cbind(seed_SRME_CS_vect, curr_match_df2[ , c("curr_sum", "curr_matches")])
seed_SRME_CS_df <- as.data.frame(seed_SRME_CS_vect)


#### ssp2 ----
ssp2_match_df_sl <- extract(PPU_ssp2_match_SRME_rast, seed_SRME_vect, fun = presence_fun)
ssp2_match_df2 <- ssp2_match_df_sl[ , -1, drop = FALSE]
ssp2_sum <- rowSums(ssp2_match_df2, na.rm = TRUE)
ssp2_matches <- apply(ssp2_match_df2, 1, function(row) { 
  ids <- names(ssp2_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
ssp2_match_df2 <- tibble(
  SL_ID =  seed_SRME_vect$SL_ID,
  ssp2_sum = ssp2_sum,
  ssp2_matches = ssp2_matches
)
seed_SRME_CS_vect <- cbind(seed_SRME_CS_vect, ssp2_match_df2[ , c("ssp2_sum", "ssp2_matches")])
seed_SRME_CS_df <- as.data.frame(seed_SRME_CS_vect)


#### ssp5 ----
ssp5_match_df_sl <- extract(PPU_ssp5_match_SRME_rast, seed_SRME_vect, fun = presence_fun)
ssp5_match_df2 <- ssp5_match_df_sl[ , -1, drop = FALSE]
ssp5_sum <- rowSums(ssp5_match_df2, na.rm = TRUE)
ssp5_matches <- apply(ssp5_match_df2, 1, function(row) { 
  ids <- names(ssp5_match_df2)[which(row ==1)]
  if (length(ids) == 0) NA_character_ else paste(ids, collapse = ",")
})
ssp5_match_df2 <- tibble(
  SL_ID =  seed_SRME_vect$SL_ID,
  ssp5_sum = ssp5_sum,
  ssp5_matches = ssp5_matches
)
seed_SRME_CS_vect <- cbind(seed_SRME_CS_vect, ssp5_match_df2[ , c("ssp5_sum", "ssp5_matches")])


# ** need to run ----
  # first need DEM for all of SRME
#### Elv medium ----

# **modify for SRME
# using ARP_DEM created in Part1A_3b
ARP_DEM_rast <- rast("ARP_DEM_rast.tif")
summary(ARP_DEM_rast) # min = 1638, max = 4276

# the DEM is in meters, but we want ft
# convert m to ft
meters_to_feet_factor <- 3.28084
ARP_DEM_ft <- ARP_DEM_rast * meters_to_feet_factor 
summary(ARP_DEM_ft) # min = 5374, max = 14030  

# extract median
Elv_med_df <- extract(ARP_DEM_ft, seed_SRME_CS_vect, fun=median)
str(Elv_med_df)
# rename col
Elv_med_df <- Elv_med_df %>% 
  rename(Elv_med_ft = USGS_1_n41w106_20230314) %>% 
  mutate(SL_ID = seed_SRME_CS_vect$SL_ID) %>% 
  select(-1)
seed_SRME_CS_vect <- cbind(seed_SRME_CS_vect, Elv_med_df)



##### write & read ----
writeVector(seed_SRME_CS_vect, "seed_SRME_CS_vect.shp")
seed_SRME_CS_vect <- vect("seed_SRME_CS_vect.shp")

seed_SRME_CS_df <- as.data.frame(seed_SRME_CS_vect)
write.csv(seed_SRME_CS_df, "seed_SRME_CS_df.csv", row.names = FALSE)


### (c) filter ----

#### ssp2, 1 match ----
SLs_matched_ssp2_vect <- seed_SRME_CS_vect %>%
  filter(ssp2_sum >= 1)

##### stats ----
# has 31 geoms
(31/158)*100 # 19.62025 % of all the SLs in the SRME are climate matched (under ssp2) with at least 1 PPUs (from the 9000-9500 EB case study)

##### write & read ----
writeVector(SLs_matched_ssp2_vect, "SLs_matched_ssp2_vect.shp")
SLs_matched_ssp2_vect <- vect("SLs_matched_ssp2_vect.shp")

SLs_matched_ssp2_points_vect <- centroids(SLs_matched_ssp2_vect)
writeVector(SLs_matched_ssp2_points_vect, "SLs_matched_ssp2_points_vect.shp")




