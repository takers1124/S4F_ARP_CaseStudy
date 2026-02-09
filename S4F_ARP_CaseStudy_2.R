# description ----

# Seeds 4 the Future ARP Case Study, the full script
  # part 2, climate match Potential Planting Units (PPUs)

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project

# setup ----

library(terra) 
library(tidyterra) 
library(dplyr)
library(ggplot2)

## AOI ----
# created in part 1
ARP_vect <- vect("ARP_vect.shp")
SRME_vect <- vect("SRME_vect.shp")

## zones ----
### FWD ----
# these polygons are for existing seedlots (in nursery) or potential collection units (PCUs)
  # use reference climate of collection unit
  # match with future climate (where best to plant)

# the PCU closest to the Lady Moon trail head has PCU_ID = 204
  # we will use this for our case study
ARP_PCUs_vect <- vect("ARP_PCUs_vect.shp")
PCU_LM <- ARP_PCUs_vect %>% 
  filter(PCU_ID == 204)


### REV ----
# these polygons were created from existing planting needs (e.g. from FACTS) 
  # in part 1A, we call them  potential planting units (PPUs)
# use future climate of planting unit
  # match with reference climate (where best to collect)

# for our case study, we chose only the planting needs (PPUs) within the Cameron Peak fire boundary
  # and also only a subset of those planting needs which are in the 9000-9500 ft elevation band (EB)
CP_PPUs_CaseStudy_vect <- vect("CP_PPUs_CaseStudy_vect.shp")



# (1) import clims ----
# just using 1 climate variable and 4 time periods 

reference_1961_1990_MCMT <- rast("Normal_1961_1990_MCMT.tif")
current_2011_2040_MCMT <- rast("UKESM10LL_ssp245_2011_2040_MCMT.tif")
ssp2_2041_2070_MCMT <- rast("UKESM10LL_ssp245_2041_2070_MCMT.tif")
ssp5_2041_2070_MCMT <- rast("UKESM10LL_ssp585_2041_2070_MCMT.tif")


# project, crop, & mask 
## reference ----
ref_projected <- project(reference_1961_1990_MCMT, "EPSG:5070")

# ARP
ref_ARP_rast <- crop(ref_projected, ARP_vect, mask = TRUE)
summary(ref_ARP_rast)  # min: -12.839, max: -1.303, mean: -7.612     
plot(ref_ARP_rast)  
names(ref_ARP_rast) <- "ref_MCMT"

# SRME 
ref_SRME_rast <- crop(ref_projected, SRME_vect, mask = TRUE)
summary(ref_SRME_rast)  # min: -13.878, max: -0.163, mean: -6.608  
plot(ref_SRME_rast) 
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
names(ref_SRME_rast) <- "ref_MCMT"

### write & read ----
writeRaster(ref_ARP_rast, "ref_ARP_rast.tif")
ref_ARP_rast <- rast("ref_ARP_rast.tif")

writeRaster(ref_SRME_rast, "ref_SRME_rast.tif")
ref_SRME_rast <- rast("ref_SRME_rast.tif")

## current ----
current_projected <- project(current_2011_2040_MCMT, "EPSG:5070")

# ARP
curr_ARP_rast <- crop(current_projected, ARP_vect, mask = TRUE)
summary(curr_ARP_rast)  # min: -10.5840, max: 0.8993  , mean: -5.3997       
plot(curr_ARP_rast)  
names(curr_ARP_rast) <- "curr_MCMT"

# SRME 
curr_SRME_rast <- crop(current_projected, SRME_vect, mask = TRUE)
summary(curr_SRME_rast)  # min: -11.478, max: 2.461, mean: -4.307     
plot(curr_SRME_rast) 
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
names(curr_SRME_rast) <- "curr_MCMT"

### write & read ----
writeRaster(curr_ARP_rast, "curr_ARP_rast.tif")
curr_ARP_rast <- rast("curr_ARP_rast.tif")

writeRaster(curr_SRME_rast, "curr_SRME_rast.tif")
curr_SRME_rast <- rast("curr_SRME_rast.tif")

## ssp2 ----
ssp2_projected <- project(ssp2_2041_2070_MCMT, "EPSG:5070") # took ~ 4 mins

# ARP
ssp2_ARP_rast <- crop(ssp2_projected, ARP_vect, mask = TRUE)
summary(ssp2_ARP_rast)  # min: -8.2840, max: 3.3150, mean: -3.0287     
plot(ssp2_ARP_rast)  
names(ssp2_ARP_rast) <- "ssp2_MCMT"

# SRME
ssp2_SRME_rast <- crop(ssp2_projected, SRME_vect, mask = TRUE)
summary(ssp2_SRME_rast) # min: -9.078, max: 5.480, mean: -1.630
plot(ssp2_SRME_rast)    
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
names(ssp2_SRME_rast) <- "ssp2_MCMT"

### write & read ----
writeRaster(ssp2_ARP_rast, "ssp2_ARP_rast.tif")
ssp2_ARP_rast <- rast("ssp2_ARP_rast.tif")

writeRaster(ssp2_SRME_rast, "ssp2_SRME_rast.tif")
ssp2_SRME_rast <- rast("ssp2_SRME_rast.tif")


## ssp5 ----
ssp5_projected <- project(ssp5_2041_2070_MCMT, "EPSG:5070") # took ~ 4 mins

# ARP
ssp5_ARP_rast <- crop(ssp5_projected, ARP_vect, mask = TRUE)
summary(ssp5_ARP_rast)  # min: -7.0859, max: 4.5150, mean: -1.8743   
plot(ssp5_ARP_rast)  
names(ssp5_ARP_rast) <- "ssp2_MCMT"

# SRME
ssp5_SRME_rast <- crop(ssp5_projected, SRME_vect, mask = TRUE)
summary(ssp5_SRME_rast)  # min: -8.078, max: 6.868, mean: -0.363 
plot(ssp5_SRME_rast)  
polys(ARP_vect, col = "black", alpha=0.01, lwd=0.5)
names(ssp5_SRME_rast) <- "ssp2_MCMT"

### write & read ----
writeRaster(ssp5_ARP_rast, "ssp5_ARP_rast.tif")
ssp5_ARP_rast <- rast("ssp5_ARP_rast.tif")

writeRaster(ssp5_SRME_rast, "ssp5_SRME_rast.tif")
ssp5_SRME_rast <- rast("ssp5_SRME_rast.tif")



# (2) extract clims ----
  # note: the zones spatVector must have unique ID for each zone (polygon geometry) as the first listed attribute
    
## create function ----
extract_clims <- function(zones, clim_rast) {
  # initialize empty vectors
  z_df <- c()
  full_df <- c()
  z_max <- c()
  z_min <- c()
  z_median <- c()
  z_TL <- c()
  
  # loop through each zone in the set of zones
  for (z in 1:nrow(zones)) {
    # extract maximum value of the variable in the zone
    # [,2] selects for just the second column of the data.frame resulting from extract()
    z_max[z] <- as.numeric(extract(clim_rast, zones[z], max, na.rm = TRUE)[,2])
    # extract minimum value of the variable in the zone
    z_min[z] <- as.numeric(extract(clim_rast, zones[z], min, na.rm = TRUE)[,2])
    # extract median value of the variable in the zone
    z_median[z] <- as.numeric(extract(clim_rast, zones[z], median, na.rm = TRUE)[,2])
    
    # calculate the transfer limit as half the range of variation in the zone
    z_TL[z] <- (z_max[[z]]-z_min[[z]])/2
  }
  
  # make a temp dataframe copying the existing attribute data for each zone from the spatvector
  z_df <- as.data.frame(zones)
  
  # append climate summary data
  z_df$zone_max <- z_max
  z_df$zone_min <- z_min
  z_df$zone_median <- z_median
  z_df$zone_TL <- z_TL
  # add to master dataframe
  full_df <- rbind(full_df,z_df)
  
  return(full_df)
}



## run it ----

### FWD ----
# using the PCU_LM poly closest to Lady Moon trailhead
  # this were created in part 1

#### ref ----
# we only want to extract the ref clim from this PCU
ref_MCMT_PCU_df <- extract_clims(PCU_LM_vect, ref_ARP_rast)
str(ref_MCMT_PCU_df) # has original PCU attributes + new extracted climate metrics


### REV ----
# using the FACTS needs polys from within the Cameron Peak fire boundary
  # we divided the FACTS needs into small (50-200 acre) polygons
  # then assigned a 500 ft elevation band (EB) to each
    # and we are just using the 9000-9500 ft EB for this part of the case study
  # we are calling them potential planting units (PPUs)
# we want to extract the MCMT values from these PPUs
  # and we do that for each of the clim periods/scenarios 

#### ref ----
  # if want to plant seed adapted to reference climate (collected between 1961-1990 from same zone)
ref_MCMT_PPU_df <- extract_clims(CP_PPUs_CaseStudy_vect, ref_ARP_rast)
str(ref_MCMT_PPU_df)

#### current ----
  # if want to plant seed adapted to current climate (collected today from same zone)
curr_MCMT_PPU_df <- extract_clims(CP_PPUs_CaseStudy_vect, curr_ARP_rast)
str(curr_MCMT_PPU_df)

#### ssp2 ----
  # if want to plant seed adapted to future ssp2 climate (collected today from different zone)
ssp2_MCMT_PPU_df <- extract_clims(CP_PPUs_CaseStudy_vect, ssp2_ARP_rast)
str(ssp2_MCMT_PPU_df)

#### ssp5 ----
  # if want to plant seed adapted to future ssp5 climate (collected today from different zone)
ssp5_MCMT_PPU_df <- extract_clims(CP_PPUs_CaseStudy_vect, ssp5_ARP_rast)
str(ssp5_MCMT_PPU_df)




# (3) match clims ----
## create function ----

match_clims <- function(zone_df, clim_rast) {
  # (1) pull in metrics from df created with extract_clims()
  z_median <- zone_df$zone_median
  
  # (2) calc the difference (distance) between climate metric from the clim_rast (each cell) and the zonal extract 
  # then divide by 0.6 degrees C to normalize (incorporate variation)
  MCMT_diff <- abs(clim_rast - z_median)/0.6
  
  # (3) calc the match score
  m <- (-1 * (MCMT_diff - 1))*100
  
  # (4) adjust score
  # make any score that would be <=0 match just be NA (filter terrible matches)
  m[m <= 0] <- NA
  # and any match > 0 is just 1 (binary values, but can still add)
  m[m >0 ] <- 1
  
  # (5) adjust layer name
  # parse out the df name
  df_name <- deparse(substitute(zone_df))
  # grab just the prefix from df name
  prefix <- sub("_.*$", "", df_name)
  # grab the unit ID values from the first column
  ids <- as.character(zone_df[[1]])
  # assign layer name: climate prefix + U for unit + ID
  set.names(m, paste0(prefix, "_U_", ids))
  return(m)
}



## run it ----
### FWD ----

#### ssp2 ARP ----
# set path to results directory where you want output files to go

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/FWD_PCU_LM"
setwd(target_directory)

for (row in 1:nrow(ref_MCMT_PCU_df)) {
  # for each "zone" or PCU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ref_MCMT_PCU_df[row, ], ssp2_ARP_rast) 
  
  # create a new filename
  file_name <- paste0("PCU_", ref_MCMT_PCU_df[row, ]$PCU_ID, "_ref_match_ARP_ssp2.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ref_MCMT_PCU_df[row, ]$PCU_ID, " - ", 
               round((row/nrow(ref_MCMT_PCU_df))*100, 2), "% complete"))
}

#### ssp2 SRME ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/FWD_PCU_LM"
setwd(target_directory)

for (row in 1:nrow(ref_MCMT_PCU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ref_MCMT_PCU_df[row, ], ssp2_SRME_rast) 
  
  # create a new filename
  file_name <- paste0("PCU_", ref_MCMT_PCU_df[row, ]$PCU_ID, "_ref_match_SRME_ssp2.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ref_MCMT_PCU_df[row, ]$PCU_ID, " - ", 
               round((row/nrow(ref_MCMT_PCU_df))*100, 2), "% complete"))
}





### REV ----

#### ref ARP ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ref_match_ARP_ref"
setwd(target_directory)

for (row in 1:nrow(ref_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ref_MCMT_PPU_df[row, ], ref_ARP_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", ref_MCMT_PPU_df[row, ]$PPU_ID, "_ref_match_ARP_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ref_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(ref_MCMT_PPU_df))*100, 2), "% complete"))
}

#### ref SRME ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ref_match_SRME_ref"
setwd(target_directory)

for (row in 1:nrow(ref_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ref_MCMT_PPU_df[row, ], ref_SRME_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", ref_MCMT_PPU_df[row, ]$PPU_ID, "_ref_match_SRME_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ref_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(ref_MCMT_PPU_df))*100, 2), "% complete"))
}


#### curr ARP ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/curr_match_ARP_ref"
setwd(target_directory)

for (row in 1:nrow(curr_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(curr_MCMT_PPU_df[row, ], ref_ARP_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", curr_MCMT_PPU_df[row, ]$PPU_ID, "_curr_match_ARP_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", curr_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(curr_MCMT_PPU_df))*100, 2), "% complete"))
}

#### curr SRME ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/curr_match_SRME_ref"
setwd(target_directory)

for (row in 1:nrow(curr_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(curr_MCMT_PPU_df[row, ], ref_SRME_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", curr_MCMT_PPU_df[row, ]$PPU_ID, "_curr_match_SRME_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", curr_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(curr_MCMT_PPU_df))*100, 2), "% complete"))
}


#### ssp2 ARP ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp2_match_ARP_ref"
setwd(target_directory)

for (row in 1:nrow(ssp2_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ssp2_MCMT_PPU_df[row, ], ref_ARP_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", ssp2_MCMT_PPU_df[row, ]$PPU_ID, "_ssp2_match_ARP_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ssp2_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
              round((row/nrow(ssp2_MCMT_PPU_df))*100, 2), "% complete"))
}

#### ssp2 SRME ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp2_match_SRME_ref"
setwd(target_directory)

for (row in 1:nrow(ssp2_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ssp2_MCMT_PPU_df[row, ], ref_SRME_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", ssp2_MCMT_PPU_df[row, ]$PPU_ID, "_ssp2_match_SRME_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ssp2_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(ssp2_MCMT_PPU_df))*100, 2), "% complete"))
}


#### ssp5 ARP ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp5_match_ARP_ref"
setwd(target_directory)

for (row in 1:nrow(ssp5_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ssp5_MCMT_PPU_df[row, ], ref_ARP_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", ssp5_MCMT_PPU_df[row, ]$PPU_ID, "_ssp5_match_ARP_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ssp5_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(ssp5_MCMT_PPU_df))*100, 2), "% complete"))
}

#### ssp5 SRME ----

target_directory <- "C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data/tif_part2/REV_PPU_CS/ssp5_match_SRME_ref"
setwd(target_directory)

for (row in 1:nrow(ssp5_MCMT_PPU_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
  # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
  # produce a raster for each zone (row in df)
  match_rast <- match_clims(ssp5_MCMT_PPU_df[row, ], ref_SRME_rast) 
  
  # create a new filename
  file_name <- paste0("PPU_", ssp5_MCMT_PPU_df[row, ]$PPU_ID, "_ssp5_match_SRME_ref.tif")
  
  # writeRaster in the results directory
  writeRaster(match_rast, file_name, overwrite=TRUE)
  # print status update 
  print(paste0("Calculating match for each cell in clim_rast for zone # ", ssp5_MCMT_PPU_df[row, ]$PPU_ID, " - ", 
               round((row/nrow(ssp5_MCMT_PPU_df))*100, 2), "% complete"))
}




# (4) sum stats: MCMT of PPUs ----
# combine together MCMT values for case study PPUs
  # and make a summary stats graph

str(ref_MCMT_PPU_df)
  # rename cols
ref_MCMT_PPU_df <- ref_MCMT_PPU_df %>%
  rename(
    ref_max = zone_max,
    ref_min = zone_min,
    ref_median = zone_median,
    ref_TL = zone_TL
  )

str(curr_MCMT_PPU_df)
  # rename cols
curr_MCMT_PPU_df <- curr_MCMT_PPU_df %>%
  rename(
    curr_max = zone_max,
    curr_min = zone_min,
    curr_median = zone_median,
    curr_TL = zone_TL
  )

str(ssp2_MCMT_PPU_df)
  # rename cols
ssp2_MCMT_PPU_df <- ssp2_MCMT_PPU_df %>%
  rename(
    ssp2_max = zone_max,
    ssp2_min = zone_min,
    ssp2_median = zone_median,
    ssp2_TL = zone_TL
  )

str(ssp5_MCMT_PPU_df)
  # rename cols
ssp5_MCMT_PPU_df <- ssp5_MCMT_PPU_df %>%
  rename(
    ssp5_max = zone_max,
    ssp5_min = zone_min,
    ssp5_median = zone_median,
    ssp5_TL = zone_TL
  )


# Select only the renamed columns from each DF
combined_df <- ref_MCMT_PPU_df %>%
  select(PPU_ID, area_acres, Elv_med_ft) %>%
  bind_cols(
    ref_MCMT_PPU_df %>% select(starts_with("ref_")),
    curr_MCMT_PPU_df %>% select(starts_with("curr_")),
    ssp2_MCMT_PPU_df %>% select(starts_with("ssp2_")),
    ssp5_MCMT_PPU_df %>% select(starts_with("ssp5_"))
  )
str(combined_df)


# Select only PPU_ID and median columns
df_long <- combined_df %>%
  select(PPU_ID, ref_median, curr_median, ssp2_median, ssp5_median) %>%
  pivot_longer(cols = starts_with(c("ref", "curr", "ssp2", "ssp5")),
               names_to = "Period_Scenario", values_to = "MCMT") %>%
  mutate(Period_Scenario = factor(Period_Scenario,
                           levels = c("ref_median", "curr_median", "ssp2_median", "ssp5_median"),
                           labels = c("Reference", "Current", "Mid-SSP2", "Mid-SSP5")))
str(df_long)


# Plot: boxplot + jittered points
ggplot(df_long, aes(x = Period_Scenario, y = MCMT)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7, color = "darkblue") +
  labs(title = "Median MCMT of PPUs by Period/Scenario",
       x = "Climate Period/Scenario",
       y = "MCMT (°C)") +
  theme_minimal(base_size = 14)


# Repeated measures ANOVA
aov_result <- aov(MCMT ~ Period_Scenario + Error(PPU_ID/Period_Scenario), data = df_long)
summary(aov_result)


# Pairwise comparisons
pairwise_result <- pairwise.t.test(df_long$MCMT, df_long$Period_Scenario,
                                   paired = TRUE, p.adjust.method = "bonferroni")
pairwise_result











