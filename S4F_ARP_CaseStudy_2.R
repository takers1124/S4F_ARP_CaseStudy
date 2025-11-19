# desctiption ----

# Seeds 4 the Future ARP Case Study, the full script
# part 2, climate match Potential Planting Units (PPUs)

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025

# see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project

# setup ----

library(terra) # primary spatial data management package
library(tidyterra) # dplyr-style data management for spatial data
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

### REV ----
# these polygons are for existing planting needs (e.g. from FACTS) or potential planting units (PPUs)
  # use future climate of planting unit
  # match with reference climate (where best to collect)

# for our case study, we chose only the planting needs (PPUs) within the Cameron Peak fire boundary
  # and also only a subset of those planting needs which are in the 8500-9000 ft elevation band (EB)
CL_PPU_8500_9000_vect <- vect("CL_PPU_8500_9000_vect.shp")


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

### write & read 
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

### write & read 
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

### write & read 
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

### write & read 
writeRaster(ssp5_ARP_rast, "ssp5_ARP_rast.tif")
ssp5_ARP_rast <- rast("ssp5_ARP_rast.tif")

writeRaster(ssp5_SRME_rast, "ssp5_SRME_rast.tif")
ssp5_SRME_rast <- rast("ssp5_SRME_rast.tif")



# (2) extract clims ----
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
# using the polys from within the Canyon Lakes RD only
  # these were created in part 1
    # just Canyon Lakes was selected for this part of the case study
# we want to extract the normal clim from these areas
# we are calling them potential collection units (PCUs)

PCU_norm_MCMT_df <- extract_clims(CL_PCUs, normal_ARP_rast)
str(PCU_norm_MCMT_df) # has all the original attributes + new extracted climate metrics


### REV ----
# using the FACTS needs polys from within the Cameron Peak fire boundary
  # we divided the FACTS needs into small (50-200 acre) polygons
  # then assigned a 500 ft elevation band (EB) to each
    # and we are just using the 8500-9000 ft EB for this part of the case study
# we want to extract the future clim from these needs
# we are calling them potential planting units (PPUs)

#### ref ----
  # if plant seed adapted to reference climate (collected between 1961-1990 from same zone)
PPU_ref_MCMT_ARP_df <- extract_clims(CL_PPU_8500_9000_vect, ref_ARP_rast)
str(PPU_ref_MCMT_ARP_df)

PPU_ref_MCMT_SRME_df <- extract_clims(CL_PPU_8500_9000_vect, ref_SRME_rast)
str(PPU_ref_MCMT_SRME_df)

#### current ----
  # if plant seed adapted to current climate (collected today from same zone)
PPU_curr_MCMT_ARP_df <- extract_clims(CL_PPU_8500_9000_vect, curr_ARP_rast)
str(PPU_curr_MCMT_ARP_df)

PPU_curr_MCMT_SRME_df <- extract_clims(CL_PPU_8500_9000_vect, curr_SRME_rast)
str(PPU_curr_MCMT_SRME_df)

#### ssp2 ----
  # if plant seed adapted to future ssp2 climate (collected today from different zone)
PPU_ssp2_MCMT_ARP_df <- extract_clims(CL_PPU_8500_9000_vect, ssp2_ARP_rast)
str(PPU_ssp2_MCMT_ARP_df)

PPU_ssp5_MCMT_SRME_df <- extract_clims(CL_PPU_8500_9000_vect, ssp5_SRME_rast)
str(PPU_ssp5_MCMT_SRME_df)

#### ssp5 ----
  # if plant seed adapted to future ssp5 climate (collected today from different zone)
PPU_ssp5_MCMT_ARP_df <- extract_clims(CL_PPU_8500_9000_vect, ssp5_ARP_rast)
str(PPU_ssp5_MCMT_ARP_df)

PPU_ssp5_MCMT_SRME_df <- extract_clims(CL_PPU_8500_9000_vect, ssp5_SRME_rast)
str(PPU_ssp5_MCMT_SRME_df)



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
  set.names(m, "match")
  return(m)
}

## run it ----
### FWD ----



### REV ----
# set path to results directory to save output files to

path <- getwd() # C:/Users/TaylorAkers/Box/Seeds_for_the_future/R_projects_and_code/S4F_ARP_CaseStudy/output_data")"

output_name <- "test_results"
results_dir <- paste0("results/",output_name)


for (row in 1:nrow(PPU_fut_MCMT_df)) {
  # for each "zone" or PPU (row in df)
  
  # match_clims(zone_df, clim_rast)
    # calc match score for each cell in the clim_rast, based on the zonal metrics in zone_df
    # produce a raster for each zone (row in df)
  PPU_fut_match_norm_rast <- match_clims(PPU_fut_MCMT_df[row, ], normal_ARP_rast) 
  
  # writeRaster in the results directory
  writeRaster(PPU_fut_match_norm_rast, paste(path, "/", results_dir, "/PPU_", PPU_fut_MCMT_df[row, ]$PPU_id, "_fut_match_norm.tif", sep = ""), overwrite=TRUE)
  # print status update 
  print(paste("Calculating match for each cell in clim_rast for zone # ", PPU_fut_MCMT_df[row, ]$PPU_id, " - ", round((row/nrow(PPU_fut_MCMT_df))*100, 2), "% complete", sep=""))
}

#### confirm ----

PPU_180_fut_match_norm <- rast("PPU_180_fut_match_norm.tif")
plot(PPU_180_fut_match_norm)


