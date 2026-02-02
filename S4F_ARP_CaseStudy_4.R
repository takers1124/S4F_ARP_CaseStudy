# description ----

# Seeds 4 the Future - ARP Case Study
  # Part 4 compares Existing Vegetation Type (EVT) across the AOI

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project & earlier parts

# setup ----

library(terra) 
library(tidyterra) 
library(dplyr)

library(tidyverse)
library(ggplot2)
library(viridis)

# (1) import data ----
## AOI ---- 
  # created in part 1
ARP_vect <- vect("ARP_vect.shp")
SRME_vect <- vect("SRME_vect.shp")
CP_PPUs_CaseStudy_vect <- vect("CP_PPUs_CaseStudy_vect.shp")

## masked data ----
# we are using LANDFIRE veg data for this section 

### EVT ----
# we want to know the EVT (existing vegetation) for any areas where we might potentially collect cones
EVT_CONUS_rast <- rast("LC24_EVT_250.tif")
  # already in EPSG: 5070 (no need to project)
is.factor(EVT_CONUS_rast) # TRUE
# EVT_CONUS_df <- read.csv("LF24_EVT_250.csv")
levels(EVT_CONUS_rast)
activeCat(EVT_CONUS_rast)

### BPS ----
# we want to know the BPS (potential vegetation) for any areas where we might potentially plant seedlings
BPS_CONUS_rast <- rast("LC20_BPS_220.tif")
  # already in EPSG: 5070 (no need to project)
is.factor(BPS_CONUS_rast) # TRUE
# BPS_CONUS_df <- read.csv("LC20_BPS_220.csv")
levels(BPS_CONUS_rast)
activeCat(BPS_CONUS_rast) <- "BPS_NAME"



## masking data ----
### clim_match ----
# climate match_sum rasters
  # created in part 3 with output from part 2
    # these are specific (climate matched) to our case study PPUs
  # only using current and ssp2 time periods
PPU_curr_match_sum_ARP_rast <- rast("PPU_curr_match_sum_ARP_rast.tif")
PPU_curr_match_sum_SRME_rast <- rast("PPU_curr_match_sum_SRME_rast.tif")
PPU_ssp2_match_sum_ARP_rast <- rast("PPU_ssp2_match_sum_ARP_rast.tif")
PPU_ssp2_match_sum_SRME_rast <- rast("PPU_ssp2_match_sum_SRME_rast.tif")


### QMD ----
# also going to mask the veg data with same QMD & EVH thresholds set for PCU creation (Part1A)
# we only want to know what the veg type is in areas that are suitable for cone collection

# filtered versions of this dataset was already created for the ARP in Part1A
ARP_QMD_filt_rast <- rast("ARP_QMD_filt_rast.tif")

# need to make filtered version for SRME
## QMD
QMD_CONUS <- rast("TreeMap2022_CONUS_QMD.tif")
  # already in 5070

# crop and mask
SRME_QMD_rast <- crop(QMD_CONUS, SRME_vect, mask=TRUE)

# reclassify with ifel()
SRME_QMD_filt_rast <- ifel(
  SRME_QMD_rast >= 5, 5, NA 
)
  # if >= 5 inches, reclassify to 5
  # if < 5 inches, reclassify to NA

### write & read ----
writeRaster(SRME_QMD_filt_rast, "SRME_QMD_filt_rast.tif")
SRME_QMD_filt_rast <- rast("SRME_QMD_filt_rast.tif")


### EVH ----
# filtered versions of this dataset was already created for the ARP in Part1A
ARP_EVH_filt_rast <- rast("ARP_EVH_filt_rast.tif")

# need to make filtered version for SRME
EVH_CONUS <- rast("LC24_EVH_250.tif")

# crop and mask
EVH_SRME <- crop(EVH_CONUS, SRME_vect, mask=TRUE)

# reclassify with ifel()
  # define conversion factor
meters_to_feet_factor <- 3.28084

SRME_EVH_filt_rast <- ifel(
  # condition 1: it is dominant veg type trees? (values 101-199)
  EVH_SRME >= 101 & EVH_SRME < 200,
  # if TRUE, 
  # condition 2: is it > 10 ft tall? 
  ifel(
    (EVH_SRME - 100) * meters_to_feet_factor > 10, # subtract offset, convert units, filter
    10, # if TRUE, reclassify to 10
    NA # if FALSE, reclassify to NA
  ),
  NA # if not a tree value (condition 1 = FALSE), reclassify to NA
)

### write & read ----
writeRaster(SRME_EVH_filt_rast, "SRME_EVH_filt_rast.tif")
SRME_EVH_filt_rast <- rast("SRME_EVH_filt_rast.tif")



# (2) process veg data ----
# what we want:
  # to know what the EVT is only for the climate_match_sum and trees area
  # a data frame for each group to use in graph-making

## EVT_ARP_curr ----
### make mask rast ----
ARP_curr_resampled <- resample(PPU_curr_match_sum_ARP_rast, ARP_EVH_filt_rast, method = "near")
  # using "near" bc the match_sum raster is categorical (with values 1-15 representing categories, not integers) 

raster_list <- list(ARP_QMD_filt_rast,
                    ARP_EVH_filt_rast,
                    ARP_curr_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
  # has 3 layers, each has same extent and resolution

# sum
ARP_curr_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
  # has values: min = 1, max = 30
  # values >= 16 will include both QMD (5) + EVH (10) + match (1-15)
plot(ARP_curr_combined_rast)

# make cell value = 1 for all areas that meet our mask requirement & value = NA if not
ARP_curr_mask_rast <- ifel(
  ARP_curr_combined_rast >= 16,
  ARP_curr_combined_rast, NA)
plot(ARP_curr_mask_rast)

### make mask poly ----
ARP_curr_mask_poly <- aggregate(as.polygons(ARP_curr_mask_rast, values = FALSE))
plot(ARP_curr_mask_poly)

#### write & read ----
writeVector(ARP_curr_mask_poly, "ARP_curr_mask_poly.shp")
ARP_curr_mask_poly <- vect("ARP_curr_mask_poly.shp")

### crop and mask ----
EVT_ARP_curr_rast <- crop(EVT_CONUS_rast, ARP_curr_mask_poly, mask = TRUE)
plot(EVT_ARP_curr_rast)

#### write & read ----
writeRaster(EVT_ARP_curr_rast, "EVT_ARP_curr_rast.tif")
EVT_ARP_curr_rast <- rast(EVT_ARP_curr_rast)

### table ----
EVT_ARP_curr_df <- as.data.frame(freq(EVT_ARP_curr_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("ARP_EVT_curr")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)


## EVT_SRME_curr ----
### make mask rast ----
SRME_curr_resampled <- resample(PPU_curr_match_sum_SRME_rast, SRME_EVH_filt_rast, method = "near")
  # using "near" bc the match_sum raster is categorical (with values 1-15 representing categories, not integers) 

raster_list <- list(SRME_QMD_filt_rast,
                    SRME_EVH_filt_rast,
                    SRME_curr_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
  # has 3 layers, each has same extent and resolution

# sum
SRME_curr_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
  # has values: min = 1, max = 30
  # values >= 16 will include both QMD (5) + EVH (10) + match (1-15)
plot(SRME_curr_combined_rast)

# make cell value = 1 for all areas that meet our mask requirement & value = NA if not
SRME_curr_mask_rast <- ifel(
  SRME_curr_combined_rast >= 16,
  SRME_curr_combined_rast, NA)
plot(SRME_curr_mask_rast)

### make mask poly V1 ----
SRME_curr_mask_poly <- aggregate(as.polygons(SRME_curr_mask_rast))
  # ran for 4 hours on my PC and remote computer - I stopped it before finished both times 
plot(SRME_curr_mask_poly)

### make mask poly V2 ----
  # try separating 2 functions
  # add values = FALSE
SRME_curr_mask_BIGpoly <- as.polygons(SRME_curr_mask_rast, values = FALSE)
  # only ~ 2 mins to run with values = FALSE
SRME_curr_mask_poly <- aggregate(SRME_curr_mask_BIGpoly)
  # started 9:50, ran to 13:20 then I stopped it

### make mask poly V3 ----
SRME_curr_mask_BIGpoly <- as.polygons(SRME_curr_mask_rast, values = FALSE)

  # try adding count = FALSE
SRME_curr_mask_poly <- aggregate(SRME_curr_mask_BIGpoly, count = FALSE)
  # started 1:50, ran to 3:00 then stopped



#### write & read ----
writeVector(SRME_curr_mask_poly, "SRME_curr_mask_poly.shp")
SRME_curr_mask_poly <- vect("SRME_curr_mask_poly.shp")

### crop and mask ----
EVT_SRME_curr_rast <- crop(EVT_CONUS_rast, SRME_curr_mask_poly, mask = TRUE)
plot(EVT_SRME_curr_rast)

#### write & read ----
writeRaster(EVT_SRME_curr_rast, "EVT_SRME_curr_rast.tif")
EVT_SRME_curr_rast <- rast(EVT_SRME_curr_rast)

### table ----
EVT_SRME_curr_df <- as.data.frame(freq(EVT_SRME_curr_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("SRME_EVT_curr")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)



## EVT_ARP_ssp2 ----
### make mask rast ----
ARP_ssp2_resampled <- resample(PPU_ssp2_match_sum_ARP_rast, ARP_EVH_filt_rast, method = "near")
# using "near" bc the match_sum raster is categorical (with values 1-15 representing categories, not integers) 

raster_list <- list(ARP_QMD_filt_rast,
                    ARP_EVH_filt_rast,
                    ARP_ssp2_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 3 layers, each has same extent and resolution

# sum
ARP_ssp2_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 1, max = 30
# values >= 16 will include both QMD (5) + EVH (10) + match (1-15)

# make cell value = 1 for all areas that meet our mask requirement & value = NA if not
ARP_ssp2_mask_rast <- ifel(
  ARP_ssp2_combined_rast >= 16,
  ARP_ssp2_combined_rast, NA)

### make mask poly ----
ARP_ssp2_mask_poly <- aggregate(as.polygons(ARP_ssp2_mask_rast, values = FALSE))

#### write & read ----
writeVector(ARP_ssp2_mask_poly, "ARP_ssp2_mask_poly.shp")
ARP_ssp2_mask_poly <- vect("ARP_ssp2_mask_poly.shp")

### crop and mask ----
EVT_ARP_ssp2_rast <- crop(EVT_CONUS_rast, ARP_ssp2_mask_poly, mask = TRUE)
plot(EVT_ARP_ssp2_rast)

#### write & read ----
writeRaster(EVT_ARP_ssp2_rast, "EVT_ARP_ssp2_rast.tif")
EVT_ARP_ssp2_rast <- rast(EVT_ARP_ssp2_rast)

### table ----
EVT_ARP_ssp2_df <- as.data.frame(freq(EVT_ARP_ssp2_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("ARP_EVT_ssp2")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)



## EVT_SRME_ssp2 ----
### make mask rast ----
SRME_ssp2_resampled <- resample(PPU_ssp2_match_sum_SRME_rast, SRME_EVH_filt_rast, method = "near")
# using "near" bc the match_sum raster is categorical (with values 1-15 representing categories, not integers) 

raster_list <- list(SRME_QMD_filt_rast,
                    SRME_EVH_filt_rast,
                    SRME_ssp2_resampled)

# create a multi-layer raster stack 
resampled_rast_stack <- rast(raster_list)
# has 3 layers, each has same extent and resolution

# sum
SRME_ssp2_combined_rast <- app(resampled_rast_stack, fun = "sum", na.rm = TRUE)
# has values: min = 1, max = 30
# values >= 16 will include both QMD (5) + EVH (10) + match (1-15)

# make cell value = 1 for all areas that meet our mask requirement & value = NA if not
SRME_ssp2_mask_rast <- ifel(
  SRME_ssp2_combined_rast >= 16,
  SRME_ssp2_combined_rast, NA)

### make mask poly ----
SRME_ssp2_mask_poly <- aggregate(as.polygons(SRME_ssp2_mask_rast, values = FALSE))

#### write & read ----
writeVector(SRME_ssp2_mask_poly, "SRME_ssp2_mask_poly.shp")
SRME_ssp2_mask_poly <- vect("SRME_ssp2_mask_poly.shp")

### crop and mask ----
EVT_SRME_ssp2_rast <- crop(EVT_CONUS_rast, SRME_ssp2_mask_poly, mask = TRUE)
plot(EVT_SRME_ssp2_rast)

#### write & read ----
writeRaster(EVT_SRME_ssp2_rast, "EVT_SRME_ssp2_rast.tif")
EVT_SRME_ssp2_rast <- rast(EVT_SRME_ssp2_rast)

### table ----
EVT_SRME_ssp2_df <- as.data.frame(freq(EVT_SRME_ssp2_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("SRME_EVT_ssp2")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)



## EVT_PPUs ----
# note, if I want to combine the BPS data with the EVT data for graphing (step 3 below),
# I will need to make the column name the same for all datasets
# so, for ease of graphing and visualization, I will set the veg category name as EVT eventhough this is BPS data

### crop and mask ----
EVT_PPU_rast <- crop(BPS_CONUS_rast, CP_PPUs_CaseStudy_vect, mask = TRUE)
plot(EVT_PPU_rast)

### table ----
EVT_PPU_df <- freq(EVT_PPU_rast) %>%
  as.data.frame() %>% 
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("PPUs_BPS")) %>% # retain BPS for group name (for x axis)
  rename(EVT_NAME = value) %>%  
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)
  


# (3) combine & plot ----
# combine 5 dataframes set up in step 2
EVT_combined_df <- bind_rows(BPS_PPU_df,
                             EVT_ARP_curr_df,
                             EVT_ARP_ssp2_df,
                             EVT_SRME_curr_df,
                             EVT_SRME_ssp2_df)




## new code (untested) ----
library(forcats)
library(scales)

# adjust dataframe for graphing
  # lump rare categories (< 1%) within each Group
evt_lumped <- EVT_combined_df %>%
  group_by(Group) %>%
  mutate(
    EVT_name_lumped = if_else(REL_PERCENT < 1, "Other", EVT_name)
  ) %>%
  # aggregate to a single "Other" per Group
  group_by(Group, EVT_name_lumped) %>%
  summarise(REL_PERCENT = sum(REL_PERCENT), .groups = "drop") %>%
  # order legend by overall contribution across all groups
  group_by(EVT_name_lumped) %>%
  mutate(total_rel = sum(REL_PERCENT)) %>%
  ungroup() %>% # make a factor and order by total_rel
  mutate(EVT_name_lumped = fct_reorder(EVT_name_lumped, total_rel, .desc = TRUE)) 

# sanity check: each group sums to 100
evt_lumped %>%
  group_by(Group) %>%
  summarise(total = sum(REL_PERCENT))

# plot

EVT_plot <- ggplot(evt_lumped, aes(x = Group, y = REL_PERCENT, fill = EVT_name_lumped)) +
  geom_bar(stat = "identity", width = 0.8, color = "white", size = 0.2) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_fill_manual(
    values = c("Other" = "grey70"),
    guide = guide_legend(title = "Vegetation type")
  ) +
  labs(x = NULL, y = "Relative percent", title = "Vegetation composition by group") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

  # try as-is (above) and try changes (below)
geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3) +
  scale_fill_viridis_d(option = "B", direction = 1) +

  
  
  
## old code ----
# pick top 10 EVTs for each group
EVT_top10 <- EVT_combined_df %>% 
  slice_head(, n= 10, by = Group)

unique(EVT_top10$EVT_NAME) # 19 unique EVTs

# try 1
EVT_combined_stackbar <- 
  ggplot(EVT_top10, aes(x = Group, y = REL_PERCENT, fill = EVT_NAME, order = REL_PERCENT)) +
  geom_col(position = "stack") + 
  labs(x = NULL, y = "Relative Percent", fill = "EVT_NAME") +
  theme_minimal()
# EVT bars not in order
  
# try 2
EVT_combined_stackbar <- 
  ggplot(EVT_top10, aes(x = Group, y = REL_PERCENT, fill = EVT_NAME, order = REL_PERCENT)) +
  geom_col(position = position_stack(reverse = TRUE)) + # adjusted 
  labs(x = NULL, y = "Relative Percent", fill = "EVT_NAME") +
  theme_minimal()
# some are in order, but not all...

# try 3
# adjust the df - same top-bottom ordering for every group, based on global trends
evt_order <- EVT_top10 %>% 
  group_by(EVT_NAME) %>% 
  summarise(total = sum(REL_PERCENT, na.rm = TRUE), .groups = "drop") %>% 
  arrange(total) %>% 
  pull(EVT_NAME)
  
EVT_top10_2 <- EVT_top10 %>% 
  mutate(EVT_NAME = factor(EVT_NAME, levels = evt_order))
str(EVT_top10_2)
str(EVT_top10)

EVT_combined_stackbar <- 
  ggplot(EVT_top10_2, aes(x = Group, y = REL_PERCENT, fill = EVT_NAME)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3) +
  scale_fill_viridis_d(option = "B", direction = 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = "Percent", fill = "EVT") +
  theme_minimal()
  # has the largest overall (global) EVT category on top for each bar and for the legend order
  # still not exactly what we want, but good enough for now... 


## add elevation rank? ----
# we want the evt legend to be ordered by elevation
  # we will rank each EVT based on natural history data


  
  

  
  
  
  

