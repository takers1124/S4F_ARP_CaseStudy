# description ----

# Seeds 4 the Future - ARP Case Study
  # Part 4 compares Existing Vegetation Type (EVT) across the AOI

# this script was created by Taylor Akers (ORISE fellow with USFS, RMRS), Fall-Winter 2025
  # see ARP_CaseStudy_Overview_RMRS.pdf for a full description of the project & earlier parts

# setup ----

library(terra) # primary spatial data management package
library(tidyterra) # dplyr-style data management for spatial data
library(dplyr)
library(ggplot2)

# data prep ----
## import ----
# AOI 
  # created in part 1
ARP_vect <- vect("ARP_vect.shp")
SRME_vect <- vect("SRME_vect.shp")
CL_PPU_9000_9500_vect <- vect("CL_PPU_9000_9500_vect.shp")

# climate match_sum rasters (for masking)
  # created in part 3 with output from part 2
    # these are specific (climate matched) to our case study PPUs
  # only using current and ssp2 time periods
PPU_curr_match_sum_ARP_rast <- rast("PPU_curr_match_sum_ARP_rast.tif")
PPU_curr_match_sum_SRME_rast <- rast("PPU_curr_match_sum_SRME_rast.tif")
PPU_ssp2_match_sum_ARP_rast <- rast("PPU_ssp2_match_sum_ARP_rast.tif")
PPU_ssp2_match_sum_SRME_rast <- rast("PPU_ssp2_match_sum_SRME_rast.tif")

plot(PPU_curr_match_sum_ARP_rast)
plot(is.na(PPU_curr_match_sum_ARP_rast))

# EVT
  # already in EPSG: 5070 (no need to project)
EVT_24CONUS_rast <- rast("LC24_EVT_250.tif")
is.factor(EVT_24CONUS_rast) # TRUE
EVT_24CONUS_df <- read.csv("LF24_EVT_250.csv")

EVT_16CONUS_rast <- rast("LC16_EVT_200.tif")
is.factor(EVT_16CONUS_rast) # TRUE
EVT_16CONUS_df <- read.csv("LF16_EVT_200.csv")

## crop and mask ----
### EVT_ARP_curr ----
# first, going to convert the match_sum rasters to polygons to make mask easier 
curr_match_ARP_vect <- aggregate(as.polygons(PPU_curr_match_sum_ARP_rast))
EVT_ARP_curr_rast <- crop(EVT_24CONUS_rast, curr_match_ARP_vect, mask = TRUE)
plot(EVT_ARP_curr_rast)

### EVT_SRME_curr ----
curr_match_SRME_vect <- aggregate(as.polygons(PPU_curr_match_sum_SRME_rast))
EVT_SRME_curr_rast <- crop(EVT_24CONUS_rast, curr_match_SRME_vect, mask = TRUE)
plot(EVT_SRME_curr_rast)

### EVT_ARP_ssp2 ----
ssp2_match_ARP_vect <- aggregate(as.polygons(PPU_ssp2_match_sum_ARP_rast))
EVT_ARP_ssp2_rast <- crop(EVT_24CONUS_rast, ssp2_match_ARP_vect, mask = TRUE)
plot(EVT_ARP_ssp2_rast)

### EVT_SRME_ssp2 ----
ssp2_match_SRME_vect <- as.polygons(PPU_ssp2_match_sum_SRME_rast)
EVT_SRME_ssp2_rast <- crop(EVT_24CONUS_rast, ssp2_match_SRME_vect, mask = TRUE)
plot(EVT_SRME_ssp2_rast)

### EVT16_PPUs ----
EVT16_PPUs_rast <- crop(EVT_16CONUS_rast, CL_PPU_9000_9500_vect, mask = TRUE)
plot(EVT16_PPUs_rast)


## tables ----
  # get frequency table & make ready to graph

### EVT_ARP_curr ----
EVT_ARP_curr_df <- as.data.frame(freq(EVT_ARP_curr_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("ARP_curr")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)

### EVT_SRME_curr ----
EVT_SRME_curr_df <- as.data.frame(freq(EVT_SRME_curr_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("SRME_curr")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)

### EVT_ARP_ssp2 ----
EVT_ARP_ssp2_df <- as.data.frame(freq(EVT_ARP_ssp2_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("ARP_ssp2")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)

### EVT_SRME_ssp2 ----
EVT_SRME_ssp2_df <- as.data.frame(freq(EVT_SRME_ssp2_rast)) %>%
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("SRME_ssp2")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)

### EVT16_PPUs ----
EVT16_PPUs_df <- freq(EVT16_PPUs_rast) %>%
  as.data.frame() %>% 
  mutate(REL_PERCENT = round((count / sum(count)) * 100, 3),
         Group = factor("PPUs")) %>% 
  rename(EVT_NAME = value) %>% 
  arrange(desc(REL_PERCENT)) %>% 
  select(Group, EVT_NAME, REL_PERCENT)



  
  
## combine ----
EVT_combined_df <- bind_rows(EVT16_PPUs_df)


EVT_combined_stackbar <- 
  ggplot(EVT_combined_df, aes(x = Group, y = REL_PERCENT, fill = EVT_NAME)) +
  geom_bar(stat = "identity") + 
  theme_minimal()

