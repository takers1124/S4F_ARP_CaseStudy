# for the instructor

# road ----
road_rast4 <- rast("road_rast4(Geom).tif")
plot(road_rast4)


# EVH ----
EVH_CO <- rast("EVH_CO.tif") # in EPSG: 5070

### crop / mask ----
EVH_ARP <- crop(EVH_CO, ARP_vect, mask=TRUE)

### classify 1 ----
# create matrix so values <= 100 become NA (see all veg)
EVH_matrix <- matrix(c(-Inf, 100, NA), ncol = 3, byrow = TRUE)

# Classify the raster using the matrix, keeping unmatched values
EVH_classified <- classify(EVH_ARP, EVH_matrix, others = NULL)

#### plot ----
plot(EVH_classified)
polys(ARP, col = "black", alpha=0.01, lwd=2)
points(CSU, pch = 19, col = "orange", cex = 1.5)
points(CU, pch = 19, col = "orange", cex = 1.5)

### classify 2 ----
# create matrix so values <= 100 become NA & values >= 200 become NA (see only trees)
EVH_matrix2 <- matrix(c(-Inf, 100, NA, 200, Inf, NA), ncol = 3, byrow = TRUE)
EVH_classified2 <- classify(EVH_ARP, EVH_matrix2, others = NULL) # values 101:125

# make special color pallet 
green_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(25)  # Creates 25 shades of green

#### plot ----
plot(EVH_classified2, col = green_palette)

### classify 3 ----
# create matrix so only including trees > 20ft & also making scale in ft

# 1. Define the conversion factor from meters to feet
# 1 meter is approximately 3.28084 feet
meters_to_feet_factor <- 3.28084

# 2. Create the reclassification matrix
# This matrix will define the "from", "to", and "becomes" values.
#  - First, convert the original meter values (101-125) to their true meter representation (1-25)
#  - Then, convert these true meter values to feet.
#  - Finally, set values less than 20 feet to NA.

# 3. Initialize an empty matrix
rcl_matrix <- matrix(NA, ncol = 2, byrow = TRUE)

# 4. Create rows for the matrix based on meter ranges
# Loop through the *original* meter values (101-125)
for (original_meter_value in 101:125) {
  # Calculate the true meter value by subtracting the offset (100)
  true_meter_value <- original_meter_value - 100
  
  # Convert the true meter value to feet
  feet_value <- true_meter_value * meters_to_feet_factor
  
  if (feet_value < 20) {
    rcl_matrix <- rbind(rcl_matrix, c(original_meter_value, NA))  # Values < 20 ft become NA
  } else {
    rcl_matrix <- rbind(rcl_matrix, c(original_meter_value, feet_value)) # Values >= 20 ft retain converted value
  }
}

# 5. Remove the initial NA row
rcl_matrix <- rcl_matrix[-1, ]

# 6. Classify the raster using the reclassification matrix
EVH_classified3 <- classify(EVH_classified2, rcl_matrix, others = NULL) 

#### plot ----
plot(EVH_classified3, col = green_palette)





