# Author: Ryan Heneghan and Anthony J. Richardson
# Date: October 2019
# This is a simplified version of Jennings et al.'s (2008)
# MACROECOLOGICAL global marine ecosystem model (doi:10.1098/rspb.2008.0192).
# We use RCP (Representative Concentration Pathways) 8.5 future scenario for greenhouse
# gas emissions during the 21st century and explore changes in the global
# fish biomass
# Last Updated: 39/9/2020

#### 1. PRELIMINARIES ####
require(raster)     # For working with rasters
require(ggplot2)    # For making figures
require(colorRamps) # for Matlab like colour scheme
require(RNetCDF)    # For reading/manipulating netCDFs
require(dplyr)
require(rnaturalearth) # install.packages(c("rnaturalearth", "rnaturalearthdata"))
require(sf) # For simple geographic features

# LOAD AND EXPLORE ENVIRONMENTAL VARIABLES
# cesm_rcp85 is a GCM (General Circulation Model)
# with greenhouse gas forcing under RCP8.5 (high emissions) from 2006-2100
# The integrated primary production comeS from a biogeochemical model (for nutrients and phytoplankton)
# forced by the GCM

SST_nc <- open.nc("Input/cesm_rcp85_temp_zs_annual_200601-210012_remap.nc") # Sea surface temperature (SST)
print.nc(SST_nc) # Look at metadata and structure of netcdf
# Time period: Jan 2006 to Dec 2100
SST <- var.get.nc(SST_nc, 'to')   # Extract SST data from SST_nc and put into an array
dim(SST)                          # Dimensions of array - what do they refer to?
# Remove first and last year of data because they are slightly wrong
# Now data goes from 2007-2100 = 94 years
SST <- SST[,,2:95]
dim(SST)

Lats <- var.get.nc(SST_nc, 'lat') # Extract lats from netcdf
Lons <- var.get.nc(SST_nc, 'lon') # Extract lons from netcdf

# How hot will it get?
# What is the mean temperature increase in the ocean?
First10yrs <- 1:10 # First 10 years of monthly netcdf data
Last10yrs <- (dim(SST)[3] - (10)+1):dim(SST)[3] # Last 10 years of netcdf data

MeanSSTnow <- mean(apply(SST[,,First10yrs], 3, mean, na.rm = TRUE)) #
MeanSSTfuture <- mean(apply(SST[,,Last10yrs], 3, mean, na.rm = TRUE)) #
MeanSSTfuture - MeanSSTnow
# Why do we take 10 year time slices?

# For plotting later, let's calculate the SST map for 10 year time slices now and in the future
SSTnow <- apply(SST[,,First10yrs], c(1,2), mean, na.rm = TRUE)
SSTfuture <- apply(SST[,,Last10yrs], c(1,2), mean, na.rm = TRUE)

# What will happen to Primary Production in the future?
IntPP_nc <- open.nc("Input/cesm_rcp85_intpp_zint_annual_200601-210012_remap.nc") # Primary production
IntPP <- var.get.nc(IntPP_nc, 'intpp')  # Extract intpp data from intpp_nc
dim(IntPP)

# Remove first and last year of data because they are slightly wrong
# Now data goes from 2007-2100 = 94 years
IntPP <- IntPP[,,2:95]
dim(IntPP)

# For plotting later, let's calculate the IntPP map for 10 year time slices now and in the future
IntPPnow <- apply(IntPP[,,First10yrs], c(1,2), mean, na.rm = TRUE)
IntPPfuture <- apply(IntPP[,,Last10yrs], c(1,2), mean, na.rm = TRUE)

# # Let's have a look... Use ggplot to make a plot of the difference in mean SST
# # between 2090s and the period 2000s
# df <- expand.grid(Lon = Lons, Lat = Lats) %>%
#   mutate(SSTnow = as.vector(SSTnow),
#          SSTfuture = as.vector(SSTfuture),
#          IntPPnow = as.vector(IntPPnow),
#          IntPPfuture = as.vector(IntPPfuture))
#
# sdf <- rasterFromXYZ(df, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # Convert first two columns as lon-lat and third as value
# sdf_poly <- rasterToPolygons(sdf, fun=NULL, n=4, na.rm=TRUE, digits=8, dissolve=FALSE) # Convert to polygon which is better for plotting
# sf <- st_as_sf(sdf_poly) %>% # Convert raster to sf
#   st_transform(crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # Alter the CRS to robinson
#
# rm(sdf, sdf_poly) # Clean up
#
# # Get world outline
# # Download and process world outline
# world <- ne_countries(scale = "small", returnclass = "sf") %>%
#   st_transform(world, crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # Convert to different CRS
#
# # Plot SST for now
# ggplot(data = sf, aes(fill = SSTnow)) +
#   geom_sf(colour = NA) +
#   geom_sf(data = world, size = 0.05, fill = "grey50") +
#   scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar") +
#   theme_bw() +
#   labs(fill = "SST (°C)") +
#   ggtitle("2000s")
# ggsave("Output/SSTnow.png", dpi = 150)
#
# # Plot change in SST
# ggplot(data = sf, aes(fill = SSTfuture-SSTnow)) +
#   geom_sf(colour = NA) +
#   geom_sf(data = world, size = 0.05, fill = "grey50") +
#   scale_fill_gradient2(limits = c(-6, 6), midpoint = 0, low = "blue", mid = "white", high = "red", guide = "colourbar") +
#   theme_bw() +
#   labs(fill = "delta SST (°C)") +
#   ggtitle("Change in SST (2090s-2000s)")
# ggsave("Output/SSTchange.png", dpi = 150)
#
# # Plot Integrated PP
# ggplot(data = sf, aes(fill = IntPPnow)) +
#   geom_sf(colour = NA) +
#   geom_sf(data = world, size = 0.05, fill = "grey50") +
#   scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar") +
#   theme_bw() +
#   labs(fill = "IntPP") +
#   ggtitle("Integrated PP 2000s")
# ggsave("Output/IntPPnow.png", dpi = 150)
#
# # Plot change in PP
# ggplot(data = sf, aes(fill = IntPPfuture-IntPPnow)) +
#   geom_sf(colour = NA) +
#   geom_sf(data = world, size = 0.05, fill = "grey50") +
#   scale_fill_gradient2(limits = c(-0.0002, 0.0002), midpoint = 0, low = "blue", mid = "white", high = "red", guide = "colourbar") +
#   theme_bw() +
#   labs(fill = "delta IntPP") +
#   ggtitle("Change in integrated PP (2090s-2000s)")
# ggsave("Output/IntPPchange.png", dpi = 150)
#
# rm(sf, IntPP_nc, SST_nc, df, SSTnow, SSTfuture, IntPPnow, IntPPfuture) # Clean up



#### 2. CONVERT UNITS OF INTEGRATED PP TO STANDARD UNITS AND CALCULATE PP PER M^3 ####
# Convert units of Integrated PP (which is integrated from the surface to the bottom of the ocean)
IntPP <- IntPP*12*(60*60*24)  # Convert mmolC/m2/s to mgC/m2/s (12 = carbon atomic weight)
                              # Then from mgC/m2/s to mgC/m2/d



#### 3. CALCULATE PHYTOPLANKTON MEDIAN CELL SIZE FOR EACH GRID AND MONTH ####
# Need Chl-a for estimating median cell size, but no Chl-a provided,
# so estimate from Fig. 1b of Maranon et al. (2014) doi:10.1371/journal.pone.0099312
Chl <- 10^((log10(IntPP/100)-1.58)/1.29) # Convert PP into Chl-a (mg/m3)
# This equation requires PP in mgC/m3/d
# NOTE: Convert IntPP (mgC/m2/d) to PP (mgC/m3/d) by diving by 100 m, assuming all phyto equally through top 100 m (sunlit zone)

# Estimate median phytoplankton cell size (Wm) using equation from Table 3
# in Barnes et al. (2011) doi:10.1093/plankt/fbq088
Wm <- 10^(1.34-0.043*SST+0.929*log10(Chl)) # Wm (in pgC)
Wm <- (Wm/1e12)*10 # Wm (in g) = PgC to gC, then multiply by 10 to get g (wet or live weight)

# Now PP is by phytoplankton of varying sizes. So spread PP across 61 size bins
# (assuming PP equal across all classes: Li 2002), 30 smaller than Wm, 30 larger
# so only 1/61 of PP at median phytoplankton cell size (Wm)

PP_Wm <- IntPP/61 # PP_Wm (mgC/m2/d) is the primary production available at median phytoplankton size Wm
# Note use IntPP not IntPP/100 because all of the IntPP is available to the foodweb
PP_Wm <- (PP_Wm / 1000) * 10 # mgC/m2/d to g/m2/d

# rm(Chl, IntPP) # Clean up



#### 4. CALCULATE ABUNDANCE OF PHYTOPLANKTON AT Wm ####
# Set up variables for the model
Alpha <- 0.125      # Trophic transfer efficiency (proportion of energy from trophic level n that reaches n+1
                      # Default Alpha = 0.125
Beta  <- 1000         # Predator Prey Mass Ratio (ratio of mass of predator to that of prey)
                      # Default Beta = 1000
E     <- 0.63         # Activation energy of metabolism (for calculating the change in physiological rates)
                      # Default = 0.63
k_b   <- 8.62e-05     # Boltzmann's constant (for calculating the change in physiological rates)
SST     <- SST + 273.15 # Temperature in Kelvin (SST is from the data given)

# Calculate abundance of phytoplankton
# General equation for annual production per individual From Brown et al. (2004) Fig. 2
# Fits for organisms from bacteria to whales. Let's use it for phytoplankton
P_Wm <- (exp(25.22 - E/(k_b*SST))*Wm^0.76) # P_Wm in g/ind/yr

# Convert to per day
P_Wm <- P_Wm/365 # P_Wm (in g/ind/d)

# Abundance of phytoplankton at Wm
N_Wm <- PP_Wm/P_Wm
# N_Wm = The number of phytoplankton at median phytoplankton size (Wm)
# Number of individuals = The primary production of all phytoplankton in size bin Wm divided by their per individual production
# N_Wm (ind/m^2) = (g/m^2/d) / (g/ind/d)



#### 5. CALCULATE INTERCEPT AND SLOPE OF ABUNDANCE SPECTRUM ####
# Now the slope is independent of the amount of phytoplankton, and is only a function of:
# Alpha (the trophic transfer efficiency) and the Beta (the Predator Prey Mass Ratio, PPMR)
# *** See extra notes for derivation ***
b <- log10(Alpha)/log10(Beta)-0.75
# b = Slope of size spectrum (log10(N) vs log10(W))

# Calculate intercept of abundance spectrum
# We know that N = aw^b, so a = N/w^b
# So we can calculate the intercept a if we know the slope B and a point on the line (Wm, N_Wm)
a <- N_Wm/(Wm^b)
# a = Intercept of size spectrum (log10(abundance) with log10(body size))
# NOTE: It varies spatially



#### 6. CALCULATE BIOMASS OF FISH AND PLOT ####
# Calculate biomass spectrum B(W) by integrating the abundance spectrum N(W)
# Integral N(W)dW = aW^b dW = a/(b+1)*W^b+1

# What is the biomass of the fish in the world?
# Calculate total biomass between 1 g and 1 tonne (and assume they are all fish)
Wmin <- 1    # Wmin = Minimum mass
Wmax <- 1e6  # Wmax = Maximum mass
# Biom_conc <- (a/(b+1))*(Wmax^(b+1) - Wmin^(b+1)) # Definite integral of N(W)dw between Wmax and Wmin
Biom_conc <- (a/(b+1))*(Wmax^(b+1) - Wmin^(b+1))

# Biom_conc (g/m^3) = biomass concentration
Biom_conc <- Biom_conc/10^6 # Convert to tonnes/m^2

# rm(a, N_Wm, P_Wm, PP_Wm, SST) # Clean up

# Now we can't just sum up every grid square - why?
# Multiply by approximate surface area of grid squares to get total biomass
Area_grid <- t(as.matrix(area(raster())))*1000*1000 # Area of 360x180 grid squares (in m^2)
# raster() gives a default layer of 1o grids globally
# area() gives the area in km^2 for an object of type raster
# To convert km^2 to m^2: m^2 = km^2*1000*1000
# And converted to matrix with correct orientation for the world

# Biom_total (tonnes) = tonnes/m2 * m2 = Total biomass in each grid square each year
Biom_total <- sweep(x = Biom_conc, MARGIN = c(1,2), FUN = '*', Area_grid)  # Total biomass (in tonnes) in each grid square = Product of area on lat/lon grid * Biom_conc)

# Biom_month <- apply(Biom_total, 3, sum, na.rm = TRUE)   # Total global biomass in each month. Sums fish biomass over 3rd margin (months)
# Biom_year  <- colMeans(matrix(Biom_month, 12)) # Total global biomass in each year
Biom_year  <- apply(Biom_total, 3, sum, na.rm = TRUE) # Total global biomass in each year

# What is the total fish catch? Fish catch in tonnes
format(mean(Biom_year[First10yrs]), scientific = TRUE)
# Jennings et al. (2008) = 8.991 x 10^8 tonnes
# Now: 2.844372e+12 tonnes

# Plot relative change in % over 21st century
dat <- data.frame(Years = 2007:2100,
                  PercChange = 100*Biom_year/Biom_year[1]-100) # % Change each year from 2007

ggplot(data = dat,
       aes(Years, PercChange)) +
  geom_line() +
  ylab("% change") +
  ggtitle("Change in Total Consumer Biomass \n  under RCP8.5") +
  theme_bw()
ggsave("Output/FishTimeSeries.png", dpi = 150)

# Calculate change in biomass between 2006-2015 and 2091-2100, as a %
Biom_now <- apply(Biom_conc[,,First10yrs], c(1,2), mean, na.rm = TRUE)
Biom_future <- apply(Biom_conc[,,Last10yrs], c(1,2), mean, na.rm = TRUE)
Biom_change <- 100*(Biom_future/Biom_now)-100

# Modify high outliers, everything above or below 50%
hist(Biom_change) # Raw distribution of changes
Biom_change[Biom_change > 50] <- 50
Biom_change[Biom_change < -50] <- -50



###### 6. GLOBAL MAPS OF FISH BIOMASS
## Use ggplot to make plot map of change in fish biomass
df <- expand.grid(Lon = Lons, Lat = Lats) %>%
  mutate(Biom_change = as.vector(Biom_change))

# Re-project
sdf <- rasterFromXYZ(df, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # Convert first two columns as lon-lat and third as value
sdf_poly <- rasterToPolygons(sdf, fun = NULL, n = 4, na.rm = TRUE, digits = 8, dissolve = FALSE) # Convert to polygon which is better for plotting
sf <- st_as_sf(sdf_poly) %>% # Convert to sf
  st_transform(crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # Alter the CRS to robinson

rm(sdf, sdf_poly) # Clean up

# world <- ne_countries(scale = "small", returnclass = "sf") %>%
#   st_transform(world, crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # Convert to different CRS

ggplot(data = sf, aes(fill = Biom_change)) +
  geom_sf(colour = NA) +
  geom_sf(data = world, size = 0.05, fill = "grey50") +
  scale_fill_gradient2(limits = c(-50, 50), midpoint = 0, low = "blue", mid = "white", high = "red", guide = "colourbar") +
  theme_bw() +
  labs(fill = "% change") +
  ggtitle("Fish biomass change 2090s - 2000s")
ggsave("Output/FishBiomassChange.png", dpi = 150)



# NEW
a_Mn <- mean(apply(a[,,First10yrs], MARGIN = c(1,2), FUN = mean, na.rm = TRUE), na.rm = TRUE) #mean a value over first 10 years
Wm_Mn <- mean(apply(Wm[,,First10yrs], MARGIN = c(1,2), FUN = mean, na.rm = TRUE), na.rm = TRUE) #mean Wm value over first 10 years

#plot mean abundance spectrum (log10(N) vs log10(W))
ggplot(data.frame(x = c(Wm_Mn, 1e6)), aes(x)) +
  stat_function(fun = function(x)a_Mn*x^b) +
  scale_x_log10() +
  scale_y_log10()

Wmin <- 1    # Wmin = Minimum mass
Wmax <- 10^6  # Wmax = Maximum mass
log10(Wmax)
