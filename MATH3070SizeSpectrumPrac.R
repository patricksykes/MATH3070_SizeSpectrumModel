# Author: Ryan Heneghan and Anthony J. Richardson
# Date: October 2019
# This is a simplified version of Jennings et al.'s (2008)
# MACROECOLOGICAL global marine ecosystem model (doi:10.1098/rspb.2008.0192).
# We use RCP(Representative Concentration Pathways) 8.5 future scenario for greenhouse
# gas emissions during the 21st century and explore changes in the global
# fish biomass
# Last Updated: 29/9/2020

#### PRELIMINARIES ####
library(raster)     # For working with rasters
library(ggplot2)    # For making figures
library(colorRamps) # for Matlab like colour scheme
library(RNetCDF)    # For reading/manipulating netCDFs
library(maptools)   # For mapping
library(dplyr)
data(wrld_simpl)    # World map

#### LOAD AND EXPLORE ENVIRONMENTAL VARIABLES ####
# cesm_rcp85 is a GCM (General Circulation Model)
# with greenhouse gas forcing under RCP8.5 (high emissions) from 2006-2100
# The integrated primary production comeS from a biogeochemical model (for nutrients and phytoplankton)
# forced by the GCM

SST_nc <- open.nc("cesm_rcp85_temp_zs_monthly_200601-210012.nc") # Sea surface temperature (SST)
print.nc(SST_nc) # Look at metadata and structure of netcdf
# Time period: Jan 2006 to Dec 2100
SST <- var.get.nc(SST_nc, 'to')   # Extract SST data from SST_nc and put into an array
dim(SST)                          # Dimensions of array - what do they refer to?
Lats <- var.get.nc(SST_nc, 'lat') # Extract lats from netcdf
Lons <- var.get.nc(SST_nc, 'lon') # Extract lons from netcdf

# How hot will it get?
# What is the mean temperature increase in the ocean?
First10yrs <- 1:(12*10) # First 10 years of monthly netcdf data
Last10yrs <- (dim(SST)[3] - (12*10)+1):dim(SST)[3]# Last 10 years of netcdf data

MeanSSTnow <- mean(apply(SST[,,First10yrs], 3, mean, na.rm = TRUE)) #
MeanSSTfuture <- mean(apply(SST[,,Last10yrs], 3, mean, na.rm = TRUE)) #
MeanSSTfuture - MeanSSTnow
# Why do we take 10 year time slices?

# Let's plot the first and last 10 yr time slices
# Start by calculating the mean SST map for now and the future
SSTnow <- apply(SST[,,First10yrs], c(1,2), mean, na.rm = TRUE)
SSTfuture <- apply(SST[,,Last10yrs], c(1,2), mean, na.rm = TRUE)

# Let's have a look... Use ggplot to make a plot of the difference in mean SST
# between 2091-2100 and the period 2006-2015
df <- expand.grid(Lon = Lons, Lat = Lats)

df <- df %>% mutate(SSTnow = as.vector(SSTnow),
                    SSTfuture = as.vector(SSTfuture))

# Plot SST for now
ggplot(data = df, aes(x = Lon, y = Lat, fill = SSTnow)) +
  geom_raster() + scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar", na.value = "gray") +
  theme_bw() + labs(fill = "") + ggtitle("2006-2015")

# Plot SST for future
ggplot(data = df, aes(x = Lon, y = Lat, fill = SSTfuture)) +
  geom_raster() + scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar", na.value = "gray") +
  theme_bw() + labs(fill = "") + ggtitle("2090s")

# Plot anomaly
ggplot(data = df, aes(x = Lon, y = Lat, fill = SSTfuture - SSTnow)) +
  geom_raster() + scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar", na.value = "gray") +
  theme_bw() + labs(fill = "") + ggtitle("Anomaly")

# What will happen to Primary Production?
# Let's do the same for Integrated Primary Production
IntPP_nc <- open.nc("cesm_rcp85_intpp_zint_monthly_200601-210012.nc") # Primary production
IntPP <- var.get.nc(IntPP_nc, 'intpp')  # Extract intpp data from intpp_nc
dim(IntPP)


df$IntPP_2006 <- as.vector(IntPP[,,6])   # Dec 2006
df$IntPP_2100 <- as.vector(IntPP[,,1140]) # Dec 2100

# Plot Integrated PP
ggplot(data = df, aes(x = Lon, y = Lat, fill = IntPP_2006)) +
  geom_raster() + scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar", na.value = "gray") +
  theme_bw() + labs(fill = "") + ggtitle("Integrated primary production Dec 2006")

# Anomaly of 2100 - 2006 for Primary Production
PP_2006 <- apply(IntPP[,,1:120], c(1,2), mean, na.rm = TRUE)
PP_2100 <- apply(IntPP[,,1021:1140], c(1,2), mean, na.rm = TRUE)
PP_frame <- expand.grid(Lon = Lons, Lat = Lats)
PP_frame$PP_2006 <- as.vector(PP_2006)
PP_frame$PP_2100 <- as.vector(PP_2100)

hist(PP_frame$PP_2006)
hist(PP_frame$PP_2100)

# Plot Anomaly PP
ggplot(data = PP_frame, aes(x = Lon, y = Lat, fill = PP_2100-PP_2006)) +
  geom_raster() + scale_fill_gradientn(colours = matlab.like(12), guide = "colorbar", na.value = "gray", limits = c(-0.0002, 0.0002)) +
  theme_bw() + labs(fill = "") + ggtitle("Integrated primary production 2090s-2006s")

rm(IntPP_nc, SST_nc) # Clean up

###### 1.CONVERT UNITS OF INTEGRATED PP TO STANDARD UNITS AND CALCULATE PP PER M^3
# Convert units of Integrated PP (which is integrated from the surface to the bottom of the ocean)
IntPP <- IntPP*12*(60*60*24)  # Convert mmolC/m^2/s to mgC/m^2/s
                              # Then from mgC/m^2/s to mgC/m^2/d

PP <- IntPP/100               # Now IntPP is throughout the water column.
# Convert mgC/m^2/d to mgC/m^3/d by diving by 100 m, assuming all phyto equally through top 100 m (euphotic zone)

###### 2. CALCULATE PHYTOPLANKTON MEDIAN CELL SIZE FOR EACH GRID AND MONTH
# Need Chl-a for estimating median cell size, but no Chl-a provided,
# so estimate from Fig. 1b of Maranon et al. (2014) doi:10.1371/journal.pone.0099312
Chl <- 10^((log10(PP)-1.58)/1.29) # Convert PP into Chl-a (mg/m^3)

# Estimate median phytoplankton cell size (Wm) using equation from Table 3
# in Barnes et al. (2011) doi:10.1093/plankt/fbq088
# Wm (gC) = Median phytoplankton cell size
W_m <- 10^(1.34-0.043*SST+0.929*log10(Chl)) # Median phytoplankton cell size (in pgC)
W_m <- (W_m/1e12)*10 # PgC to gC, then multiply by 10 to get wet weight

# Now PP is by phytoplankton of varying sizes. So spread PP across 61 size bins
# (assuming PP equal across all classes: Li 2002), 30 smaller than W_m, 30 larger
# so only 1/61 of PP at median phytoplankotn cell size (Wm)
PP_Wm <- IntPP/61 # PP_Wm (mgC/m^3/d) = All the primary production available at median phytoplankton size Wm
# Note use IntPP not PP because all of the PP is available to the foodweb

###### 2. CALCULATE ABUNDANCE OF PHYTOPLANKTON AT Wm
# Set up variables for the model
Alpha <- 0.125        # Trophic transfer efficiency (proportion of energy from trophic level n that reaches n+1
                      # Default Alpha = 0.125
Beta  <- 1000         # Predator Prey Mass Ratio (ratio of mass of predator to that of prey)
                      # Default Beta = 1000
E     <- 0.63         # Activation energy of metabolism (for calculating the change in physiological rates)
                      # Default = 0.63
k_b   <- 8.62e-05     # Boltzmann's constant (for calculating the change in physiological rates)
K     <- SST + 273.15 # Temperature in Kelvin (SST is from the data given)

# Calculate abundance of phytoplankton
# General equation for daily production per individual From Brown et al. (2004)
# Fits for organisms from bacteria to whales. Let's use it for phytoplankton
P_Wm <- (exp(25.22 - E/(k_b*K))*W_m^0.75)/365
# P_Wm (mgC/individual/d) = Daily production per individual (for Wm). Equation for Annual production per individual from Fig. 2 in Brown et al. (2004)

N_Wm <- PP_Wm/P_Wm # Abundance of phytoplankton at Wm
# N_Wm = The number of phytoplankton at median phytoplankton size (Wm)
# Number of individuals = The primary production of all phytoplankton in size bin Wm divided by their per individual production
# N_Wm (individuals/m^3) = (mgC/m^3/d) / (mgC/individual/day)

###### 3. CALCULATE INTERCEPT AND SLOPE OF ABUNDANCE SPECTRUM
# Now the slope is independent of the amount of phytoplankton, and is only a function of:
# Alpha (the trophic transfer efficiency) and the Beta (the Predator Prey Mass Ratio, PPMR)
# *** See extra notes for derivation ***
b <- log10(Alpha)/log10(Beta)-0.75
# b = Slope of size spectrum (log10(abundance) vs log10(body size))

# Calculate intercept of abundance spectrum
# We know that N = aw^b, so a = N/w^b
# So we can calculate the intercept a if we know the slope B and a point on the line (W_m, N_Wm)
a <- N_Wm/(W_m^b)
# a = Intercept of size spectrum (log10(abundance) with log10(body size))

###### 4. CALCULATE BIOMASS BETWEEN ANY SIZE CLASSES (WEIGHTS)
# Calculate biomass spectrum B(W) by integrating the abundance spectrum N(W)
# Integral N(W)dW = aW^b dW = a/(b+1)*W^b+1

# What is the biomass of the fish in the world?
# Calculate total biomass between 1 g and 1 tonne (and assume they are all fish)
W_min <- 1    # W_min = Minimum mass
W_max <- 1e6  # W_max = Maximum mass
Biom_conc <- (a/(b+1))*(W_max^(b+1) - W_min^(b+1)) # Definite integral of N(W)dw between W_max and W_min
# Biom_conc (g/m^3) = biomass concentration
Biom_conc <- Biom_conc/10^6 # Convert to tonnes/m^3

# Now we can't just sum up every grid square - why?
# Multiply by approximate surface area of grid squares to get total biomass
Area_grid <- t(as.matrix(area(raster())))*1000*1000 # Area of 360x180 grid squares (in m^2)
# raster() gives a default layer of 1o grids globally
# area() gives the area in km^2 for an object of type raster
# To convert km^2 to m^2: m^2 = km^2*1000*1000
# And converted to matrix with correct orientation for the world

# Biom_total (tonnes) = tonnes/m2 * m2 = Total biomass in each grid square
Biom_total <- sweep(Biom_conc, c(1,2), Area_grid, '*')  # Total biomass (in tonnes) in each grid square = Product of area on lat/lon grid * Biom_conc)
Biom_month <- apply(Biom_total, 3, sum, na.rm = TRUE)   # Total global biomass in each month. Sums fish biomass over 3rd margin (months)
Biom_year  <- colMeans(matrix(Biom_month, 12)) # Total global biomass in each year

# What is the total fish catch? Fish catch in tonnes
format(Biom_year[1], scientific = TRUE) # Jennings et al. (2008) = 7.91 x 10^8 tonnes
# Compare with our estimate for whichever year

###### 5. TIME SERIES PLOTS OF FISH
# Plot relative change in % over 21st century
Years <- 2006:2100
PercChange <- 100*Biom_year/Biom_year[1]-100 # % Change each year from 2006

plot(Years, PercChange, type = 'l', lwd = 1.2, ylab = '% Change',
     xlab = 'Year', main = 'Change in Total Consumer Biomass \n  under RCP8.5')

# Calculate change in biomass between 2006-2015 and 2091-2100, as a %
Biom_2006_2015 <- apply(Biom_conc[,,1:120], c(1,2), mean, na.rm = TRUE)
Biom_2091_2100 <- apply(Biom_conc[,,1021:1140], c(1,2), mean, na.rm = TRUE)
Biom_change <- 100*(Biom_2091_2100/Biom_2006_2015)-100

# Modify high outliers, everything above + or -50%
hist(Biom_change) # Raw distribution of changes
Biom_change[Biom_change > 50] <- 50
Biom_change[Biom_change < -50] <- -50

###### 6. GLOBAL MAPS OF FISH BIOMASS
## Use ggplot to make plot map of change in fish biomass
Biom_frame <- expand.grid(Lon = Lons, Lat = Lats)
Biom_frame$Biom_change <- as.vector(Biom_change)

ggplot(data = Biom_frame, aes(x = Lon, y = Lat, fill = Biom_change)) +
  geom_raster() + scale_fill_gradientn(colours = rev(matlab.like(12)), guide = "colorbar", na.value = "gray") +
  theme_bw() + labs(fill = "") + ggtitle("Fish biomass change 2090s - 2000s")
