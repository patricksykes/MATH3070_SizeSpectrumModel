#!/bin/bash

# script to regrid Ryan's files

# Annual Average
cdo yearmean cesm_rcp85_temp_zs_monthly_200601-210012.nc cesm_rcp85_temp_zs_annual_200601-210012.nc                     

# Change longitude gridding from 0-360 to -180:180
cdo -remapbil,global_1, cesm_rcp85_temp_zs_annual_200601-210012.nc cesm_rcp85_temp_zs_annual_200601-210012_remap.nc

# Annual Average
cdo yearmean cesm_rcp85_intpp_zint_monthly_200601-210012.nc cesm_rcp85_intpp_zint_annual_200601-210012.nc

# Change longitude gridding from 0-360 to -180:180
cdo -remapbil,global_1, cesm_rcp85_intpp_zint_annual_200601-210012.nc cesm_rcp85_intpp_zint_annual_200601-210012_remap.nc