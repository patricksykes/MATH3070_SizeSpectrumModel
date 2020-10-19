#!/bin/bash


indir="/Users/jason/GitHub/MATH3070/Input/rcp85/raw/"
cdo -O -mergetime $indir*intpp* "rcp85/intpp_Omon_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc"
cdo -yearmean "rcp85/intpp_Omon_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc" "rcp85/intpp_Oyr_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc"
cdo -L -remapbil,global_1 "rcp85/intpp_Oyr_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc" "intpp_Oyr_CESM2_ssp585_r4i1p1f1_onedeg_201501-210012.nc"

cdo -O -mergetime $indir*tos* "rcp85/tos_Omon_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc"
cdo -yearmean "rcp85/tos_Omon_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc" "rcp85/tos_Oyr_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc"
cdo -L -remapbil,global_1 "rcp85/tos_Oyr_CESM2_ssp585_r4i1p1f1_gn_201501-210012.nc" "tos_Oyr_CESM2_ssp585_r4i1p1f1_onedeg_201501-210012.nc"