#~~~
# Script for processing spectrophotometer data and calculating sulfide concentration from vertical porewater samples
# By: R. Johnson
#
# Vertical porewater sulfide - from vertical sediment cores
# Summer 2024 Florida Keys sampling
#~~~


setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance")

library(tidyverse)

# need to run "data_S-std-curves" script first
source("C:/Users/rajohnson6/Desktop/Local-Repos/Disturbance-Edge-Effects/Data-Scripts/data_S-std-curves.R")


# create processing functions in 'data_spatial-porewater' scripts 
#  check_stds(), rm_zbsc(), calc_vial_S()


# --> vertical PW sulfide data for site S3.1 are in the file "2025.07.26 - FLK24_vertical porewater_S3.1.csv"

## --> there are also re-runs in the file "2025.07.26 - FLK24_porewater reruns from 07.26 run.csv"

## --> remaining samples are in the file "2026.02.06 - FLK24_vertical porewater_S1.1-S3.2.csv"

