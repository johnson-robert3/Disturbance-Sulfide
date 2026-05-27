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
source("C:/Users/rajohnson6/Desktop/Local-Repos/Disturbance-Sulfide/Data-Scripts/data_S-std-curves.R")

# Create processing functions in 'data_spatial-porewater' script
#  check_stds(), rm_zbsc(), calc_vial_S()



#-- Site 3.1 --#
# Standard curve to use: April 2025
raw_v01 = read_csv("Data/Spec Data/2025.07.26 - FLK24_vertical porewater_S3.1.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_v01, std_apr25)
  # a bit off at the low end


# Pre-process data sheets, remove unnecessary data/rows
v01 = rm_zbsc(raw_v01)

# check agreement between sample dupes
v01 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))


# Sulfide concentration in vials (units = uM)
v01 = calc_vial_S(v01, raw_v01, std_apr25)



#-- Site 3.1, reruns --#
# Standard curve to use: April 2025
raw_v01.r = read_csv("Data/Spec Data/2025.07.26 - FLK24_porewater reruns from 07.26 run.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards (just a continuation of the above run, so stds are exactly the same)
check_stds(raw_v01.r, std_apr25)


# Pre-process data sheets, remove unnecessary data/rows
v01.r = rm_zbsc(raw_v01.r)

# no sample dupes to check within the rerun samples 


# Sulfide concentration in vials (units = uM)
v01.r = calc_vial_S(v01.r, raw_v01.r, std_apr25) %>%
   # keep only vertical samples, remove spatial
   filter(str_detect(sample_id, "cm"))



#-- Sites 1.1 - 3.2, including reruns --#
# Standard curve to use: Dec 2025
raw_v02 = read_csv("Data/Spec Data/2026.02.06 - FLK24_vertical porewater_S1.1-S3.2.csv") %>% 
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_v02, std_dec25)
  # L2 is a bit off, all others good

# Pre-process data sheets, remove unnecessary data/rows
v02 = rm_zbsc(raw_v02)

# check agreement between sample dupes
v02 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup"))) %>% print(n=Inf)
  # look acceptable


# Inspect flagged samples and reruns
   # view all vials from OG (1:1) that that were flagged to be rerun
   tmp = v02 %>% filter(dilution_pre == 1 & flag %in% c('L','H')) %>% filter(!(str_detect(sample_id, pattern="dup"))) 
   # view all vials that were reruns (1:10)
   tmp2 = v02 %>% filter(dilution_pre == 10) %>% filter(!(str_detect(sample_id, pattern="dup"))) 
   # view all vials that still need to be rerun again at a higher DF
   tmp3 = v02 %>% filter(dilution_pre == 10 & flag %in% c('L','H')) %>% filter(!(str_detect(sample_id, pattern="dup"))) 
   # view the vials that were rerun, but were not flagged with the OG
   tmp.c = setdiff(tmp2$sample_id, tmp$sample_id)
   
   v02 %>% filter(sample_id %in% tmp.c) %>% View
   rm(list = ls(pattern='tmp'))


# Sulfide concentration in vials (units = uM)
v02 = calc_vial_S(v02, raw_v02, std_dec25) %>%
   # remove the reruns where the original sample (at 1:1) was within the curve (i.e., did not need to be rerun)
   filter_out(str_detect(notes, "disregard"))




#--
# Porewater Sulfide Concentration
#--

# Porewater sample data
vert_pw_sample_data = read.csv("Data/FLK24_vertical_porewater.csv")


# Combine spec runs and calculate sulfide concentration (units = uM)
fk_pw_vertical = bind_rows(v01, v01.r, v02) %>%
   # correct measured sulfide concentration for any dilution prior to adding diamine reagent (units = uM)
   mutate(scint_S_uM = vial_S_uM * dilution_pre) %>%
   # for samples below DL at dilution 1:2 (abs < 0.02), vial_S becomes 2.0, but should be 1.0uM, replace conc with half the DL (DL=2uM)
   mutate(scint_S_uM = replace(scint_S_uM, flag %in% c("DL"), 1))


#- Sulfide concentration in original porewater
fk_pw_vertical = fk_pw_vertical %>%
   select(subsample_id = sample_id, scint_S_uM) %>%
   # make sample ID structure match with sample data file
   mutate(subsample_id = paste0("FLK24-", subsample_id)) %>% 
   # combine with sample data
   left_join(vert_pw_sample_data) %>%
   relocate(scint_S_uM, .after=last_col()) %>%
   # calculate porewater S concentration (units = uM)
   mutate(
      # total S in vial (concentration times total aqueous volume)
      scint_S_umol = scint_S_uM * ((pw_sample_vol_cc + pw_znac_vol_cc) / 1000),
      # concentration of S in porewater
      porewater_S_uM = scint_S_umol / (pw_sample_vol_cc / 1000))


