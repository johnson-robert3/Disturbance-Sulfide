#~~~
# Script for processing spectrophotometer data for suflide standard curves
# By: R. Johnson
#~~~


setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance")

library(tidyverse)


#-
# Low curve, April 2025
#- 

# Standard curve using MQ water instead of ZnAc for all standards, blanks, and dilutions

   # S std date: 6/20/2023
   # Diamine reag. (L) date: 4/05/2025


# raw data from spec run
std_curve_apr25 = read_csv("Data/Spec Data/Sulfide_Low_standard_curve_Apr_2025.csv") %>%
   janitor::remove_empty(which = 'rows')


# Clean up the data
std_apr25 = std_curve_apr25 %>%
   # remove 'zero'
   filter(!(vial == "Zero")) %>%
   # remove checks
   filter(!(str_detect(vial, pattern='chk')))


# Calculate corrected absorbance for standards
std_apr25 = std_apr25 %>%
   filter(!(vial == "Blank")) %>%
   # correct for blank absorbance
   mutate(abs_blk_corr = abs_667 - (std_apr25 %>% filter(vial == "Blank") %>% pull(abs_667) %>% mean)) %>%
   # correct for dilution factor
   mutate(abs_corr = abs_blk_corr * dilution)


# View the curve
windows(); ggplot(std_apr25, aes(x = abs_corr, y = conc_uM)) + geom_point()
graphics.off()
summary(lm(conc_uM ~ abs_corr, data = std_apr25))  # R2 = 0.995



#~~~
# Equation for concentration - absorbance relationship
#~~~

# lm(conc_uM ~ abs_df_corr, data = std_curve) %>% coef()

calc_S_conc = function(.abs, .std_curve) {
   
   intercept = coef(lm(conc_uM ~ abs_corr, data = .std_curve))[[1]]
   slope = coef(lm(conc_uM ~ abs_corr, data = .std_curve))[[2]]
   
   concentration = (.abs * slope) + intercept
   
   return(concentration)
}

