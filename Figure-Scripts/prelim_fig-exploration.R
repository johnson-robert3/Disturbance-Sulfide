#
# Playing with data and preliminary exploration of seagrass and sediment variables
#


library(tidyverse)

setwd("C:/Users/rajohnson6/Box/Projects/Seagrass Disturbance")


# need to run "data_meadow-data" script first to create meadow datasets
source("C:/Users/rajohnson6/Desktop/Local-Repos/Disturbance-Sulfide/Data-Scripts/data_meadow-data.R")


# Figures

# Thalassia shoot density 
windows(); ggplot(meadow, aes(x = transect_location_m, y = Tt_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = Tt_density, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = Tt_density, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Thalassia blade length 
windows(); ggplot(meadow, aes(x = transect_location_m, y = blade_length, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   ylim(0, 45) +
   theme_bw()


# Thalassia biomass
windows(); ggplot(meadow, aes(x = transect_location_m, y = Tt_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = Tt_biomass, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = Tt_biomass, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Thalassia LAI 
windows(); ggplot(meadow, aes(x = transect_location_m, y = lai, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Macroalgal density 
windows(); ggplot(meadow, aes(x = transect_location_m, y = total_ma_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = total_ma_density, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = total_ma_density, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Macroalgal biomass 
windows(); ggplot(meadow, aes(x = transect_location_m, y = total_ma_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = total_ma_biomass, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = total_ma_biomass, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Drift algae biomass 
windows(); ggplot(meadow, aes(x = transect_location_m, y = drift_biomass, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()



#-- Sediments --# 

# Sediment bulk density 
windows(); ggplot(meadow, aes(x = transect_location_m, y = dbd, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = dbd, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = dbd, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))
   

# DBD box plots by site and treatment
windows(height=3.5, width=5)
ggplot(meadow %>%
          mutate(site = factor(site, levels = c('1', '2', '3')))) +
   #
   geom_boxplot(aes(x = site, y = dbd, group = interaction(site, treatment), color = treatment)) +
   # 
   scale_y_continuous(name = expression(Dry~bulk~density~(g~cm^-3))) +
   scale_color_manual(breaks = vu_break, 
                      values = vu_fill) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Sediment percent OM
windows(); ggplot(meadow, aes(x = transect_location_m, y = perc_om, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = perc_om, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = perc_om, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# OM box plots by site and treatment
windows(height=3.5, width=5)
ggplot(meadow %>%
          mutate(site = factor(site, levels = c('1', '2', '3')))) +
   #
   geom_boxplot(aes(x = site, y = perc_om, group = interaction(site, treatment), color = treatment)) +
   # 
   scale_y_continuous(name = expression(OM~("%"))) +
   scale_color_manual(breaks = vu_break, 
                      values = vu_fill) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))




# Small burrows
windows(); ggplot(meadow, aes(x = transect_location_m, y = Burrow_sm_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()

# based on transect position (distance from edge), by site
windows(height=3.5, width=10)
ggplot(meadow) +
   #
   geom_line(aes(x = transect_location_m, y = Burrow_sm_density, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = transect_location_m, y = Burrow_sm_density, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   # scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   scale_x_continuous(name = "Distance from edge (m)", breaks = seq(-5, 5, 2)) +
   facet_wrap(facets = vars(site), labeller = label_both) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# Large burrows
windows(); ggplot(meadow, aes(x = transect_location_m, y = Burrow_lg_density, color = site_id, group = interaction(treatment, site_id))) +
   #
   geom_line() +
   geom_point() +
   theme_bw()


# Total burrow density box plots by site and treatment
windows(height=3.5, width=5)
ggplot(meadow %>%
          mutate(site = factor(site, levels = c('1', '2', '3')))) +
   #
   geom_boxplot(aes(x = site, y = burrow_density, group = interaction(site, treatment), color = treatment)) +
   # 
   scale_y_continuous(name = expression(Burrow~density~(num.~m^-2))) +
   scale_color_manual(breaks = vu_break, 
                      values = vu_fill) +
   # xlab("FK sites") +
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


