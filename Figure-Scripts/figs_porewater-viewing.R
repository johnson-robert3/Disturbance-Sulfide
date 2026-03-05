#
# Figures for viewing porewater sulfide data
#

library(tidyverse)


# run 'data_spatial-porewater-S' and 'data_meadow-data' scripts first to generate data

se = function(.x) { sd(.x, na.rm=T) / sqrt(length(na.omit(.x))) }

# add a new variable for graphing distance along the transect
meadow = meadow %>%
   mutate(distance = if_else(treatment=="unvegetated", transect_location_m * -1, transect_location_m))



#===
# Spatial Porewater along Transects
#===


#-- Surface Porewater S viewed along transects --#

# Surface PW S based on distance from edge, all sites and treatments together
windows(height=3.5, width=5)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = surface_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
   geom_point(aes(x = distance, y = surface_pwS, fill = site_id), shape = 21, size=3) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Porewater~sulfide~(mu*M))) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

      # log
      windows(height=3.5, width=5)
      ggplot(meadow) +
         #
         geom_line(aes(x = distance, y = log(surface_pwS), group = interaction(site_id, treatment), linetype = treatment, color = site_id)) +
         geom_point(aes(x = distance, y = log(surface_pwS), fill = site_id), shape = 21, size=3) +
         geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
         #
         scale_y_continuous(name = expression(ln~Porewater~sulfide~(mu*M))) +
         #
         theme_classic() +
         theme(panel.border = element_rect(color="black", fill=NA))
  

# Surface PW S based on distance from edge, by site
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   # geom_smooth(aes(x = distance, y = surface_pwS, group = treatment), color="black", linewidth=1) +
   geom_line(aes(x = distance, y = surface_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surface_pwS, fill = site_id, color=site_id), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~(mu*M))) +
   # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
   facet_wrap(facets = vars(site_name)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Surface PW S based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = surface_pwS, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surface_pwS), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

      # log
      windows(height=3.5, width=8)
      ggplot(meadow) +
         #
         geom_line(aes(x = distance, y = log(surface_pwS), group = interaction(site_id, treatment), linetype = treatment), 
                   linewidth= 0.75, alpha=0.5) +
         geom_point(aes(x = distance, y = log(surface_pwS)), size=3, alpha = 0.5) +
         geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
         #
         scale_y_continuous(name = expression(ln~Surface~sulfide~(mu*M))) +
         facet_wrap(facets = vars(site_id), nrow=2) +
         #
         theme_classic() +
         theme(panel.border = element_rect(color="black", fill=NA))



# Surface sulfide vs distance from the veg edge
#  all data pooled, separated by treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = surface_pwS, color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = surface_pwS, color = treatment), method="lm", se=F)

# log transform
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(surface_pwS), color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = log(surface_pwS), color = treatment), method="lm", se=F)


# with separate lines by treatment
#  all data pooled, separated by site and treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = surface_pwS, color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = surface_pwS, color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)

# log transform
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(surface_pwS), color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = log(surface_pwS), color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)

# viewing sites individually
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(surface_pwS), color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = log(surface_pwS), color = treatment, 
                   group=interaction(treatment, site_name), linetype=site_name), 
               method="lm", se=F) +
   facet_wrap(facet = vars(site_name), nrow=1)


# surface sulfide, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(surface_pwS, na.rm=T), se = se(surface_pwS), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "Surface sulfide") +
   theme_classic()

# log
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(log(surface_pwS), na.rm=T), se = se(log(surface_pwS)), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "log Surface sulfide") +
   theme_classic() +
   theme_classic()




#-- Rhizome Porewater S viewed along transects --#

# Rhizome porewater based on distance from edge, by site
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = rhizome_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhizome_pwS, fill = site_id, color=site_id), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site_name)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Rhizome PW S based on distance from edge, all sites individually
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   geom_line(aes(x = distance, y = rhizome_pwS, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhizome_pwS), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~(mu*M))) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

      # log
      windows(height=3.5, width=8)
      ggplot(meadow) +
         #
         geom_line(aes(x = distance, y = log(rhizome_pwS), group = interaction(site_id, treatment), linetype = treatment), 
                   linewidth= 0.75, alpha=0.5) +
         geom_point(aes(x = distance, y = log(rhizome_pwS)), size=3, alpha = 0.5) +
         geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
         #
         scale_y_continuous(name = expression(ln~Rhizome~sulfide~(mu*M))) +
         facet_wrap(facets = vars(site_id), nrow=2) +
         #
         theme_classic() +
         theme(panel.border = element_rect(color="black", fill=NA))
      

# Rhizome porewater based on distance from edge, but treatments separated (by patch and treatment)
windows(height=3.5, width=8)
ggplot(meadow) +
   #
   # geom_smooth(aes(x = transect_location_m, y = rhizome_pwS, group = treatment), color="black", linewidth=1) +
   geom_line(aes(x = transect_location_m, y = rhizome_pwS, group = interaction(site_id, treatment), linetype = treatment, color = site_id), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS, fill = site_id, color=site_id), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   geom_hline(aes(yintercept=0), linetype=3, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~(mu*M))) +
   # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
   facet_grid(rows = vars(treatment),
              cols = vars(site_id)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# Rhizome sulfide vs distance from the veg edge
#  all data pooled, separated by treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS, color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = rhizome_pwS, color = treatment), method="lm", se=F)

# log transform
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(rhizome_pwS), color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = log(rhizome_pwS), color = treatment), method="lm", se=F)


# with separate lines by treatment
#  all data pooled, separated by site and treatment
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS, color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = rhizome_pwS, color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)

# log transform
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(rhizome_pwS), color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = log(rhizome_pwS), color = treatment, group=interaction(treatment, site_name), linetype=site_name), method="lm", se=F)

# viewing sites individually
ggplot(meadow) +
   geom_point(aes(x = transect_location_m, y = log(rhizome_pwS), color = treatment), position = position_jitter(height=0, width=0.07)) + 
   geom_smooth(aes(x = transect_location_m, y = log(rhizome_pwS), color = treatment, 
                   group=interaction(treatment, site_name), linetype=site_name), 
               method="lm", se=F) +
   facet_wrap(facet = vars(site_name), nrow=1)


# rhizome sulfide, all sites pooled, mean + SE by treatment
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(rhizome_pwS, na.rm=T), se = se(rhizome_pwS), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "Rhizome sulfide") +
   theme_classic()

# log
windows(height=3, width=5)
ggplot(meadow %>%
          summarize(mean = mean(log(rhizome_pwS), na.rm=T), se = se(log(rhizome_pwS)), .by = c(treatment, distance))) +
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "log Rhizome sulfide") +
   theme_classic()




#- Surface and Rhizome porewater, all patches and separated by treatment
windows(height=3.5, width=8)
ggplot(meadow) +
   # surface - Green
   geom_line(aes(x = transect_location_m, y = surface_pwS), 
             linetype = 1, linewidth= 0.75, alpha=0.5, color="seagreen4") +
   geom_point(aes(x = transect_location_m, y = surface_pwS), size=2, alpha = 0.5, shape=22, color="seagreen4") +
   # rhizome - Blue
   geom_line(aes(x = transect_location_m, y = rhizome_pwS), 
             linetype = 1, linewidth= 0.75, alpha=0.5, color="cornflowerblue") +
   geom_point(aes(x = transect_location_m, y = rhizome_pwS), size=2, alpha = 0.5, shape=17, color="cornflowerblue") +
   # 
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   geom_hline(aes(yintercept=0), linetype=3, color="gray50") +
   #
   scale_y_continuous(name = expression(Sulfide~(mu*M))) +
   # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
   facet_grid(rows = vars(treatment),
              cols = vars(site_id)) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))

      # log
      windows(height=3.5, width=8)
      ggplot(meadow %>% mutate(across(c(surface_pwS, rhizome_pwS), ~log(.)))) +
         # surface - Green
         geom_line(aes(x = transect_location_m, y = surface_pwS), 
                   linetype = 1, linewidth= 0.75, alpha=0.5, color="seagreen4") +
         geom_point(aes(x = transect_location_m, y = surface_pwS), size=2, alpha = 0.5, shape=22, color="seagreen4") +
         # rhizome - Blue
         geom_line(aes(x = transect_location_m, y = rhizome_pwS), 
                   linetype = 1, linewidth= 0.75, alpha=0.5, color="cornflowerblue") +
         geom_point(aes(x = transect_location_m, y = rhizome_pwS), size=2, alpha = 0.5, shape=17, color="cornflowerblue") +
         # 
         geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
         geom_hline(aes(yintercept=0), linetype=3, color="gray50") +
         #
         scale_y_continuous(name = expression(ln~Sulfide~(mu*M))) +
         # scale_shape_manual(values = c('surface' = 21, 'rhizome' = 25)) +
         facet_grid(rows = vars(treatment),
                    cols = vars(site_id)) +
         #
         theme_classic() +
         theme(panel.border = element_rect(color="black", fill=NA))



#-- Porewater S, Bare relative to Seagrass --#

# for viewing differences between Veg and Unveg areas at the individual patch level

# create temporary dataset for difference in sulfide between Veg and Unveg treatments
tmp = meadow %>%
   arrange(site_id, transect_location_m, desc(treatment)) %>% 
   select(site_id, transect_location_m, treatment, contains("pwS")) %>%
   # calculate the difference between veg and unveg for corresponding distances on the transect for each patch
   #- positive (+) value means sulfide is higher in the seagrass
   #- negative (-) value means suflide is higher in the bare patch
   summarize(surf_s_diff = surface_pwS[treatment=="vegetated"] - surface_pwS[treatment=="unvegetated"], 
             rhiz_s_diff = rhizome_pwS[treatment=="vegetated"] - rhizome_pwS[treatment=="unvegetated"], 
             .by = c(site_id, transect_location_m))


# Surface S, view variation in the data across transect distances
ggplot(tmp) +
   geom_point(aes(x = transect_location_m, y = surf_s_diff), position = position_jitter(height=0, width=0.07))

# view data at a given distance (change in filter)
ggplot(tmp %>% filter(transect_location_m==1.0)) + 
   geom_point(aes(x = site_id, y = surf_s_diff)) +
   geom_hline(yintercept=0, linetype=3)

# view V-U diff along the transect length, for each site individually
ggplot(tmp) +
   geom_point(aes(x = transect_location_m, y = surf_s_diff)) +
   geom_line(aes(x = transect_location_m, y = surf_s_diff)) +
   facet_wrap(facet = vars(site_id), scales = 'free_y')



# Rhizome S, view variation in the data across transect distances
ggplot(tmp) +
   geom_point(aes(x = transect_location_m, y = rhiz_s_diff), position = position_jitter(height=0, width=0.07))

# view data at a given distance (change in filter)
ggplot(tmp %>% filter(transect_location_m==1.0)) + 
   geom_point(aes(x = site_id, y = rhiz_s_diff)) +
   geom_hline(yintercept=0, linetype=3)

# view V-U diff along the transect length, for each site individually
ggplot(tmp) +
   geom_point(aes(x = transect_location_m, y = rhiz_s_diff)) +
   geom_line(aes(x = transect_location_m, y = rhiz_s_diff)) +
   facet_wrap(facet = vars(site_id), scales = 'free_y')




#-- Porewater S standardized to sediment variables

tmp = meadow %>%
   mutate(
      # relative to OM
          surf_s_by_om = surface_pwS / perc_om,
          rhiz_s_by_om = rhizome_pwS / perc_om,
      # relative to DBD
          surf_s_by_bd = surface_pwS / dbd,
          rhiz_s_by_bd = rhizome_pwS / dbd,
      # relative to thalassia biomass
          surf_s_by_agb = if_else(is.na(Tt_biomass), surface_pwS, surface_pwS/Tt_biomass),
          rhiz_s_by_agb = if_else(is.na(Tt_biomass), rhizome_pwS, rhizome_pwS/Tt_biomass),
      # relative to rhizome biomass
          surf_s_by_bgb = if_else(is.na(bg_biomass), surface_pwS, surface_pwS/bg_biomass),
          rhiz_s_by_bgb = if_else(is.na(bg_biomass), rhizome_pwS, rhizome_pwS/bg_biomass),
      # relative to thalassia LAI
          surf_s_by_lai = if_else(lai=='NaN', surface_pwS, surface_pwS/lai),
          rhiz_s_by_lai = if_else(lai=='NaN', rhizome_pwS, rhizome_pwS/lai),
      # relative to burrow density
         surf_by_burr = surface_pwS / (burrow_density+1),
         rhiz_by_burr = rhizome_pwS / (burrow_density+1))


# view Surface S standardized to sed OM along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = surf_s_by_om, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surf_s_by_om), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~by~OM)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Surface S standardized to sed DBD along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = surf_s_by_bd, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surf_s_by_bd), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~by~DBD)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Surface S standardized to Tt biomass along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = surf_s_by_agb, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = surf_s_by_agb), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Surface~sulfide~by~Tt~biomass)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))



# view Rhizome S standardized to sed OM along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = rhiz_s_by_om, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhiz_s_by_om), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~by~OM)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Rhizome S standardized to sed DBD along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = rhiz_s_by_bd, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhiz_s_by_bd), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~by~DBD)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Rhizome S standardized to Tt LAI along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = rhiz_s_by_lai, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhiz_s_by_lai), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~by~LAI)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Rhizome S standardized to burrow density along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = rhiz_by_burr, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhiz_by_burr), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(Rhizome~sulfide~by~burrows)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Rhizome S standardized to porosity along transect distance, all sites individually 
windows(height=3.5, width=8)
ggplot(tmp) +
   #
   geom_line(aes(x = distance, y = rhiz_by_por, group = interaction(site_id, treatment), linetype = treatment), 
             linewidth= 0.75, alpha=0.5) +
   geom_point(aes(x = distance, y = rhiz_by_por), size=3, alpha = 0.5) +
   geom_vline(aes(xintercept = 0), linetype=2, color="gray50") +
   #
   scale_y_continuous(name = expression(log~Rhizome~sulfide~by~porosity)) +
   facet_wrap(facets = vars(site_id), nrow=2) +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))


# view Rhizome S standardized to porosity along transect distance, all sites pooled (mean+SE) 
windows(height=3.5, width=8)
ggplot(meadow %>%
          mutate(tmp = log(rhizome_pwS / porosity)) %>%
          summarize(mean = mean(tmp, na.rm=T), se = se(tmp), .by = c(treatment, distance))) +
   #
   geom_errorbar(aes(x = distance, ymin = mean - se, ymax = mean + se), width=0) +
   geom_point(aes(x = distance, y = mean), size=2) +
   geom_line(aes(x = distance, y = mean)) +
   geom_vline(xintercept = 0, linetype=2) +
   labs(y = "log Rhizome S by porosity") +
   #
   theme_classic() +
   theme(panel.border = element_rect(color="black", fill=NA))




#===
# Vertical Porewater Profiles
#===

# test - just view Craig Key at the 0.5m mark, seagrass vs disturbed
ggplot(fk_pw_vertical %>% 
          filter(site == 1, transect_location_m == 0.5) %>%
          mutate(core_depth_cm = replace_values(core_depth_cm, 14 ~ 15, 19 ~ 20)) %>%
          summarize(mean = mean(porewater_S_uM, na.rm=T), se = se(porewater_S_uM), .by = c(treatment, core_depth_cm))) +
   # 
   geom_line(aes(x = core_depth_cm, y = mean, color = treatment)) +
   geom_errorbar(aes(x = core_depth_cm, ymin = mean - se, ymax = mean + se, color = treatment), width = 0.5) +
   geom_point(aes(x = core_depth_cm, y = mean, color = treatment), size = 3) +
   #
   coord_flip() +
   scale_x_reverse(name = "Depth", limits = c(20, 0)) +
   scale_y_continuous(name = "Porewater Sulfide (uM)") +
   theme_classic()


# all together
windows(height=4, width=8)
ggplot(fk_pw_vertical %>% 
          mutate(core_depth_cm = replace_values(core_depth_cm, 14 ~ 15, 19 ~ 20)) %>% 
          summarize(mean = mean(porewater_S_uM, na.rm=T), se = se(porewater_S_uM), .by = c(site_name, treatment, transect_location_m, core_depth_cm))) +
   # 
   geom_line(aes(x = core_depth_cm, y = mean, color = treatment)) +
   geom_errorbar(aes(x = core_depth_cm, ymin = mean - se, ymax = mean + se, color = treatment), width = 0.5) +
   geom_point(aes(x = core_depth_cm, y = mean, color = treatment), size = 3) +
   #
   coord_flip() +
   scale_x_reverse(name = "Depth", limits = c(25, 0)) +
   scale_y_continuous(name = "Porewater Sulfide (uM)") +
   #
   facet_grid(rows = vars(transect_location_m), cols = vars(site_name)) +
   theme_bw()









