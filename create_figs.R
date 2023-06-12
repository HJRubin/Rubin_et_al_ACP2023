#This script creates the figures for Rubin et al., (2023).

library(ggplot2)
library(ggpmisc)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(readxl)

setwd("~/R/MMF")
# obs = read_excel('comp_final_wetdepOnly.xlsx')
# obs$Actual.Obs_mg = obs$NH4 * 100* 14/18
# obs_new = na.omit(obs)
# obs_new = obs_new[obs_new$Actual.Obs_mg > 0, ]
# obs_new$Tdep = obs_new$Tdep * 100

# setwd("~/R/Wet Deposition")
# 
# wet = read_xls('ntn_wet_nh4.xls')
# wet = wet[wet$nh4_ww_201 > 0,]
# prj =  read_xls('ntn_030922_TableToExcel2.xls')
# prj = prj[prj$nh4_ww2010 > 0,]
# # prj = prj[prj$status == "A", ]
# plot(prj$nh4_ww_201, prj$nh4_ww2010)
# prj$NH4_mg = prj$NH4*100 *14/18
# prj$nh4_ww2010_mg = prj$nh4_ww2010*100

obs_new = read_excel("Wet Deposition/obs_extract1deg.xlsx")
obs_new$NH4_mgm2 = as.numeric(obs_new$NH4_mgm2)
obs_new$NO3_mgm2 = as.numeric(obs_new$NO3_mgm2)
obs_new$SO4_mgm2 = as.numeric(obs_new$SO4_mgm2)
obs_new$Tdep = obs_new$dep_nh4_2010 * 100
obs_new$HTAP = obs_new$wetnh4_htap_31540000


#Wet Dep NH4
formula <- y ~ x
ggplot(obs_new, aes(x = NH4_mgm2, y = Tdep)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#00BFC4')  +  
  ylab(bquote('TDep ' ~NH[4]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~NH[4]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,750)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1,750)) +
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)

obs_new$nh4_1 = obs_new$main_nh4_idw31540000 

formula <- y ~ x
ggplot(obs_new, aes(x = NH4_mgm2, y = nh4_5deg)) + #main_nh4_03072023
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#7CAE00')  +  
  ylab(bquote('MMF ' ~NH[4]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~NH[4]~ (mg~N/m^2))) + 
  # scale_x_continuous(expand = c(0, 0), limits = c(-1,750)) + 
  # scale_y_continuous(expand = c(0, 0), limits = c(-1,750)) + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)
  

ggplot(obs_new, aes(x = NH4_mgm2, y = wetnh4_htap_31540000)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#F8766D')  + 
  ylab(bquote('HTAP II ' ~NH[4]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~NH[4]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,750)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1,750)) + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)


#################Combo graph########################


data_ggp <- data.frame(x = obs_new$NH4_mgm2,                            # Reshape data frame
                       y = c(obs_new$main_nh4_03072023, obs_new$main_nh4_25deg, obs_new$nh4_5deg,
                             obs_new$Tdep, obs_new$wetnh4_htap_31540000),
                       Model = c(rep("MMF (1 degree)", nrow(obs_new)),
                                 rep("MMF (2.5 degrees)", nrow(obs_new)),
                                 rep("MMF (5 degrees)", nrow(obs_new)),
                                 rep("TDep", nrow(obs_new)),
                                 rep("HTAP II", nrow(obs_new))))
ggp <- ggplot(data_ggp, aes(x, y, col = Model)) +             # Create ggplot2 plot
  geom_point() + 
  xlab(bquote('Observed ' ~NH[4]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,730)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1,730)) + 
  ylab(bquote('Modeled ' ~NH[4]~ (mg~N/m^2))) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, size = 1)+
  theme(text = element_text(size = 16), panel.background = element_rect(fill = 'White', color = 'black'))
ggp 




##############################################
em = read_excel('R/MMF/emep_export_alldegs3.xlsx')
em$NO3_mgm2 = as.numeric(em$NO3_mgm2)
em$NH4_mgm2 = as.numeric(em$NH4_mgm2)

MMF5 = as.data.frame(em$total_NHx + em$mosaic_no3_new)
MMF25 = as.data.frame(em$main_nh4_idw31540000 + em$main_no3_25_raster)
MMF1 = as.data.frame(em$mosaic_no3_1deg_1 + em$mosaic_nh4_1deg_1)
HTAP = as.data.frame(em$wetnh4_htap_31540000 + em$wetno3_3154)
EMEP = as.data.frame(em$NH4_mgm2 + em$NO3_mgm2)
MMF5$name = "MMF (5 degrees)"
MMF25$name = "MMF (2.5 degrees)"
MMF1$name = "MMF (1 degree)"
HTAP$name = "HTAP II"  
EMEP$name = "EMEP"  
colnames(MMF5)[which(names(MMF5) == 'em$total_NHx + em$mosaic_no3_new')] <- "value"
colnames(MMF25)[which(names(MMF25) == 'em$main_nh4_idw31540000 + em$main_no3_25_raster')] <- "value"
colnames(MMF1)[which(names(MMF1) == 'em$mosaic_no3_1deg_1 + em$mosaic_nh4_1deg_1')] <- "value"
colnames(HTAP)[which(names(HTAP) == 'em$wetnh4_htap_31540000 + em$wetno3_3154')] <- "value"
colnames(EMEP)[which(names(EMEP) == 'em$NH4_mgm2 + em$NO3_mgm2')] <- "value"
a = rbind(MMF5, MMF25, MMF1, HTAP, EMEP)

ggplot(a, aes(x=name, y=value, fill=name)) +
  geom_boxplot() + 
  scale_fill_manual(values=c("lightyellow", "grey", "purple", "lightblue", "forestgreen")) +
  # geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(text = element_text(size = 16), panel.background = element_rect(fill = 'White', color = 'black'), legend.position = "none") + 
  xlab("") + 
  ylab('mg N/m2')



################NO3_Plots_US###################
###############################################

# all_obs = read_excel('all_points_us_Sites_n.xlsx')
# obs_new = all_obs[all_obs$NO3_mgm2 > 0,]
# obs_new = all_obs[all_obs$TDEP_NO3 > 0,]
# obs_new$TDEP_NO3 = obs_new$TDEP_NO3 * 100
# obs_new$mmf_idw_no = as.numeric(obs_new$mmf_idw_no)
# obs_new$dep_no3_ = as.numeric(obs_new$dep_no3_) * 100
# obs_new$noyclip = as.numeric(obs_new$noyclip) / 10

obs_new$dep_no3_2010_scale = obs_new$dep_no3_2010 * 100
obs_new$wetno3_3154_scale = obs_new$wetno3_3154 
obs_new$wetno3_htap_scale = obs_new$wetno3_3154 * 10000000 * 31540000
obs_new$no3_5deg_scale = obs_new$no3_5deg 
obs_new$main_no3_25deg_scale = obs_new$main_no3_25deg 
obs_new$main_no3_03072024_scale = obs_new$main_no3_03072024 

formula <- y ~ x
ggplot(obs_new, aes(x = NO3_mgm2, y = dep_no3_2010_scale)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#00BFC4')  +  
  ylab(bquote('TDep ' ~NO[3]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~NO[3]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1400)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1400)) + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)

# obs_new$no3_extract_scale = obs_new$no3_extract * 10

ggplot(obs_new, aes(x = NO3_mgm2, y = main_no3_25deg)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#7CAE00')  +  
  ylab(bquote('MMF ' ~NO[3]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~NO[3]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1400)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1400)) +
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)


ggplot(obs_new, aes(x = NO3_mgm2, y = wetno3_3154_scale)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#F8766D')  +  
  ylab(bquote('HTAP ' ~NO[3]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~NO[3]~ (mg~N/m^2))) + 
  # scale_x_continuous(expand = c(0, 0), limits = c(-1,1400)) + 
  # scale_y_continuous(expand = c(0, 0), limits = c(-1,1400)) + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)


data_ggp <- data.frame(x = obs_new$NO3_mgm2,                            # Reshape data frame
                       y = c(obs_new$no3_5deg, obs_new$main_no3_03072024, obs_new$main_no3_25deg, 
                             obs_new$dep_no3_2010_scale, obs_new$wetno3_3154_scale),
                       Model = c(rep("MMF (5 degrees)", nrow(obs_new)),
                                 rep("MMF (1 degree)", nrow(obs_new)),
                                 rep("MMF (2.5 degrees)", nrow(obs_new)),
                                 rep("TDep", nrow(obs_new)),
                                 rep("HTAP II", nrow(obs_new))))
ggplot(data_ggp, aes(x, y, col = Model)) +             # Create ggplot2 plot
  geom_point() + 
  xlab(bquote('Observed ' ~NO[3]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1500)) +
  ylab(bquote('Modeled ' ~NO[3]~ (mg~N/m^2))) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, size = 1)+
  theme(text = element_text(size = 16), panel.background = element_rect(fill = 'White', color = 'black'))
 

#######################
# all_obs1 = read_excel('all_points_us_Sites_new.xlsx')
# obs_new = all_obs1[all_obs1$SO4_mgm2 > 0,]
# obs_new = obs_new[obs_new$TDEPSO4 > 0,]
# obs_new$SO4_mgm2 = as.numeric(obs_new$SO4_mgm2)
# obs_new$TDEPSO4 = obs_new$TDEPSO4 * 100
# obs_new$wetso4_Lay = obs_new$wetso4_Lay * 100000000000000 * 14/96
# obs_new$MMF_SO4_US = as.numeric(obs_new$MMF_SO4_US)

obs_new$dep_so4_2010_scale = obs_new$dep_so4_2010 * 100

formula <- y ~ x
ggplot(obs_new, aes(x = SO4_mgm2, y = dep_so4_2010_scale)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#00BFC4')  +  
  ylab(bquote('TDep ' ~SO[4]~ (mg~S/m^2))) + 
  xlab(bquote('Observed ' ~SO[4]~ (mg~S/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1500)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1500)) + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)


ggplot(obs_new, aes(x = SO4_mgm2, y = obs_new$main_so4_03072023)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#7CAE00')  +  
  ylab(bquote('MMF ' ~SO[4]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~SO[4]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1500)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1500)) +
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)


ggplot(obs_new, aes(x = SO4_mgm2, y = wetso4_htap_31540000)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = formula, se = FALSE, color = '#F8766D')  +  
  ylab(bquote('HTAP ' ~SO[4]~ (mg~N/m^2))) + 
  xlab(bquote('Observed ' ~SO[4]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1500)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1500)) + 
  theme(text = element_text(size = 20), panel.background = element_rect(fill = 'White', color = 'black')) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formula, parse = TRUE, size = 10) + 
  geom_abline(intercept = 0, slope = 1, size = 1)


data_ggp <- data.frame(x = obs_new$SO4_mgm2,                            # Reshape data frame
                       y = c(obs_new$main_so4_03072023, obs_new$main_so4_25deg, obs_new$so4_5deg,
                             obs_new$dep_so4_2010_scale, obs_new$wetso4_htap_31540000),
                       Model = c(rep("MMF (1 degree)", nrow(obs_new)),
                                 rep("MMF (2.5 degrees)", nrow(obs_new)),
                                 rep("MMF (5 degrees)", nrow(obs_new)),
                                 rep("TDep", nrow(obs_new)),
                                 rep("HTAP II", nrow(obs_new))))
ggp <- ggplot(data_ggp, aes(x, y, col = Model)) +             # Create ggplot2 plot
  geom_point() + 
  xlab(bquote('Observed ' ~SO[4]~ (mg~N/m^2))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(-1,1500)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1500)) + 
  ylab(bquote('Modeled ' ~SO[4]~ (mg~N/m^2))) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, size = 1)+
  theme(text = element_text(size = 16), panel.background = element_rect(fill = 'White', color = 'black'))
ggp 
