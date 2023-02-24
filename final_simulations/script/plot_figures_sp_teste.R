#-------------------------------------------------------------------------------
# SCRIPT PLOT RESULTS FROM IBM SIMULATION - HABITAT SELECTION SETTLEMENT DISPERSAL
#-------------------------------------------------------------------------------
#-------------------------PLOT FUNCTION OF PLASTICITY---------------------------
En <- seq(0,1,0.01)
e_C <- c(0,0.5,1,1.5,2)
#Ci <- 1
#C <- Ci*(En^(e_C))
#plot(En,C,ylim = c(0,1),type="l")
d <- data.frame(C=double(),En=double(),e_C=double())
for (i in e_C) {
  Ci <- 1
  C <- Ci*(En^(e_C))
  di <- cbind(C,En,e_C)
  d <- rbind(di,d)
}
tiff("Plasticity_level_simulated.tiff",width = 600,height = 400,res = 100)
ggplot(d,aes(x=En,y=C,colour=as.factor(e_C),group=as.factor(e_C)))+
  geom_line(size=0.75) +
  scale_colour_brewer(palette = "Paired") +
  scale_x_reverse() +
  theme(strip.text = element_text(size=10), strip.background = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10,hjust = 0.5), legend.title=element_text(size=10),legend.position="bottom")+
  labs(colour="Plasticity Level(e_C)",y="Habitat Quality Threshold (C)", x="Energetic Condition (En)")
dev.off()

#-------------------------IMPORT OUTPUT DATA -----------------------------------
datao <- read.table(file = "~/Documents/phd_project/Chapter_2/output/final_simulations/output.txt",sep = ",",header = T)
datap <- read.table(file = "~/Documents/phd_project/Chapter_2/output/final_simulations/sim_parameters.txt",sep = ",",header = T)
data = merge(x = datao, y = datap, by = "run_number",
                all.x = TRUE)
# set directory to save figures
setwd("~/Documents/phd_project/Chapter_2/output/final_simulations")
#-------------------------PACKAGES ---------------------------------------------
library(ggplot2)
library(cowplot)
library(dplyr)
library(mgcv)
library(viridis)
library(ggridges)
#-------------------------------------------------------------------------------
# PLOTS DATA SETTLEMENT, HAB QUALITY, ENERGETICS BOXPLOT
#-------------------------------------------------------------------------------
# data frame
data$specie= factor(data$specie, levels = c("DA","PQ","MP"))
specie_names <- c('DA' = "D.aurita",'PQ' = "P. quica",'MP' = "M. paraguayana")
data$prop_dist <- data$total_dist / data$linear_dist

data_means <- data %>% 
  group_by(run_number,behaviour,hab_amount,clumpiness,initial_individuals,specie,decision) %>% 
  summarise(n = length(turtle),
            meanQ = mean(Hab_quality_area),
            meanen = mean(final_En),
            meandt = mean(total_dist) / 1000,
            meanld = mean(linear_dist) / 1000,
            meanrd = mean(prop_dist)) %>%
  mutate (rate = n / (initial_individuals /length(unique(data$behaviour)))) 

theme <- theme(panel.border = element_blank(),
               strip.text.x = element_text(size = 10, face = ("italic")),panel.spacing = unit(0, "lines"),
               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Settlement rate

g1 <- ggplot(data_means, aes(x=behaviour, y=rate, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.3) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  theme + ylim(0,1) +
  theme(legend.position = "none",
        legend.margin=margin(0,0,0,20),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        #plot.margin = unit(c(.5,9,0,9),"lines")
        ) +
  labs(x="",y = "Settlement\nRate",
       colour="Plasticity Level")
g1

# Habitat quality in settlement

g2 <- ggplot(data_means, aes(x=behaviour, y=meanQ, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.3) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  theme + ylim(0,1) +
  theme(strip.background =element_blank(),
        #strip.text.x = element_blank(),
        #panel.background = element_rect(colour = "white"),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        #axis.text.y = element_blank(),axis.ticks.y=element_blank(),
        #plot.margin = unit(c(0,.5,0,.5),"lines")
        ) +
  labs(x="", y= "Mean Habitat Quality\nof Settlement Area") + 
  guides(colour="none")
g2

# Energetic Condition on settlement

g3 <- ggplot(data_means, aes(x=behaviour, y=meanen, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_smooth(method = "glm", se=TRUE, aes(group=1)) +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.3) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  theme + ylim(0,1) +
  theme(legend.position = "bottom",
        #legend.position = c(0.1,0.9),legend.title=element_text(size=8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 7)) +
  theme(strip.background =element_blank(),
        #strip.text.x = element_blank(),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        #axis.text.y = element_blank(),axis.ticks.y=element_blank(),
        #plot.margin = unit(c(0,.5,0,.5),"lines")
        ) +
  labs(x = "",y="Mean Energetic \nCondition on Settlement",
       colour="Plasticity Level") +
  guides(colour = "none")
g3

#-------------------------------------------------------------------------------
# PLOTS DISTANCE DATA BOXPLOT
#-------------------------------------------------------------------------------
# Distance total

g4 <- ggplot(data_means, aes(x=behaviour, y=meandt, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.3) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme + 
  theme(strip.background = element_blank(),
       # strip.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        #plot.margin = unit(c(.5,.5,.5,.5),"lines")
       ) +
  labs(x="",y= "Mean Total\nDistance Moved (km)") + 
  guides(colour="none")
g4

# Distance linear

g5 <- ggplot(data_means, aes(x=behaviour, y=meanld, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.3) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme + 
  theme(legend.position = c(0.5,-0.1),legend.title=element_text(size=12),
        legend.direction = "horizontal",
        legend.text = element_text(size = 7)) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        #plot.margin = unit(c(0,.5,0,.5),"lines"
        ) +
  labs(x="",y = "Mean Euclidean\nDistance Moved (km)",
       colour="Plasticity Level")
g5


g6 <- ggplot(data_means, aes(x=behaviour, y=meanrd, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.3) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme + 
  theme(legend.position = c(0,-0.2),legend.title=element_text(size=10),
        legend.direction = "horizontal",
        legend.text = element_text(size = 7)) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        #plot.margin = unit(c(0,.5,0,.5),"lines"
  ) +
  labs(x="",y = "Mean Ratio Distance\n(Total / Euclidean Distance Moved)",
       colour="Plasticity Level") +
  guides(colour="none")
g6

#-------------------------------------------------------------------------------
# EXPORT FIGURE BOXPLOT
#-------------------------------------------------------------------------------
#setwd("Documents/phd_project/Chapter_2/output/final_simulations/")
library("gridExtra")
png(file="data.png",width=2000, height=1300,res = 150)
plot_grid(g1,                                   # bar plot spaning two columns
             g2, g3,                               # box plot and scatter plot
             g4, g5, g6,
             ncol = 3, nrow = 3, rel_heights=c(1,1,0.1), rel_widths=c(1,1,1),
             layout_matrix = rbind(c(1,2,3), c(4,5,6)),
             labels = c("(A)","(B)","(C)","(D)","(E)","(F)"))
dev.off()
#-------------------------------------------------------------------------------
# PLOTS DATA SETTLEMENT, HAB QUALITY, ENERGETICS LINES GLM
#-------------------------------------------------------------------------------
# Settlement rate

data$specie= factor(data$specie, levels = c("DA","PQ","MP"))
specie_names <- c('DA' = "D.aurita",'PQ' = "P. quica",'MP' = "M. paraguayana")

data$prop_dist <- data$total_dist / data$linear_dist

data_means <- data %>% 
  group_by(run_number,behaviour,hab_amount,clumpiness,initial_individuals,specie) %>% 
  summarise(n = length(turtle),
            meanQ = mean(Hab_quality_area),
            meanen = mean(final_En),
            meandt = mean(total_dist) / 1000,
            meanld = mean(linear_dist) / 1000,
            meanrd = mean(prop_dist)) %>%
  mutate (rate = n / (initial_individuals /length(unique(data$behaviour)))) 

theme <- theme(panel.border = element_blank(),
               strip.text.x = element_text(size = 10, face = ("italic")),panel.spacing = unit(0, "lines"),
               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
               panel.background = element_blank())


g1 <- ggplot(data_means, aes(x=plasticity, y=rate, colour=specie,fill=specie)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),
              se=F, aes(group=specie),linewidth = 1)  + 
  theme + ylim(0,1) +
  scale_colour_viridis_d(option = "D") +
  theme(legend.position = "none",
        legend.margin=margin(0,0,0,20),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        #plot.margin = unit(c(.5,9,0,9),"lines")
  ) +
  labs(x="Plasticity Level",y = "Settlement\nRate",
       colour="Species")
g1

# Habitat quality in settlement

g2 <- ggplot(data_means, aes(x=behaviour, y=meanQ, colour=specie,fill=specie)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),
              se=F, aes(group=specie),linewidth = 1)  + 
  theme + ylim(0,1) +
  scale_colour_viridis_d(option = "D") +
  theme(legend.position = "none",
        legend.margin=margin(0,0,0,20),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        #plot.margin = unit(c(.5,9,0,9),"lines")
  ) +
  labs(x="Plasticity Level", y= "Mean Habitat Quality\nof Settlement Area") 
g2

# Energetic Condition on settlement

g3 <- ggplot(data_means, aes(x=behaviour, y=meanen, colour=specie,fill=specie)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),
              se=F, aes(group=specie),linewidth = 1)  + 
  theme + ylim(0,1) +
  scale_colour_viridis_d(option = "D") +
  theme(legend.position = "none",
        legend.margin=margin(0,0,0,20),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        #plot.margin = unit(c(.5,9,0,9),"lines")
  ) +
  labs(x = "Plasticity Level",y="Mean Energetic \nCondition on Settlement",
       colour="Species") 
g3

#-------------------------------------------------------------------------------
# PLOTS DISTANCE DATA LINES GLM
#-------------------------------------------------------------------------------
# Distance total

g4 <- ggplot(data_means, aes(x=behaviour, y=meandt, colour=specie,fill=specie)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),
              se=F, aes(group=specie),linewidth = 1)  + 
  theme +
  scale_colour_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme(legend.position = "none",
        legend.margin=margin(0,0,0,20),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        #plot.margin = unit(c(.5,9,0,9),"lines")
  ) +
  labs(x="Plasticity Level",y= "Mean Total\nDistance Moved (km)") 
g4

# Distance linear

g5 <- ggplot(data_means, aes(x=behaviour, y=meanld, colour=specie,fill=specie)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),
              se=F, aes(group=specie),linewidth = 1)  + 
  theme + 
  scale_colour_viridis_d(option = "D",labels=specie_names) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme(legend.position = c(0.5,-0.2), legend.direction = "horizontal",
        legend.margin=margin(0,0,0,20), legend.title = element_text(face="bold",size = 13),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 12,face="italic")
        #plot.margin = unit(c(.5,9,0,9),"lines")
  ) +
  labs(x="Plasticity Level",y = "Mean Euclidean\nDistance Moved (km)",
       colour="Specie") + 
  guides(fill="none")
g5


g6 <- ggplot(data_means, aes(x=behaviour, y=meanrd, colour=specie,fill=specie)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian"),
              se=F, aes(group=specie),linewidth = 1)  + 
  #facet_grid(~decision, labeller = labeller(decision=c("En"="Energetic Condition","H"="Habitat Quality"))) +
  theme +
  scale_colour_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme(title = element_text(size = 10),
        legend.position = "none",
        legend.margin=margin(0,0,0,20),
        legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        #plot.margin = unit(c(.5,9,0,9),"lines")
  ) +
  labs(#title="Decision to settle by:",
       x="Plasticity Level",y = "Mean Ratio Distance\n(Total / Euclidean Distance Moved)") +
  guides(colour="none")
g6

#-------------------------------------------------------------------------------
# EXPORT FIGURE LINES GLM
#-------------------------------------------------------------------------------
#setwd("Documents/phd_project/Chapter_2/output/final_simulations/")
library("gridExtra")
png(file="dataline.png",width=2000, height=1300,res = 150)
plot_grid(g1,                                   # bar plot spaning two columns
          g2, g3,                               # box plot and scatter plot
          g4, g5, g6,
          ncol = 3, nrow = 3, 
          rel_heights=c(1,1,0.1), rel_widths=c(1,1,1),
          layout_matrix = rbind(c(1,2,3), c(4,5,6)),
          labels = c("(A)","(B)","(C)","(D)","(E)","(F)")
)

dev.off()
#-------------------------------------------------------------------------------
# Landscape Plots 
#------------------------------------------------------------------------------
# organize data
lab_beh <- c("C = 0.0","C = 0.5","C = 1.0","C = 1.5","C = 2.0")
names(lab_beh) <- c("0","0.5","1","1.5","2")

data$behaviour <- factor(data$behaviour)

data_land <- data %>% 
  group_by(run_number,behaviour,hab_amount,clumpiness,specie) %>% 
  summarise(meanq = mean(Hab_quality_area),
            meanen = mean(final_En),
            meanld = mean(linear_dist),
            meantd = mean(total_dist),
            meanrd = mean(prop_dist),
            srate = length(turtle) / (initial_individuals /length(unique(data$behaviour)))) %>%
  mutate(cat_land=cut(hab_amount, breaks=c(0,20,40,60,80), 
                      labels=c("0-20","20-40","40-60","60-80"),include.lowest = TRUE))

data_land$meanld <- data_land$meanld / 1000
data_land$meantd <- data_land$meantd / 1000

# define theme
theme1 <- theme(
  panel.grid.major = element_line(colour = "gray80",size=0.2),
  panel.grid.minor = element_blank(),panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=0.3),
  strip.background = element_blank(),
  axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8),
  axis.title = element_text(size = 10),
  strip.text.x = element_text(size = 10, face = "bold"),
  strip.text.y= element_text(size = 10,face = "italic"),
  plot.title = element_text(hjust = 0.5,size = 10),
  legend.key = element_rect(fill = NA, colour = NA))

#-------------------------------------------------------------------------------
# Landscape Settlement 
#------------------------------------------------------------------------------

#data_land<- data_land[which(data_land$behaviour == 0 |  data_land$behaviour == 1 | data_land$behaviour == 2),]
gg <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_smooth(aes(y = srate), method = "loess",se = F, linewidth = 0.7) +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Settlement Rate", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg

png(file="landscape__set.png",width=1500, height=1000,res = 150)
gg
dev.off()

#SETTLEMENT GAM

gam_set <-gam(srate~s(clumpiness)+s(hab_amount)+behaviour+specie,data=data_land, family=gaussian, select=T) # Fits the gam 

gam1 <- plot_smooths(
  model = gam_set,
  series = clumpiness,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Clumpiness (%)", y="Settlement rate",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

gam2 <- plot_smooths(
  model = gam_set,
  series = hab_amount,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "bottom",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Habitat Amount (%)", y="Settlement rate",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

png(file="landscape__set_gam.png",width=1200, height=800,res = 150)
plot_grid(gam1,gam2,
          ncol=1,
          rel_heights = c(1,1.2),
          labels = c("(A)","(B)"))
dev.off()

#-------------------------------------------------------------------------------
# Landscape Habitat Quality 
#------------------------------------------------------------------------------
gg1 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_smooth(aes(y = meanq), method = "loess",se = F, size = 0.7) +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Habitat Quality\nin Settlement", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg1

png(file="landscape__habq.png",width=1500, height=1000,res = 150)
gg1
dev.off()

gam_q <-gam(Hab_quality_area~s(clumpiness)+s(hab_amount)+behaviour+specie,data=data, family=gaussian, select=T) # Fits the gam 

gam1.1 <- plot_smooths(
  model = gam_q,
  series = clumpiness,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Clumpiness (%)", y="Habitat Quality in Settlement",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

gam1.2 <- plot_smooths(
  model = gam_q,
  series = hab_amount,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "bottom",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Habitat Amount (%)", y="Habitat Quality in Settlement",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

png(file="landscape__habq_gam.png",width=1200, height=800,res = 150)
plot_grid(gam1.1,gam1.2,
          ncol=1,
          rel_heights = c(1,1.2),
          labels = c("(A)","(B)"))
dev.off()

#-------------------------------------------------------------------------------
# Landscape Energetic Condition
#------------------------------------------------------------------------------

gg2 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_smooth(aes(y = meanen), method = "loess",se =F, size = 0.7) +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Energy Condition\nin Settlement", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg2

png(file="landscape__en.png",width=1500, height=1000,res = 150)
gg2
dev.off()

gam_en <-gam(final_En~s(clumpiness)+s(hab_amount)+behaviour+specie,data=data, family=gaussian, select=T) # Fits the gam 

gam2.1 <- plot_smooths(
  model = gam_en,
  series = clumpiness,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Clumpiness (%)", y="Energetic Condition in Settlement",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

gam2.2 <- plot_smooths(
  model = gam_en,
  series = hab_amount,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "bottom",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Habitat Amount (%)", y="Energetic Condition in Settlement",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

png(file="landscape__en_gam.png",width=1200, height=800,res = 150)
plot_grid(gam2.1,gam2.2,
          ncol=1,
          rel_heights = c(1,1.2),
          labels = c("(A)","(B)"))
dev.off()

#-------------------------------------------------------------------------------
# Landscape Total Distance
#------------------------------------------------------------------------------

gg3 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_smooth(aes(y = meantd), method = "loess",se=F, size = 0.7) +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names),
             scales="free_y") +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x="Clumpiness",y= "Total Distance\nMoved", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg3


png(file="landscape__totaldist.png",width=1500, height=1000,res = 150)
gg3
dev.off()

gam_dt <-gam(total_dist~s(clumpiness)+s(hab_amount)+behaviour+specie,data=data, family=gaussian, select=T) # Fits the gam 

gam3.1 <- plot_smooths(
  model = gam_dt,
  series = clumpiness,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "right",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Clumpiness (%)", y="Distance Total Moved",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

gam3.2 <- plot_smooths(
  model = gam_dt,
  series = hab_amount,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "right",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Habitat Amount (%)", y="Distance Total Moved",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

png(file="landscape__totaldist_gam.png",width=1200, height=800,res = 150)
plot_grid(gam3.1,gam3.2,
          ncol=1,
          rel_heights = c(1,1.2),
          labels = c("(A)","(B)"))
dev.off()

#-------------------------------------------------------------------------------
# Landscape Euclidean  Distance
#------------------------------------------------------------------------------

gg4 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_smooth(aes(y = meanld), method = "loess",se=F, size = 0.7) +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names),
             scales="free_y") +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x="Clumpiness",y= "Euclidian Distance\nMoved", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg4

png(file="landscape__lineardist.png",width=1500, height=1000,res = 150)
gg4
dev.off()

gam_ld <-gam(linear_dist~s(clumpiness)+s(hab_amount)+behaviour+specie,data=data, family=gaussian, select=T) # Fits the gam 

gam4.1 <- plot_smooths(
  model = gam_ld,
  series = clumpiness,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Clumpiness (%)", y="Euclidean Distance Moved",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

gam4.2 <- plot_smooths(
  model = gam_ld,
  series = hab_amount,
  comparison = behaviour,
  facet_terms = specie
) + 
  facet_grid(~specie, labeller = as_labeller(specie_names)) +
  theme1 +
  theme(legend.position = "bottom",
        strip.text.x = element_text(face = "italic"),
        legend.key = element_rect(fill = NA, colour = NA)) +
  labs(x="Habitat Amount (%)", y="Euclidean Distance Moved",color = "Plasticity Level",fill = "",linetype = "Plasticity Level") +
  guides(fill="none")

png(file="landscape__lineardist_gam.png",width=1200, height=800,res = 150)
plot_grid(gam4.1,gam4.2,
          ncol=1,
          rel_heights = c(1,1.2),
          labels = c("(A)","(B)"))
dev.off()


#-------------------------------------------------------------------------------
# SUMMARY DATA CSV
#-------------------------------------------------------------------------------
set_table_sum <- set_table %>%
  group_by(behaviour,specie) %>%
  summarise(m_set = mean(rate),
            std_set = sd(rate))

data_sum <- data %>% 
  group_by(behaviour,specie) %>%
  summarise(meanQ = mean(Hab_quality_area),
            stQ = sd(Hab_quality_area),
            meanEn = mean(final_En),
            sdEn = sd(final_En),
            meandt = mean(total_dist),
            sddt = sd(total_dist),
            meandl = mean(linear_dist),
            sddl = sd(linear_dist))

data_sum <- cbind(data_sum,set_table_sum[,c(3,4)])

write.csv(x = data_sum,file = "~/Documents/phd_project/Chapter_2/output/final_simulations/summary_data.csv")

#-------------------------------------------------------------------------------
# Landscape - density plots
#------------------------------------------------------------------------------
# Habitat amount
# convert hab amount to categories
data <- data %>%
  mutate(cat_land=cut(hab_amount, breaks=c(0,10,20,30,40,50,60,70), 
                      labels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70"),include.lowest = TRUE)) %>%
  mutate(cat_frag=cut(clumpiness, breaks=c(0,20,40,60,80,100), 
                      labels=c("0-20","20-40","40-60","60-80","80-100"),include.lowest = TRUE))
data$specie= factor(data$specie, levels = c("DA","PQ","MP"))
data$behaviour <- factor(data$behaviour)

library(ggridges)
library(viridis)


data_density <- data[which(data$behaviour == 0 | data$behaviour == 1 | data$behaviour == 2),]

d1 <- ggplot(data_density, aes(x = Hab_quality_area, y = cat_land,colour=behaviour,fill=behaviour)) +
  geom_density_ridges(alpha=0.3,size = 0.8,scale=1) +
  facet_grid(~specie,labeller = as_labeller(specie_names)) +
  labs(colour="Plasticity\nLevels",x="",y="Habitat Amount") +
  scale_fill_viridis_d(option = "D") +
  scale_colour_viridis_d(option = "D")+
  theme_classic() + 
  theme(panel.spacing = unit(0.5, "lines"),
        panel.grid.major = element_line(colour = "gray80",size=0.2),
        strip.background = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 10, face = ("italic")),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  guides(fill="none",colour="none")
d1
d2 <- ggplot(data_density, aes(x = Hab_quality_area, y = cat_frag,colour=behaviour,fill=behaviour)) +
  geom_density_ridges(alpha=0.3,size = 0.8,scale=1) +
  labs(colour="Plasticity\nLevels",x="Habitat Quality in Settlement",y="Clumpiness") +
  scale_fill_viridis_d(option = "D") +
  scale_colour_viridis_d(option = "D")+
  facet_grid(~specie,labeller = as_labeller(specie_names)) +
  theme_classic() + 
  theme(panel.spacing = unit(0.5, "lines"),
        panel.grid.major = element_line(colour = "gray80",size=0.2),
        strip.background = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom") +
  guides(fill = "none",color=guide_legend(override.aes=list(fill=NA)))
d2
#-------------------------------------------------------------------------------
# EXPORT FIGURE
#------------------------------------------------------------------------------
png(file="density_plots.png",width=1000, height=1200,res = 130)
plot_grid(d1,d2,ncol =1)
dev.off()


#------------------------------------------------------------------------------
# LANDSCAPE SIMPLE FIGURE
#------------------------------------------------------------------------------
png(file="land_set.png",width=1000, height=800,res = 130)
ggplot(data_land,aes(x=clumpiness,group=behaviour,colour=behaviour)) +
  geom_smooth(aes(y = srate), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(~cat_land) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Settlement Rate", title="Habitat Amount (%)",
       colour = "Plasticity Level")
dev.off()

png(file="land_q.png",width=1000, height=800,res = 130)
ggplot(data_land,aes(x=clumpiness,group=behaviour,colour=behaviour)) +
  geom_smooth(aes(y = meanq), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(~cat_land) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Habitat Quality\nin Settlement", title="Habitat Amount (%)",
       colour = "Plasticity Level")
dev.off()

png(file="land_en.png",width=1000, height=800,res = 130)
ggplot(data_land,aes(x=clumpiness,group=behaviour,colour=behaviour)) +
  geom_smooth(aes(y = meanen), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Energetic Condition\nin Settlement", title="Habitat Amount (%)",
       colour = "Plasticity Level")
dev.off()


png(file="land_td.png",width=1000, height=800,res = 130)
ggplot(data_land,aes(x=clumpiness,group=behaviour,colour=behaviour)) +
  geom_smooth(aes(y = meantd), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(~cat_land) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Mean Total\nDistance Moved)", title="Habitat Amount (%)",
       colour = "Plasticity Level")
dev.off()


png(file="land_ld.png",width=1000, height=800,res = 130)
ggplot(data_land,aes(x=clumpiness,group=behaviour,colour=behaviour)) +
  geom_smooth(aes(y = meanld), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(~cat_land) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Mean Euclidean\nDistance Moved)", title="Habitat Amount (%)",
       colour = "Plasticity Level")
dev.off()

png(file="land_rd.png",width=1000, height=800,res = 130)
ggplot(data_land,aes(x=clumpiness,group=behaviour,colour=behaviour)) +
  geom_smooth(aes(y = meanrd), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(~cat_land) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Mean Ratio Distance\n(Total / Euclidean Distance Moved)", title="Habitat Amount (%)",
       colour = "Plasticity Level")
dev.off()
#------------------------------------------------------------------------------
# LANDSCAPE SUPPLEMENTARY FIGURES
#------------------------------------------------------------------------------
png(file="land_set_SP.png",width=1200, height=1000,res = 130)
gg <- ggplot(data_land,aes(x=clumpiness,group=plasticity,colour=plasticity)) +
  geom_smooth(aes(y = srate), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land,labeller = labeller(specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Settlement Rate", title="Habitat Amount (%)",
       colour = "Plasticity Level")
gg
dev.off()

png(file="land_hab_SP.png",width=1200, height=1000,res = 130)
gg1 <- ggplot(data_land,aes(x=clumpiness,group=plasticity,colour=plasticity)) +
  geom_smooth(aes(y = meanq), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land,labeller = labeller(specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Habitat Quality\nin Settlement", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg1
dev.off()

png(file="land_en_SP.png",width=1200, height=1000,res = 130)
gg2 <- ggplot(data_land,aes(x=clumpiness,group=plasticity,colour=plasticity)) +
  geom_smooth(aes(y = meanen), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land,labeller = labeller(specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x="Clumpiness",y= "Energy Condition\nin Settlement", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg2
dev.off()

png(file="land_td_SP.png",width=1200, height=1000,res = 130)
gg3 <- ggplot(data_land,aes(x=clumpiness,group=plasticity,colour=plasticity)) +
  geom_smooth(aes(y = meantd), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land,labeller = labeller(specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x="Clumpiness",y= "Total Distance\nMoved", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg3
dev.off()

png(file="land_ld_SP.png",width=1200, height=1000,res = 130)
gg4 <-ggplot(data_land,aes(x=clumpiness,group=plasticity,colour=plasticity)) +
  geom_smooth(aes(y = meanld), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land,labeller = labeller(specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x="Clumpiness",y= "Euclidian Distance\nMoved", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg4
dev.off()

png(file="land_rd_SP.png",width=1200, height=1000,res = 130)
gg4 <-ggplot(data_land,aes(x=clumpiness,group=plasticity,colour=plasticity)) +
  geom_smooth(aes(y = meanrd), method = "gam",
              formula = y ~ s(x, bs = "cs",k=3),
              se = F, linewidth = 0.7) +
  facet_grid(specie~cat_land,labeller = labeller(specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  scale_x_reverse() +
  xlim(100,0) +
  theme1 +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x="Clumpiness",y= "Mean Ratio Distance\n(Total / Euclidean Distance Moved)", title="Level of Plasticity on Habitat Selection",
       colour = "Habitat amount")
gg4
dev.off()
