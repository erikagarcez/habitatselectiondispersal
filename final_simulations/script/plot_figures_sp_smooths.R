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
  theme(strip.text = element_text(size=10), strip.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
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
#devtools::install_github("hrbrmstr/hrbrthemes")
#library(hrbrthemes)
library(viridis)
library(ggridges)
#-------------------------------------------------------------------------------
# PLOTS GENERAL DATA
#-------------------------------------------------------------------------------
# Mortality rate ~ settlement rate

##teste <- datanew[which(datanew$run_number == 97),]
#data <- sort(unique(data$landscape_number)) # run 97 duplicated

set_table <- data %>% 
  group_by(run_number,behaviour,hab_amount,clumpiness,initial_individuals,specie) %>% 
  summarise(n = length(turtle)) %>%
  mutate (rate = n / (initial_individuals /length(unique(data$behaviour))))
set_table$specie= factor(set_table$specie, levels = c("DA","PQ","MP"))
#length(unique(datanew$run_number))
#set_table <- set_table[which(set_table$hab_amount > 10),]

theme <- theme(panel.border = element_blank(),
               strip.text.x = element_text(size = 10, face = ("italic")),panel.spacing = unit(0, "lines"),
               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
specie_names <- c('DA' = "D.aurita",'PQ' = "P. quica",'MP' = "M. paraguayana")

g1 <- ggplot(set_table, aes(x=behaviour, y=rate, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  #geom_density_ridges() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.9) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  theme + ylim(0,1) +
  theme(strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        plot.margin = unit(c(.5,.5,0,.5),"lines")) +
  labs(x="",y = "Settlement\nRate") + 
  guides(colour="none")
g1



# Habitat quality in settlement
dataq <- data %>% 
  group_by(run_number,behaviour,clumpiness,hab_amount,specie) %>% 
  summarise(meanQ = mean(Hab_quality_area))

g2 <- ggplot(dataq, aes(x=behaviour, y=meanQ, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.5) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  theme + ylim(0,1) +
  theme(strip.background =element_blank(),
        strip.text.x = element_blank(),
        #panel.background = element_rect(colour = "white"),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        #axis.text.y = element_blank(),axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,.5,0,.5),"lines")) +
  labs(x="", y= "Mean Habitat Quality\nof Settlement Area") + 
  guides(colour="none")
g2

# Energetic Condition on settlement
dataen <- data %>% 
  group_by(run_number,behaviour,clumpiness,hab_amount,specie) %>% 
  summarise(meanen = mean(final_En))

g3 <- ggplot(dataen, aes(x=behaviour, y=meanen, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.5) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  theme + ylim(0,1) +
  theme(legend.position = "bottom",
        #legend.position = c(0.1,0.9),legend.title=element_text(size=8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 7)) +
  theme(strip.background =element_blank(),
        strip.text.x = element_blank(),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        #axis.text.y = element_blank(),axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,.5,0,.5),"lines")) +
  labs(x = "",y="Mean Energetic \nCondition on Settlement",
       colour="Plasticity Level") 
g3

#-------------------------------------------------------------------------------
# PLOTS GENERAL DISTANCE DATA
#-------------------------------------------------------------------------------
# Distance total
datadt <- data %>% 
  group_by(run_number,behaviour,clumpiness,hab_amount,specie) %>% 
  summarise(meandt = mean(total_dist) / 1000)
datadt$specie= factor(datadt$specie, levels = c("DA","PQ","MP"))

g4 <- ggplot(datadt, aes(x=behaviour, y=meandt, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.5) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme + 
  theme(strip.background = element_blank(),
       # strip.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        plot.margin = unit(c(.5,.5,.5,.5),"lines")) +
  labs(x="",y= "Mean Total\nDistance Moved (km)") + 
  guides(colour="none")
g4

# Distance linear
datadl <- data %>% 
  group_by(run_number,behaviour,clumpiness,hab_amount,specie) %>% 
  summarise(meandl = mean(linear_dist) / 1000)

g5 <- ggplot(datadl, aes(x=behaviour, y=meandl, group=behaviour,colour=factor(behaviour))) +
  geom_boxplot() +
  geom_jitter(shape=1,size = 1,position=position_jitter(0.2),alpha=0.5) +
  facet_grid(~specie, labeller = as_labeller(specie_names))+
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme + 
  theme(legend.position = "bottom",legend.title=element_text(size=10),
        legend.text = element_text(size = 7)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        plot.margin = unit(c(0,.5,0,.5),"lines"),
        legend.spacing = unit(0, "cm")) +
  labs(x="",y = "Mean Euclidian\nDistance Moved (km)",
       colour="Plasticity Level") 
g5

#-------------------------------------------------------------------------------
# EXPORT FIGURE
#-------------------------------------------------------------------------------
setwd("Documents/phd_project/Chapter_2/output/final_simulations/")
png(file="general_mean_all.png",width=1200, height=1500,res = 150)
plot_grid(g1,g2,g3,
          axis = "blt",
          rel_widths = c(0.6,0.6,0.6),
          scale = c(1,1,1),
          label_size = 12,ncol = 1)
dev.off()
png(file="general_mean_dist.png",width=1000, height=1000,res = 150)
plot_grid(g4,g5,
          axis = "blt",
          rel_widths = c(0.6,0.6),
          scale = c(1,1),
          label_size = 12,ncol = 1)
dev.off()

library("gridExtra")

grid.arrange(g1,                                   # bar plot spaning two columns
             g2, g3,                               # box plot and scatter plot
             g4, g5,
             ncol = 2, nrow = 3, 
             layout_matrix = rbind(c(1,1), c(2,3), c(4,5)))
  
#-------------------------------------------------------------------------------
# GLM ANOVA ANALYSIS
#-------------------------------------------------------------------------------
m_set <- glm(rate ~ behaviour + specie, data=set_table)
#check residuals
olsrr::ols_plot_resid_qq(m_set)
olsrr::ols_test_normality(m_set)
olsrr::ols_test_correlation(m_set)
olsrr::ols_plot_resid_fit(m_set)
olsrr::ols_plot_resid_hist(m_set)

summary(m_set)
anova(m_set)
#-------------------------------------------------------------------------------
# Landscape - density plots
#------------------------------------------------------------------------------
# Habitat amount
# convert hab amount to categories
data <- data %>%
  mutate(cat_land=cut(hab_amount, breaks=c(0,10,20,30,40,50,60,70), 
                      labels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70"),include.lowest = TRUE))
data$specie= factor(data$specie, levels = c("DA","PQ","MP"))

# plot
data$behaviour <- factor(data$behaviour)
d1 <- ggplot(data, aes(x = Hab_quality_area, y = cat_land,colour=behaviour,fill=behaviour)) +
  geom_density_ridges(alpha=0.3,size = 0.8) +
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

# Clumpiness
# convert clumpiness to categories
data <- data %>%
  mutate(cat_frag=cut(clumpiness, breaks=c(0,20,40,60,80,100), 
                      labels=c("0-20","20-40","40-60","60-80","80-100"),include.lowest = TRUE))
# plot
d2 <- ggplot(data, aes(x = Hab_quality_area, y = cat_frag,colour=behaviour,fill=behaviour)) +
  geom_density_ridges(alpha=0.3,size = 0.8) +
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
png(file="density_plots.png",width=1000, height=800,res = 130)
plot_grid(d1,d2,ncol =1)
dev.off()

#-------------------------------------------------------------------------------
# Landscape - points 
#------------------------------------------------------------------------------
#N_beh = 1000/5
data_land <- data %>% 
  group_by(run_number,behaviour,hab_amount,clumpiness,specie) %>% 
  summarise(meanq = mean(Hab_quality_area),
            meanen = mean(final_En),
            meanld = mean(linear_dist),
            meantd = mean(total_dist),
            srate = length(turtle) / (initial_individuals /length(unique(data$behaviour))))

data_land$meanld <- data_land$meanld / 1000
data_land$meantd <- data_land$meantd / 1000
data_land$specie= factor(data_land$specie, levels = c("DA","PQ","MP"))

data_land<- data_land %>%
  mutate(cat_land=cut(hab_amount, breaks=c(0,10,20,30,40,50,60,70), 
                      labels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70"),include.lowest = TRUE))


theme1 <- theme(panel.grid.major = element_line(colour = "gray80",size=0.2),
panel.grid.minor = element_blank(),panel.background = element_blank(),
panel.border = element_rect(colour = "black", fill=NA, size=0.3),
strip.background = element_blank(),
axis.text.x = element_blank(), axis.ticks.x = element_blank())

lab_beh <- c("C = 0.0","C = 0.5","C = 1.0","C = 1.5","C = 2.0")
names(lab_beh) <- c("0","0.5","1","1.5","2")
gg <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_point(aes(y=srate),shape =16,size=2,alpha=0.9) +
  geom_smooth(aes(y = srate), method = "lm") +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 1) +
  theme1 +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.y= element_text(face = "italic"),
        plot.margin = unit(c(1,1,0,1),"lines"),
        plot.title = element_text(hjust = 0.5,size = 12)) +
  scale_alpha(guide = 'none') +
  labs(x="",y= "Settlement\nRate", title="Level of Plasticity on Habitat Selection")
gg
 gg1 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_point(aes(y=meanq),shape =1,size=2,alpha=0.1) +
   geom_smooth(aes(y = meanq), method = "lm") +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "none",
        strip.text = element_text(face = "italic"),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,1,0,1),"lines")) +
  scale_alpha(guide = 'none') +
  labs(x="",y= "Habitat Quality\nin Settlement")
gg1

gg2 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_point(aes(y=meanen),shape =1,size=2,alpha=0.1) +
  geom_smooth(aes(y = meanen), method = "lm") +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "italic"),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,1,0,1),"lines")) +
  scale_alpha(guide = 'none') +
  labs(x="",y="Energy Condition\nin Settlement")
gg2

gg3 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_point(aes(y=meanld),shape =1,size=2,alpha=0.1) +
  geom_smooth(aes(y = meanld), method = "lm") +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "italic"),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,1,0,1),"lines")) +
  scale_alpha(guide = 'none') +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x="",y="Euclidian Distance\nMoved")
gg3

gg4 <- ggplot(data_land,aes(clumpiness,colour=cat_land)) +
  geom_point(aes(y=meantd),shape =1,size=2,alpha=0.1) +
  geom_smooth(aes(y = meantd), method = "lm") +
  facet_grid(specie~behaviour,labeller = labeller(behaviour=lab_beh,specie=specie_names)) +
  scale_colour_viridis_d(option = "D", alpha = 0.8) + 
  theme1 +
  theme(legend.position = "bottom",
        legend.margin=margin(t=-5),
        strip.text = element_text(face = "italic"),
        strip.text.x = element_blank(),
        axis.ticks.x = element_line(),
        axis.text.x = element_text(),
        plot.margin = unit(c(0,1,1,1),"lines")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_alpha(guide = 'none') +
  labs(x="Clumpiness",y="Total Distance\nMoved",colour="Habitat Amount")
gg4
#-------------------------------------------------------------------------------
# EXPORT FIGURE
#------------------------------------------------------------------------------
png(file="landscape__pointsplots_line.png",width=3500, height=5000,res = 250)
plot_grid(gg,gg1,gg2,
          ncol =1,
          rel_heights = c(1.1,1,1.4))
dev.off()
png(file="landscapedist__pointsplots_line.png",width=3500, height=5000,res = 250)
plot_grid(gg3,gg4,
          ncol =1,
          rel_heights = c(1.1,1.4))
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

write.csv(x = data_sum,file = "Documents/phd_project/Chapter_2/output/final_simulations/summary_data.csv")

#------------------------------------------------------------------------------
# GLM ANOVA ANALYSIS
#------------------------------------------------------------------------------
library(mgcv)
mod <-gam(rate~s(clumpiness)+s(hab_amount)+behaviour+specie,data=set_table, family=gaussian, select=T) # Fits the gam 
plot(mod)
summary(mod)
#anova(mod)
par(mfrow=c(2,2))  
gam.check(mod)



