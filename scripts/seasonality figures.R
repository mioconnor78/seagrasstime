### plotting seasonal variation in seagrass events

library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
install.packages("ggExtra")
library(ggExtra)
library(cowplot)
install.packages("data.table")
library(data.table)
library(lubridate)

## add in missing months; only had to do this once for Best and Stachowicz data, and then replaced the main datafile.
data <- read_csv("./older data/sg.seasonality.data.csv")
vals <- data %>%
  unite(ID, variable, site, sep = "/")

vals1 <- as.data.table(vals)
setkey(vals1, ID, month)
vals2 <- as.data.frame(vals1[CJ(unique(ID), seq(min(month), max(month)))])
data1 <- vals2 %>%
  separate(ID, c("variable", "site"), sep = "/")
write.csv(data1, "sg.seasonality.csv")


## load data
data <- read_csv("sg.seasonality.csv")
data$month <- as.numeric(data$month)
head(data)
View(data)

#base figures ####
fig <- function(x) ggplot(x, aes(x = month, y = relative.value), group_by(variable)) +
  theme_bw() +
  removeGrid() +
  ylab("Relative Value") +
  xlab("Month") +
  scale_x_continuous(breaks = TW$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE)

figTL <- function(x) ggplot(x, aes(x = month, y = relative.value), group_by(trophic.grp)) +
  theme_bw() +
  removeGrid() +
  ylab("Relative Value") +
  xlab("Month") +
  scale_x_continuous(breaks = TW$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE)


sgb <- data %>%
  filter(., variable == "seagrass shoot biomass") %>%
  group_by(site)

sbg.fig <-
  ggplot(sgb, aes(x = month, y = relative.value)) +
  geom_jitter(aes(x = month, y = relative.value, color = site), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = site), se = FALSE) +
  ggtitle("seagrass biomass")

sbg.fig

class(data$site)
levels(unique(as.factor(data$site)))

#GWI####
GWI <- data %>%
  filter(., site == "Goodwin Islands") 
View(GWI)

GWI.fig <- figTL(GWI) +
  theme(legend.position = c(.11, .78), legend.text=element_text(size=6)) +
  scale_color_manual(values = c("brown","springgreen2","blue","orange", "forest green", "gray"), name = "") +
  ggtitle("Goodwin Islands 2004-2009")

GWI.fig

ggsave("./figures/GWI.jpg", GWI.fig, width = 7, height = 2.5)
ggsave("GWItrophic.jpg", GWI.fig, width = 7, height = 2.5)

#Tsawwassen####
TW <- data %>%
  filter(., site == "Tsawwassen" & year.start == "2012") %>%
  filter(., variable != "shoot density")

View(TW)

TW.fig <- figTL(TW) +
  theme(legend.position = c(.9, .9)) +
  scale_color_manual(values = c("springgreen2","brown", "orange", "forest green"), name = "") +
  ggtitle("Tsawsassen BC 2012")

TW.fig
ggsave("./figures/TWFig.jpg", TW.fig, width = 7, height = 2.5)


TW2015 <- read_csv("./TWdata_epi.csv")
TW2015 <- as.data.frame(TW2015)
TW2015$date1 <- dmy(TW2015$date)
TW2015a <- TW2015 %>%
  dplyr::select(date1, zm_dry_wt, epi_dry_wt) %>% 
  tidyr::separate(date1, c("year", "month", "date")) 

TW2015c <- (TW2015a)
#TW2015c$month <- as.factor(TW2015c$month)
#TW2015c$year <- as.factor(TW2015c$year)
#TW2015c$zm_dry_wt <- as.numeric(TW2015c$year)

View(TW2015c)

TW2015b <- TW2015c %>% 
  group_by(., month, year) %>% 
  summarise(mean(zm_dry_wt, na.rm = TRUE)) 

View(TW2015b)


TW15 <- data %>%
  filter(., site == "Tsawwassen" & year.start == "2015") 

TW15.fig <- figTL(TW15) +
  theme(legend.position = c(.9, .9)) +
  scale_color_manual(values = c("springgreen2","darkgreen"), name = "") +
  ggtitle("Tsawsassen BC 2016")

TW15.fig
ggsave("./figures/TW16Fig.jpg", TW15.fig, width = 7, height = 2.5)





#Willapa Bay ####
WP <- data %>%
  filter(., site == "Willapa Bay")

WP.fig <- figTL(WP) +
  theme(legend.position = c(.11, .9)) +
  scale_color_manual(values = c("darkgoldenrod4", "springgreen2", "brown", "orange", "forest green"), name = "") +
  ggtitle("Willapa Bay 2011")

WP.fig
ggsave("./figures/WPFig.jpg", WP.fig, width = 7, height = 2.5)

#San Franscisco Bay####
SF <- data %>%
  filter(., site == "San Francisco Bay - KB") #%>% View

SF.fig <- figTL(SF) +
  theme(legend.position = c(.15, .5)) +
  scale_color_manual(values = c("lightgreen","brown","forest green"), name = "trophic group") +
  ggtitle("San Francisco - KB 2007")

SF.fig
ggsave("./figures/SFFig.jpg", SF.fig, width = 7, height = 2.5)

#San Jan Islands####
SJI <- data %>%
  filter(., site == "San Juan Islands")
  #filter(., variable != "productivity")
#View(SJI)

SJI.fig <- figTL(SJI) +
  theme(legend.position = c(.16, .9)) +
  scale_color_manual(values = c("springgreen2","blue", "forest green",  "brown"), name = "") +
  ggtitle("San Juan Islands 1990")

SJI.fig
ggsave("./figures/SJI.jpg", SJI.fig, width = 7, height = 2.5)

#Padilla Bay Fig#### just shoots and snails
PB <- data %>%
  filter(., site == "Padilla Bay") 
View(PB)

PB.fig <- figTL(PB) +
  scale_color_manual(values = c("forest green", "brown"), name = "trophic group") +
  ggtitle("Padilla Bay")

PB.fig
ggsave("./figures/PB.jpg", PB.fig)

#Crecent Beach, BC ####
CB <- data %>%
  filter(., site == "Crescent Beach BC") %>%
  filter(., variable != "epiphyte biomass")

View(CB)

CB.fig <- figTL(CB) +
  theme(legend.position = c(.9, .9)) +
  scale_color_manual(values = c("blue", "black", "orange"), name = "") +
  ggtitle("Crescent Beach 2012")

CB.fig
ggsave("./figures/CB.jpg", CB.fig, width = 7, height = 2.5)

#Odawa Bay, Japan ####
OB <- data %>%
  filter(., site == "Odawa Bay, Japan") 
View(OB)

OB.fig <- figTL(OB) +
  scale_color_manual(values = c("brown", "lightgreen", "forestgreen"), name = "trophic group") +
  ggtitle("Odawa Bay")

OB.fig
ggsave("./figures/OB.jpg", OB.fig)

#Bodega Bay, CA ####
BB.GA <- data %>%
  filter(., site == "BodegaHarbor_bed_GA") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GA)

BB.GA.fig <- figTL(BB.GA) +
  theme(legend.position = c(.13, .8), legend.text=element_text(size=8)) +
  scale_color_manual(values = c("brown", "lightgreen", "orange", "gray", "black", "forestgreen"), name = "") +
  ggtitle("Bodega Harbor (GA) 2009")

BB.GA.fig
ggsave("./figures/BBGA.jpg", BB.GA.fig, width = 7, height = 2.5)

BB.GD <- data %>%
  filter(., site == "BodegaHarbor_bed_GD") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GA)

BB.GD.fig <- figTL(BB.GD) +
  scale_color_manual(values = c("brown", "lightgreen", "orange", "gray", "black", "forestgreen"), name = "trophic group") +
  ggtitle("Bodega Bay GD")

BB.GD.fig
ggsave("./figures/BBGD.jpg", BB.GD.fig)

BB.GG <- data %>%
  filter(., site == "BodegaHarbor_bed_GG") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GG)

BB.GG.fig <- figTL(BB.GG) +
  scale_color_manual(values = c("brown", "lightgreen", "orange", "gray", "black", "forestgreen"), name = "trophic group") +
  ggtitle("Bodega Bay GG")

BB.GG.fig
ggsave("./figures/BBGG.jpg", BB.GG.fig)

BB.GB <- data %>%
  filter(., site == "BodegaHarbor_bed_GB") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GB)

BB.GB.fig <- figTL(BB.GB) +
  scale_color_manual(values = c("brown", "lightgreen", "orange", "gray", "black", "forestgreen"), name = "trophic group") +
  ggtitle("Bodega Bay GB")

BB.GB.fig
ggsave("./figures/BBGB.jpg", BB.GB.fig)


BB.GF <- data %>%
  filter(., site == "BodegaHarbor_bed_GF") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GF)

BB.GF.fig <- figTL(BB.GF) +
  scale_color_manual(values = c("brown", "lightgreen", "orange", "gray", "black", "forestgreen"), name = "trophic group") +
  ggtitle("Bodega Bay GF")

BB.GF.fig
ggsave("./figures/BBGF.jpg", BB.GF.fig)

## all Bodega figs at once
theme_set(theme_cowplot(font_size = 10))
BB.all <- plot_grid(BB.GA.fig, BB.GB.fig, BB.GD.fig, BB.GG.fig, BB.GF.fig, nrow = 3, ncol = 2, scale = 0.9, label_size = 9, align = "v")

ggsave("BB.all.jpg", plot = BB.all, width = 20, height = 12)

BB.all

#Roscoff, France####  is there more data than just SG data?
RF <- data %>%
  filter(., site == "Roscoff, France") 
View(RF)

RF.fig <- fig(RF) +
  ggtitle("Roscoff France")

RF.fig

#Beaufort, NC 1976 ####
NC <- data %>%
  filter(., site == "Beaufort, NC") 
View(NC)

NC.fig <- fig(NC) +
  scale_color_manual(values = c("blue", "forestgreen"), name = "trophic group") +
  ggtitle("Beaufort, NC 1976")

NC.fig
ggsave("./figures/NC1976.jpg", NC.fig)


#Beaufort, NC 2010 ####
NC2 <- data %>%
  filter(., site == "Beaufort, NC.MM") 
View(NC2)

NC2.fig <- fig(NC2) +
  scale_color_manual(values = c("blue","brown","forestgreen"), name = "trophic group") +
  ggtitle("Beaufort, NC 2010")

NC2.fig
ggsave("./figures/NC2010.jpg", NC2.fig)


#Akkeshi-ko Estuary ####
AK <- data %>%
  filter(., site == "Akkeshi-ko Estuary, Japan") 
View(AK)

AK.fig <- fig(AK) +
  scale_color_manual(values = c("lightgreen", "forestgreen"), name = "trophic group") +
  #scale_color_brewer(type = "qual", palette = "Set1") +
  ggtitle("Akkeshi-ko")

AK.fig
ggsave("./figures/AK.jpg", AK.fig)

# Otsuchi Bay, Japan ####
OB <- data %>%
  filter(., site == "Otsuchi Bay, Japan")  #%>%
  #filter(., variable!= "gastropods")

OB.fig <- fig(OB) +
  theme(legend.position = c(.9, .95), legend.text=element_text(size=6)) +
  scale_color_manual(values = c("brown", "springgreen2","orange", "forestgreen"), name = "") +
  #scale_color_brewer(type = "qual", palette = "Set1") +
  ggtitle("Otsuchi Bay 1996")

OB.fig
ggsave("./figures/OB.jpg", OB.fig, width = 7, height = 2.5)

#all figs at once ####
all <- plot_grid(GWI.fig,TW.fig, SF.fig, CB.fig, OB.fig, nrow = 3, ncol = 2, scale = 0.9, label_size = 9, align = "v")

cowplot::ggsave("allsites.jpg", plot = all, width = 8, height = 10)


### ok, this will work to visualize the patterns. next step, try to get our (and others?) seasonal data into a figure...
library(synchrony)


View(OB)
OB.wide <- OB %>%
  select(month, variable, relative.value) %>%
  spread(key = variable, value = relative.value)

synchrony::community.sync(OB.wide[!is.na(OB.wide),-1])


com.sync <- function(x) {varC <- var(rowSums(x, na.rm = TRUE)) 
  return(varC/(sum(sqrt(diag(var(x, na.rm=TRUE))))^2))
         }
  
com.sync(OB.wide[,-1])
