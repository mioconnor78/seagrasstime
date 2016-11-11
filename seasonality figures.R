### plotting seasonal variation in seagrass events

library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(cowplot)
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
  scale_x_continuous(breaks = OB$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE)

figTL <- function(x) ggplot(x, aes(x = month, y = relative.value), group_by(trophic.grp)) +
  theme_bw() +
  removeGrid() +
  ylab("Relative Value") +
  xlab("Month") +
  scale_x_continuous(breaks = OB$month) +
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
  scale_color_manual(values = c("brown","lightgreen","blue","orange", "forest green", "gray"), name = "trophic group") +
  ggtitle("Goodwin Islands")

GWI.fig

ggsave("./figures/GWI.jpg", GWI.fig)
ggsave("GWItrophic.jpg", GWI.fig, scale = .8, width = 8, height = 6)

#Tsawwassen####
TW <- data %>%
  filter(., site == "Tsawwassen") 

TW.fig <- figTL(TW) +
  scale_color_manual(values = c("lightgreen","brown", "orange", "forest green","darkgreen"), name = "trophic group") +
  ggtitle("Tsawsassen BC 2012")

TW.fig
ggsave("./figures/TWFig.jpg", TW.fig, width = 6, height = 3)


TW2015 <- read_csv("./TWdata_zm.csv")
TW2015 <- as.data.frame(TW2015)
TW2015$date1 <- dmy(TW2015$date)
TW2015a <- TW2015 %>%
  dplyr::select(date1, zm_dry_wt) %>% 
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

    

#Willapa Bay ####
WP <- data %>%
  filter(., site == "Willapa Bay") 

WP.fig <- figTL(WP) +
  scale_color_manual(values = c("darkgoldenrod4", "lightgreen","brown", "orange", "forest green"), name = "trophic group") +
  ggtitle("Willapa Bay 2011")

WP.fig
ggsave("./figures/WPFig.jpg", WP.fig)

#San Franscisco Bay####
SF <- data %>%
  filter(., site == "San Francisco Bay - KB") #%>% View

SF.fig <- figTL(SF) +
  scale_color_manual(values = c("lightgreen","brown","forest green"), name = "trophic group") +
  ggtitle("San Francisco - KB 2007")

SF.fig
ggsave("./figures/SFFig.jpg", SF.fig)

#San Jan Islands####
SJI <- data %>%
  filter(., site == "San Juan Islands") 
View(SJI)

SJI.fig <- figTL(SJI) +
  scale_color_manual(values = c("lightgreen","forest green", "seagreen", "brown"), name = "trophic group") +
  ggtitle("San Juan Islands")

SJI.fig
ggsave("./figures/SJI.jpg", SJI.fig)

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
  filter(., site == "Crescent Beach BC") 
View(CB)

CB.fig <- figTL(CB) +
  scale_color_manual(values = c("blue", "lightgreen", "dark gray", "brown"), name = "trophic group") +
  ggtitle("Crescent Beach")

CB.fig
ggsave("./figures/CB.jpg", CB.fig, width = 6, height = 3)

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
  scale_color_manual(values = c("brown", "lightgreen", "orange", "gray", "black", "forestgreen"), name = "trophic group") +
  ggtitle("Bodega Bay GA")

BB.GA.fig
ggsave("./figures/BBGA.jpg", BB.GA.fig)

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
  filter(., site == "Otsuchi Bay, Japan")  %>%
  filter(., variable!= "gastropods")

OB.fig <- fig(OB) +
  scale_color_manual(values = c("brown", "lightgreen", "forestgreen"), name = "trophic group") +
  #scale_color_brewer(type = "qual", palette = "Set1") +
  ggtitle("Otsuchi Bay 1996")

OB.fig
ggsave("./figures/OB.jpg", OB.fig)

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
